module Decode = Decode.AsResult.OfParseError;

let fail = (a, j) => Belt.Result.Error(Decode.ParseError.Val(a, j));

let matchString = s =>
  Decode.flatMap(
    (v, j) =>
      if (v == s) {
        Decode.pure((), j);
      } else {
        fail(`ExpectedValidOption, j);
      },
    Decode.string,
  );

let operator = (name, make) =>
  Decode.Pipeline.(
    succeed(_ => make) |> field("operator", matchString(name))
  );

let outcomeDecoder = json =>
  Decode.Pipeline.(
    operator("outcome", v => Expression.Outcome(v))
    |> field("value", string)
    |> run(json)
  );

let calculatedCharacteristicDecoder = json =>
  Decode.Pipeline.(
    operator("calculated-characteristic", name =>
      Expression.CalculatedCharacteristic(name)
    )
    |> field("name", string)
    |> run(json)
  );

let longDecoder = json =>
  Decode.Pipeline.(
    operator("long", name => Expression.Long(name))
    |> field("value", string)
    |> run(json)
  );

let floatDecoder = json =>
  Decode.Pipeline.(
    operator("float", name => Expression.Float(name))
    |> field("value", string)
    |> run(json)
  );

let stringDecoder = json => {
  Decode.(map(v => Expression.String(v), string, json));
};

let characteristicDecoder = (name, json) =>
  Decode.Pipeline.(
    operator(name, (p, v) => Expression.Characteristic(p, v))
    |> field("data_source", string)
    |> field("characteristic", string)
    |> run(json)
  );

let stringCharacteristicDecoder =
  characteristicDecoder("string-characteristic");
let longCharacteristicDecoder = characteristicDecoder("long-characteristic");

let (!) = Lazy.force;

let rec ifElseDecoder = json =>
  Decode.Pipeline.(
    operator("if-else", (c, l, r) => Expression.IfElse(c, l, r))
    |> field("condition", !lazyExpressionDecoder)
    |> field("if_true", !lazyExpressionDecoder)
    |> field("if_false", !lazyExpressionDecoder)
    |> run(json)
  )
and valueDecoder = (name, make, json) =>
  Decode.Pipeline.(
    operator(name, make)
    |> field("value", !lazyExpressionDecoder)
    |> run(json)
  )
and leftRightDecoder = (name, make, json) =>
  Decode.Pipeline.(
    operator(name, make)
    |> field("left", !lazyExpressionDecoder)
    |> field("right", !lazyExpressionDecoder)
    |> run(json)
  )
and comparisonDecoder = (op, sym) =>
  leftRightDecoder(op, (l, r) => Expression.Comparison(sym, l, r))
and coalesceDecoder = json =>
  Decode.Pipeline.(
    operator("coalesce", (v, default) => Expression.Coalesce(v, default))
    |> field("value", !lazyExpressionDecoder)
    |> field("default", !lazyExpressionDecoder)
    |> run(json)
  )
and containsDecoder = json =>
  Decode.Pipeline.(
    operator("contain", (a, b) => Expression.Contains(a, b))
    |> field("item", !lazyExpressionDecoder)
    |> field("list", !lazyExpressionDecoder)
    |> run(json)
  )
and stripDecoder = json =>
  Decode.Pipeline.(
    operator("strip", a => Expression.Strip(a))
    |> field("expression", !lazyExpressionDecoder)
    |> run(json)
  )
and listDecoder = json =>
  Decode.Pipeline.(
    operator("list", l => Expression.List(l))
    |> field("items", list(!lazyExpressionDecoder))
    |> run(json)
  )
and lookupDecoder = json =>
  Decode.Pipeline.(
    operator("lookup", (k, m) => Expression.Lookup(k, m))
    |> field("key", !lazyExpressionDecoder)
    |> field("map", !lazyExpressionDecoder)
    |> run(json)
  )
and mapDecoder = json =>
  Decode.Pipeline.(
    operator("map", l => Expression.Map(l))
    |> field(
         "value",
         list(j =>
           succeed((a, b) => (a, b))
           |> field("key", !lazyExpressionDecoder)
           |> field("value", !lazyExpressionDecoder)
           |> run(j)
         ),
       )
    |> run(json)
  )
and rangeDecoder = json =>
  Decode.Pipeline.(
    operator("range", (from, to_) => Expression.Range(from, to_))
    |> optionalField("from", !lazyExpressionDecoder)
    |> optionalField("to", !lazyExpressionDecoder)
    |> run(json)
  )
and lazyExpressionDecoder =
  lazy (
    Decode.oneOf(
      outcomeDecoder,
      [
        stringDecoder,
        stringCharacteristicDecoder,
        longCharacteristicDecoder,
        ifElseDecoder,
        valueDecoder("str-to-long", v => Expression.ToLong(v)),
        valueDecoder("is-defined", v => Expression.IsDefined(v)),
        valueDecoder("not", v => Expression.Not(v)),
        valueDecoder("float-to-long", v => Expression.ToLong(v)),
        valueDecoder("long-to-float", v => Expression.ToFloat(v)),
        valueDecoder("str-to-date", v => Expression.ToDate(v)),
        calculatedCharacteristicDecoder,
        longDecoder,
        comparisonDecoder("less-than-or-equals", "<="),
        comparisonDecoder("greater-than-or-equals", ">="),
        comparisonDecoder("less-than", "<"),
        comparisonDecoder("greater-than", ">"),
        comparisonDecoder("equals", "=="),
        comparisonDecoder("not-equals", "!="),
        coalesceDecoder,
        leftRightDecoder("or", (l, r) => Expression.Or(l, r)),
        leftRightDecoder("and", (l, r) => Expression.And(l, r)),
        leftRightDecoder("plus", (l, r) => Expression.Plus(l, r)),
        leftRightDecoder("times", (l, r) => Expression.Times(l, r)),
        leftRightDecoder("minus", (l, r) => Expression.Minus(l, r)),
        leftRightDecoder("div", (l, r) => Expression.Divide(l, r)),
        leftRightDecoder("date-diff-years", (l, r) => Expression.DateDiffYears(l, r)),
        containsDecoder,
        stripDecoder,
        listDecoder,
        mapDecoder,
        floatDecoder,
        lookupDecoder,
        rangeDecoder,
      ],
    )
  );

let expressionDecoder = !lazyExpressionDecoder;