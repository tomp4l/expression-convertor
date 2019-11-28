type t =
  | Outcome(string)
  | CalculatedCharacteristic(string)
  | Long(string)
  | Float(string)
  | String(string)
  | IfElse(t, t, t)
  | Comparison(string, t, t)
  | Or(t, t)
  | And(t, t)
  | Not(t)
  | Plus(t, t)
  | Times(t, t)
  | Characteristic(string, string)
  | ToLong(t)
  | Coalesce(t, t)
  | IsDefined(t)
  | Contains(t, t)
  | Strip(t)
  | List(list(t))
  | Lookup(t, t)
  | Map(list((t, t)))
  | Range(option(t), option(t));

let rec show = (~indent=0, e) => {
  let spaces = Relude.String.repeat(indent, " ");

  switch (e) {
  | Outcome(outcome) => "Outcome(" ++ outcome ++ ")"
  | CalculatedCharacteristic(name) =>
    "CalculatedCharacteristic(" ++ name ++ ")"
  | Long(value) => value ++ "L"
  | Float(value) => value ++ "F"
  | String(value) => "\"" ++ value ++ "\""
  | IfElse(cond, ifT, ifF) =>
    "if( "
    ++ show(~indent, cond)
    ++ " ){\n"
    ++ "  "
    ++ spaces
    ++ show(~indent=indent + 2, ifT)
    ++ "\n"
    ++ spaces
    ++ "} else {\n"
    ++ "  "
    ++ spaces
    ++ show(~indent=indent + 2, ifF)
    ++ "\n"
    ++ spaces
    ++ "}"
  | Comparison(v, l, r) => show(l) ++ " " ++ v ++ " " ++ show(r)
  | Or(l, r) => "(" ++ show(l) ++ " || " ++ show(r) ++ ")"
  | And(l, r) => show(l) ++ " && " ++ show(r)
  | Characteristic(provider, key) => provider ++ "." ++ key
  | ToLong(v) => "(long)" ++ show(v)
  | Coalesce(a, b) => show(a) ++ " ?? " ++ show(b)
  | IsDefined(a) => "IsDefined(" ++ show(a) ++ ")"
  | Contains(a, b) => "Contains(" ++ show(a) ++ ", " ++ show(b) ++ ")"
  | Plus(a, b) => show(a) ++ " + " ++ show(b)
  | Times(a, b) => show(a) ++ " * " ++ show(b)
  | Strip(a) => "Strip(" ++ show(a) ++ ")"
  | List(l) =>
    "["
    ++ (l |> Relude.List.map(show) |> Relude.List.String.joinWith(", "))
    ++ "]"
  | Not(a) => "!" ++ show(a)
  | Lookup(key, map) => "Lookup(" ++ show(key) ++ ", " ++ show(map) ++ ")"
  | Map(map) =>
    "{\n"
    ++ (
      map
      |> Relude.List.map(((k, v)) =>
           spaces ++ "  " ++ show(k) ++ ": " ++ show(v)
         )
      |> Relude.List.String.joinWith(",\n")
    )
    ++ spaces
    ++ "\n}"
  | Range(from, to_) => 
    let mapOption = o => o |> Relude.Option.map(show) |> Relude.Option.getOrElse("");
    mapOption(from)  ++ ".." ++ mapOption(to_)
  };
};