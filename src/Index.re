let parsed =
  StdinToJson.readInput->Future.map(json =>
    Belt.Option.map(json, ExpressionDecoder.expressionDecoder)
    ->Belt.Option.map(v =>
        Relude.Result.mapError(
          ExpressionDecoder.Decode.ParseError.failureToDebugString,
          v,
        )
        |> Relude.Result.map(Expression.show)
      )
    ->Belt.Option.getWithDefault(Belt.Result.Error("Invalid JSON"))
  );

parsed->Future.tapOk(Js.log)->Future.tapError(Js.Console.error);