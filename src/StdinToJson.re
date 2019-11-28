type stdin;

[@bs.val] [@bs.scope "process"] external stdin : stdin = "stdin";

[@bs.send] external resume : stdin => unit = "resume";

[@bs.send] external setEncoding : (stdin, string) => unit = "setEncoding";

[@bs.send]
external on :
  (
    stdin,
    [@bs.string] [
      | `data(string => unit)
      | [@bs.as "end"] `end_(unit => unit)
    ]
  ) =>
  unit =
  "on";

let readInput = {
  let fullInput = ref("");
  stdin -> resume;
  stdin -> setEncoding("utf-8");
  stdin -> on(`data(data => fullInput := fullInput^ ++ data));
  Future.make(resolve =>
    stdin
    -> on(
         `end_(
           () =>
             (
               try (Some(Js.Json.parseExn(fullInput^))) {
               | _ => None
               }
             )
             -> resolve,
         ),
       )
  );
};