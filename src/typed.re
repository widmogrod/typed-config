module StringMap = Map.Make(String);

type expression =
  | EObject(StringMap.t(expression))
  | EArray(list(expression))
  | EString(string)
  /* | ENumber */
  | ETrue
  | EFalse;
/* | ENull; */

module TypeSet = Set.Make(TT);

type scheme =
  | Scheme(list(string), TT.typ);

type envType = StringMap.t(TT.typ);

let infereKeyType = (env: envType, key: string) =>
  StringMap.fold(
    (k, v, agg) =>
      switch (agg) {
      | None =>
        if (Js.Re.(test(key, fromString(k)))) {
          Some(v);
        } else {
          None;
        }
      | Some(_) => agg
      },
    env,
    None,
  );

let rec doesValueMatchType = (v: expression, t: TT.typ) =>
  switch (v, t) {
  | (_, TDefined(_, t)) => doesValueMatchType(v, t)
  | (ETrue, TT.TLit(TT.LBool)) => true
  | (EFalse, TT.TLit(TT.LBool)) => true
  | (EString(_), TT.TLit(TT.LString)) => true
  | (EString(path), TT.TIO) => Node.Fs.existsSync(path)
  | (EString(pass), TT.TPassword) => String.length(pass) > 5
  | (EString(str), TT.TConst(expected)) => str == expected
  | (EString(str), TT.TRegexp(regexp)) =>
    if (Js.Re.(test(str, fromString(regexp)))) {
      true;
    } else {
      Js.log3({|value does not match type|}, str, regexp);
      false;
    }
  | (_, TSum(a, b)) => doesValueMatchType(v, a) || doesValueMatchType(v, b)
  | _ =>
    Js.log3({|unknown type matching|}, v, TT.showType(t));
    false;
  };

let rec typeInference = (env: envType, e: expression) =>
  switch (e) {
  | EFalse => TT.TLit(TT.LBool)
  | ETrue => TT.TLit(TT.LBool)
  | EString(_) => TT.TLit(TT.LString)
  | EArray(list) => TT.TList(collectTypes(env, list))
  | EObject(map) =>
    TT.TRecord(
      StringMap.mapi(
        (k, v) => {
          let x = infereKeyType(env, k);
          switch (x) {
          | None =>
            Js.log2({|Cannot find type for key=|}, k);
            typeInference(env, e);
          | Some(t) =>
            if (!doesValueMatchType(v, t)) {
              Js.log3({|Type invalid: |}, v, TT.showType(t));
            };
            t;
          };
        },
        map,
      ),
    )
  }
and collectTypes = (env: envType, ls: list(expression)) => {
  let types =
    List.fold_right(
      (a, agg) => TypeSet.(agg |> add(typeInference(env, a))),
      ls,
      TypeSet.empty,
    );
  let rec red = xs =>
    switch (xs) {
    | [x, ...xs] =>
      switch (xs) {
      | [] => x
      | _ => TT.TSum(x, red(xs))
      }
    | [] => TT.TVar
    };

  red(TypeSet.elements(types));
};