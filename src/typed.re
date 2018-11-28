open TT;

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
  | Scheme(list(string), TT.t);

type envType = StringMap.t(TT.t);

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

let rec doesValueMatchType = (v: expression, t: TT.t) =>
  switch (v, t) {
  | (_, TDefined(_, t)) => doesValueMatchType(v, t)
  | (ETrue, TLit(LBool)) => true
  | (EFalse, TLit(LBool)) => true
  | (EString(_), TLit(LString)) => true
  | (EString(path), TIO) => Node.Fs.existsSync(path)
  | (EString(pass), TPassword) => String.length(pass) > 5
  | (EString(str), TConst(expected)) => str == expected
  | (EString(str), TRegexp(regexp)) =>
    if (Js.Re.(test(str, fromString(regexp)))) {
      true;
    } else {
      Js.log3({|value does not match type|}, str, regexp);
      false;
    }
  | (_, TSum(a, b)) => doesValueMatchType(v, a) || doesValueMatchType(v, b)
  | _ =>
    Js.log3({|unknown type matching|}, v, showType(t));
    false;
  };

let rec typeInference = (env: envType, e: expression) =>
  switch (e) {
  | EFalse => TLit(LBool)
  | ETrue => TLit(LBool)
  | EString(_) => TLit(LString)
  | EArray(list) => TList(collectTypes(env, list))
  | EObject(map) =>
    TRecord(
      StringMap.mapi(
        (k, v) => {
          let x = infereKeyType(env, k);
          switch (x) {
          | None =>
            Js.log2({|Cannot find type for key=|}, k);
            typeInference(env, e);
          | Some(t) =>
            if (!doesValueMatchType(v, t)) {
              Js.log3({|Type invalid: |}, v, showType(t));
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
      | _ => TSum(x, red(xs))
      }
    | [] => TVar
    };

  red(TypeSet.elements(types));
};