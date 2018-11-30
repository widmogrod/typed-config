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

/* type scheme = */
/* | Scheme(list(string), TT.t); */

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

type type_error =
  | TypeCheckNotImplemented(expression, TT.t)
  | RegexpDoesNotMatchValue(string, string);

exception Error(type_error);

let print_error = e =>
  switch (e) {
  | TypeCheckNotImplemented(_, t) =>
    Printf.printf("Unknow expression type. %s", showType(t))
  | RegexpDoesNotMatchValue(pattern, value) =>
    Printf.printf("Regexp=%s does not match value = %s", pattern, value)
  };

let rec doesValueMatchType = (v: expression, t: TT.t) =>
  switch (v, t) {
  | (_, TDefined(_, t)) => doesValueMatchType(v, t)
  | (ETrue, TLit(LBool)) => true
  | (EFalse, TLit(LBool)) => true
  | (EString(_), TLit(LString)) => true
  | (EString(path), TIO) =>
    /** TODO Make it to exception or Either type */ Node.Fs.existsSync(path)
  | (EString(pass), TPassword) =>
    /** TODO implement it properly */ String.length(pass) > 5
  | (EString(str), TConst(expected)) => str == expected
  | (EString(str), TRegexp(pattern)) =>
    if (Js.Re.(test(str, fromString(pattern)))) {
      true;
    } else {
      raise(Error(RegexpDoesNotMatchValue(pattern, str)));
    }
  | (_, TSum(a, b)) => doesValueMatchType(v, a) || doesValueMatchType(v, b)
  | _ => raise(Error(TypeCheckNotImplemented(v, t)))
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