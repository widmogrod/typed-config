module StringOrd = {
  type t = string;
  let compare = String.compare;
};

module StringMap = Map.Make(StringOrd);

type expression =
  | EObject(StringMap.t(expression))
  | EArray(list(expression))
  | EString(string)
  /* | ENumber */
  | ETrue
  | EFalse;
/* | ENull; */

type typ =
  /* | TBool */
  /* | TNumber */
  | TIO
  | TVar
  | TList(typ)
  /* | TDate */
  /* | TSet */
  | TPassword
  | TRecord(StringMap.t(typ))
  | TLit(lit)
  | TSum(typ, typ)
  | TRegexp(string)
  | TDefined(string, typ)
and lit =
  | LInt
  | LBool
  | LString(string); /* This should be more like atom/const */

module TypeSetOrd = {
  type t = typ;
  let rec compare = (a: typ, b: typ) =>
    switch (a, b) {
    | (TLit(LInt), TLit(LInt)) => 0
    | (TLit(LBool), TLit(LBool)) => 0
    | (TLit(LString(a)), TLit(LString(b))) => String.compare(a, b)
    | (TIO, TIO) => 0
    | (TVar, TVar) => 0
    | (TList(a), TList(b)) => compare(a, b)
    | (TSum(a1, a2), TSum(b1, b2)) =>
      let cmp = compare(a1, b1);
      if (cmp === 0) {
        compare(a2, b2);
      } else {
        cmp;
      };
    | (TRegexp(a), TRegexp(b)) => String.compare(a, b)
    | (TDefined(an, at), TDefined(bn, bt)) =>
      let cmp = String.compare(an, bn);
      if (cmp === 0) {
        compare(at, bt);
      } else {
        cmp;
      };
    | _ => (-1)
    };
};

module TypeSet = Set.Make(TypeSetOrd);

type scheme =
  | Scheme(list(string), typ);

type envType = StringMap.t(typ);

let rec showType = t =>
  switch (t) {
  | TIO => "io"
  | TPassword => "password"
  | TLit(l) =>
    switch (l) {
    | LInt => "int"
    | LBool => "bool"
    | LString(v) => {|"|} ++ v ++ {|"|} ++ {| of string|}
    }
  | TSum(a, b) => {|sum [|} ++ showType(a) ++ {|, |} ++ showType(b) ++ {|]|}
  | TDefined(definedType, o) => definedType ++ {| of |} ++ showType(o)
  | TRegexp(_) => "regexp"
  | TVar => "of var type!"
  | TList(t) => "[] of " ++ showType(t)
  | TRecord(map) =>
    Format.sprintf(
      "{%s}",
      StringMap.fold(
        (k, v, agg) => agg ++ k ++ {| = |} ++ showType(v),
        map,
        "",
      ),
    )
  };

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

let rec doesValueMatchType = (v: expression, t: typ) =>
  switch (v, t) {
  | (_, TDefined(_, t)) => doesValueMatchType(v, t)
  | (ETrue, TLit(LBool)) => true
  | (EFalse, TLit(LBool)) => true
  | (EString(path), TIO) => Node.Fs.existsSync(path)
  | (EString(pass), TPassword) => String.length(pass) > 5
  | (EString(str), TLit(LString(expected))) => str == expected
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
  | EString(v) => TLit(LString(v))
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
let (++) = (a, b) => Format.sprintf({|%s%s|}, a, b);