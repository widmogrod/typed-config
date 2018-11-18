module StringOrd = {
  type t = string;
  let compare = String.compare;
};

module StringMap = Map.Make(StringOrd);

type expression =
  | EObject(StringMap.t(expression))
  /* | EArray(list(expression)) */
  | EString(string)
  /* | ENumber */
  | ETrue
  | EFalse;
/* | ENull; */

type typ =
  /* | TBool */
  /* | TNumber */
  | TIO
  /* | TList */
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
    | LString(v) => v ++ {| of string|}
    }
  | TSum(a, b) => {|sum [|} ++ showType(a) ++ {|, |} ++ showType(b) ++ {|]|}
  | TDefined(definedType, o) => definedType ++ {| of |} ++ showType(o)
  | TRegexp(_) => "regexp"
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
  | (EString(str), TRegexp(regexp)) =>
    if (Js.Re.(test(str, fromString(regexp)))) {
      true;
    } else {
      Js.log3({|value does not match type|}, str, regexp);
      false;
    }
  | (ETrue, TLit(LBool)) => true
  | (EFalse, TLit(LBool)) => true
  | (EString(path), TIO) => Node.Fs.existsSync(path)
  | (EString(pass), TPassword) =>
    /* TODO proof of concept*/
    String.length(pass) > 5
  | _ =>
    Js.log3({|unknown type matching|}, v, showType(t));
    false;
  };

let rec typeInference = (env: envType, e: expression) =>
  switch (e) {
  | EFalse => TLit(LBool)
  | ETrue => TLit(LBool)
  | EString(v) => TLit(LString(v))
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
  };

let (++) = (a, b) => Format.sprintf({|%s%s|}, a, b);