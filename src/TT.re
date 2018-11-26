module StringMap = Map.Make(String);

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
  | TConst(string)
  | TSum(typ, typ)
  | TRegexp(string)
  | TDefined(string, typ)
and lit =
  | LInt
  | LBool
  | LString;

let rec showType = t =>
  switch (t) {
  | TIO => "io"
  | TPassword => "password"
  | TLit(l) =>
    switch (l) {
    | LInt => "int"
    | LBool => "bool"
    | LString => "string"
    }
  | TConst(v) => {|"|} ++ v ++ {|"|} ++ {| of string|}
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

type t = typ;

let b2i = a =>
  switch (a) {
  | false => (-1)
  | true => 0
  };
let i2b = a =>
  switch (a) {
  | 0 => true
  | _ => false
  };

let rec compare = (a: typ, b: typ) =>
  switch (a, b) {
  | (TLit(LInt), TLit(LInt)) => 0
  | (TLit(LBool), TLit(LBool)) => 0
  | (TLit(LString), TLit(LString)) => 0
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
  | (TRecord(ax), TRecord(bx)) =>
    StringMap.equal((a, b) => compare(a, b) |> i2b, ax, bx) |> b2i
  | _ => (-1)
  };