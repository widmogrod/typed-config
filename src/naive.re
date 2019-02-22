type expr =
  | ENum(int)
  | ESum(expr, expr)
  | ETrue
  | EFalse;

type typ =
  | TNat
  | TBool;

type type_error =
  | NoTypingRuleFor(expr, typ)
  | NotNatNum(int);

exception Error(type_error);

let typeEquality = (a: typ, b: typ) =>
  switch (a, b) {
  | (TNat, TNat) => true
  | (TBool, TBool) => true
  | _ => false
  };

let rec typeCheck = (e: expr, t: typ) =>
  switch (e) {
  /* (Val +) rule */
  | ESum(m, n) =>
    typeCheck(m, TNat);
    typeCheck(n, TNat);
  /* (Val n) rule */
  | ENum(n) =>
    if (n >= 0 && typeEquality(t, TNat)) {
      t;
    } else {
      raise(Error(NotNatNum(n)));
    }
  /* Typing error */
  | _ => raise(Error(NoTypingRuleFor(e, t)))
  };