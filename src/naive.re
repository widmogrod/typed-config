type expr =
  | ENum(int)
  | ESum(expr, expr)
  | ETrue
  | EFalse
  | EInfinity;

type typ =
  /* TODO as dependent TDependentNat(EInfinity) */
  | TNat
  | TBool
  /* TODO: For dependant nat to be dependant
        Definition of function application should also be introduced
     */
  | TDependentNat(expr);

let lte = (a: int, b: int) => a < b;

type type_error =
  | NoTypingRuleFor(expr, typ)
  | NotNatNum(int)
  | OutOfBoundOfDependentNat(int, int);

exception Error(type_error);

let typeEquality = (a: typ, b: typ) =>
  switch (a, b) {
  | (TNat, TNat) => true
  | (TBool, TBool) => true
  | _ => false
  };

let rec typeCheck = (e: expr, t: typ) =>
  switch (e, t) {
  /* (Val +) rule */
  | (ESum(m, n), _) =>
    typeCheck(m, TNat);
    typeCheck(n, TNat);
  /* (Val n) rule */
  | (ENum(n), TNat) =>
    if (n >= 0 && typeEquality(t, TNat)) {
      t;
    } else {
      raise(Error(NotNatNum(n)));
    }
  /* (Val Nat_n) rule */
  | (ENum(i), TDependentNat(ENum(n))) =>
    if (i <= n) {
      t;
    } else {
      raise(Error(OutOfBoundOfDependentNat(i, n)));
    }
  /* Typing error */
  | _ => raise(Error(NoTypingRuleFor(e, t)))
  };