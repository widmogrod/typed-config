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
    /** TODO: TDependant to implement (Type Nat_n) rule
      plus implement function application thanks to that
      rule (Type Nat_n) could look like this:
      | (ENum(i), TDependentNat(ENum(n), TAbs(n))) =>
        e1 = TApp(TAbs(n), ENum(i))
        typeCheck(e1, TBool)
        !!! Although - TBool must be true?
        How to solve it?

      */
    (
      if (i <= n) {
        t;
      } else {
        raise(Error(OutOfBoundOfDependentNat(i, n)));
      }
    )
  /* Typing error */
  | _ => raise(Error(NoTypingRuleFor(e, t)))
  };