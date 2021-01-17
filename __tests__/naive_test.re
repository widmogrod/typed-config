open Naive;
open Jest;

let () = {
  test("1+2:Nat ", () => {
    let e = ESum(ENum(1), ENum(2));
    let r = TNat;
    Expect.(typeCheck(e, r) |> expect |> toBe(r));
  });

  test("true+2:Nat ", () => {
    let e = ESum(ETrue, ENum(2));
    let r = TNat;
    let t = () => typeCheck(e, r);
    Expect.(
      t |> expect |> toThrowException(Error(NoTypingRuleFor(ETrue, TNat)))
    );
  });
  test("2:Nat_₂ ", () => {
    let e = ENum(2);
    let r = TDependentNat(ENum(2));
    Expect.(typeCheck(e, r) |> expect |> toBe(r));
  });

  test("3:Nat_₂ ", () => {
    let e = ENum(3);
    let r = TDependentNat(ENum(2));
    let t = () => typeCheck(e, r);
    Expect.(
      t |> expect |> toThrowException(Error(OutOfBoundOfDependentNat(3, 2)))
    );
  });
};

/* Expect.(typeCheck(e1) |> typeEquality(r1) |> expect |> toBe(true)); */