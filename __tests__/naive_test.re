open Naive;
open Jest;

let () = {
  test("1+2:Nat ", () => {
    /* 1+2:Nat */
    let e = ESum(ENum(1), ENum(2));
    let r = TNat;
    Expect.(typeCheck(e, r) |> expect |> toBe(r));
  });

  test("true+2:Nat ", () => {
    /* true+2:Nat */
    let e = ESum(ETrue, ENum(2));
    let r = TNat;
    let t = () => typeCheck(e, r);
    Expect.(
      t |> expect |> toThrowException(Error(NoTypingRuleFor(ETrue, TNat)))
    );
  });
};

/* Expect.(typeCheck(e1) |> typeEquality(r1) |> expect |> toBe(true)); */