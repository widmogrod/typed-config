# Typed-config Type System
## JSON spec

object  ::= { members }
members ::= member | member, memebers
member  ::= type : value
type    ::= string
value   ::= number | string
string  ::= "a" | "b", ...
number  ::= digit | digit digit
digit   ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 

A,B ::=                         types
    Bool
    Nat

a,b ::=                         terms
    value
    

### Atomic types

| type name | JSON value |
|-----------|------------|
| number    | number     |
| bool      | bool       |
| string    | string     |
| null      | null       |
| list      | array      |
| records   | object     |

### Type constructors
#### Bool

    Γ ⊢ ♢
    -------- (Type Bool)
    Γ ⊢ Bool

    Γ ⊢ ♢
    --------------- (Val true)
    Γ ⊢ true: Bool

    Γ ⊢ ♢
    --------------- (Val false)
    Γ ⊢ false: Bool

#### Nat type

#### List type constructor (todo)

    TODO

    Γ ⊢ ♢
    --------- (Type List)
    Γ ⊢ List

    Γ ⊢ ♢  Γ ⊢ a: A 
    ----------------- (Val list)
    Γ ⊢ [a] : List 

    -----------------
    Γ ⊢ [] : List 

#### Record type constructor - named tuples

TODO

#### Product type - touple

    Γ ⊢ A    Γ ⊢ B
    --------------------- (Type Product)
    Γ ⊢ (A x B) : Type

    Γ ⊢ a : A    Γ ⊢ b : B
    ----------------------- (Val product)
    Γ ⊢ (a, b) : (A x B)

    Γ ⊢ a : (A x B)     Γ, x : A, y : B ⊢ c : C
    -------------------------------------------- (Val with)
    Γ ⊢ (with (x:A,y:B) of a in c): C

#### Sum type

    Γ ⊢ A    Γ ⊢ B
    --------------------- (Type Sum)
    Γ ⊢ (A + B) : Type

    Γ ⊢ a : A   Γ ⊢ B
    -------------------------- (Val inLeft)
    Γ ⊢ inLeft_b a : (A + B)

    Γ ⊢ A   Γ ⊢ b: B
    -------------------------- (Val inRight)
    Γ ⊢ inRight_b b : (A + B)

    Γ ⊢ a : (A + B)     Γ x : A ⊢ b₁ : B     Γ y : B ⊢ b₂ : B
    ---------------------------------------------------------- (Val case)
    Γ ⊢ (case a of
            | x:A then b₁
            | y:B then b₂) : B


## Refereneces
- [A] https://www.json.org
