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

Γ ⊢ ♢       Γ is well formed environment
Γ ⊢ A       A is a type in well formed environment
Γ ⊢ a:A     a is a tern of type A in well formed environment

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

    Γ ⊢ ♢
    -------- (Val n) (n = 1,2,3,4,..)
    Γ ⊢ n : Nat

    ----- dependent natural number ----------

        Γ ⊢ ♢
        ----------------------------- (Val <=)
        Γ ⊢ i <= n

        Γ ⊢ ♢
        ----------------------- (Type alias ;))
        Γ ⊢ Nat : Nat_infinity

        Γ, i : Nat, n : Nat ⊢ i <= n
        ----------------------------- (Type Nat_n)
        Γ ⊢ Nat_n(i) : Nat -> i <= n

        Γ ⊢ Nat_n(i)
        ------------- (Val Nat_n) (i = 0, 1, 2, 3)
        Γ ⊢ i : Nat_n

    ------ experiment below ------

    Γ ⊢ a : A, B : A -> Type

    transitive : a < b and b < c then a < c

        Γ ⊢ A < B    Γ ⊢ B < C
        -----------------------
        Γ ⊢ A < C

    indepotent  : f(f(x)) == f(x) 

        Γ ⊢ 
        -----------------------
        Γ ⊢ 

    commutative : a + b = b + a

        Γ ⊢ 
        -----------------------
        Γ ⊢ 

    associative : (a + b) + c = a + (b + c)

        Γ ⊢ 
        -----------------------
        Γ ⊢ 

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


#### Record Type

    Γ ⊢ A₁  ...  Γ ⊢ A𝚗
    -------------------------- (Type Record) (l𝚒 distinct)
    Γ ⊢ {l₁: A₁, ..., l𝚗: A𝚗}

    Γ ⊢ a₁: A₁  ...  Γ ⊢ a𝚗:A𝚗
    ----------------------------------------------------- (Val record)
    Γ ⊢ {l₁=a₁, ..., l𝚗=a𝚗} : {l₁: A₁, ..., l𝚗: A𝚗}


## Example of type derivation
### Bool

    Γ ⊢ ♢
    --------------- by (Val true)
    Γ ⊢ true: Bool
    ---------------------------------------------------- by (Val record)
    Γ ⊢ {market_enabled: true} : {market_enabled: Bool}

### TODO: String

    TODO string type & term [?]
    ---------------
    Γ ⊢ "<string>": String
    -------------------------------------------------------
    Γ ⊢ {market_name: "some name"} : {market_name: String}

### Type reconstruction algorithm
JSON doesn't have types.
To enable posibility to run type checker algorithm on type-less JSON file, then
- Type of typeless JSON file must be provided
- Type of typeless JSON file must be reconstructed

TODO.

## Refereneces
- [A] https://www.json.org
