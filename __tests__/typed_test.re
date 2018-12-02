open Jest;
open Typed;
open TT;

/*

 type match _url$ = Regexp(#\w+://.+#i)
 type sufix _url = Regexp(#\w+://.+#i)

 {
   "__types__": {
     "sufix _url":["Regexp", "#\w+://.+#i"],
   },
   "some_url": "http://ho.ha",
   "icon_path::type": ["io"],
   "icon_path": "./somepath.jpg",
 }

  */

/* https://gist.github.com/curtisz/11139b2cfcaef4a261e0 */
let urlRegexp = {|^(([^:/?#]+):)?(\/\/([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?|};

let env: envType =
  StringMap.(
    empty
    |> add("_url$", TDefined("_url", TRegexp(urlRegexp)))
    |> add("_enabled$", TDefined("_enabled", TLit(LBool)))
    |> add("_password$", TDefined("_password", TPassword))
    |> add("_path$", TDefined("_path", TIO))
    |> add(
         "_tracing_type$",
         TDefined(
           "_tracing_type",
           TSum(
             TConst("jaeger"),
             TSum(TConst("opentracing"), TConst("none")),
           ),
         ),
       )
  );

Js.log(env);

/** Url type */
let e1 =
  EObject(
    StringMap.(
      empty
      |> add(
           "postgres_url",
           EString(
             "postgres://dbuser:dbpass@database:5432/srv_user_purchased_product?sslmode=disable",
           ),
         )
    ),
  );

let r1 = {|{postgres_url = _url of regexp}|};

/* Password type */
let e2 =
  EObject(
    StringMap.(
      empty |> add("mysql_slave_password", EString("$!L!K@L!@KJ$LKH@!$"))
    ),
  );
let r2 = {|{mysql_slave_password = _password of password}|};

/* Bool type */
let e3 = EObject(StringMap.(empty |> add("is_enabled", ETrue)));
let r3 = {|{is_enabled = _enabled of bool}|};

/* Union example */
let e4 =
  EObject(
    StringMap.(
      empty |> add("distributed_tracing_type", EString("opentracing"))
    ),
  );
let r4 = {|{distributed_tracing_type = _tracing_type of sum ["jaeger" of string, sum ["opentracing" of string, "none" of string]]}|};

/** File type */
let e5 =
  EObject(
    StringMap.(
      empty |> add("icon_path", EString("./__tests__/typed_test.re"))
    ),
  );
let r5 = {|{icon_path = _path of io}|};

let e6 = EArray([EFalse, ETrue]);
let r6 = {|[] of bool|};

let e7 = EArray([EArray([EFalse, ETrue])]);
let r7 = {|[] of [] of bool|};

let e8 = EArray([EString("a"), ETrue]);
let r8 = {|[] of sum [string, bool]|};

let e9 =
  EArray([
    EString("a"),
    ETrue,
    EObject(
      StringMap.(
        empty |> add("icon_path", EString("./__tests__/typed_test.re"))
      ),
    ),
  ]);
let r9 = {|[] of sum [string, sum [bool, {icon_path = _path of io}]]|};

let m1 =
  EObject(
    StringMap.(
      empty |> add("icon_path", EString("./__tests__/typed_test.re"))
    ),
  );
let m2 = EObject(StringMap.(empty |> add("market_enabled", ETrue)));
let m3 = EObject(StringMap.(empty |> add("market_enabled", ETrue)));
let m4 =
  EObject(
    StringMap.(
      empty |> add("market_enabled", ETrue) |> add("username_enabled", ETrue)
    ),
  );

let e11 = EArray([m1, m2, m3]);
let r11 = {|[] of sum [{icon_path = _path of io}, {market_enabled = _enabled of bool}]|};

let e12 = EArray([m2, m3, m4]);
let r12 = {|[] of sum [{market_enabled = _enabled of bool}, {market_enabled = _enabled of boolusername_enabled = _enabled of bool}]|};

let e10 =
  EObject(StringMap.(empty |> add("icon_path", EString("./some-rubish"))));
let r10 = Error(IONotExists("./some-rubish"));

/* let e13 = EObject(StringMap.(empty |> add("postgres_url", EFalse)));
   let r13 = Error(TypeCheckNotImplemented(e13, StringMap.find("_url", env)))s; */

let a1 = typeInference(env, e11);
let a2 = typeInference(env, e11);

let () = {
  Expect.(TT.compare(a1, a2) |> expect |> toBe(0));

  testAll(
    "Typed.typeInference",
    [
      (e1, r1),
      (e2, r2),
      (e3, r3),
      (e4, r4),
      (e5, r5),
      (e6, r6),
      (e7, r7),
      (e8, r8),
      (e9, r9),
      (e11, r11),
      (e12, r12),
    ],
    ((expr, xa)) =>
    Expect.(typeInference(env, expr) |> showType |> expect |> toBe(xa))
  );
  testAll(
    "Typed.typeInference error",
    [(e10, r10)],
    ((expr, exc)) => {
      let subject = () => typeInference(env, expr);

      Expect.(expect(subject) |> toThrowException(exc));
    },
  );
};