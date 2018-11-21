open Jest;
open Typed;

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
             TLit(LString("jaeger")),
             TSum(TLit(LString("opentracing")), TLit(LString("none"))),
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

let () =
  testAll(
    "Typed.typeInference",
    [(e1, r1), (e2, r2), (e3, r3), (e4, r4), (e5, r5), (e6, r6)],
    ((expr, xa)) =>
    Expect.(typeInference(env, expr) |> showType |> expect |> toBe(xa))
  );