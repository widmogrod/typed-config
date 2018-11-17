module StringOrd = {
  type t = string;
  let compare = String.compare;
};

module StringMap = Map.Make(StringOrd);

type expression =
  | EObject(StringMap.t(expression))
  /* | EArray(list(expression)) */
  | EString(string)
  /* | ENumber */
  | ETrue
  | EFalse;
/* | ENull; */

type typ =
  /* | TBool */
  /* | TNumber */
  | TIO
  /* | TList */
  /* | TDate */
  /* | TSet */
  | TPassword
  | TRecord(StringMap.t(typ))
  | TLit(lit)
  | TSum(typ, typ)
  | TRegexp(string)
  | TDefined(string, typ)
and lit =
  | LInt
  | LBool
  | LString(string); /* This should be more like atom/const */

type scheme =
  | Scheme(list(string), typ);

type envType = StringMap.t(typ);
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

let env: envType =
  StringMap.(
    empty
    |> add("_url$", TDefined("_url", TRegexp("#\w+://.+#i")))
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
      |> add("postgres_url", EString("tcp://postgress@postgress:postgress/"))
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
let r4 = {|{distributed_tracing_type = _tracing_type of sum [jaeger of string, sum [opentracing of string, none of string]]}|};

/** File type */
let e5 =
  EObject(
    StringMap.(
      empty |> add("icon_path", EString("./path/to/local/file.png"))
    ),
  );
let r5 = {|{icon_path = _path of io}|};

let infereRecord = (env: envType, key: string, e: expression) =>
  StringMap.fold(
    (k, v, agg) =>
      switch (agg) {
      | None =>
        if (Js.Re.(test(key, fromString(k)))) {
          Some(v);
        } else {
          None;
        }
      | Some(_) => agg
      },
    env,
    None,
  );

let rec typeInference = (env: envType, e: expression) =>
  switch (e) {
  | EFalse => TLit(LBool)
  | ETrue => TLit(LBool)
  | EString(v) => TLit(LString(v))
  | EObject(map) =>
    TRecord(
      StringMap.mapi(
        (k, v) => {
          let x = infereRecord(env, k, v);
          switch (x) {
          | None =>
            Js.log2({|Cannot find type for key=|}, k);
            typeInference(env, e);
          | Some(t) => t
          };
        },
        map,
      ),
    )
  };

let (++) = (a, b) => Format.sprintf({|%s%s|}, a, b);

let rec showType = t =>
  switch (t) {
  | TIO => "io"
  | TPassword => "password"
  | TLit(l) =>
    switch (l) {
    | LInt => "int"
    | LBool => "bool"
    | LString(v) => v ++ {| of string|}
    }
  | TSum(a, b) => {|sum [|} ++ showType(a) ++ {|, |} ++ showType(b) ++ {|]|}
  | TDefined(definedType, o) => definedType ++ {| of |} ++ showType(o)
  | TRegexp(_) => "regexp"
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

let test_it = p =>
  switch (p) {
  | (e: expression, r: string) =>
    Js.log2("run expression of", e);
    Js.log2("given   :", showType(typeInference(env, e)));
    Js.log2("expected:", r);
    assert(showType(typeInference(env, e)) == r);
  };

let () = Js.log("typed");

[(e1, r1), (e2, r2), (e3, r3), (e4, r4), (e5, r5)] |> List.map(test_it);