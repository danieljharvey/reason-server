let conn =
  MySql.Connection.make(
    ~host="127.0.0.1",
    ~port=3306,
    ~database="",
    ~user="",
    ~password="",
    ()
  );

/*
 MySql.Query.with_params(conn, "SELECT 1 + ? + ? as result", [|5, 6|], result =>
   switch result {
   | Error(e) => Js.log2("ERROR: ", e)
   | Mutation(m) => Js.log2("MUTATION: ", m)
   | Select(s) => Js.log2("SELECT: ", s)
   }
 );*/
type result('a, 'b) =
  | Error('a)
  | Ok('b);

let queryCallback = (result: Response.response) =>
  switch result {
  | Error(e) => Error(e)
  | Mutation(m) => Error(m)
  | Select(s) => Ok(s)
  };

let queryNamedParams = (conn, sql, params, callback) =>
  MySql.Query.with_named_params(conn, sql, params, callback);

queryNamedParams(
  conn,
  "SELECT * FROM scores WHERE levelID=:levelID",
  {"levelID": 10},
  queryCallback
);

open Express;

let checkProperty = (req, next, property, k) => {
  let reqData = Request.asJsonObject(req);
  switch (Js.Dict.get(reqData, property)) {
  | None => next(Next.route)
  | Some(x) =>
    switch (Js.Json.decodeBoolean(x)) {
    | Some(b) when b == Js.true_ => k()
    | _ => next(Next.route)
    }
  };
};

/* same as [checkProperty] but with a list of properties */
let checkProperties = (req, next, properties, k) => {
  let rec aux = properties =>
    switch properties {
    | [] => k()
    | [p, ...tl] => checkProperty(req, next, p, () => aux(tl))
    };
  aux(properties);
};

/* [setProperty req property] sets the [property] in the [req] Request
   value */
let setProperty = (req, property) => {
  let reqData = Request.asJsonObject(req);
  Js.Dict.set(reqData, property, Js.Json.boolean(Js.true_));
};

/* return the string value for [key], None if the key is not in [dict]
   TODO once BOption.map is released */
let getDictString = (dict, key) =>
  switch (Js.Dict.get(dict, key)) {
  | Some(json) => Js.Json.decodeString(json)
  | _ => None
  };

let getDictNumber = (dict, key) =>
  switch (Js.Dict.get(dict, key)) {
  | Some(json) => Js.Json.decodeNumber(json)
  | _ => None
  };

/* make a common JSON object representing success */
let makeSuccessJson = () => {
  let json = Js.Dict.empty();
  Js.Dict.set(json, "success", Js.Json.boolean(Js.true_));
  Js.Json.object_(json);
};

let makeFailureJson = () => {
  let json = Js.Dict.empty();
  Js.Dict.set(json, "failure", Js.Json.boolean(Js.true_));
  Js.Json.object_(json);
};

let makeCantFindJson = i => {
  let json = Js.Dict.empty();
  Js.Dict.set(json, "Could not find id", Js.Json.string(i));
  Js.Json.object_(json);
};

let app = express();

App.get(app, ~path="/") @@
Middleware.from((_req, res, _next) =>
  Response.sendJson(res, makeSuccessJson())
);

App.getWithMany(
  app,
  ~path="/levels/:levelID",
  [|
    Middleware.from((req, res, next) =>
      switch (getDictString(Request.params(req), "levelID")) {
      | Some("123") => Response.sendJson(res, makeSuccessJson())
      | Some(i) => Response.sendJson(res, makeCantFindJson(i))
      | _ => next(Next.route)
      }
    )
  |]
);

App.get(app, ~path="/baseUrl") @@
Middleware.from((req, res, next) =>
  switch (Request.baseUrl(req)) {
  | "" => Response.sendJson(res, makeSuccessJson())
  | _ => next(Next.route)
  }
);

let onListen = (port, e) =>
  switch e {
  | exception (Js.Exn.Error(e)) =>
    Js.log(e);
    Node.Process.exit(1);
  | _ => Js.log @@ "Listening at http://127.0.0.1:" ++ string_of_int(port)
  };

App.listen(app, ~onListen=onListen(3000), ());
/* Other examples are
   App.listen app ();
   App.listen app port::1000 ();
   App.listen app port::1000 onListen::(fun e => Js.log e) ();
   */
/* -- Test the server --
   npm run start && cd tests && ./test.sh
   */