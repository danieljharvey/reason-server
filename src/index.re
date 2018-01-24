Js.log("Hello, BuckleScript and Reason!");

type thingFace =
  | NoFace
  | SomeFace(string);

let slop1 = NoFace;

let slop2 = SomeFace("Plumbum");

let greeting = person =>
  switch person {
  | NoFace => "No fucking idea pal"
  | SomeFace(name) => "Oh it's that prick " ++ name
  };

let slopName1 = greeting(slop1);

let slopName2 = greeting(slop2);

Js.log("Hello " ++ slopName1);

Js.log("And also hello to " ++ slopName2);

let listFace = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];

let double = x => x * 2;

let doubleList = List.map(double);

let doubleMap = List.map(doubleList);

let newList = doubleMap(listFace);

List.map(x => Js.log(Array.of_list(x)), newList);

/*
 let getOverTwo = x =>
   switch (x) {
   | (x < 5) => None
   | x => Some x
   };
 */
/*
 let thing1 = getOverTwo(1);

 let thing2 = getOverTwo(7);

 let thing3 = thing1.map(double);
 */
let conn =
  Mysql.createConnection(
    ~host="127.0.0.1",
    ~port=3306,
    ~user="helloegg",
    ~password="niceegg",
    ~database="itistheegg",
    ()
  );

let fetchLevel = levelID =>
  Mysql.query(
    conn,
    "SELECT * FROM levels WHERE levelID=1",
    levelID,
    (error, results, fields) =>
    switch (Js.Nullable.to_opt(error)) {
    | None =>
      Js.log(results);
      Js.log(fields);
    | Some(error) => Js.log(error##message)
    }
  );

let levelID = 1;

let result = fetchLevel(levelID);

Mysql.endConnection(conn);

let plop = () => Js.log("plop");

open Express;

/* The tests below relies upon the ability to store in the Request
      objects abritrary JSON properties.
      Each middleware will both check that previous middleware
      have been called by making properties exists in the Request object and
      upon success will themselves adds another property to the Request.
   */
/* [checkProperty req next property k] makes sure [property] is
   present in [req]. If success then [k()] is invoked, otherwise
   [Next.route] is called with next */
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
Middleware.from((req, res, next) => Response.sendJson(res, makeSuccessJson()));

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