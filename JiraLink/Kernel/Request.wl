(* ::Package:: *)

(* Request.wl -- the one path to the wire.

   The pre-1.0 package dispatched over four HTTP implementations (URLExecute
   with a bare HTTPRequest, URLFetch, URLExecute, and a shelled-out curl). Only
   the URLFetch branch honoured the request method and body; the default branch
   built HTTPRequest[url, <||>], which is always a GET with an empty body. That
   made JiraCreateIssue and JiraDeleteIssue silent no-ops.

   There is now exactly one code path. Method, headers, query and body are
   assembled into a single HTTPRequest and sent with URLRead, which returns an
   HTTPResponse carrying the status code -- so failures can be reported as
   Failure objects rather than being mistaken for data. *)


$JiraLogRequests = False;

$JiraRequestLog = {};

$jiraMaxRequestLog = 100;


(* ::Section:: *)
(* Request construction *)

Options[jiraBuildRequest] = Join[$jiraCommonOptions, {"Multipart" -> None}];

jiraBuildRequest[method_String, api_String, segments_List, opts : OptionsPattern[]] := Module[
  {conn, prefix, path, query, headers, body, multipart, assoc},

  conn = jiraConnection[OptionValue["Connection"]];
  If[conn === $Failed, Return[$Failed]];

  prefix = Lookup[$jiraAPIPrefix, api, $Failed];
  If[prefix === $Failed, Message[JiraLink::badapi, api]; Return[$Failed]];

  path = Join[
    {""},
    Lookup[conn, "ContextPath", {}],
    prefix,
    ToString /@ segments
  ];

  query = jiraNormalizeQuery[OptionValue["Parameters"]];

  headers = Join[
    jiraAuthHeaders[conn],
    {"Accept" -> "application/json"},
    (* Jira refuses multipart requests that lack this header; it is the
       cross-site-request-forgery guard, and applies to every multipart
       endpoint, not only attachment upload. *)
    If[OptionValue["Multipart"] =!= None, {"X-Atlassian-Token" -> "no-check"}, {}],
    jiraNormalizeQuery[OptionValue["Headers"]]
  ];

  body = OptionValue["Body"];
  multipart = OptionValue["Multipart"];

  assoc = <|
    "Scheme" -> conn["Scheme"],
    "Domain" -> conn["Domain"],
    "Port"   -> conn["Port"],
    "Path"   -> path,
    "Query"  -> query,
    "Method" -> method,
    "Headers" -> headers
  |>;

  Which[
    multipart =!= None,
      assoc["Body"] = multipart,

    body =!= None,
      With[{json = jiraToJSON[body]},
        If[json === $Failed,
          Message[JiraLink::badjson, shorten[body]];
          Return[$Failed]
        ];
        assoc["ContentType"] = "application/json";
        assoc["Body"] = json
      ]
  ];

  HTTPRequest[assoc]
];


Options[JiraRequestObject] = Options[jiraBuildRequest];

JiraRequestObject[method_String, segments_List, opts : OptionsPattern[]] :=
  jiraBuildRequest[method, OptionValue["API"], segments,
    jiraFilterOptions[Flatten[{opts}], jiraBuildRequest]];


(* ::Section:: *)
(* Response interpretation *)

jiraStatusFailure[code_Integer, method_String, url_String, body_String] := Module[
  {parsed, msgs, errs, tag},

  parsed = Quiet @ Check[ImportString[body, "RawJSON"], $Failed];
  {msgs, errs} = If[AssociationQ[parsed],
    {Lookup[parsed, "errorMessages", {}], Lookup[parsed, "errors", <||>]},
    {If[body === "", {}, {shorten[body]}], <||>}
  ];

  tag = Which[
    code === 401 || code === 403, "JiraAuthenticationError",
    code === 404,                 "JiraNotFoundError",
    code === 429,                 "JiraRateLimitError",
    True,                         "JiraHTTPError"
  ];

  Failure[tag, <|
    "MessageTemplate"   :> JiraLink::http,
    "MessageParameters" -> {code, method, url,
      Which[
        Length[msgs] > 0, StringRiffle[msgs, "; "],
        Length[errs] > 0, StringRiffle[KeyValueMap[#1 <> ": " <> ToString[#2] &, errs], "; "],
        True,             "no message"
      ]},
    "StatusCode"    -> code,
    "ErrorMessages" -> msgs,
    "Errors"        -> errs,
    "Method"        -> method,
    "URL"           -> url,
    "Body"          -> body
  |>]
];


(* Jira Data Center adds X-RateLimit-* headers to every authenticated response
   when rate limiting is switched on, and retry-after carries 0 when tokens are
   still available. The headers are simply absent when it is switched off. *)
jiraRetryAfter[headers_List] := Module[{v},
  v = SelectFirst[headers, ToLowerCase[First[#]] === "retry-after" &, None];
  If[v === None, None, Quiet @ Check[ToExpression[Last[v]], None]]
];


(* ::Section:: *)
(* Execution *)

Options[jiraRequest] = Join[Options[jiraBuildRequest], {"Retries" -> 3}];

jiraRequest[method_String, api_String, segments_List, opts : OptionsPattern[]] := Module[
  {req, resp, code, body, headers, retries, attempt, wait, tc, result},

  req = jiraBuildRequest[method, api, segments,
    jiraFilterOptions[Flatten[{opts}], jiraBuildRequest]];
  If[req === $Failed, Return[$Failed]];

  retries = OptionValue["Retries"];
  tc = OptionValue[TimeConstraint];
  attempt = 0;

  While[True,
    attempt++;

    resp = Quiet @ Check[
      If[tc === Automatic,
        URLRead[req, Interactive -> False],
        URLRead[req, Interactive -> False, TimeConstraint -> tc]
      ],
      $Failed
    ];

    If[TrueQ[$JiraLogRequests],
      $JiraRequestLog = Take[
        Append[$JiraRequestLog, <|"Method" -> method, "URL" -> req["URL"],
                                  "Response" -> resp|>],
        -Min[$jiraMaxRequestLog, Length[$JiraRequestLog] + 1]
      ]
    ];

    (* A network-level problem gives back $Failed or a Failure rather than an
       HTTPResponse. *)
    If[!MatchQ[resp, _HTTPResponse],
      Return[Failure["JiraConnectionError", <|
        "MessageTemplate"   :> JiraLink::conn,
        "MessageParameters" -> {req["URL"], If[FailureQ[resp], resp["Message"], "no response"]},
        "Method"            -> method,
        "URL"               -> req["URL"]
      |>]]
    ];

    code = resp["StatusCode"];
    body = resp["Body"];
    headers = resp["Headers"];

    (* Retry only what is worth retrying: an explicit rate-limit rejection, or a
       transient gateway error. *)
    If[MemberQ[{429, 502, 503, 504}, code] && attempt <= retries,
      wait = jiraRetryAfter[headers];
      Pause[If[NumericQ[wait] && wait > 0, Min[wait, 60], 2^attempt]];
      Continue[]
    ];

    Break[]
  ];

  If[code < 200 || code >= 300,
    Return[jiraStatusFailure[code, method, req["URL"], body]]
  ];

  (* 204 No Content is Jira's normal answer to DELETE and to many PUTs. An empty
     body is a success, not a parse failure. *)
  result = If[code === 204 || StringMatchQ[body, WhitespaceCharacter...],
    Null,
    jiraFromJSON[body]
  ];

  jiraFormat[result, OptionValue["Format"]]
];


(* Convenience wrappers used throughout the resource files. *)
jiraGet[api_String, segments_List, opts___]    := jiraRequest["GET", api, segments, opts];
jiraPost[api_String, segments_List, opts___]   := jiraRequest["POST", api, segments, opts];
jiraPut[api_String, segments_List, opts___]    := jiraRequest["PUT", api, segments, opts];
jiraDelete[api_String, segments_List, opts___] := jiraRequest["DELETE", api, segments, opts];


(* ::Section:: *)
(* Public generic access *)

Options[JiraApiExecute] = Join[Options[jiraRequest], {
  (* retained so that pre-1.0 calls passing these do not error *)
  "JiraWebsiteURL"      -> Automatic,
  "JiraWebsiteUsername" -> Automatic,
  "JiraWebsitePassword" -> Automatic,
  "Method"              -> Automatic
}];

(* Present-day form: an explicit method and a path given as segments. *)
JiraApiExecute[method_String, segments_List, opts : OptionsPattern[]] :=
  jiraRequest[ToUpperCase[method], OptionValue["API"], segments,
    jiraFilterOptions[jiraLegacyConnectionOptions[Flatten[{opts}]], jiraRequest]];

(* A single string is accepted as a path too, so that JiraApiExecute["GET", "myself"]
   and JiraApiExecute["GET", "issue/ABC-1"] both work. *)
JiraApiExecute[method_String, path_String, opts : OptionsPattern[]] :=
  JiraApiExecute[method, jiraSplitPath[path], opts];

jiraSplitPath[path_String] := DeleteCases[StringSplit[path, "/"], ""];
