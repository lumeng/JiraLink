(* ::Package:: *)

(* Common.wl -- shared constants, messages and helpers.

   Read by JiraLink.wl from inside JiraLink`Private`, so everything defined
   here is private to the package unless it was declared in JiraLink.wl. *)


(* ::Section:: *)
(* Messages *)

JiraLink::nocred = "No Jira credential is available for `1`. Use JiraConnect, or store one with \
JiraStoreCredential, or set the JIRA_URL and JIRA_TOKEN environment variables.";

JiraLink::noconn = "No Jira connection has been established. Use JiraConnect[url] first.";

JiraLink::badurl = "`1` is not a valid Jira base URL.";

JiraLink::badkey = "`1` is not a valid Jira issue key.";

JiraLink::badjson = "The server response could not be read as JSON: `1`";

JiraLink::badopt = "Invalid setting `2` for option `1`.";

JiraLink::http = "Jira returned status `1` for `2` `3`: `4`";

JiraLink::conn = "Could not reach the Jira server at `1`: `2`";

JiraLink::nofile = "The file `1` does not exist.";

JiraLink::legacy = "The pre-1.0 configuration file `1` was not found.";

JiraLink::badapi = "`1` is not a known Jira API. Valid settings are \"platform\", \"agile\", \
\"servicedesk\", \"pat\" and \"auth\".";

JiraLink::notdc = "The endpoint `1` is not available on this Jira instance. It requires Jira \
Data Center, or a plugin that is not installed.";

JiraLink::truncated = "Jira limited the page size of `1` to `2`; retrieving the requested results \
takes more than one request.";

JiraLink::nointeract = "No credential could be read: this session cannot prompt. Pass the \
credential explicitly, or set the JIRA_TOKEN environment variable.";

JiraLink::cancelled = "Credential entry was cancelled; nothing was stored.";

JiraLink::nosecret = "The credential contains no secret field. Give either \"Token\" or \"Password\".";

JiraLink::storefail = "The credential could not be written to secure storage under the key `1`.";


(* ::Section:: *)
(* API families

   Jira Data Center serves several REST APIs under different prefixes. The
   platform API is version 2 -- version 3 is Cloud-only and is deliberately
   not offered here. *)

$jiraAPIPrefix = <|
  "platform"    -> {"rest", "api", "2"},
  "agile"       -> {"rest", "agile", "1.0"},
  "servicedesk" -> {"rest", "servicedeskapi"},
  "pat"         -> {"rest", "pat", "latest"},
  "auth"        -> {"rest", "auth", "1"}
|>;

$jiraAPINames = Keys[$jiraAPIPrefix];


(* ::Section:: *)
(* Issue keys *)

(* Anchored: an issue key is PROJECT-123 and nothing else. Project keys are
   uppercase alphanumeric and may contain underscores. *)
$jiraIssueKeyPattern = RegularExpression["[A-Z][A-Z0-9_]*-[0-9]+"];

JiraIssueKeyQ[s_String] := StringMatchQ[s, $jiraIssueKeyPattern];
JiraIssueKeyQ[_] := False;

(* Accept an issue key, a numeric id, or an integer wherever Jira says
   "issueIdOrKey". *)
jiraIssueRef[s_String] := s;
jiraIssueRef[n_Integer] := ToString[n];
jiraIssueRef[other_] := (Message[JiraLink::badkey, other]; $Failed);

projectKeyFromIssueKey[k_String] := StringReplace[k, RegularExpression["^(.*)-[0-9]+$"] -> "$1"];


(* ::Section:: *)
(* JSON *)

(* Jira sends UTF-8. ImportString[..., "RawJSON"] yields Associations, which is
   the single response shape used throughout the package -- the pre-1.0 code
   mixed Associations and lists of rules, and callers disagreed about which. *)
jiraFromJSON[""] := Null;
jiraFromJSON[s_String] := Module[{res},
  res = Quiet @ Check[ImportString[s, "RawJSON"], $Failed, {Import::jsonarg, Import::fmterr}];
  If[res === $Failed, jiraParseFailure[s], res]
];
jiraFromJSON[b_ByteArray] := jiraFromJSON[ByteArrayToString[b, "UTF-8"]];

jiraParseFailure[s_String] := Failure["JiraParseError", <|
  "MessageTemplate"   :> JiraLink::badjson,
  "MessageParameters" -> {shorten[s]},
  "Body"              -> s
|>];

jiraToJSON[expr_] := Module[{res},
  res = Quiet @ Check[ExportString[expr, "JSON", "Compact" -> True], $Failed];
  If[StringQ[res], res, $Failed]
];

shorten[s_String, n_Integer: 300] :=
  If[StringLength[s] > n, StringTake[s, n] <> "...", s];
shorten[other_, n_Integer: 300] := shorten[ToString[other, InputForm], n];


(* ::Section:: *)
(* Base URL handling

   The pre-1.0 code hardcoded a "/jira" path segment into every request. Whether
   that segment is present depends on how the server is deployed, so it is now
   derived from whatever base URL the user supplies and carried on the
   connection. *)

jiraNormalizeURL[url_String] := Module[{withScheme, parsed, path},
  withScheme = If[
    StringMatchQ[url, RegularExpression["(?i)^[a-z][a-z0-9+.\\-]*://.*"]],
    url,
    (* No scheme given. Default to https: commit b033588 established that the
       Wolfram instance requires https, and plain http would silently send
       credentials in the clear. *)
    "https://" <> url
  ];
  parsed = URLParse[withScheme];
  If[!StringQ[parsed["Domain"]] || parsed["Domain"] === "",
    Message[JiraLink::badurl, url];
    Return[$Failed]
  ];
  path = DeleteCases[Replace[parsed["Path"], Except[_List] -> {}], "" | None];
  <|
    "Scheme"      -> Replace[parsed["Scheme"], (_?(# === "" &) | None) -> "https"],
    "Domain"      -> parsed["Domain"],
    "Port"        -> parsed["Port"],
    "ContextPath" -> path
  |>
];

jiraBaseURLString[conn_Association] := URLBuild[<|
  "Scheme" -> conn["Scheme"],
  "Domain" -> conn["Domain"],
  "Port"   -> conn["Port"],
  "Path"   -> Prepend[Lookup[conn, "ContextPath", {}], ""]
|>];


(* ::Section:: *)
(* Query parameters

   Jira wants strings. Booleans must become "true"/"false" rather than
   "True"/"False", lists become comma-separated, and Nothing/None/Automatic
   drop out entirely. *)

jiraQueryValue[True]  := "true";
jiraQueryValue[False] := "false";
jiraQueryValue[s_String] := s;
jiraQueryValue[n_Integer] := ToString[n];
jiraQueryValue[r_Real] := ToString[r];
jiraQueryValue[l_List] := StringRiffle[jiraQueryValue /@ l, ","];
jiraQueryValue[other_] := ToString[other];

jiraNormalizeQuery[params_] := Module[{rules},
  rules = Replace[params, {
    a_Association :> Normal[a],
    Automatic | None | {} -> {},
    r_Rule :> {r}
  }];
  If[!MatchQ[rules, {___Rule}], Return[{}]];
  DeleteCases[
    Map[
      Function[r,
        If[MatchQ[Last[r], None | Automatic | Null],
          Nothing,
          ToString[First[r]] -> jiraQueryValue[Last[r]]
        ]
      ],
      rules
    ],
    Nothing
  ]
];


(* ::Section:: *)
(* Options

   Every user-facing function accepts at least these. Individual functions add
   their own on top. Note the use of the System` symbols MaxItems and
   TimeConstraint rather than package-local look-alikes. *)

$jiraCommonOptions = {
  "Connection"    -> Automatic,
  "Parameters"    -> {},
  "Headers"       -> {},
  "API"           -> "platform",
  "Body"          -> None,
  "Fields"        -> Automatic,
  "Expand"        -> None,
  "Format"        -> Automatic,
  MaxItems        -> All,
  "PageSize"      -> Automatic,
  "StartAt"       -> 0,
  TimeConstraint  -> Automatic
};

(* Pass through only those options that the callee declares, so that a caller
   may hand its whole option sequence down without provoking optx messages. *)
jiraFilterOptions[opts_List, sym_Symbol] := Sequence @@ FilterRules[opts, Options[sym]];
jiraFilterOptions[opts_List, known_List]  := Sequence @@ FilterRules[opts, known];


(* ::Section:: *)
(* Result shaping

   "Format" -> Automatic returns plain Associations and Lists, which is what
   most code wants. Dataset is offered because Jira results are naturally
   tabular. *)

jiraFormat[res_, Automatic] := res;
jiraFormat[res_, Association] := res;
jiraFormat[res_?FailureQ, _] := res;
jiraFormat[res_, Dataset] := Dataset[res];
jiraFormat[res_, List] := If[AssociationQ[res], Normal[res], res];
jiraFormat[res_, f_] := (Message[JiraLink::badopt, "Format", f]; res);


(* ::Section:: *)
(* Durations

   Worklog entries take either a Jira duration string ("3h 30m") or, more
   naturally in the Wolfram Language, a Quantity of time. *)

jiraTimeSpent[s_String] := <|"timeSpent" -> s|>;
jiraTimeSpent[q_Quantity] := Module[{secs},
  secs = QuantityMagnitude[UnitConvert[q, "Seconds"]];
  If[NumericQ[secs],
    <|"timeSpentSeconds" -> Round[secs]|>,
    (Message[JiraLink::badopt, "TimeSpent", q]; $Failed)
  ]
];
jiraTimeSpent[n_?NumericQ] := <|"timeSpentSeconds" -> Round[n]|>;
jiraTimeSpent[other_] := (Message[JiraLink::badopt, "TimeSpent", other]; $Failed);

(* Jira timestamps look like 2024-03-05T11:22:33.000+0000. DateObject round-trips
   them, but the offset has no colon, which DateObject's ISO parser wants. *)
jiraDateString[d_DateObject] :=
  StringReplace[
    DateString[d, {"ISODateTime", ".", "Millisecond", "ISOTimeZone"}],
    RegularExpression["([+\\-]\\d{2}):(\\d{2})$"] -> "$1$2"
  ];
jiraDateString[s_String] := s;
