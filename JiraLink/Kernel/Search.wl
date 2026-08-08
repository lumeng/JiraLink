(* ::Package:: *)

(* Search.wl -- JQL search.

   Jira Data Center 9.12 has only the classic startAt-paginated /search. The
   token-paginated /search/jql introduced for Cloud does not exist here.

   Two caps matter. /search defaults to 50 results per page, and the server
   silently truncates any maxResults above jira.search.views.default.max
   (stock 1000) -- so the paginator believes the maxResults it is given back
   rather than the one it asked for. *)


$jiraSearchPageSize = 100;

(* Beyond roughly this length a query string starts running into server and
   proxy URL limits, so the query moves into a POST body instead. *)
$jiraJQLPostThreshold = 1500;


(* ::Section:: *)
(* JQL escaping

   Values containing reserved characters (space + . , ; ? | * / % ^ $ # @ [ ])
   or reserved words (and, or, not, empty, null, order, by, ...) must be
   quoted. Inside a quoted literal a backslash escapes the quote character and
   the backslash itself. *)

JiraJQLEscape[s_String] :=
  "\"" <> StringReplace[s, {"\\" -> "\\\\", "\"" -> "\\\""}] <> "\"";

JiraJQLEscape[d_DateObject] := JiraJQLEscape[DateString[d, {"Year", "-", "Month", "-", "Day"}]];

JiraJQLEscape[n_?NumericQ] := ToString[n];

JiraJQLEscape[l_List] := "(" <> StringRiffle[JiraJQLEscape /@ l, ", "] <> ")";


(* ::Section:: *)
(* The underlying request

   GET while the query is short, POST once it is long. Note that the documented
   POST body schema for 9.12 carries jql, startAt, maxResults, fields and
   validateQuery but not expand, so a request needing expansion stays on GET. *)

Options[jiraSearchRequest] = Join[$jiraCommonOptions, {"ValidateQuery" -> True}];

jiraSearchRequest[jql_String, startAt_Integer, pageSize_Integer, opts : OptionsPattern[]] := Module[
  {fields, expand, usePost, body, params},

  fields = OptionValue["Fields"];
  expand = OptionValue["Expand"];

  usePost = StringLength[jql] > $jiraJQLPostThreshold && (expand === None || expand === Automatic);

  If[usePost,
    body = <|
      "jql" -> jql,
      "startAt" -> startAt,
      "maxResults" -> pageSize,
      "validateQuery" -> TrueQ[OptionValue["ValidateQuery"]]
    |>;
    If[fields =!= Automatic && fields =!= None,
      body["fields"] = Flatten[{fields}]
    ];
    jiraPost["platform", {"search"}, "Body" -> body,
      jiraFilterOptions[Flatten[{opts}], jiraRequest]]
    ,
    params = Join[
      {"jql" -> jql,
       "startAt" -> ToString[startAt],
       "maxResults" -> ToString[pageSize],
       "validateQuery" -> TrueQ[OptionValue["ValidateQuery"]]},
      jiraFieldParams[fields, expand],
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ];
    jiraGet["platform", {"search"}, "Parameters" -> params,
      jiraFilterOptions[Flatten[{opts}], jiraRequest]]
  ]
];


(* ::Section:: *)
(* JiraJqlSearch -- one page, envelope intact *)

Options[JiraJqlSearch] = Join[Options[jiraSearchRequest], {
  (* The pre-1.0 defaults were MaxResults -> 10 and StartAt -> 1. StartAt was
     off by one: Jira indexes results from zero. *)
  "MaxResults" -> $jiraSearchPageSize,
  "StartAt"    -> 0
}];

JiraJqlSearch[jql_String, opts : OptionsPattern[]] :=
  jiraSearchRequest[jql, OptionValue["StartAt"], OptionValue["MaxResults"],
    jiraFilterOptions[Flatten[{opts}], jiraSearchRequest]];


(* ::Section:: *)
(* JiraSearch -- all pages *)

Options[JiraSearch] = Options[jiraSearchRequest];

JiraSearch[jql_String, opts : OptionsPattern[]] := Module[
  {maxItems, pageSize, startAt, collected, res, issues, total, guard},

  maxItems = OptionValue[MaxItems];
  pageSize = Replace[OptionValue["PageSize"], Automatic :>
    If[IntegerQ[maxItems], Min[maxItems, $jiraSearchPageSize], $jiraSearchPageSize]];
  startAt  = OptionValue["StartAt"];

  collected = {};
  guard = 0;

  While[True,
    guard++;
    If[guard > 10000, Break[]];

    res = jiraSearchRequest[jql, startAt, pageSize,
      jiraFilterOptions[Flatten[{opts}], jiraSearchRequest]];
    If[FailureQ[res] || res === $Failed, Return[res]];
    If[!AssociationQ[res], Return[res]];

    issues = Lookup[res, "issues", {}];
    collected = Join[collected, issues];

    total = Lookup[res, "total", Missing[]];

    (* The server may hand back a smaller page than was asked for. *)
    With[{m = Lookup[res, "maxResults", pageSize]},
      If[IntegerQ[m] && m > 0, pageSize = m]
    ];

    Which[
      IntegerQ[maxItems] && Length[collected] >= maxItems, Break[],
      Length[issues] === 0, Break[],
      IntegerQ[total] && startAt + Length[issues] >= total, Break[]
    ];

    startAt += Length[issues];
  ];

  If[IntegerQ[maxItems], collected = Take[collected, UpTo[maxItems]]];

  jiraFormat[collected, OptionValue["Format"]]
];

JiraSearch[jql_String, fields : (_String | {___String}), opts : OptionsPattern[]] :=
  JiraSearch[jql, "Fields" -> fields,
    jiraFilterOptions[DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Fields", _]], JiraSearch]];


(* ::Section:: *)
(* JiraIssueCount *)

Options[JiraIssueCount] = Options[jiraSearchRequest];

JiraIssueCount[jql_String, opts : OptionsPattern[]] := Module[{res},
  (* Ask for a single field and a zero-length page: Jira still reports total. *)
  res = jiraSearchRequest[jql, 0, 0, "Fields" -> "summary",
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Fields", _]],
      jiraSearchRequest]];
  If[FailureQ[res] || !AssociationQ[res], Return[res]];
  Lookup[res, "total", Missing["NotAvailable"]]
];


(* ::Section:: *)
(* JiraValidateJQL *)

Options[JiraValidateJQL] = Options[jiraSearchRequest];

JiraValidateJQL[jql_String, opts : OptionsPattern[]] := Module[{res},
  res = jiraSearchRequest[jql, 0, 0, "ValidateQuery" -> True,
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["ValidateQuery", _]],
      jiraSearchRequest]];
  If[FailureQ[res], res, True]
];


(* ::Section:: *)
(* JiraFindIssues

   Kept from the pre-1.0 API, with the same five argument forms. The return
   shape changes: these used to give nested lists of rules, and now give
   associations keyed by issue key. *)

Options[JiraFindIssues] = Options[JiraSearch];

JiraFindIssues::badres = "This JQL query did not find any issues: `1`";

jiraIssuesByKey[issues_List] :=
  Association @ Map[
    Lookup[#, "key", Missing["NotAvailable"]] -> Lookup[#, "fields", <||>] &,
    issues
  ];

customFieldKeyQ[k_] := StringMatchQ[ToString[k], RegularExpression["customfield_[0-9]+"]];

(* Bare form: just the keys. *)
JiraFindIssues[jql_String, opts : OptionsPattern[]] := Module[{res},
  res = JiraSearch[jql, "Fields" -> "summary",
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Fields", _]], JiraSearch]];
  If[FailureQ[res] || res === $Failed, Return[res]];
  Lookup[#, "key", Missing["NotAvailable"]] & /@ res
];

JiraFindIssues[jql_String, "Properties", opts : OptionsPattern[]] := Module[{res},
  res = JiraSearch[jql, jiraFilterOptions[Flatten[{opts}], JiraSearch]];
  If[FailureQ[res] || res === $Failed, Return[res]];
  If[res === {}, Message[JiraFindIssues::badres, jql]];
  jiraIssuesByKey[res]
];

JiraFindIssues[jql_String, "CoreProperties", opts : OptionsPattern[]] := Module[{res},
  res = JiraFindIssues[jql, "Properties", opts];
  If[FailureQ[res] || res === $Failed, Return[res]];
  KeySelect[#, !customFieldKeyQ[#] &] & /@ res
];

JiraFindIssues[jql_String, field_String, opts : OptionsPattern[]] := Module[{res},
  res = JiraSearch[jql, "Fields" -> field,
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Fields", _]], JiraSearch]];
  If[FailureQ[res] || res === $Failed, Return[res]];
  (* jiraIssuesByKey already unwraps "fields", so look the field up directly. *)
  Lookup[#, field, Missing["NotAvailable"]] & /@ jiraIssuesByKey[res]
];

JiraFindIssues[jql_String, fields : {__String}, opts : OptionsPattern[]] := Module[{res},
  res = JiraSearch[jql, "Fields" -> fields,
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Fields", _]], JiraSearch]];
  If[FailureQ[res] || res === $Failed, Return[res]];
  KeyTake[#, fields] & /@ jiraIssuesByKey[res]
];


(* ::Section:: *)
(* Issue picker *)

Options[JiraIssuePicker] = $jiraCommonOptions;

JiraIssuePicker[query_String, opts : OptionsPattern[]] :=
  jiraGet["platform", {"issue", "picker"},
    "Parameters" -> Join[{"query" -> query}, jiraNormalizeQuery[OptionValue["Parameters"]]],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];
