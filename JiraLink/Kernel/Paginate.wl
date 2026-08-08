(* ::Package:: *)

(* Paginate.wl -- walking Jira's paginated responses.

   Jira 9.12 uses three different shapes for a list of things, and a client has
   to cope with all of them:

     1. A paged envelope
          {"startAt":0,"maxResults":50,"total":200,"values":[...]}
        where the array key may instead be "issues", "comments", "worklogs",
        "histories" or "groups", and where "isLast" is present on some
        endpoints (most Agile ones, /screens, /notificationscheme) but absent
        on others (notably /search).

     2. A bare JSON array with no envelope at all: /project, /field, /status,
        /priority, /resolution, /issuetype, /project/{k}/versions,
        /project/{k}/components, /user/search, /issue/{k}/remotelink, ...

     3. A bare array under a single wrapper key: /issueLinkType gives
        {"issueLinkTypes":[...]}, /user/picker gives {"users":[...],...}.

   Atlassian's own documentation warns that "total" may change between requests
   and may be omitted when it is expensive to compute, and that a requested page
   may come back empty. So termination cannot rely on "total" alone. *)


$jiraArrayKeys = {"values", "issues", "comments", "worklogs", "histories", "groups",
                  "users", "issueLinkTypes", "projects", "sprints", "boards"};


(* Pull the array out of whatever shape came back. *)
jiraExtractItems[res_List, _] := res;
jiraExtractItems[res_Association, key_String] := Lookup[res, key, {}];
jiraExtractItems[res_Association, Automatic] := Module[{k},
  k = SelectFirst[$jiraArrayKeys, ListQ[Lookup[res, #, None]] &, None];
  If[k === None, {}, res[k]]
];
jiraExtractItems[_, _] := {};


(* Page size caps differ per endpoint and Jira silently clamps anything larger,
   so the walker always believes the maxResults it gets back rather than the one
   it asked for. *)
jiraDefaultPageSize = 50;


Options[jiraPaginate] = Join[Options[jiraRequest], {"ItemsKey" -> Automatic}];

jiraPaginate[method_String, api_String, segments_List, opts : OptionsPattern[]] := Module[
  {maxItems, pageSize, startAt, key, collected, res, items, total, isLast,
   returnedMax, params, callOpts, guard},

  maxItems = OptionValue[MaxItems];
  pageSize = OptionValue["PageSize"];
  startAt  = OptionValue["StartAt"];
  key      = OptionValue["ItemsKey"];
  params   = jiraNormalizeQuery[OptionValue["Parameters"]];
  callOpts = jiraFilterOptions[
    DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Parameters", _]],
    jiraRequest
  ];

  If[pageSize === Automatic,
    pageSize = If[IntegerQ[maxItems], Min[maxItems, jiraDefaultPageSize], jiraDefaultPageSize]
  ];

  collected = {};
  guard = 0;

  While[True,
    guard++;
    (* A backstop against a server that never advances startAt. *)
    If[guard > 10000, Break[]];

    res = jiraRequest[method, api, segments,
      "Parameters" -> Join[params,
        {"startAt" -> ToString[startAt], "maxResults" -> ToString[pageSize]}],
      callOpts
    ];

    If[FailureQ[res] || res === $Failed, Return[res]];

    (* Shape 2: a bare array is the whole answer -- there is nothing to page. *)
    If[ListQ[res],
      collected = res;
      Break[]
    ];

    If[!AssociationQ[res], Return[res]];

    items = jiraExtractItems[res, key];

    (* Shape 3: a wrapper key but no pagination fields. *)
    If[!KeyExistsQ[res, "startAt"] && !KeyExistsQ[res, "isLast"],
      collected = items;
      Break[]
    ];

    collected = Join[collected, items];

    total       = Lookup[res, "total", Missing[]];
    isLast      = Lookup[res, "isLast", Missing[]];
    returnedMax = Lookup[res, "maxResults", pageSize];
    If[IntegerQ[returnedMax] && returnedMax > 0, pageSize = returnedMax];

    Which[
      (* Enough collected to satisfy the caller. *)
      IntegerQ[maxItems] && Length[collected] >= maxItems, Break[],
      (* The server said this was the final page. *)
      TrueQ[isLast], Break[],
      (* An empty page means there is nothing further, whatever total claims. *)
      Length[items] === 0, Break[],
      (* No isLast, so fall back on total when the server provided one. *)
      IntegerQ[total] && startAt + Length[items] >= total, Break[]
    ];

    startAt += Length[items];
  ];

  If[IntegerQ[maxItems], collected = Take[collected, UpTo[maxItems]]];

  jiraFormat[collected, OptionValue["Format"]]
];


(* Fetch a single page and return the envelope untouched, for callers that want
   the pagination metadata rather than just the items. *)
Options[jiraPage] = Options[jiraRequest];

jiraPage[method_String, api_String, segments_List, opts : OptionsPattern[]] := Module[
  {params, startAt, pageSize},
  startAt  = OptionValue["StartAt"];
  pageSize = Replace[OptionValue["PageSize"], Automatic -> jiraDefaultPageSize];
  params   = jiraNormalizeQuery[OptionValue["Parameters"]];
  jiraRequest[method, api, segments,
    "Parameters" -> Join[params,
      {"startAt" -> ToString[startAt], "maxResults" -> ToString[pageSize]}],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Parameters", _]],
      jiraRequest
    ]
  ]
];
