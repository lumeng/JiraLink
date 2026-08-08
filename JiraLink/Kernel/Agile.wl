(* ::Package:: *)

(* Agile.wl -- boards, sprints and epics, under /rest/agile/1.0.

   Agile responses use the {maxResults, startAt, total, isLast, values} envelope,
   with "issues" in place of "values" on the issue-returning endpoints. Unlike
   the platform API they do carry isLast, so the paginator terminates on that. *)


$jiraSprintIssueBatch = 50;


(* ::Section:: *)
(* Boards *)

Options[JiraBoards] = Join[$jiraCommonOptions, {"Type" -> None, "Name" -> None}];

JiraBoards[opts : OptionsPattern[]] :=
  jiraPaginate["GET", "agile", {"board"},
    "ItemsKey" -> "values",
    "Parameters" -> Join[
      If[StringQ[OptionValue["Type"]], {"type" -> OptionValue["Type"]}, {}],
      If[StringQ[OptionValue["Name"]], {"name" -> OptionValue["Name"]}, {}],
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraPaginate]];

JiraBoards[project_String, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "agile", {"board"},
    "ItemsKey" -> "values",
    "Parameters" -> Join[
      {"projectKeyOrId" -> project},
      If[StringQ[OptionValue["Type"]], {"type" -> OptionValue["Type"]}, {}],
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraPaginate]];


Options[JiraBoard] = $jiraCommonOptions;
JiraBoard[id_, opts : OptionsPattern[]] :=
  jiraGet["agile", {"board", ToString[id]}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraCreateBoard] = $jiraCommonOptions;
JiraCreateBoard[name_String, type_String, filterId_, opts : OptionsPattern[]] :=
  jiraPost["agile", {"board"},
    "Body" -> <|"name" -> name, "type" -> type, "filterId" -> filterId|>,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraDeleteBoard] = $jiraCommonOptions;
JiraDeleteBoard[id_, opts : OptionsPattern[]] :=
  jiraDelete["agile", {"board", ToString[id]}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraBoardConfiguration] = $jiraCommonOptions;
JiraBoardConfiguration[id_, opts : OptionsPattern[]] :=
  jiraGet["agile", {"board", ToString[id], "configuration"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* Board issue lists all accept the same jql/fields/expand filters. *)
jiraBoardIssueParams[jql_, fields_, expand_, extra_] := Join[
  If[StringQ[jql], {"jql" -> jql}, {}],
  jiraFieldParams[fields, expand],
  jiraNormalizeQuery[extra]
];

Options[JiraBoardIssues] = Join[$jiraCommonOptions, {"JQL" -> None}];
JiraBoardIssues[id_, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "agile", {"board", ToString[id], "issue"},
    "ItemsKey" -> "issues",
    "Parameters" -> jiraBoardIssueParams[OptionValue["JQL"], OptionValue["Fields"],
      OptionValue["Expand"], OptionValue["Parameters"]],
    jiraFilterOptions[Flatten[{opts}], jiraPaginate]];

Options[JiraBoardBacklog] = Options[JiraBoardIssues];
JiraBoardBacklog[id_, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "agile", {"board", ToString[id], "backlog"},
    "ItemsKey" -> "issues",
    "Parameters" -> jiraBoardIssueParams[OptionValue["JQL"], OptionValue["Fields"],
      OptionValue["Expand"], OptionValue["Parameters"]],
    jiraFilterOptions[Flatten[{opts}], jiraPaginate]];

Options[JiraBoardEpics] = $jiraCommonOptions;
JiraBoardEpics[id_, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "agile", {"board", ToString[id], "epic"},
    "ItemsKey" -> "values", jiraFilterOptions[Flatten[{opts}], jiraPaginate]];

Options[JiraBoardSprints] = Join[$jiraCommonOptions, {"State" -> None}];
JiraBoardSprints[id_, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "agile", {"board", ToString[id], "sprint"},
    "ItemsKey" -> "values",
    "Parameters" -> Join[
      If[OptionValue["State"] === None, {}, {"state" -> jiraQueryValue[OptionValue["State"]]}],
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraPaginate]];

Options[JiraBoardProjects] = $jiraCommonOptions;
JiraBoardProjects[id_, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "agile", {"board", ToString[id], "project"},
    "ItemsKey" -> "values", jiraFilterOptions[Flatten[{opts}], jiraPaginate]];

Options[JiraBoardVersions] = $jiraCommonOptions;
JiraBoardVersions[id_, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "agile", {"board", ToString[id], "version"},
    "ItemsKey" -> "values", jiraFilterOptions[Flatten[{opts}], jiraPaginate]];


(* ::Section:: *)
(* Sprints *)

Options[JiraSprint] = $jiraCommonOptions;
JiraSprint[id_, opts : OptionsPattern[]] :=
  jiraGet["agile", {"sprint", ToString[id]}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraCreateSprint] = Join[$jiraCommonOptions, {
  "StartDate" -> None, "EndDate" -> None, "Goal" -> None
}];

JiraCreateSprint[boardId_, name_String, opts : OptionsPattern[]] := Module[{body},
  body = <|"name" -> name, "originBoardId" -> boardId|>;
  If[OptionValue["StartDate"] =!= None, body["startDate"] = jiraDateString[OptionValue["StartDate"]]];
  If[OptionValue["EndDate"] =!= None, body["endDate"] = jiraDateString[OptionValue["EndDate"]]];
  If[StringQ[OptionValue["Goal"]], body["goal"] = OptionValue["Goal"]];
  jiraPost["agile", {"sprint"}, "Body" -> body,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]]
];

(* POST is the partial update; PUT replaces the whole sprint. Partial is what
   callers almost always want, so it is the default. *)
Options[JiraUpdateSprint] = Join[$jiraCommonOptions, {"Replace" -> False}];
JiraUpdateSprint[id_, data_Association, opts : OptionsPattern[]] :=
  jiraRequest[If[TrueQ[OptionValue["Replace"]], "PUT", "POST"], "agile",
    {"sprint", ToString[id]}, "Body" -> data,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraDeleteSprint] = $jiraCommonOptions;
JiraDeleteSprint[id_, opts : OptionsPattern[]] :=
  jiraDelete["agile", {"sprint", ToString[id]}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraSprintIssues] = Options[JiraBoardIssues];
JiraSprintIssues[id_, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "agile", {"sprint", ToString[id], "issue"},
    "ItemsKey" -> "issues",
    "Parameters" -> jiraBoardIssueParams[OptionValue["JQL"], OptionValue["Fields"],
      OptionValue["Expand"], OptionValue["Parameters"]],
    jiraFilterOptions[Flatten[{opts}], jiraPaginate]];

Options[JiraMoveIssuesToSprint] = $jiraCommonOptions;

(* The Agile API accepts at most 50 issues per call, so longer lists are sent in
   batches and the individual results returned together. *)
JiraMoveIssuesToSprint[id_, issues : {__String}, opts : OptionsPattern[]] :=
  Map[
    jiraPost["agile", {"sprint", ToString[id], "issue"},
      "Body" -> <|"issues" -> #|>,
      jiraFilterOptions[Flatten[{opts}], jiraRequest]] &,
    Partition[issues, UpTo[$jiraSprintIssueBatch]]
  ];

Options[JiraMoveIssuesToBacklog] = $jiraCommonOptions;
JiraMoveIssuesToBacklog[issues : {__String}, opts : OptionsPattern[]] :=
  Map[
    jiraPost["agile", {"backlog", "issue"},
      "Body" -> <|"issues" -> #|>,
      jiraFilterOptions[Flatten[{opts}], jiraRequest]] &,
    Partition[issues, UpTo[$jiraSprintIssueBatch]]
  ];


(* ::Section:: *)
(* Epics *)

Options[JiraEpic] = $jiraCommonOptions;
JiraEpic[key_String, opts : OptionsPattern[]] :=
  jiraGet["agile", {"epic", key}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

(* The Agile API updates an epic with POST, not PUT. *)
Options[JiraUpdateEpic] = $jiraCommonOptions;
JiraUpdateEpic[key_String, data_Association, opts : OptionsPattern[]] :=
  jiraPost["agile", {"epic", key}, "Body" -> data,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraEpicIssues] = Options[JiraBoardIssues];

JiraEpicIssues[key_String, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "agile", {"epic", key, "issue"},
    "ItemsKey" -> "issues",
    "Parameters" -> jiraBoardIssueParams[OptionValue["JQL"], OptionValue["Fields"],
      OptionValue["Expand"], OptionValue["Parameters"]],
    jiraFilterOptions[Flatten[{opts}], jiraPaginate]];

(* Issues belonging to no epic at all. *)
JiraEpicIssues[None, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "agile", {"epic", "none", "issue"},
    "ItemsKey" -> "issues",
    "Parameters" -> jiraBoardIssueParams[OptionValue["JQL"], OptionValue["Fields"],
      OptionValue["Expand"], OptionValue["Parameters"]],
    jiraFilterOptions[Flatten[{opts}], jiraPaginate]];

Options[JiraMoveIssuesToEpic] = $jiraCommonOptions;

JiraMoveIssuesToEpic[key_String, issues : {__String}, opts : OptionsPattern[]] :=
  jiraPost["agile", {"epic", key, "issue"}, "Body" -> <|"issues" -> issues|>,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

(* Removing issues from their epic is a move to the "none" epic. *)
JiraMoveIssuesToEpic[None, issues : {__String}, opts : OptionsPattern[]] :=
  jiraPost["agile", {"epic", "none", "issue"}, "Body" -> <|"issues" -> issues|>,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraRankEpic] = Join[$jiraCommonOptions, {
  "Before" -> None, "After" -> None, "RankCustomFieldId" -> None
}];

JiraRankEpic[key_String, opts : OptionsPattern[]] := Module[{body},
  body = <||>;
  If[StringQ[OptionValue["Before"]], body["rankBeforeEpic"] = OptionValue["Before"]];
  If[StringQ[OptionValue["After"]],  body["rankAfterEpic"]  = OptionValue["After"]];
  If[OptionValue["RankCustomFieldId"] =!= None,
    body["rankCustomFieldId"] = OptionValue["RankCustomFieldId"]];
  If[Length[body] === 0,
    Message[JiraLink::badopt, "Before", None];
    Return[$Failed]
  ];
  jiraPut["agile", {"epic", key, "rank"}, "Body" -> body,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]]
];


(* ::Section:: *)
(* Ranking issues *)

Options[JiraRankIssues] = Join[$jiraCommonOptions, {
  "Before" -> None, "After" -> None, "RankCustomFieldId" -> None
}];

JiraRankIssues[issues : {__String}, opts : OptionsPattern[]] := Module[{body},
  body = <|"issues" -> issues|>;
  If[StringQ[OptionValue["Before"]], body["rankBeforeIssue"] = OptionValue["Before"]];
  If[StringQ[OptionValue["After"]],  body["rankAfterIssue"]  = OptionValue["After"]];
  If[OptionValue["RankCustomFieldId"] =!= None,
    body["rankCustomFieldId"] = OptionValue["RankCustomFieldId"]];
  If[Length[body] === 1,
    Message[JiraLink::badopt, "Before", None];
    Return[$Failed]
  ];
  jiraPut["agile", {"issue", "rank"}, "Body" -> body,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]]
];
