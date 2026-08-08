(* ::Package:: *)

(* Issues.wl -- issues and everything hanging off them. *)


(* Shared helper: turn the "Fields" and "Expand" options into query parameters.
   Jira accepts a comma-separated list, and supports *all, *navigable and
   negation such as *all,-comment. *)
jiraFieldParams[fields_, expand_] := Join[
  If[fields === Automatic || fields === None, {},
    {"fields" -> jiraQueryValue[fields]}],
  If[expand === None || expand === Automatic, {},
    {"expand" -> jiraQueryValue[expand]}]
];


(* ::Section:: *)
(* Reading issues *)

Options[JiraIssue] = Join[$jiraCommonOptions, {"Properties" -> None}];

JiraIssue[key_, opts : OptionsPattern[]] := Module[{ref},
  ref = jiraIssueRef[key];
  If[ref === $Failed, Return[$Failed]];
  jiraGet["platform", {"issue", ref},
    "Parameters" -> Join[
      jiraFieldParams[OptionValue["Fields"], OptionValue["Expand"]],
      If[OptionValue["Properties"] === None, {},
        {"properties" -> jiraQueryValue[OptionValue["Properties"]]}],
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]
  ]
];


Options[JiraIssueData] = Options[JiraIssue];

JiraIssueData[key_, opts : OptionsPattern[]] := JiraIssueData[key, All, opts];

JiraIssueData[key_, field : (_String | All | {___String}), opts : OptionsPattern[]] := Module[
  {requested, res, fields},

  (* Ask the server only for what was requested, rather than fetching every
     field and discarding most of it. *)
  requested = Which[
    OptionValue["Fields"] =!= Automatic, OptionValue["Fields"],
    field === All,                       Automatic,
    True,                                Flatten[{field}]
  ];

  res = JiraIssue[key,
    "Fields" -> requested,
    jiraFilterOptions[DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Fields", _]], JiraIssue]
  ];
  If[FailureQ[res] || res === $Failed, Return[res]];

  fields = Lookup[res, "fields", Missing["NotAvailable"]];
  If[!AssociationQ[fields], Return[fields]];

  Switch[field,
    All,        fields,
    _String,    Lookup[fields, field, Missing["NotAvailable"]],
    {___String}, KeyTake[fields, field]
  ]
];


(* ::Section:: *)
(* Creating issues *)

Options[JiraCreateIssue] = Join[$jiraCommonOptions, {
  "Priority"       -> Automatic,
  "OpenQ"          -> False,
  "UpdateHistory"  -> False
}];

JiraCreateIssue[project_String, summary_String, opts : OptionsPattern[]] :=
  JiraCreateIssue[project, summary, "Task", <||>, opts];

JiraCreateIssue[project_String, summary_String, issueType_String, opts : OptionsPattern[]] :=
  JiraCreateIssue[project, summary, issueType, <||>, opts];

JiraCreateIssue[projectOrParent_String, summary_String, issueType_String,
  moreFields_Association, opts : OptionsPattern[]] := Module[
  {subtaskQ, projectKey, fields, res, key},

  subtaskQ = MemberQ[{"subtask", "sub-task"}, ToLowerCase[issueType]];

  (* For a subtask the first argument is the parent issue key, and the project
     is implied by it. *)
  projectKey = If[subtaskQ, projectKeyFromIssueKey[projectOrParent], projectOrParent];

  fields = <|
    "project"   -> <|"key" -> projectKey|>,
    "summary"   -> summary,
    "issuetype" -> <|"name" -> If[subtaskQ, "Sub-task", issueType]|>
  |>;

  If[subtaskQ, fields["parent"] = <|"key" -> projectOrParent|>];

  (* The pre-1.0 code always sent priority "Major", which fails outright on any
     instance whose priority scheme has no such value. It is now opt-in. *)
  If[StringQ[OptionValue["Priority"]],
    fields["priority"] = <|"name" -> OptionValue["Priority"]|>
  ];

  fields = Join[fields, moreFields];

  res = jiraPost["platform", {"issue"},
    "Body" -> <|"fields" -> fields|>,
    "Parameters" -> Join[
      {"updateHistory" -> OptionValue["UpdateHistory"]},
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]
  ];

  If[FailureQ[res] || res === $Failed, Return[res]];

  (* The pre-1.0 code tested the result against {__Rule}, which never matched
     the association actually returned, so "OpenQ" -> True silently did nothing. *)
  If[TrueQ[OptionValue["OpenQ"]],
    key = Lookup[res, "key", None];
    If[StringQ[key], JiraIssueOpen[key, jiraFilterOptions[Flatten[{opts}], JiraIssueOpen]]]
  ];

  res
];


Options[JiraCreateIssues] = $jiraCommonOptions;

JiraCreateIssues[specs : {__Association}, opts : OptionsPattern[]] :=
  jiraPost["platform", {"issue", "bulk"},
    "Body" -> <|"issueUpdates" -> (If[KeyExistsQ[#, "fields"], #, <|"fields" -> #|>] & /@ specs)|>,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* ::Section:: *)
(* Updating and deleting *)

Options[JiraUpdateIssue] = Join[$jiraCommonOptions, {
  "Update"      -> None,
  "NotifyUsers" -> True
}];

JiraUpdateIssue[key_, fields_Association, opts : OptionsPattern[]] := Module[{ref, body},
  ref = jiraIssueRef[key];
  If[ref === $Failed, Return[$Failed]];
  body = <|"fields" -> fields|>;
  If[AssociationQ[OptionValue["Update"]], body["update"] = OptionValue["Update"]];
  jiraPut["platform", {"issue", ref},
    "Body" -> body,
    "Parameters" -> Join[
      {"notifyUsers" -> OptionValue["NotifyUsers"]},
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]
  ]
];


Options[JiraDeleteIssue] = Join[$jiraCommonOptions, {"DeleteSubtasks" -> False}];

JiraDeleteIssue[key_, opts : OptionsPattern[]] := Module[{ref, del},
  ref = jiraIssueRef[key];
  If[ref === $Failed, Return[$Failed]];

  del = Replace[OptionValue["DeleteSubtasks"], {
    True | "true" | "True"    -> True,
    False | "false" | "False" -> False,
    other_ :> (Message[JiraLink::badopt, "DeleteSubtasks", other]; False)
  }];

  jiraDelete["platform", {"issue", ref},
    "Parameters" -> Join[
      {"deleteSubtasks" -> del},
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]
  ]
];


Options[JiraAssignIssue] = $jiraCommonOptions;

(* Jira Data Center identifies users by name. "-1" asks for the project's
   automatic assignee; a null name leaves the issue unassigned. *)
JiraAssignIssue[key_, user : (_String | Automatic | None), opts : OptionsPattern[]] := Module[{ref},
  ref = jiraIssueRef[key];
  If[ref === $Failed, Return[$Failed]];
  jiraPut["platform", {"issue", ref, "assignee"},
    "Body" -> <|"name" -> Replace[user, {Automatic -> "-1", None -> Null}]|>,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]
  ]
];


(* ::Section:: *)
(* Transitions *)

Options[JiraTransitions] = $jiraCommonOptions;

JiraTransitions[key_, opts : OptionsPattern[]] := Module[{ref},
  ref = jiraIssueRef[key];
  If[ref === $Failed, Return[$Failed]];
  jiraExtractItems[
    jiraGet["platform", {"issue", ref, "transitions"},
      "Parameters" -> Join[
        (* Transition screen fields are omitted unless asked for by name. *)
        If[OptionValue["Expand"] === None, {}, {"expand" -> jiraQueryValue[OptionValue["Expand"]]}],
        jiraNormalizeQuery[OptionValue["Parameters"]]
      ],
      jiraFilterOptions[Flatten[{opts}], jiraRequest]
    ],
    "transitions"
  ]
];


Options[JiraTransitionIssue] = Join[$jiraCommonOptions, {"Update" -> None}];

JiraTransitionIssue[key_, transition_, opts : OptionsPattern[]] :=
  JiraTransitionIssue[key, transition, <||>, opts];

JiraTransitionIssue[key_, transition_, fields_Association, opts : OptionsPattern[]] := Module[
  {ref, id, available, match, body},

  ref = jiraIssueRef[key];
  If[ref === $Failed, Return[$Failed]];

  (* Accept a transition name as well as an id: names are what people actually
     know, ids are what the API wants. *)
  id = Which[
    IntegerQ[transition], ToString[transition],
    StringQ[transition] && StringMatchQ[transition, DigitCharacter ..], transition,
    StringQ[transition],
      available = JiraTransitions[key, jiraFilterOptions[Flatten[{opts}], JiraTransitions]];
      If[FailureQ[available], Return[available]];
      match = SelectFirst[available,
        ToLowerCase[Lookup[#, "name", ""]] === ToLowerCase[transition] &, None];
      If[match === None,
        Message[JiraLink::badopt, "Transition", transition];
        Return[$Failed]
      ];
      ToString[match["id"]],
    True, Return[$Failed]
  ];

  body = <|"transition" -> <|"id" -> id|>|>;
  If[Length[fields] > 0, body["fields"] = fields];
  If[AssociationQ[OptionValue["Update"]], body["update"] = OptionValue["Update"]];

  jiraPost["platform", {"issue", ref, "transitions"},
    "Body" -> body,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]
  ]
];


(* ::Section:: *)
(* Comments *)

Options[JiraComments] = $jiraCommonOptions;

JiraComments[key_, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "platform", {"issue", jiraIssueRef[key], "comment"},
    "ItemsKey" -> "comments",
    jiraFilterOptions[Flatten[{opts}], jiraPaginate]];

Options[JiraComment] = $jiraCommonOptions;

JiraComment[key_, id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"issue", jiraIssueRef[key], "comment", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraAddComment] = Join[$jiraCommonOptions, {"Visibility" -> None}];

JiraAddComment[key_, text_String, opts : OptionsPattern[]] := Module[{body},
  body = <|"body" -> text|>;
  If[AssociationQ[OptionValue["Visibility"]], body["visibility"] = OptionValue["Visibility"]];
  jiraPost["platform", {"issue", jiraIssueRef[key], "comment"},
    "Body" -> body, jiraFilterOptions[Flatten[{opts}], jiraRequest]]
];

Options[JiraUpdateComment] = Join[$jiraCommonOptions, {"Visibility" -> None}];

JiraUpdateComment[key_, id_, text_String, opts : OptionsPattern[]] := Module[{body},
  body = <|"body" -> text|>;
  If[AssociationQ[OptionValue["Visibility"]], body["visibility"] = OptionValue["Visibility"]];
  jiraPut["platform", {"issue", jiraIssueRef[key], "comment", ToString[id]},
    "Body" -> body, jiraFilterOptions[Flatten[{opts}], jiraRequest]]
];

Options[JiraDeleteComment] = $jiraCommonOptions;

JiraDeleteComment[key_, id_, opts : OptionsPattern[]] :=
  jiraDelete["platform", {"issue", jiraIssueRef[key], "comment", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* ::Section:: *)
(* Work logs *)

Options[JiraWorklogs] = $jiraCommonOptions;

JiraWorklogs[key_, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "platform", {"issue", jiraIssueRef[key], "worklog"},
    "ItemsKey" -> "worklogs",
    jiraFilterOptions[Flatten[{opts}], jiraPaginate]];

Options[JiraAddWorklog] = Join[$jiraCommonOptions, {
  "Comment"        -> None,
  "Started"        -> None,
  "AdjustEstimate" -> Automatic,
  "NewEstimate"    -> None,
  "ReduceBy"       -> None
}];

JiraAddWorklog[key_, timeSpent_, opts : OptionsPattern[]] := Module[{body, spent, params},
  spent = jiraTimeSpent[timeSpent];
  If[spent === $Failed, Return[$Failed]];

  body = spent;
  If[StringQ[OptionValue["Comment"]], body["comment"] = OptionValue["Comment"]];
  If[OptionValue["Started"] =!= None, body["started"] = jiraDateString[OptionValue["Started"]]];

  params = Join[
    If[OptionValue["AdjustEstimate"] === Automatic, {},
      {"adjustEstimate" -> OptionValue["AdjustEstimate"]}],
    If[OptionValue["NewEstimate"] === None, {}, {"newEstimate" -> OptionValue["NewEstimate"]}],
    If[OptionValue["ReduceBy"] === None, {}, {"reduceBy" -> OptionValue["ReduceBy"]}],
    jiraNormalizeQuery[OptionValue["Parameters"]]
  ];

  jiraPost["platform", {"issue", jiraIssueRef[key], "worklog"},
    "Body" -> body, "Parameters" -> params,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]]
];

Options[JiraUpdateWorklog] = $jiraCommonOptions;

JiraUpdateWorklog[key_, id_, data_Association, opts : OptionsPattern[]] :=
  jiraPut["platform", {"issue", jiraIssueRef[key], "worklog", ToString[id]},
    "Body" -> data, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraDeleteWorklog] = Join[$jiraCommonOptions, {"AdjustEstimate" -> Automatic}];

JiraDeleteWorklog[key_, id_, opts : OptionsPattern[]] :=
  jiraDelete["platform", {"issue", jiraIssueRef[key], "worklog", ToString[id]},
    "Parameters" -> Join[
      If[OptionValue["AdjustEstimate"] === Automatic, {},
        {"adjustEstimate" -> OptionValue["AdjustEstimate"]}],
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* ::Section:: *)
(* Attachments *)

Options[JiraAddAttachment] = $jiraCommonOptions;

JiraAddAttachment[key_, file_String, opts : OptionsPattern[]] :=
  JiraAddAttachment[key, {file}, opts];

JiraAddAttachment[key_, files : {__String}, opts : OptionsPattern[]] := Module[{missing},
  missing = Select[files, !FileExistsQ[#] &];
  If[missing =!= {},
    Message[JiraLink::nofile, First[missing]];
    Return[$Failed]
  ];
  (* The form field must be named "file", repeated once per attachment, and the
     request must carry X-Atlassian-Token: no-check -- jiraBuildRequest adds
     that header whenever "Multipart" is set. *)
  jiraRequest["POST", "platform", {"issue", jiraIssueRef[key], "attachments"},
    "Multipart" -> (("file" -> File[#]) & /@ files),
    jiraFilterOptions[Flatten[{opts}], jiraRequest]
  ]
];

Options[JiraAttachment] = $jiraCommonOptions;

JiraAttachment[id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"attachment", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraDeleteAttachment] = $jiraCommonOptions;

JiraDeleteAttachment[id_, opts : OptionsPattern[]] :=
  jiraDelete["platform", {"attachment", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraAttachmentSettings] = $jiraCommonOptions;

JiraAttachmentSettings[opts : OptionsPattern[]] :=
  jiraGet["platform", {"attachment", "meta"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraAttachmentDownload] = $jiraCommonOptions;

(* Attachment bytes are not served from /rest/api/2; the metadata carries a
   "content" URL under /secure/attachment, which needs the same credentials. *)
JiraAttachmentDownload[id_, file_String, opts : OptionsPattern[]] := Module[
  {meta, url, conn, resp},

  meta = JiraAttachment[id, jiraFilterOptions[Flatten[{opts}], JiraAttachment]];
  If[FailureQ[meta] || !AssociationQ[meta], Return[meta]];

  url = Lookup[meta, "content", None];
  If[!StringQ[url], Return[$Failed]];

  conn = jiraConnection[OptionValue["Connection"]];
  If[conn === $Failed, Return[$Failed]];

  resp = URLRead[
    HTTPRequest[url, <|"Headers" -> jiraAuthHeaders[conn]|>],
    Interactive -> False
  ];
  If[!MatchQ[resp, _HTTPResponse] || resp["StatusCode"] =!= 200,
    Return[jiraStatusFailure[
      If[MatchQ[resp, _HTTPResponse], resp["StatusCode"], 0], "GET", url,
      If[MatchQ[resp, _HTTPResponse], resp["Body"], ""]]]
  ];

  Export[file, resp["BodyByteArray"], "Binary"]
];


(* ::Section:: *)
(* Links *)

Options[JiraRemoteLinks] = $jiraCommonOptions;

JiraRemoteLinks[key_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"issue", jiraIssueRef[key], "remotelink"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraAddRemoteLink] = Join[$jiraCommonOptions, {"GlobalId" -> None, "Relationship" -> None}];

JiraAddRemoteLink[key_, url_String, title_String, opts : OptionsPattern[]] := Module[{body},
  body = <|"object" -> <|"url" -> url, "title" -> title|>|>;
  If[StringQ[OptionValue["GlobalId"]], body["globalId"] = OptionValue["GlobalId"]];
  If[StringQ[OptionValue["Relationship"]], body["relationship"] = OptionValue["Relationship"]];
  jiraPost["platform", {"issue", jiraIssueRef[key], "remotelink"},
    "Body" -> body, jiraFilterOptions[Flatten[{opts}], jiraRequest]]
];

Options[JiraDeleteRemoteLink] = $jiraCommonOptions;

JiraDeleteRemoteLink[key_, id_, opts : OptionsPattern[]] :=
  jiraDelete["platform", {"issue", jiraIssueRef[key], "remotelink", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraCreateIssueLink] = Join[$jiraCommonOptions, {"Comment" -> None}];

JiraCreateIssueLink[inward_String, type_String, outward_String, opts : OptionsPattern[]] := Module[{body},
  body = <|
    "type"         -> <|"name" -> type|>,
    "inwardIssue"  -> <|"key" -> inward|>,
    "outwardIssue" -> <|"key" -> outward|>
  |>;
  If[StringQ[OptionValue["Comment"]], body["comment"] = <|"body" -> OptionValue["Comment"]|>];
  jiraPost["platform", {"issueLink"}, "Body" -> body,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]]
];

Options[JiraIssueLink] = $jiraCommonOptions;

JiraIssueLink[id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"issueLink", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraDeleteIssueLink] = $jiraCommonOptions;

JiraDeleteIssueLink[id_, opts : OptionsPattern[]] :=
  jiraDelete["platform", {"issueLink", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* ::Section:: *)
(* Votes and watchers *)

Options[JiraVotes] = $jiraCommonOptions;
JiraVotes[key_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"issue", jiraIssueRef[key], "votes"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraAddVote] = $jiraCommonOptions;
JiraAddVote[key_, opts : OptionsPattern[]] :=
  jiraPost["platform", {"issue", jiraIssueRef[key], "votes"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraDeleteVote] = $jiraCommonOptions;
JiraDeleteVote[key_, opts : OptionsPattern[]] :=
  jiraDelete["platform", {"issue", jiraIssueRef[key], "votes"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraWatchers] = $jiraCommonOptions;
JiraWatchers[key_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"issue", jiraIssueRef[key], "watchers"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraAddWatcher] = $jiraCommonOptions;
(* The body here is a bare JSON string, not an object -- an oddity of this
   endpoint. *)
JiraAddWatcher[key_, username_String, opts : OptionsPattern[]] :=
  jiraPost["platform", {"issue", jiraIssueRef[key], "watchers"},
    "Body" -> username, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraDeleteWatcher] = $jiraCommonOptions;
JiraDeleteWatcher[key_, username_String, opts : OptionsPattern[]] :=
  jiraDelete["platform", {"issue", jiraIssueRef[key], "watchers"},
    "Parameters" -> {"username" -> username},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* ::Section:: *)
(* Notification *)

Options[JiraNotify] = Join[$jiraCommonOptions, {
  "To"       -> <|"watchers" -> True|>,
  "Restrict" -> None,
  "HTML"     -> False
}];

JiraNotify[key_, subject_String, text_String, opts : OptionsPattern[]] := Module[{body},
  body = <|
    "subject" -> subject,
    If[TrueQ[OptionValue["HTML"]], "htmlBody" -> text, "textBody" -> text],
    "to" -> OptionValue["To"]
  |>;
  If[AssociationQ[OptionValue["Restrict"]], body["restrict"] = OptionValue["Restrict"]];
  jiraPost["platform", {"issue", jiraIssueRef[key], "notify"},
    "Body" -> body, jiraFilterOptions[Flatten[{opts}], jiraRequest]]
];


(* ::Section:: *)
(* Change history

   Jira Data Center has no /issue/{key}/changelog resource -- that is Cloud
   only. History comes back inside the issue when changelog is expanded. *)

Options[JiraIssueChangelog] = $jiraCommonOptions;

JiraIssueChangelog[key_, opts : OptionsPattern[]] := Module[{res},
  res = JiraIssue[key, "Expand" -> "changelog", "Fields" -> "summary",
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Expand" | "Fields", _]],
      JiraIssue]];
  If[FailureQ[res] || !AssociationQ[res], Return[res]];
  Lookup[Lookup[res, "changelog", <||>], "histories", {}]
];


(* ::Section:: *)
(* Entity properties *)

Options[JiraIssueProperties] = $jiraCommonOptions;
JiraIssueProperties[key_, opts : OptionsPattern[]] :=
  jiraExtractItems[
    jiraGet["platform", {"issue", jiraIssueRef[key], "properties"},
      jiraFilterOptions[Flatten[{opts}], jiraRequest]],
    "keys"];

Options[JiraIssueProperty] = $jiraCommonOptions;
JiraIssueProperty[key_, prop_String, opts : OptionsPattern[]] :=
  jiraGet["platform", {"issue", jiraIssueRef[key], "properties", prop},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraSetIssueProperty] = $jiraCommonOptions;
JiraSetIssueProperty[key_, prop_String, value_, opts : OptionsPattern[]] :=
  jiraPut["platform", {"issue", jiraIssueRef[key], "properties", prop},
    "Body" -> value, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraDeleteIssueProperty] = $jiraCommonOptions;
JiraDeleteIssueProperty[key_, prop_String, opts : OptionsPattern[]] :=
  jiraDelete["platform", {"issue", jiraIssueRef[key], "properties", prop},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* ::Section:: *)
(* Field metadata *)

Options[JiraEditMeta] = $jiraCommonOptions;

JiraEditMeta[key_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"issue", jiraIssueRef[key], "editmeta"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


Options[JiraCreateMeta] = $jiraCommonOptions;

(* Jira 9 removed the server-wide /issue/createmeta resource (deprecated in 8.4,
   disabled in 8.9, gone in 9.0). Only the per-project forms remain. *)
JiraCreateMeta[project_String, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "platform", {"issue", "createmeta", project, "issuetypes"},
    "ItemsKey" -> "values", jiraFilterOptions[Flatten[{opts}], jiraPaginate]];

JiraCreateMeta[project_String, issueTypeId_, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "platform",
    {"issue", "createmeta", project, "issuetypes", ToString[issueTypeId]},
    "ItemsKey" -> "values", jiraFilterOptions[Flatten[{opts}], jiraPaginate]];


(* ::Section:: *)
(* Archiving (Data Center) *)

Options[JiraArchiveIssue] = $jiraCommonOptions;
JiraArchiveIssue[key_, opts : OptionsPattern[]] :=
  jiraPut["platform", {"issue", jiraIssueRef[key], "archive"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraRestoreIssue] = $jiraCommonOptions;
JiraRestoreIssue[key_, opts : OptionsPattern[]] :=
  jiraPut["platform", {"issue", jiraIssueRef[key], "restore"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* ::Section:: *)
(* Browser URLs *)

Options[JiraIssueURL] = $jiraCommonOptions;

JiraIssueURL[key_String, opts : OptionsPattern[]] := Module[{conn},
  If[!JiraIssueKeyQ[key], Message[JiraLink::badkey, key]; Return[$Failed]];
  conn = jiraConnection[OptionValue["Connection"]];
  If[conn === $Failed, Return[$Failed]];
  URLBuild[<|
    "Scheme" -> conn["Scheme"],
    "Domain" -> conn["Domain"],
    "Port"   -> conn["Port"],
    "Path"   -> Join[{""}, Lookup[conn, "ContextPath", {}], {"browse", key}]
  |>]
];

Options[JiraIssueOpen] = $jiraCommonOptions;

JiraIssueOpen[key_String, opts : OptionsPattern[]] := Module[{url},
  url = JiraIssueURL[key, jiraFilterOptions[Flatten[{opts}], JiraIssueURL]];
  If[StringQ[url], SystemOpen[url]; url, url]
];
