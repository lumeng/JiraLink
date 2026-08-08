(* ::Package:: *)

(* Projects.wl -- projects, versions and components.

   Several of these return a bare JSON array rather than a paged envelope
   (/project, /project/{k}/versions, /project/{k}/components,
   /project/{k}/statuses), which jiraPaginate detects and passes through. *)


Options[JiraProjects] = Join[$jiraCommonOptions, {"Recent" -> None, "IncludeArchived" -> False}];

JiraProjects[opts : OptionsPattern[]] :=
  jiraGet["platform", {"project"},
    "Parameters" -> Join[
      If[OptionValue["Expand"] === None, {}, {"expand" -> jiraQueryValue[OptionValue["Expand"]]}],
      If[OptionValue["Recent"] === None, {}, {"recent" -> OptionValue["Recent"]}],
      If[TrueQ[OptionValue["IncludeArchived"]], {"includeArchived" -> True}, {}],
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


Options[JiraProject] = $jiraCommonOptions;

JiraProject[key_String, opts : OptionsPattern[]] :=
  jiraGet["platform", {"project", key},
    "Parameters" -> Join[
      If[OptionValue["Expand"] === None, {}, {"expand" -> jiraQueryValue[OptionValue["Expand"]]}],
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


Options[JiraCreateProject] = $jiraCommonOptions;
JiraCreateProject[data_Association, opts : OptionsPattern[]] :=
  jiraPost["platform", {"project"}, "Body" -> data,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraUpdateProject] = $jiraCommonOptions;
JiraUpdateProject[key_String, data_Association, opts : OptionsPattern[]] :=
  jiraPut["platform", {"project", key}, "Body" -> data,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraDeleteProject] = $jiraCommonOptions;
JiraDeleteProject[key_String, opts : OptionsPattern[]] :=
  jiraDelete["platform", {"project", key},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


Options[JiraProjectVersions] = Join[$jiraCommonOptions, {"Paginated" -> False}];

JiraProjectVersions[key_String, opts : OptionsPattern[]] :=
  If[TrueQ[OptionValue["Paginated"]],
    (* /project/{k}/version is the paged variant and accepts orderBy. *)
    jiraPaginate["GET", "platform", {"project", key, "version"},
      "ItemsKey" -> "values", jiraFilterOptions[Flatten[{opts}], jiraPaginate]],
    jiraGet["platform", {"project", key, "versions"},
      jiraFilterOptions[Flatten[{opts}], jiraRequest]]
  ];

Options[JiraProjectComponents] = $jiraCommonOptions;
JiraProjectComponents[key_String, opts : OptionsPattern[]] :=
  jiraGet["platform", {"project", key, "components"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraProjectStatuses] = $jiraCommonOptions;
JiraProjectStatuses[key_String, opts : OptionsPattern[]] :=
  jiraGet["platform", {"project", key, "statuses"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraProjectRoles] = $jiraCommonOptions;
JiraProjectRoles[key_String, opts : OptionsPattern[]] :=
  jiraGet["platform", {"project", key, "role"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];
JiraProjectRoles[key_String, id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"project", key, "role", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraProjectCategories] = $jiraCommonOptions;
JiraProjectCategories[opts : OptionsPattern[]] :=
  jiraGet["platform", {"projectCategory"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraProjectProperties] = $jiraCommonOptions;
JiraProjectProperties[key_String, opts : OptionsPattern[]] :=
  jiraGet["platform", {"project", key, "properties"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* ::Section:: *)
(* Versions *)

Options[JiraVersion] = $jiraCommonOptions;
JiraVersion[id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"version", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraCreateVersion] = Join[$jiraCommonOptions, {
  "Description" -> None, "StartDate" -> None, "ReleaseDate" -> None, "Released" -> None
}];

JiraCreateVersion[project_String, name_String, opts : OptionsPattern[]] := Module[{body},
  body = <|"name" -> name, "project" -> project|>;
  If[StringQ[OptionValue["Description"]], body["description"] = OptionValue["Description"]];
  If[OptionValue["StartDate"] =!= None,
    body["startDate"] = DateString[OptionValue["StartDate"], {"Year", "-", "Month", "-", "Day"}]];
  If[OptionValue["ReleaseDate"] =!= None,
    body["releaseDate"] = DateString[OptionValue["ReleaseDate"], {"Year", "-", "Month", "-", "Day"}]];
  If[BooleanQ[OptionValue["Released"]], body["released"] = OptionValue["Released"]];
  jiraPost["platform", {"version"}, "Body" -> body,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]]
];

Options[JiraUpdateVersion] = $jiraCommonOptions;
JiraUpdateVersion[id_, data_Association, opts : OptionsPattern[]] :=
  jiraPut["platform", {"version", ToString[id]}, "Body" -> data,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraDeleteVersion] = $jiraCommonOptions;
JiraDeleteVersion[id_, opts : OptionsPattern[]] :=
  jiraDelete["platform", {"version", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraVersionIssueCounts] = $jiraCommonOptions;
JiraVersionIssueCounts[id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"version", ToString[id], "relatedIssueCounts"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraVersionUnresolvedCount] = $jiraCommonOptions;
JiraVersionUnresolvedCount[id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"version", ToString[id], "unresolvedIssueCount"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* ::Section:: *)
(* Components *)

Options[JiraComponent] = $jiraCommonOptions;
JiraComponent[id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"component", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraCreateComponent] = Join[$jiraCommonOptions, {
  "Description" -> None, "Lead" -> None, "AssigneeType" -> None
}];

JiraCreateComponent[project_String, name_String, opts : OptionsPattern[]] := Module[{body},
  body = <|"name" -> name, "project" -> project|>;
  If[StringQ[OptionValue["Description"]], body["description"] = OptionValue["Description"]];
  If[StringQ[OptionValue["Lead"]], body["leadUserName"] = OptionValue["Lead"]];
  If[StringQ[OptionValue["AssigneeType"]], body["assigneeType"] = OptionValue["AssigneeType"]];
  jiraPost["platform", {"component"}, "Body" -> body,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]]
];

Options[JiraUpdateComponent] = $jiraCommonOptions;
JiraUpdateComponent[id_, data_Association, opts : OptionsPattern[]] :=
  jiraPut["platform", {"component", ToString[id]}, "Body" -> data,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraDeleteComponent] = $jiraCommonOptions;
JiraDeleteComponent[id_, opts : OptionsPattern[]] :=
  jiraDelete["platform", {"component", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraComponentIssueCount] = $jiraCommonOptions;
JiraComponentIssueCount[id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"component", ToString[id], "relatedIssueCounts"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];
