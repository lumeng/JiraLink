(* ::Package:: *)

(* Fields.wl -- fields, issue types and the other server-wide constants.

   Most of these return bare JSON arrays. Note that Jira Data Center offers no
   way to list or edit the options of a custom field: the /field/{id}/context
   family exists only in Jira Cloud. A single option can be fetched by id. *)


Options[JiraFields] = $jiraCommonOptions;
JiraFields[opts : OptionsPattern[]] :=
  jiraGet["platform", {"field"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];


Options[JiraCustomFields] = Join[$jiraCommonOptions, {"Search" -> None}];

JiraCustomFields[opts : OptionsPattern[]] := Module[{res},
  res = jiraGet["platform", {"customFields"},
    "Parameters" -> Join[
      If[StringQ[OptionValue["Search"]], {"search" -> OptionValue["Search"]}, {}],
      (* This endpoint numbers pages from 1, unlike every other one. *)
      {"startAt" -> 1, "maxResults" -> 1000},
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

  (* Fall back to filtering the full field list if this instance lacks the
     dedicated endpoint. *)
  If[FailureQ[res],
    With[{all = JiraFields[jiraFilterOptions[Flatten[{opts}], JiraFields]]},
      If[ListQ[all], Select[all, TrueQ[Lookup[#, "custom", False]] &], res]
    ],
    jiraExtractItems[res, Automatic]
  ]
];


Options[JiraCreateCustomField] = $jiraCommonOptions;
JiraCreateCustomField[data_Association, opts : OptionsPattern[]] :=
  jiraPost["platform", {"field"}, "Body" -> data,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraCustomFieldOption] = $jiraCommonOptions;
JiraCustomFieldOption[id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"customFieldOption", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


Options[JiraIssueTypes] = $jiraCommonOptions;
JiraIssueTypes[opts : OptionsPattern[]] :=
  jiraGet["platform", {"issuetype"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraIssueType] = $jiraCommonOptions;
JiraIssueType[id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"issuetype", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraPriorities] = $jiraCommonOptions;
JiraPriorities[opts : OptionsPattern[]] :=
  jiraGet["platform", {"priority"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraResolutions] = $jiraCommonOptions;
JiraResolutions[opts : OptionsPattern[]] :=
  jiraGet["platform", {"resolution"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraStatuses] = $jiraCommonOptions;
JiraStatuses[opts : OptionsPattern[]] :=
  jiraGet["platform", {"status"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraStatusCategories] = $jiraCommonOptions;
JiraStatusCategories[opts : OptionsPattern[]] :=
  jiraGet["platform", {"statuscategory"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraIssueLinkTypes] = $jiraCommonOptions;
JiraIssueLinkTypes[opts : OptionsPattern[]] :=
  jiraExtractItems[
    jiraGet["platform", {"issueLinkType"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]],
    "issueLinkTypes"];

Options[JiraSecuritySchemes] = $jiraCommonOptions;
JiraSecuritySchemes[opts : OptionsPattern[]] :=
  jiraGet["platform", {"issuesecurityschemes"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];
