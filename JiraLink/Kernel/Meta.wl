(* ::Package:: *)

(* Meta.wl -- server information, permissions, filters and dashboards.

   Jira Data Center has no /filter/search and no dashboard create/update/delete;
   both are Cloud only. Filters can be listed as favourites, or fetched by id. *)


Options[JiraServerInfo] = Join[$jiraCommonOptions, {"HealthCheck" -> False}];

JiraServerInfo[opts : OptionsPattern[]] :=
  jiraGet["platform", {"serverInfo"},
    "Parameters" -> Join[
      If[TrueQ[OptionValue["HealthCheck"]], {"doHealthCheck" -> True}, {}],
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


Options[JiraConfiguration] = $jiraCommonOptions;
JiraConfiguration[opts : OptionsPattern[]] :=
  jiraGet["platform", {"configuration"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];


Options[JiraMyPermissions] = $jiraCommonOptions;

JiraMyPermissions[opts : OptionsPattern[]] :=
  jiraGet["platform", {"mypermissions"},
    "Parameters" -> jiraNormalizeQuery[OptionValue["Parameters"]],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

JiraMyPermissions[project_String, opts : OptionsPattern[]] :=
  jiraGet["platform", {"mypermissions"},
    "Parameters" -> Join[
      {If[JiraIssueKeyQ[project], "issueKey", "projectKey"] -> project},
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


Options[JiraPermissions] = $jiraCommonOptions;
JiraPermissions[opts : OptionsPattern[]] :=
  jiraGet["platform", {"permissions"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraApplicationRoles] = $jiraCommonOptions;
JiraApplicationRoles[opts : OptionsPattern[]] :=
  jiraGet["platform", {"applicationrole"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* ::Section:: *)
(* Filters *)

Options[JiraFilters] = $jiraCommonOptions;
JiraFilters[opts : OptionsPattern[]] :=
  jiraGet["platform", {"filter", "favourite"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraFilter] = $jiraCommonOptions;
JiraFilter[id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"filter", ToString[id]},
    "Parameters" -> Join[
      If[OptionValue["Expand"] === None, {}, {"expand" -> jiraQueryValue[OptionValue["Expand"]]}],
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraCreateFilter] = Join[$jiraCommonOptions, {
  "Description" -> None, "Favourite" -> True
}];

JiraCreateFilter[name_String, jql_String, opts : OptionsPattern[]] := Module[{body},
  body = <|"name" -> name, "jql" -> jql, "favourite" -> TrueQ[OptionValue["Favourite"]]|>;
  If[StringQ[OptionValue["Description"]], body["description"] = OptionValue["Description"]];
  jiraPost["platform", {"filter"}, "Body" -> body,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]]
];

Options[JiraUpdateFilter] = $jiraCommonOptions;
JiraUpdateFilter[id_, data_Association, opts : OptionsPattern[]] :=
  jiraPut["platform", {"filter", ToString[id]}, "Body" -> data,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraDeleteFilter] = $jiraCommonOptions;
JiraDeleteFilter[id_, opts : OptionsPattern[]] :=
  jiraDelete["platform", {"filter", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraFilterPermissions] = $jiraCommonOptions;
JiraFilterPermissions[id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"filter", ToString[id], "permission"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* ::Section:: *)
(* Dashboards *)

Options[JiraDashboards] = Join[$jiraCommonOptions, {"Filter" -> None}];

JiraDashboards[opts : OptionsPattern[]] :=
  jiraPaginate["GET", "platform", {"dashboard"},
    "ItemsKey" -> "dashboards",
    "Parameters" -> Join[
      If[StringQ[OptionValue["Filter"]], {"filter" -> OptionValue["Filter"]}, {}],
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraPaginate]];

Options[JiraDashboard] = $jiraCommonOptions;
JiraDashboard[id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"dashboard", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];
