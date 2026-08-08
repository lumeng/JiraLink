(* ::Package:: *)

(* Admin.wl -- administrative endpoints.

   Nearly all of these need Jira administrator rights, and some need Jira Data
   Center rather than Server. Calling one without the necessary permission gives
   a JiraAuthenticationError carrying status 403, which is the intended
   behaviour: the wrapper exists so that an administrator can use it, and fails
   informatively for everyone else. *)


Options[JiraWorkflows] = Join[$jiraCommonOptions, {"Name" -> None}];
JiraWorkflows[opts : OptionsPattern[]] :=
  jiraGet["platform", {"workflow"},
    "Parameters" -> Join[
      If[StringQ[OptionValue["Name"]], {"workflowName" -> OptionValue["Name"]}, {}],
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraWorkflowScheme] = $jiraCommonOptions;
JiraWorkflowScheme[id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"workflowscheme", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


Options[JiraNotificationScheme] = $jiraCommonOptions;

JiraNotificationScheme[opts : OptionsPattern[]] :=
  jiraPaginate["GET", "platform", {"notificationscheme"},
    "ItemsKey" -> "values",
    (* This endpoint caps pages at 50. *)
    "PageSize" -> Replace[OptionValue["PageSize"], Automatic -> 50],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["PageSize", _]], jiraPaginate]];

JiraNotificationScheme[id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"notificationscheme", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


Options[JiraPermissionScheme] = $jiraCommonOptions;

JiraPermissionScheme[opts : OptionsPattern[]] :=
  jiraExtractItems[
    jiraGet["platform", {"permissionscheme"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]],
    "permissionSchemes"];

JiraPermissionScheme[id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"permissionscheme", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


Options[JiraPrioritySchemes] = $jiraCommonOptions;
JiraPrioritySchemes[opts : OptionsPattern[]] :=
  jiraPaginate["GET", "platform", {"priorityschemes"},
    "ItemsKey" -> "schemes", jiraFilterOptions[Flatten[{opts}], jiraPaginate]];


Options[JiraScreens] = $jiraCommonOptions;
JiraScreens[opts : OptionsPattern[]] :=
  jiraPaginate["GET", "platform", {"screens"},
    "ItemsKey" -> "values", jiraFilterOptions[Flatten[{opts}], jiraPaginate]];

Options[JiraScreenAvailableFields] = $jiraCommonOptions;
JiraScreenAvailableFields[id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"screens", ToString[id], "availableFields"},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


Options[JiraSecurityLevel] = $jiraCommonOptions;
JiraSecurityLevel[id_, opts : OptionsPattern[]] :=
  jiraGet["platform", {"securitylevel", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* ::Section:: *)
(* Audit log

   Marked deprecated in 9.12 but still present. It pages with offset/limit
   rather than startAt/maxResults, so it does not go through jiraPaginate. *)

Options[JiraAuditRecords] = Join[$jiraCommonOptions, {
  "Offset" -> 0, "Limit" -> 1000, "Filter" -> None, "From" -> None, "To" -> None
}];

JiraAuditRecords[opts : OptionsPattern[]] :=
  jiraGet["platform", {"auditing", "record"},
    "Parameters" -> Join[
      {"offset" -> OptionValue["Offset"], "limit" -> OptionValue["Limit"]},
      If[StringQ[OptionValue["Filter"]], {"filter" -> OptionValue["Filter"]}, {}],
      If[OptionValue["From"] === None, {}, {"from" -> jiraDateString[OptionValue["From"]]}],
      If[OptionValue["To"] === None, {}, {"to" -> jiraDateString[OptionValue["To"]]}],
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* ::Section:: *)
(* Advanced settings *)

Options[JiraApplicationProperties] = Join[$jiraCommonOptions, {"KeyFilter" -> None}];

JiraApplicationProperties[opts : OptionsPattern[]] :=
  jiraGet["platform", {"application-properties"},
    "Parameters" -> Join[
      If[StringQ[OptionValue["KeyFilter"]], {"keyFilter" -> OptionValue["KeyFilter"]}, {}],
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraSetApplicationProperty] = $jiraCommonOptions;
JiraSetApplicationProperty[id_String, value_, opts : OptionsPattern[]] :=
  jiraPut["platform", {"application-properties", id},
    "Body" -> <|"id" -> id, "value" -> jiraQueryValue[value]|>,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* ::Section:: *)
(* Indexing *)

Options[JiraReindex] = Join[$jiraCommonOptions, {
  "Type"                -> "BACKGROUND_PREFERRED",
  "IndexComments"       -> False,
  "IndexChangeHistory"  -> False,
  "IndexWorklogs"       -> False
}];

JiraReindex[opts : OptionsPattern[]] :=
  jiraPost["platform", {"reindex"},
    "Parameters" -> Join[
      {"type"               -> OptionValue["Type"],
       "indexComments"      -> OptionValue["IndexComments"],
       "indexChangeHistory" -> OptionValue["IndexChangeHistory"],
       "indexWorklogs"      -> OptionValue["IndexWorklogs"]},
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraReindexStatus] = $jiraCommonOptions;
JiraReindexStatus[opts : OptionsPattern[]] :=
  jiraGet["platform", {"reindex"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraReindexIssues] = $jiraCommonOptions;
JiraReindexIssues[ids_List, opts : OptionsPattern[]] :=
  jiraPost["platform", {"reindex", "issue"},
    "Parameters" -> Join[
      {"issueId" -> ids},
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraIndexSnapshot] = $jiraCommonOptions;
JiraIndexSnapshot[opts : OptionsPattern[]] :=
  jiraGet["platform", {"index-snapshot"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* ::Section:: *)
(* Clustering (Data Center) *)

Options[JiraClusterNodes] = $jiraCommonOptions;
JiraClusterNodes[opts : OptionsPattern[]] :=
  jiraGet["platform", {"cluster", "nodes"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* ::Section:: *)
(* WebSudo and upgrades

   There is no REST endpoint to acquire WebSudo -- only to release it. *)

Options[JiraReleaseWebSudo] = $jiraCommonOptions;
JiraReleaseWebSudo[opts : OptionsPattern[]] :=
  jiraDelete["auth", {"websudo"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraUpgradeStatus] = $jiraCommonOptions;
JiraUpgradeStatus[opts : OptionsPattern[]] :=
  jiraGet["platform", {"upgrade"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraRunUpgrade] = $jiraCommonOptions;
JiraRunUpgrade[opts : OptionsPattern[]] :=
  jiraPost["platform", {"upgrade"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];
