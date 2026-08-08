(* ::Package:: *)

(* ServiceDesk.wl -- Jira Service Management, under /rest/servicedeskapi.

   This API is not part of Jira Software; it appears only where Jira Service
   Management is installed (the release paired with Jira 9.12 is JSM 5.12). If
   it is absent, requests give a JiraNotFoundError -- JiraServiceDeskInfo is the
   cheapest way to check.

   Several JSM methods are marked experimental and are refused unless the
   request carries X-ExperimentalApi: true, so it is sent on every call here. *)


$jiraServiceDeskHeaders = {"X-ExperimentalApi" -> "true"};

(* Merge the experimental-API header with whatever the caller supplied. *)
jiraSDHeaders[extra_] := Join[$jiraServiceDeskHeaders, jiraNormalizeQuery[extra]];


Options[JiraServiceDeskInfo] = $jiraCommonOptions;
JiraServiceDeskInfo[opts : OptionsPattern[]] :=
  jiraGet["servicedesk", {"info"},
    "Headers" -> jiraSDHeaders[OptionValue["Headers"]],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Headers", _]], jiraRequest]];


Options[JiraServiceDesks] = $jiraCommonOptions;
JiraServiceDesks[opts : OptionsPattern[]] :=
  jiraPaginate["GET", "servicedesk", {"servicedesk"},
    "ItemsKey" -> "values",
    "Headers" -> jiraSDHeaders[OptionValue["Headers"]],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Headers", _]], jiraPaginate]];

Options[JiraServiceDesk] = $jiraCommonOptions;
JiraServiceDesk[id_, opts : OptionsPattern[]] :=
  jiraGet["servicedesk", {"servicedesk", ToString[id]},
    "Headers" -> jiraSDHeaders[OptionValue["Headers"]],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Headers", _]], jiraRequest]];

Options[JiraRequestTypes] = $jiraCommonOptions;
JiraRequestTypes[serviceDeskId_, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "servicedesk", {"servicedesk", ToString[serviceDeskId], "requesttype"},
    "ItemsKey" -> "values",
    "Headers" -> jiraSDHeaders[OptionValue["Headers"]],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Headers", _]], jiraPaginate]];


(* ::Section:: *)
(* Customer requests *)

Options[JiraCustomerRequests] = Join[$jiraCommonOptions, {
  "ServiceDeskId" -> None, "RequestStatus" -> None, "RequestOwnership" -> None
}];

JiraCustomerRequests[opts : OptionsPattern[]] :=
  jiraPaginate["GET", "servicedesk", {"request"},
    "ItemsKey" -> "values",
    "Headers" -> jiraSDHeaders[OptionValue["Headers"]],
    "Parameters" -> Join[
      If[OptionValue["ServiceDeskId"] === None, {},
        {"serviceDeskId" -> OptionValue["ServiceDeskId"]}],
      If[OptionValue["RequestStatus"] === None, {},
        {"requestStatus" -> OptionValue["RequestStatus"]}],
      If[OptionValue["RequestOwnership"] === None, {},
        {"requestOwnership" -> OptionValue["RequestOwnership"]}],
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Headers" | "Parameters", _]],
      jiraPaginate]];

Options[JiraCustomerRequest] = $jiraCommonOptions;
JiraCustomerRequest[id_, opts : OptionsPattern[]] :=
  jiraGet["servicedesk", {"request", ToString[id]},
    "Headers" -> jiraSDHeaders[OptionValue["Headers"]],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Headers", _]], jiraRequest]];

Options[JiraCreateCustomerRequest] = $jiraCommonOptions;
JiraCreateCustomerRequest[serviceDeskId_, requestTypeId_, fields_Association,
  opts : OptionsPattern[]] :=
  jiraPost["servicedesk", {"request"},
    "Body" -> <|
      "serviceDeskId"      -> ToString[serviceDeskId],
      "requestTypeId"      -> ToString[requestTypeId],
      "requestFieldValues" -> fields
    |>,
    "Headers" -> jiraSDHeaders[OptionValue["Headers"]],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Headers", _]], jiraRequest]];


(* ::Section:: *)
(* Comments, transitions, participants, SLA *)

Options[JiraRequestComments] = Join[$jiraCommonOptions, {"Public" -> All}];

JiraRequestComments[id_, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "servicedesk", {"request", ToString[id], "comment"},
    "ItemsKey" -> "values",
    "Headers" -> jiraSDHeaders[OptionValue["Headers"]],
    "Parameters" -> Join[
      Switch[OptionValue["Public"],
        True,  {"public" -> True, "internal" -> False},
        False, {"public" -> False, "internal" -> True},
        _,     {}
      ],
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Headers" | "Parameters", _]],
      jiraPaginate]];

Options[JiraAddRequestComment] = Join[$jiraCommonOptions, {"Public" -> True}];

JiraAddRequestComment[id_, text_String, opts : OptionsPattern[]] :=
  jiraPost["servicedesk", {"request", ToString[id], "comment"},
    "Body" -> <|"body" -> text, "public" -> TrueQ[OptionValue["Public"]]|>,
    "Headers" -> jiraSDHeaders[OptionValue["Headers"]],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Headers", _]], jiraRequest]];

Options[JiraRequestTransitions] = $jiraCommonOptions;
JiraRequestTransitions[id_, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "servicedesk", {"request", ToString[id], "transition"},
    "ItemsKey" -> "values",
    "Headers" -> jiraSDHeaders[OptionValue["Headers"]],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Headers", _]], jiraPaginate]];

Options[JiraTransitionRequest] = Join[$jiraCommonOptions, {"Comment" -> None}];

JiraTransitionRequest[id_, transitionId_, opts : OptionsPattern[]] := Module[{body},
  body = <|"id" -> ToString[transitionId]|>;
  If[StringQ[OptionValue["Comment"]],
    body["additionalComment"] = <|"body" -> OptionValue["Comment"]|>];
  jiraPost["servicedesk", {"request", ToString[id], "transition"},
    "Body" -> body,
    "Headers" -> jiraSDHeaders[OptionValue["Headers"]],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Headers", _]], jiraRequest]]
];

Options[JiraRequestParticipants] = $jiraCommonOptions;
JiraRequestParticipants[id_, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "servicedesk", {"request", ToString[id], "participant"},
    "ItemsKey" -> "values",
    "Headers" -> jiraSDHeaders[OptionValue["Headers"]],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Headers", _]], jiraPaginate]];

Options[JiraRequestSLA] = $jiraCommonOptions;
JiraRequestSLA[id_, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "servicedesk", {"request", ToString[id], "sla"},
    "ItemsKey" -> "values",
    "Headers" -> jiraSDHeaders[OptionValue["Headers"]],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Headers", _]], jiraPaginate]];

Options[JiraOrganizations] = $jiraCommonOptions;
JiraOrganizations[opts : OptionsPattern[]] :=
  jiraPaginate["GET", "servicedesk", {"organization"},
    "ItemsKey" -> "values",
    "Headers" -> jiraSDHeaders[OptionValue["Headers"]],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["Headers", _]], jiraPaginate]];
