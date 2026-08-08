(* ::Package:: *)

(* Users.wl -- users and groups.

   Jira Data Center identifies users by `name` (the username) and `key` (an
   opaque JIRAUSER... id). The `accountId` used by Jira Cloud does not exist
   here, so every function below takes a username.

   The per-endpoint maxResults caps differ and Jira silently clamps:
     /user/search, /user/picker, /groupuserpicker   max 1000
     /user/assignable/search, /user/viewissue/search  max  100
     /group/member                                    max   50 *)


Options[JiraMyself] = $jiraCommonOptions;
JiraMyself[opts : OptionsPattern[]] :=
  jiraGet["platform", {"myself"}, jiraFilterOptions[Flatten[{opts}], jiraRequest]];


Options[JiraUser] = Join[$jiraCommonOptions, {"By" -> "username"}];

JiraUser[user_String, opts : OptionsPattern[]] :=
  jiraGet["platform", {"user"},
    "Parameters" -> Join[
      {OptionValue["By"] -> user},
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


Options[JiraUserSearch] = Join[$jiraCommonOptions, {
  "IncludeActive" -> True, "IncludeInactive" -> False
}];

JiraUserSearch[query_String, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "platform", {"user", "search"},
    "Parameters" -> Join[
      {"username" -> query,
       "includeActive" -> OptionValue["IncludeActive"],
       "includeInactive" -> OptionValue["IncludeInactive"]},
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraPaginate]];


Options[JiraAssignableUsers] = $jiraCommonOptions;

JiraAssignableUsers[project_String, opts : OptionsPattern[]] :=
  JiraAssignableUsers[project, "", opts];

JiraAssignableUsers[project_String, query_String, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "platform", {"user", "assignable", "search"},
    "Parameters" -> Join[
      {"project" -> project, "username" -> query},
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    (* This endpoint caps pages at 100. *)
    "PageSize" -> Replace[OptionValue["PageSize"], Automatic -> 100],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["PageSize", _]], jiraPaginate]];


Options[JiraUserPicker] = $jiraCommonOptions;

JiraUserPicker[query_String, opts : OptionsPattern[]] :=
  jiraGet["platform", {"user", "picker"},
    "Parameters" -> Join[
      {"query" -> query},
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


Options[JiraUserGroups] = $jiraCommonOptions;
JiraUserGroups[user_String, opts : OptionsPattern[]] :=
  jiraGet["platform", {"user", "groups"},
    "Parameters" -> {"username" -> user},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


Options[JiraCreateUser] = $jiraCommonOptions;
JiraCreateUser[data_Association, opts : OptionsPattern[]] :=
  jiraPost["platform", {"user"}, "Body" -> data,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraUpdateUser] = $jiraCommonOptions;
JiraUpdateUser[user_String, data_Association, opts : OptionsPattern[]] :=
  jiraPut["platform", {"user"}, "Body" -> data,
    "Parameters" -> {"username" -> user},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraDeleteUser] = $jiraCommonOptions;
JiraDeleteUser[user_String, opts : OptionsPattern[]] :=
  jiraDelete["platform", {"user"},
    "Parameters" -> {"username" -> user},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];


(* ::Section:: *)
(* Groups

   GET /group is deprecated in 9.x in favour of /group/member. *)

Options[JiraGroupMembers] = Join[$jiraCommonOptions, {"IncludeInactive" -> False}];

JiraGroupMembers[group_String, opts : OptionsPattern[]] :=
  jiraPaginate["GET", "platform", {"group", "member"},
    "Parameters" -> Join[
      {"groupname" -> group, "includeInactiveUsers" -> OptionValue["IncludeInactive"]},
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    (* This endpoint caps pages at 50. *)
    "PageSize" -> Replace[OptionValue["PageSize"], Automatic -> 50],
    jiraFilterOptions[
      DeleteCases[Flatten[{opts}], (Rule | RuleDelayed)["PageSize", _]], jiraPaginate]];

Options[JiraCreateGroup] = $jiraCommonOptions;
JiraCreateGroup[name_String, opts : OptionsPattern[]] :=
  jiraPost["platform", {"group"}, "Body" -> <|"name" -> name|>,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraDeleteGroup] = Join[$jiraCommonOptions, {"SwapGroup" -> None}];
JiraDeleteGroup[name_String, opts : OptionsPattern[]] :=
  jiraDelete["platform", {"group"},
    "Parameters" -> Join[
      {"groupname" -> name},
      If[StringQ[OptionValue["SwapGroup"]], {"swapGroup" -> OptionValue["SwapGroup"]}, {}]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraAddGroupUser] = $jiraCommonOptions;
JiraAddGroupUser[group_String, user_String, opts : OptionsPattern[]] :=
  jiraPost["platform", {"group", "user"},
    "Parameters" -> {"groupname" -> group},
    "Body" -> <|"name" -> user|>,
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraRemoveGroupUser] = $jiraCommonOptions;
JiraRemoveGroupUser[group_String, user_String, opts : OptionsPattern[]] :=
  jiraDelete["platform", {"group", "user"},
    "Parameters" -> {"groupname" -> group, "username" -> user},
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];

Options[JiraGroupPicker] = $jiraCommonOptions;
JiraGroupPicker[query_String, opts : OptionsPattern[]] :=
  jiraGet["platform", {"groups", "picker"},
    "Parameters" -> Join[
      {"query" -> query},
      jiraNormalizeQuery[OptionValue["Parameters"]]
    ],
    jiraFilterOptions[Flatten[{opts}], jiraRequest]];
