(* ::Package:: *)

(* Compat.wl -- the pre-1.0 calling conventions, on top of the new core.

   The seven symbols the package used to export keep their names and their
   argument patterns. What changes is that they now work:

     JiraApiExecute    honours the request method and body it is given; the
                       default code path used to discard both
     JiraCreateIssue   actually creates the issue, and "OpenQ" -> True actually
                       opens it
     JiraDeleteIssue   actually deletes the issue
     JiraJqlSearch     sends a real Authorization header; it used to send the
                       curl flag "u" as though it were one, so it worked only
                       against anonymously readable instances
     JiraFindIssues    returns associations rather than nested lists of rules

   The last of those is the one behaviour change a caller can notice. Code
   written against the old shape did "key" /. result; it now does result["key"].

   The pre-1.0 connection options are still accepted, so existing notebooks that
   pass "JiraWebsiteURL" and friends continue to work without a JiraConnect. *)


(* Map the pre-1.0 connection options onto a connection object. Anything that
   does not mention them is left alone, so the ordinary "Connection" option and
   $JiraConnection keep working. *)
jiraLegacyConnectionOptions[opts_List] := Module[{url, user, pass, norm, cred},
  url  = Lookup[opts, "JiraWebsiteURL", Automatic];
  user = Lookup[opts, "JiraWebsiteUsername", Automatic];
  pass = Lookup[opts, "JiraWebsitePassword", Automatic];

  If[!StringQ[url], Return[opts]];

  norm = jiraNormalizeURL[url];
  If[norm === $Failed, Return[opts]];

  cred = If[StringQ[user] && StringQ[pass],
    <|"AuthType" -> "Basic", "Username" -> user, "Password" -> pass|>,
    jiraResolveCredential[norm, {}]
  ];

  Append[
    DeleteCases[opts,
      (Rule | RuleDelayed)["JiraWebsiteURL" | "JiraWebsiteUsername" | "JiraWebsitePassword", _]],
    "Connection" -> JiraConnectionObject[Join[norm, cred, <|"APIVersion" -> "2"|>]]
  ]
];


(* ::Section:: *)
(* JiraApiExecute, pre-1.0 forms

   JiraApiExecute[resource]                  a GET of that resource
   JiraApiExecute[resource, params]          params became the JSON body
   plus "Method", "Parameters" and the connection options.

   The present-day forms JiraApiExecute[method, path] and
   JiraApiExecute[method, {segments}] are defined in Request.wl; they take two
   arguments, so the two conventions do not collide. *)

JiraApiExecute[resource_String, opts : OptionsPattern[]] :=
  jiraLegacyApiExecute[resource, <||>, Flatten[{opts}]];

JiraApiExecute[resource_String, params_Association, opts : OptionsPattern[]] :=
  jiraLegacyApiExecute[resource, params, Flatten[{opts}]];

jiraLegacyApiExecute[resource_String, params_Association, opts_List] := Module[{method, body},
  method = Replace[Lookup[opts, "Method", Automatic], Automatic -> "GET"];
  (* The pre-1.0 signature used the second argument as the request body, and
     passed <||> when there was none. *)
  body = If[Length[params] === 0, None, params];
  jiraRequest[ToUpperCase[method], "platform", jiraSplitPath[resource],
    "Body" -> body,
    jiraFilterOptions[jiraLegacyConnectionOptions[opts], jiraRequest]
  ]
];


(* ::Section:: *)
(* Legacy option names on the resource functions

   "MaxResults" and "StartAt" were the pre-1.0 names for what is now the
   MaxItems / "StartAt" pair. JiraJqlSearch keeps them natively (see Search.wl);
   JiraFindIssues accepted them too, so they are translated here. *)

jiraTranslateLegacyPaging[opts_List] := Module[{res, maxResults},
  maxResults = Lookup[opts, "MaxResults", Automatic];
  res = DeleteCases[opts, (Rule | RuleDelayed)["MaxResults", _]];
  If[IntegerQ[maxResults] && FreeQ[res, (Rule | RuleDelayed)[MaxItems, _]],
    res = Append[res, MaxItems -> maxResults]
  ];
  res
];

(* Extend JiraFindIssues and JiraSearch to accept the old paging option names. *)
Options[JiraFindIssues] = Join[Options[JiraFindIssues], {"MaxResults" -> Automatic}];
Options[JiraSearch]     = Join[Options[JiraSearch], {"MaxResults" -> Automatic}];

JiraFindIssues[jql_String, rest___, opts : OptionsPattern[]] /;
    MemberQ[Flatten[{opts}], (Rule | RuleDelayed)["MaxResults", _]] :=
  JiraFindIssues[jql, rest, Sequence @@ jiraTranslateLegacyPaging[Flatten[{opts}]]];

JiraSearch[jql_String, opts : OptionsPattern[]] /;
    MemberQ[Flatten[{opts}], (Rule | RuleDelayed)["MaxResults", _]] :=
  JiraSearch[jql, Sequence @@ jiraTranslateLegacyPaging[Flatten[{opts}]]];


(* ::Section:: *)
(* Debugging

   $debugQ and debugPrint survive from the pre-1.0 package. The richer
   replacement is $JiraLogRequests together with $JiraRequestLog. *)

$debugQ = False;

Attributes[debugPrint] = {HoldAllComplete};

debugPrint[expr_] := If[
  TrueQ[$debugQ || TrueQ[Global`$debugQ]],
  Echo[expr, "DEBUG:"],
  expr
];
