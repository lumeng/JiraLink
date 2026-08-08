(* ::Package:: *)

(* Auth.wl -- credential resolution, storage and connection objects.

   Nothing here runs at load time. The pre-1.0 package decrypted its
   configuration file as a side effect of Needs, prompting with InputString and
   DialogInput, which made the package impossible to use from wolframscript or
   from a test suite. Credentials are now resolved on the first request.

   Jira Data Center 9.12 supports personal access tokens (added in 8.14), which
   are the default here. Basic authentication still works on Data Center and is
   kept as a fallback. *)


$JiraConnection = None;

$JiraDefaultURL := Environment["JIRA_URL"];


(* ::Section:: *)
(* Connection objects

   Secrets live inside the object, but the displayed form masks them so that a
   token is not splashed across a notebook or a terminal by an accidental
   evaluation of $JiraConnection. *)

JiraConnectionQ[JiraConnectionObject[a_Association]] := AssociationQ[a] && StringQ[a["Domain"]];
JiraConnectionQ[_] := False;

JiraConnectionObject /: Normal[JiraConnectionObject[a_Association]] := a;

JiraConnectionObject /: MakeBoxes[obj : JiraConnectionObject[a_Association], form : StandardForm | TraditionalForm] :=
  BoxForm`ArrangeSummaryBox[
    JiraConnectionObject,
    obj,
    None,
    {
      BoxForm`SummaryItem[{"URL: ", jiraBaseURLString[a]}],
      BoxForm`SummaryItem[{"authentication: ", a["AuthType"]}]
    },
    {
      BoxForm`SummaryItem[{"user: ", Lookup[a, "Username", Missing["Unknown"]]}],
      BoxForm`SummaryItem[{"API version: ", "2"}]
    },
    form
  ];

(* The summary box above covers StandardForm only. A token must not leak through
   any other printed form either -- ToString, Print and terminal output all go
   via OutputForm, and InputForm is what gets copied and pasted. *)
jiraConnectionSummaryString[a_Association] :=
  "JiraConnectionObject[" <> jiraBaseURLString[a] <> ", " <>
    ToString[Lookup[a, "AuthType", "Anonymous"]] <> "]";

Format[JiraConnectionObject[a_Association], OutputForm] := jiraConnectionSummaryString[a];
Format[JiraConnectionObject[a_Association], TextForm]   := jiraConnectionSummaryString[a];
Format[JiraConnectionObject[a_Association], InputForm]  := jiraConnectionSummaryString[a];


(* ::Section:: *)
(* Secure storage *)

jiraCredentialKey[conn_Association] := jiraCredentialKey[conn["Domain"], conn["Port"]];
jiraCredentialKey[domain_String, port : (_Integer | None) : None] :=
  "JiraLink:" <> domain <> If[IntegerQ[port], ":" <> ToString[port], ""];

jiraReadStoredCredential[key_String] := Module[{v},
  v = Quiet @ SystemCredential[key];
  If[AssociationQ[v], v, None]
];

JiraStoreCredential[url_String, cred_Association] := Module[{norm, key},
  norm = jiraNormalizeURL[url];
  If[norm === $Failed, Return[$Failed]];
  key = jiraCredentialKey[norm];
  SystemCredential[key] = Join[<|"BaseURL" -> url|>, cred];
  key
];

JiraDeleteCredential[url_String] := Module[{norm, key},
  norm = jiraNormalizeURL[url];
  If[norm === $Failed, Return[$Failed]];
  key = jiraCredentialKey[norm];
  (* Unset rather than "=." so that the intent survives reformatting. *)
  Quiet @ Check[Unset[SystemCredential[key]], $Failed];
  key
];

JiraCredentialQ[url_String] := Module[{norm},
  norm = jiraNormalizeURL[url];
  norm =!= $Failed && jiraReadStoredCredential[jiraCredentialKey[norm]] =!= None
];

JiraStoredCredentials[] :=
  StringDelete[#, StartOfString ~~ "JiraLink:"] & /@ Quiet[SystemCredentialKeys["JiraLink:*"]];


(* ::Section:: *)
(* Credential resolution

   First match wins:
     1. options given on the call
     2. a credential stored for this host in secure storage
     3. the environment
   The legacy encrypted file is deliberately NOT consulted automatically -- it
   needs a password, and prompting for one is exactly the behaviour this
   rewrite removes. Import it once with JiraImportLegacyCredentials instead. *)

jiraResolveCredential[norm_Association, opts_List] := Module[
  {token, user, pass, stored, envToken, envUser, envPass},

  token = Lookup[opts, "Token", None];
  {user, pass} = Replace[
    Lookup[opts, Authentication, None],
    {
      a_Association :> {Lookup[a, "Username", None], Lookup[a, "Password", None]},
      {u_, p_} :> {u, p},
      _ -> {Lookup[opts, "Username", None], Lookup[opts, "Password", None]}
    }
  ];

  If[StringQ[token], Return[<|"AuthType" -> "Bearer", "Token" -> token|>]];
  If[StringQ[user] && StringQ[pass],
    Return[<|"AuthType" -> "Basic", "Username" -> user, "Password" -> pass|>]
  ];

  stored = jiraReadStoredCredential[jiraCredentialKey[norm]];
  If[AssociationQ[stored],
    Which[
      StringQ[stored["Token"]],
        Return[<|"AuthType" -> "Bearer", "Token" -> stored["Token"]|>],
      StringQ[stored["Username"]] && StringQ[stored["Password"]],
        Return[<|"AuthType" -> "Basic", "Username" -> stored["Username"],
                 "Password" -> stored["Password"]|>]
    ]
  ];

  envToken = Environment["JIRA_TOKEN"];
  If[StringQ[envToken] && envToken =!= "",
    Return[<|"AuthType" -> "Bearer", "Token" -> envToken|>]
  ];
  envUser = Environment["JIRA_USER"];
  envPass = Environment["JIRA_PASSWORD"];
  If[StringQ[envUser] && StringQ[envPass] && envUser =!= "",
    Return[<|"AuthType" -> "Basic", "Username" -> envUser, "Password" -> envPass|>]
  ];

  (* Some Jira instances allow anonymous reads; let those work rather than
     refusing outright. *)
  <|"AuthType" -> "Anonymous"|>
];


(* ::Section:: *)
(* Authorization headers *)

jiraAuthHeaders[conn_Association] := Switch[
  conn["AuthType"],
  "Bearer",  {"Authorization" -> "Bearer " <> conn["Token"]},
  "Basic",   {"Authorization" -> "Basic " <>
               BaseEncode[StringToByteArray[conn["Username"] <> ":" <> conn["Password"], "UTF-8"],
                          "Base64"]},
  "Cookie",  {"Cookie" -> conn["Cookie"]},
  _,         {}
];


(* ::Section:: *)
(* JiraConnect *)

Options[JiraConnect] = {
  "Token"        -> None,
  "Username"     -> None,
  "Password"     -> None,
  Authentication -> None,
  "APIVersion"   -> "2",
  "Verify"       -> False,
  "Default"      -> True
};

JiraConnect[opts : OptionsPattern[]] := Module[{url},
  url = $JiraDefaultURL;
  If[!StringQ[url] || url === "",
    (* Fall back to the single stored credential, if there is exactly one. *)
    With[{stored = JiraStoredCredentials[]},
      If[Length[stored] === 1,
        url = First[stored],
        Message[JiraLink::noconn];
        Return[$Failed]
      ]
    ]
  ];
  JiraConnect[url, opts]
];

JiraConnect[url_String, opts : OptionsPattern[]] := Module[{norm, cred, conn, check},
  norm = jiraNormalizeURL[url];
  If[norm === $Failed, Return[$Failed]];

  cred = jiraResolveCredential[norm, Flatten[{opts}]];
  conn = JiraConnectionObject[Join[norm, cred, <|"APIVersion" -> OptionValue["APIVersion"]|>]];

  If[TrueQ[OptionValue["Default"]], $JiraConnection = conn];

  If[TrueQ[OptionValue["Verify"]],
    check = JiraAuthenticationTest["Connection" -> conn];
    If[FailureQ[check], Return[check]]
  ];

  conn
];

JiraDisconnect[] := ($JiraConnection = None;);


(* Resolve whichever connection a call should use. *)
jiraConnection[Automatic] := jiraConnection[$JiraConnection];
jiraConnection[obj_JiraConnectionObject] := Normal[obj];
jiraConnection[a_Association] := a;
jiraConnection[url_String] := Module[{norm},
  norm = jiraNormalizeURL[url];
  If[norm === $Failed, $Failed, Join[norm, jiraResolveCredential[norm, {}]]]
];
jiraConnection[None] := (Message[JiraLink::noconn]; $Failed);
jiraConnection[other_] := (Message[JiraLink::badopt, "Connection", other]; $Failed);


(* ::Section:: *)
(* Verification *)

Options[JiraAuthenticationTest] = $jiraCommonOptions;

JiraAuthenticationTest[opts : OptionsPattern[]] :=
  jiraRequest["GET", "platform", {"myself"}, jiraFilterOptions[Flatten[{opts}], JiraAuthenticationTest]];


(* ::Section:: *)
(* Migration from the pre-1.0 encrypted configuration file *)

(* Configuration.m is retained from the pre-1.0 layout purely so that a user who
   moved the encrypted file elsewhere still has it found. *)
jiraLegacyConfigFile[] := Module[{cfgFile, cfg},
  cfgFile = FileNameJoin[{$JiraLinkDirectory, "Configuration.m"}];
  cfg = If[FileExistsQ[cfgFile], Quiet @ Check[Get[cfgFile], $Failed], $Failed];
  If[AssociationQ[cfg] && StringQ[Lookup[cfg, "EncryptedLoginInfoFile", None]],
    cfg["EncryptedLoginInfoFile"],
    FileNameJoin[{$HomeDirectory, ".jiralinkconfig.m"}]
  ]
];

JiraImportLegacyCredentials[password_String] := Module[{file, raw, decrypted, url},
  file = jiraLegacyConfigFile[];
  If[!FileExistsQ[file],
    Message[JiraLink::legacy, file];
    Return[$Failed]
  ];
  raw = Get[file];
  decrypted = Quiet @ Decrypt[password, raw];
  If[!AssociationQ[decrypted],
    Message[JiraLink::nocred, file];
    Return[$Failed]
  ];
  url = Lookup[decrypted, "JiraWebsiteURL", None];
  If[!StringQ[url],
    Message[JiraLink::badurl, url];
    Return[$Failed]
  ];
  JiraStoreCredential[url, <|
    "Username" -> Lookup[decrypted, "JiraWebsiteUsername", None],
    "Password" -> Lookup[decrypted, "JiraWebsitePassword", None]
  |>]
];


(* ::Section:: *)
(* Personal access tokens

   Jira Data Center exposes /rest/pat/latest/tokens so that a user can mint the
   very token this package then authenticates with. Only the creation endpoint
   is documented by Atlassian for 9.12; listing and revocation follow the same
   base path and are the community-standard usage, so they are offered here but
   may not exist on every deployment. *)

Options[JiraTokens] = $jiraCommonOptions;
JiraTokens[opts : OptionsPattern[]] :=
  jiraRequest["GET", "pat", {"tokens"}, jiraFilterOptions[Flatten[{opts}], JiraTokens]];

Options[JiraCreateToken] = $jiraCommonOptions;
JiraCreateToken[name_String, opts : OptionsPattern[]] :=
  JiraCreateToken[name, None, opts];
JiraCreateToken[name_String, days : (_Integer | None), opts : OptionsPattern[]] :=
  jiraRequest["POST", "pat", {"tokens"},
    "Body" -> DeleteCases[
      <|"name" -> name, "expirationDuration" -> days|>,
      None
    ],
    jiraFilterOptions[Flatten[{opts}], JiraCreateToken]
  ];

Options[JiraDeleteToken] = $jiraCommonOptions;
JiraDeleteToken[id_, opts : OptionsPattern[]] :=
  jiraRequest["DELETE", "pat", {"tokens", ToString[id]},
    jiraFilterOptions[Flatten[{opts}], JiraDeleteToken]];


(* ::Section:: *)
(* Cookie-based sessions

   Offered for completeness. Atlassian's own guidance for 9.12 is that basic
   authentication, or better a personal access token, is preferable. *)

Options[JiraSessionLogin] = $jiraCommonOptions;
JiraSessionLogin[username_String, password_String, opts : OptionsPattern[]] :=
  jiraRequest["POST", "auth", {"session"},
    "Body" -> <|"username" -> username, "password" -> password|>,
    jiraFilterOptions[Flatten[{opts}], JiraSessionLogin]];

Options[JiraSessionInfo] = $jiraCommonOptions;
JiraSessionInfo[opts : OptionsPattern[]] :=
  jiraRequest["GET", "auth", {"session"}, jiraFilterOptions[Flatten[{opts}], JiraSessionInfo]];

Options[JiraSessionLogout] = $jiraCommonOptions;
JiraSessionLogout[opts : OptionsPattern[]] :=
  jiraRequest["DELETE", "auth", {"session"}, jiraFilterOptions[Flatten[{opts}], JiraSessionLogout]];
