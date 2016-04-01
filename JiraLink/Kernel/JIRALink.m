(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: JIRALink *)
(* :Context: JIRALink` *)
(* :Author: Meng LU <lumeng.dev@gmail.com> *)
(* :Date: 2016-03-25 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 Meng LU <lumeng.dev@gmail.com> *)
(* :Keywords: JIRA, external link, JiraLink, API, REST *)
(* :Discussion: *)

BeginPackage["JiraLink`"]

JiraExecute::usage = "JiraExecute[resourceName, headerData] executes a query \
conforming to the JIRA REST API (https://docs.atlassian.com/jira/REST/latest/).";

JiraIssueData::usage = "JiraIssueData[issueKey, field] returns the properties \
the specified issue.";

JiraCreateIssue::usage = "JiraCreateIssue[project, summary, issueType, \
moreProperties] creates a new issue in the specified project with the \
specified summary (title). Specify additional properties (moreProperties) \
as an Association expression.";

JiraCreateSubtaskIssue::usage = "JiraCreateSubtaskIssue[parentIssueKey, \
summary, moreProperties] creates a new issue in the specified project with the \
specified summary (title). Specify additional properties (moreProperties) \
as an Association expression.";

Begin["`Private`"]

(* ::Section:: *)
(*******************************************************************************
## Debug
*)

$debugQ = False;


(* ::Section:: *)
(*******************************************************************************
## Constants
*)

JiraLink::badversion = "This package is designed for Mathematica version 10 and
later. This kernel session has version number `1`";

If[
    $VersionNumber < 10,
    Message[JiraLink::badversion, $VersionNumber];
    Abort[]
];

$JIRAIssueKeyRegex = RegularExpression["[A-Z]+-[0-9]+"];

(* ::Section:: *)
(*******************************************************************************
## Helper functions
*)

RemoveCharactersNotSupported[str_String] := StringReplace[
    str,
    RegularExpression["[^\\x0-\\xFFFF]"] -> "<XXX/>"
]

(* ::Section:: *)
(*******************************************************************************
## JiraIssueOpen
*)

JiraIssueOpen[issueIdOrKey_String] := If[
    StringMatchQ[issueIdOrKey, $JIRAIssueKeyRegex],
    With[
        {url = URLBuild[{OptionValue["Host"], "jira", "browse", key}]},
        SystemOpen[url]
    ]
];


(* ::Section:: *)
(*******************************************************************************
## Set up login info.
*)

$ConfiguartionFile = FileNameJoin[{DirectoryName[$InputFileName], "Configuration.m"}];

$EncryptedLoginInfoFile = "EncryptedLoginInfoFile" /. Get[$ConfiguartionFile];

ClearAll[GenerateEncryptedLoginInfoFile];

Attributes[GenerateEncryptedLoginInfoFile] = {HoldAll};

GenerateEncryptedLoginInfoFile[password_] := Export[
    $EncryptedLoginInfoFile,
    With[
        {
            encryptPassword = password,
            host = InputString["JIRA site host name (e.g. \"http://jira.example.com:8080\"): "],
            username = InputString["LDAP username: "],
            password = InputString["LDAP password: "]
        },
        Encrypt[encryptPassword, <|"Host" -> host, "Username" -> username, "Password" -> password|>]
    ]
];

If[
    !FileExistsQ[$EncryptedLoginInfoFile],
    GenerateEncryptedLoginInfoFile[
        InputString[
            "Create " <> $EncryptedLoginInfoFile <> ". Password for encrypting/decrypting " <>
            $EncryptedLoginInfoFile <>
            " (ideally different from your LDAP password): "
        ]
    ]
];

$JiraLogin = Decrypt[
    InputString["Decrypt " <> $EncryptedLoginInfoFile <> ". Password for encrypting/decrypting "<>$EncryptedLoginInfoFile<>" (possibly different from your LDAP password): "],
    Get[$EncryptedLoginInfoFile]
];


(* ::Section:: *)
(*******************************************************************************
## JiraExecute
*)

ClearAll[JiraExecute];

JiraExecute::usage = "Executes a command using the Jira REST API \
(https://docs.atlassian.com/jira/REST/latest/).";

Options[JiraExecute] = {
    "Host" -> "http://jira.example.com:8080",
    "Username" -> None,
    "Password" -> None,
    "Method" -> "GET"
};

SetOptions[JiraExecute, Normal[$JiraLogin]];

JiraExecute::err = "Jira command `1` failed with message: `2`";

JiraExecute::badprop = "Properties `1` could not be converted to JSON. Abort.";

JiraExecute[resourceName_String, properties_Association: <||>, OptionsPattern[]] := Module[
    {host, username, password, loginInfo, method, jsonData, result},
    host = OptionValue["Host"];
    username  = OptionValue["Username"];
    password = OptionValue["Password"];
    method = OptionValue["Method"];

    jsonData = Check[
        ExportString[properties, "JSON"],
        Message[JiraExecute::badprop, properties];
        Abort[]
    ];

    loginInfo = If[
        StringQ[username],
        "-u " <> username <> ":" <> If[StringQ[password], password, ""],
        ""
    ];

    result = Import["!curl "
        <> URLBuild[{host, "jira", "rest", "api", "2", resourceName}] <> " "
        <> loginInfo <> " "
        <> "-X " <> method <> " "
        <> "-H \"Content-Type: application/json\" -d '" <> jsonData <> "'",
        "Text"
    ];

    If[TrueQ[$debugQ], Print[xImport["!curl "
        <> URLBuild[{host, "jira", "rest", "api", "2", resourceName}] <> " "
        <> loginInfo <> " "
        <> "-X " <> method <> " "
        <> "-H \"Content-Type: application/json\" -d '" <> jsonData <> "'",
        "Text"
    ]]];

    If[!StringQ[result], $Failed, result]
];


(* ::Section:: *)
(*******************************************************************************
## JiraCreateIssue

* An example of the JSON data used to create a ticket:

    {
        "fields": {
           "project":{"key": "TEST"},
           "summary": "This is the summary of the new issue",
           "description": "This is the description.",
           "issuetype": {"name": "Task"}
       }
    }

* Example command:

    JiraCreateIssue["MYPROJECTXXX", "issue summary XXX", "Bug", <|"assignee"-> <|"name"-> OptionValue[JiraExecute,"Username"]|>, "components"-> {<|"name"-> "componentXXX"|>,<|"name"-> "componentYYY"|>},"labels"-> {"labelXXX", "labelYYY"}|>,"OpenQ"-> True]

*)

ClearAll[JiraCreateIssue];

Options[JiraCreateIssue] = FilterRules[
    Options[JiraExecute],
    {"Host", "Username", "Password"}
] ~Join~ {
    "OpenQ" -> Automatic
};

JiraCreateIssue[project_String, summary_String, issueType_String: "Task",
    moreProperties_Association: <||>, opts:OptionsPattern[]] := Module[
    {key, result, url, properties, jsonData, openQ},
    openQ = OptionValue["OpenQ"];

    properties = <|
        "project" -> <|"key" -> project|>,
        "summary" -> summary,
        "issuetype" -> <|"name" -> issueType|>,
        "priority" -> <|"name" -> "Major"|>
    |>;

    properties = <|"fields" -> Join[properties, moreProperties]|>;

    result = JiraExecute["issue", properties, "Method" -> "POST",
        Sequence@@FilterRules[opts, Options[JiraExecute]]];

    If[
        TrueQ[openQ],
        key = "key" /. ImportString[result, "JSON"];
        url = URLBuild[{OptionValue["Host"], "jira", "browse", key}];
        SystemOpen[url];
    ];

    result
];


(* ::Section:: *)
(*******************************************************************************
## JiraCreateSubtaskIssue

* An example of the JSON data used to create a ticket:

{
    "fields": {
        "parent":{"key":"10420"},
        "project":{"key":"10300"},
        "summary":"a test sub-task of my first issue",
        "issuetype":{"id":"5"},
        "description":"description"
    }
}

* Example command call:

    JiraCreateSubtaskIssue["MYPROJECT","MYPROJECT-1234","XXX",<|"description"-> "XXX","assignee"-> <|"name"-> OptionValue[JiraCreateSubtaskIssue,"Username"]|>|>,"OpenQ"-> True]

*)

ClearAll[JiraCreateSubtaskIssue];

Options[JiraCreateSubtaskIssue] = Options[JiraCreateIssue];

JiraCreateSubtaskIssue[parentIssueKey_String, summary_String,
    moreProperties_Association: <||>, opts:OptionsPattern[]] := Module[
    {key, result, url, properties, jsonData, projectKey},

    openQ = OptionValue["OpenQ"];

    projectKey = StringReplace[parentIssueKey,
        RegularExpression["^(.*)-[0-9]+$"] -> "$1"];

    properties = <|
        "project" -> {"key" -> projectKey},
        "parent" -> {"key" -> parentIssueKey},
        "summary" -> summary,
        "issuetype" -> {"name" -> "Sub-task"},
        "priority" -> {"name" ->  "Major"}
    |>;

    properties = <|"fields" -> Join[properties, moreProperties]|>;

    result = JiraExecute["issue", properties, "Method" -> "POST",
        Sequence@@FilterRules[opts, Options[JiraExecute]]];

    If[TrueQ[$debugQ], Print["API call result JSON:"]; Print[result]];

    If[
        TrueQ[openQ],
        key = "key" /. ImportString[result, "JSON"];
        url = URLBuild[{OptionValue["Host"], "jira", "browse", key}];
        SystemOpen[url];
    ];

    result
];

End[] (* `Private` *)

EndPackage[]
