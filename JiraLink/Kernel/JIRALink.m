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

## Developer note

* Use `URLFetch`

    URLFetch[
        "http://jira.example.com:8080/jira/rest/api/2/issue/MYPROJECT-123",
        "Headers"->  {
            "u"-> "USER:PASSWORD",
            "Content-Type"-> "application/json; charset=utf8"
        },
        Method-> "GET"
    ]

*)

ClearAll[JiraExecute];

JiraExecute::usage = "Executes a command using the Jira REST API \
(https://docs.atlassian.com/jira/REST/latest/).";

Options[JiraExecute] = {
    "Host" -> "http://jira.example.com:8080",
    "Username" -> None,
    "Password" -> None,
    "Method" -> "GET",
    "HTTPRequestImplementation" -> {Automatic, Import, URLFetch}[[2]]
};

SetOptions[JiraExecute, Normal[$JiraLogin]];

JiraExecute::err = "Jira command `1` failed with message: `2`";

JiraExecute::badprop = "Properties `1` could not be converted to JSON. Abort.";

JiraExecute::badjsonexpr = "JSON string `1` cannot be converted to a Wolfram \
Language expression using ImportString[in, \"JSON\"]. Use String expression
instead of the more structured list of rules, numbers, and strings, etc. to \
represent the JSON object.";

JiraExecute[resourceName_String, headerData_Association: <||>, OptionsPattern[]] := Module[
    {host, apiUrl, username, password, loginInfo, method, contentType, jsonData, result, header},
    host = OptionValue["Host"];
    username  = OptionValue["Username"];
    password = OptionValue["Password"];
    method = OptionValue["Method"];

    apiUrl = URLBuild[{host, "jira", "rest", "api", "2", resourceName}];


    loginInfo = If[
        StringQ[username],
        username <> ":" <> If[StringQ[password], password, ""],
        ""
    ];

    (** TODO If may be redundant to specify the charset=utf-8, c.f.
    * http://stackoverflow.com/questions/9254891/what-does-content-type-application-json-charset-utf-8-really-mean
    * http://stackoverflow.com/questions/3995559/json-character-encoding
    **)
    contentType = "application/json; charset=utf-8";

    jsonData = Check[
        ExportString[headerData, "JSON"],
        Message[JiraExecute::badprop, headerData];
        Abort[]
    ];

    result = Switch[
        OptionValue["HTTPRequestImplementation"],

        Import,

        Import["!curl "
            <> apiUrl <> " "
            <> If[loginInfo =!= "", "-u " <> loginInfo <> " ", ""]
            <> "-H "
            <> "\"" <> "Content-Type: " <> "application/json;" <> "\"" <> " "
            <> "-d " <> "'" <> jsonData <> "'" <> " "
            <> "-X " <> method,
            "Text"
        ],

        _,

        URLFetch[
            apiUrl,
            "Headers"->  {
                "u"-> loginInfo,
                "Content-Type"-> contentType,
                "d" -> jsonData
            },
            Method-> method
        ]
    ];

    Check[
        ImportString[ToString[result, CharacterEncoding->"UTF-8"], "JSON"],
        Message[JiraExecute::badjsonexpr, result];
        result,
        {Import::fmterr}
    ]

];


(* ::Section:: *)
(*******************************************************************************
## JiraIssueData

### References:
* https://docs.atlassian.com/jira/REST/latest/#api/2/issue-getIssue

*)

ClearAll[JiraIssueData];

Options[JiraIssueData] = Options[JiraExecute];

JiraIssueData[issueKey_String, field_String: All, opts:OptionsPattern[]] := Module[
    {result, jsonData, resourceName},

    resourceName = URLBuild[{"issue", issueKey}];

    jsonData = JiraExecute[resourceName, "Method" -> "GET",
        Sequence@@FilterRules[Flatten[{opts}], Options[JiraExecute]]];

    (*result = If[StringQ[result], ImportString[jsonData, "JSON"]];*)

    result = jsonData;

    result
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
    {key, result, url, properties, headerData, jsonData, openQ},
    openQ = OptionValue["OpenQ"];

    properties = <|
        "project" -> <|"key" -> project|>,
        "summary" -> summary,
        "issuetype" -> <|"name" -> issueType|>,
        "priority" -> <|"name" -> "Major"|>
    |>;

    headerData = <|"fields" -> Join[properties, moreProperties]|>;

    result = JiraExecute["issue", headerData, "Method" -> "POST",
        Sequence@@FilterRules[Flatten[{opts}], Options[JiraExecute]]];

    If[
        TrueQ[openQ],
        key = If[MatchQ[result, {__Rule}], "key" /. result];
        JiraIssueOpen[key]
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
        Sequence@@FilterRules[Flatten[{opts}], Options[JiraExecute]]];

    If[
        TrueQ[openQ],
        key = If[MatchQ[result, {__Rule}], "key" /. result];
        JiraIssueOpen[key]
    ];

    result
];


End[] (* `Private` *)

EndPackage[]
