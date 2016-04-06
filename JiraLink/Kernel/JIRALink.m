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

BeginPackage["JiraLink`"];

JiraIssueOpen::usage = "JiraIssueOpen[issueKey] opens the JIRA issue using \
SystemOpen.";

JiraApiExecute::usage = "JiraApiExecute[resourceName, headerData] executes a query \
conforming to the JIRA REST API (https://docs.atlassian.com/jira/REST/latest/).";

JiraIssueData::usage = "JiraIssueData[issueKey, field] returns the properties \
the specified issue.";

JiraCreateIssue::usage = "JiraCreateIssue[project, summary, issueType, \
moreProperties] creates a new issue in the specified project with the \
specified summary (title). Specify additional properties (moreProperties) \
as an Association expression.";

JiraDeleteIssue::usage = "JiraDeleteIssue[issueKey] deletes an issue.";

JiraCreateSubtaskIssue::usage = "JiraCreateSubtaskIssue[parentIssueKey, \
summary, moreProperties] creates a new issue in the specified project with the \
specified summary (title). Specify additional properties (moreProperties) \
as an Association expression.";

JiraJqlSearch::usage = "JiraJqlSearch[jqlQuery] performs a JQL query";

Begin["`Private`"];

(* ::Section:: *)
(*******************************************************************************
## Debug
*)

$debugQ = False;


JiraLink::badversion = "This package is designed for Mathematica version 10 and
later. This kernel session has version number `1`";

If[
    $VersionNumber < 10,
    Message[JiraLink::badversion, $VersionNumber];
    Abort[]
];


ClearAll[debugPrint];

Attributes[debugPrint] = {HoldAllComplete};

debugPrint[expr_] := If[
    TrueQ[$debugQ||Global`$debugQ],
    Echo[
        expr,
        "DEBUG:\n" <> ToString[
            Replace[
                HoldComplete[expr],
                Verbatim[HoldComplete][h_[args___]] :> Hold[h][args]
            ],
            InputForm
        ] <> ":\n"
    ],
    expr
];


(* ::Section:: *)
(*******************************************************************************
## Constants
*)

$JiraIssueKeyRegex = RegularExpression["[A-Z]+-[0-9]+"];


(** TODO If may be redundant to specify the charset=UTF-8, c.f.
* http://stackoverflow.com/questions/9254891/what-does-content-type-application-json-charset-utf-8-really-mean
* http://stackoverflow.com/questions/3995559/json-character-encoding
**)
$JiraApiHttpContentType1 = "application/json";
$JiraApiHttpContentType2 = "application/json; charset=UTF-8"; (*< this one seems unnecessary *)
$JiraApiHttpContentType = $JiraApiHttpContentType1;

(* ::Section:: *)
(*******************************************************************************
## Helper functions
*)


(* ::Subsection:: *)
(*------------------------------------------------------------------------------
### filterOptions

* Filters out options of symbol f1 that are compatible with symbol f2.
*)

filterOptions[f1 -> f2] := FilterRules[
    Options[f1],
    Options[f2]
];

(* ::Subsection:: *)
(*------------------------------------------------------------------------------
### jsonStringToExpression

* The result is a list of rules where the right hand side can be expressions of
types Real, Integer, String, and List which in turn can iteratively contain the
aforementioned types of expressions.
*)

ClearAll[jsonStringToExpression];

jsonStringToExpression::badjsonexpr = "The JSON object represented as a Mathematica
String expression cannot be converted to a structured Mathematica expression,
namely a nested list of rules, where the right hand sides contains numbers,
strings, lists, XXX.";

jsonStringToExpression[jsonStr_String] := Check[
    (** An alternative way to "fix" the string so it can be fed to
    `ImportString[..., "JSON"]` is:

        FromCharacterCode @ (ToCharacterCode[#, "UTF-8"] &) @ jsonStr

    **)
    ImportString[ToString[jsonStr, CharacterEncoding -> "UTF-8"], "JSON"],
    Message[JiraApiExecute::badjsonexpr, jsonStr];
    jsonStr,
    {Import::fmterr}
];


(* ::Section:: *)
(*******************************************************************************
## Set up login info.
*)

$ConfiguartionFile = FileNameJoin[{DirectoryName[$InputFileName], "Configuration.m"}];

$EncryptedLoginInfoFile = "EncryptedLoginInfoFile" /. Get[$ConfiguartionFile];

ClearAll[GenerateEncryptedLoginInfoFile];

GenerateEncryptedLoginInfoFile[encryptPassword_] := Module[
    {jiraHost, jiraUsername, jiraPassword},

    {jiraHost, jiraUsername, jiraPassword} = DialogInput[
        {
            jiraHost = "https://jira.example.com:8080",
            jiraUsername = "",
            jiraPassword = ""
        },
        Grid[
            {
                {
                    "JIRA website URL: ",
                    InputField[Dynamic[jiraHost], String]
                },
                {"JIRA website username: ", InputField[Dynamic[jiraUsername], String]},
                {
                    "JIRA website password: ",
                    InputField[Dynamic[jiraPassword], String, FieldMasked -> True]
                },
                {
                    Item[#, Alignment -> Center]& @
                        Row @
                        {CancelButton[], DefaultButton["Save", DialogReturn[#]]}& @
                        {jiraHost, jiraUsername, jiraPassword},

                    SpanFromLeft
                }
            },
            Alignment -> {Right, Automatic}
        ]
    ];

    {jiraHost, jiraUsername, jiraPassword}//debugPrint;

    Export[
        $EncryptedLoginInfoFile,
        Encrypt[encryptPassword, <|"JiraWebsiteURL" -> jiraHost, "JiraWebsiteUsername" -> jiraUsername, "JiraWebsitePassword" -> jiraPassword|>]
    ]
];

If[
    !FileExistsQ[$EncryptedLoginInfoFile],
    GenerateEncryptedLoginInfoFile[
        InputString[
            "Enter a password for encrypting/decrypting " <>
            $EncryptedLoginInfoFile <> ": ",
            FieldMasked -> True
        ]
    ]
];

$JiraLogin = Decrypt[
    InputString[
        "Enter the password for decrypting " <> $EncryptedLoginInfoFile,
        FieldMasked -> True
    ],
    Get[$EncryptedLoginInfoFile]
];


(* ::Section:: *)
(*******************************************************************************
## JiraApiExecute

### Usage examples:

### Developer note

* Use `URLFetch`

    URLFetch[
        "http://jira.example.com:8080/jira/rest/api/2/issue/MYPROJECT-123",
        "Headers"->  {
            "u"-> "USER:PASSWORD",
            "Content-Type"-> "application/json; charset=UTF-8"
        },
        Method-> "GET"
    ]


    With[
        {auth = "Basic " <> Developer`EncodeBase64[StringTemplate["`JiraWebisteUsername`:`JiraWebistePassword`"][Association[Options[JiraApiExecute]]]]}
        URLFetch[
            "http://jira.example.com:8080/jira/rest/api/2/issue/MYPROJECT-123",
            "Headers"->  {
                "Authorization" -> auth,
                "Content-Type" -> "application/json"
            },
            Method-> "GET"
        ]
    ]

*)

ClearAll[JiraApiExecute];

Options[JiraApiExecute] = {
    "JiraWebsiteURL" -> "http://jira.example.com:8080",
    "JiraWebsiteUsername" -> None,
    "JiraWebsitePassword" -> None,
    "Method" -> "GET",
    "HTTPRequestImplementation" -> {Automatic, Import, URLFetch}[[1]]
};

SetOptions[JiraApiExecute, Normal[$JiraLogin]];

JiraApiExecute::err = "Jira command `1` failed with message: `2`";

JiraApiExecute::badprop = "Properties `1` could not be converted to JSON. Abort.";

JiraApiExecute::badjsonexpr = "JSON string `1` cannot be converted to a Wolfram \
Language expression using ImportString[in, \"JSON\"]. Use String expression
instead of the more structured list of rules, numbers, and strings, etc. to \
represent the JSON object.";

JiraApiExecute[resourceName_String, headerData_Association: <||>, OptionsPattern[]] := Module[
    {host, apiUrl, username, password, loginInfo, method, contentType, jsonData, result, header, authorization},
    host = OptionValue["JiraWebsiteURL"];
    username  = OptionValue["JiraWebsiteUsername"];
    password = OptionValue["JiraWebsitePassword"];
    method = OptionValue["Method"];

    apiUrl = URLBuild[{host, "jira", "rest", "api", "2", resourceName}];


    loginInfo = If[
        StringQ[username],
        username <> ":" <> If[StringQ[password], password, ""],
        ""
    ];

    authorization = "Basic " <>
        Developer`EncodeBase64[username <> ":" <> password];

    contentType = $JiraApiHttpContentType;

    jsonData = Check[
        ExportString[headerData, "JSON"],
        Message[JiraApiExecute::badprop, headerData];
        Abort[]
    ];

    result = Switch[
        OptionValue["HTTPRequestImplementation"],

        Import,

        Import["!curl "
            <> apiUrl <> " "
            <> If[loginInfo =!= "", "-u " <> loginInfo <> " ", ""]
            <> "-H "
            <> "\"" <> "Content-Type: " <> contentType <> "\"" <> " "
            <> "-d " <> "'" <> jsonData <> "'" <> " "
            <> "-X " <> method,
            "Text"
        ]//debugPrint,

        _,

        URLFetch[
            apiUrl,
            "Headers"->  {
                (*"u"-> loginInfo,*)
                "Authorization" -> authorization,
                "Content-Type"-> contentType
            },
            "Body" -> jsonData,
            Method-> method
        ]//debugPrint
    ];

    jsonStringToExpression[result]

];


(* ::Section:: *)
(*******************************************************************************
## JiraIssueOpen
*)
ClearAll[JiraIssueOpen];

JiraIssueOpen::invalidkey = "Invalid Jira issue key: `1`";

Options[JiraIssueOpen] := {
    "JiraWebsiteURL" -> OptionValue[JiraApiExecute, "JiraWebsiteURL"]
};

JiraIssueOpen[issueKey_String, OptionsPattern[]] := If[
    StringMatchQ[issueKey, $JiraIssueKeyRegex],
    With[
        {url = URLBuild[{OptionValue["JiraWebsiteURL"], "jira", "browse", issueKey}]},
        SystemOpen[url]
    ],
    Message[JiraIssueOpen::invalidkey, issueKey]
];


(* ::Section:: *)
(*******************************************************************************
## JiraIssueData

### References:
* https://docs.atlassian.com/jira/REST/latest/#api/2/issue-getIssue

*)

ClearAll[JiraIssueData];

Options[JiraIssueData] := FilterRules[
    Options[JiraApiExecute],
    {"JiraWebsiteURL", "JiraWebsiteUsername", "JiraWebsitePassword"}
];

JiraIssueData[issueKey_String, field_String: All, opts:OptionsPattern[]] := Module[
    {result, jsonData, resourceName},

    resourceName = URLBuild[{"issue", issueKey}];

    jsonData = JiraExecute[resourceName, "Method" -> "GET",
        Sequence@@FilterRules[Flatten[{opts}], Options[JiraExecute]]];

    jsonData
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

    JiraCreateIssue[
        "MYPROJECTXXX",
        "issue summary XXX",
        "Bug",
        <|
            "assignee" -> <|"name"-> OptionValue[JiraApiExecute,"JiraWebsiteUsername"]|>,
             "components"-> {<|"name"-> "componentXXX"|>, <|"name"-> "componentYYY"|>},
             "labels"-> {"labelXXX", "labelYYY"}
        |>,
        "OpenQ" -> True
    ]

*)

ClearAll[JiraCreateIssue];

Options[JiraCreateIssue] = FilterRules[
    Options[JiraApiExecute],
    {"JiraWebsiteURL", "JiraWebsiteUsername", "JiraWebsitePassword"}
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


(* ::Section:: *)
(*******************************************************************************
## JiraDeleteIssue

* Example command:

    JiraDeleteIssue

*)

ClearAll[JiraDeleteIssue];

Options[JiraDeleteIssue] = FilterRules[
    Options[JiraApiExecute],
    {"JiraWebsiteURL", "JiraWebsiteUsername", "JiraWebsitePassword"}
] ~Join~ {
    "DeleteSubtasks" -> "false"
};


JiraDeleteIssue::badoptvalue = "Bad option value `1` for option `2`.";

JiraDeleteIssue[issueKey_String, field_String: All, opts:OptionsPattern[]] := Module[
    {result, jsonData, resourceName, deleteSubtasks},

    deleteSubtasks = Switch[
        OptionValue["DeleteSubtasks"],
        "true", "true",
        "false", "false",
        _,
        Message[JiraDeleteIssue::badoptvalue, OptionValue["DeleteSubtasks"], "DeleteSubtasks"];
        "false"
    ];

    resourceName = URLBuild[{"issue", issueKey}];

    jsonData = JiraApiExecute[
        resourceName,
        "Method" -> "DELETE",
        "Parameters" -> {
            "deleteSubtasks" -> OptionValue["DeleteSubtasks"]
        },
        opts
    ];

    jsonStringToExpression[jsonData]
];

(* ::Section:: *)
(*******************************************************************************
## JiraJqlSearch

* Example command call:

    JiraJqlSearch

*)

ClearAll[JiraJqlSearch];

Options[JiraJqlSearch] := FilterRules[
    Options[JiraApiExecute],
    {"JiraWebsiteURL", "JiraWebsiteUsername", "JiraWebsitePassword"}
] ~Join~ {
    "MaxResults" -> 10,
    "StartAt" -> 1
};

JiraJqlSearch[jqlQuery_String, opts:OptionsPattern[]] := Module[
    {
        result, host, resourceName = "search", apiUrl, urlParams,
        username, password,
        loginInfo, contentType, method
    },
    host = OptionValue["JiraWebsiteURL"];
    username = OptionValue["JiraWebsiteUsername"];
    password = OptionValue["JiraWebsitePassword"];

    loginInfo = If[
        StringQ[username],
        username <> ":" <> If[StringQ[password], password, ""],
        ""
    ];

    apiUrl = URLBuild[{host, "jira", "rest", "api", "2", resourceName}];

    urlParams = {
        "jql" -> jqlQuery,
        "maxResults" -> ToString@OptionValue["MaxResults"],
        "startAt" -> ToString@OptionValue["StartAt"]
    };

    result = URLFetch[
        apiUrl,
        "Headers"->  {
            "u"-> loginInfo,
            "Content-Type"-> $JiraApiHttpContentType
        },
        "Parameters" -> urlParams,
        Method-> "GET"
    ];

    jsonStringToExpression[result]

];


JiraJqlSearch[jqlQuery_, "Issues", opts:OptionsPattern[]] := Module[
    {result},
    result = JiraJqlSearch[jqlQuery, opts];
    If[!MatchQ[result, {__Rule}], Return[$Failed]];
    result = Composition[
        Map[
            ("key"/.#) -> {
                "summary" -> ("summary" /. ("fields" /. #))
            } &,
            #
        ] &,
        If[
            MatchQ[#, {___, _["issues", _], ___}],
            "issues" /. #,
            Return[$Failed]
        ] &
    ][result];
    result
]


End[]; (* `Private` *)

EndPackage[];
