(* ::Package:: *)

(* Unit.wlt -- offline tests. No network access, no credentials, no prompting.

   Run with

       wolframscript -code 'PacletDirectoryLoad["<repo root>"]; \
         TestReport[FileNameJoin[{"<repo root>", "JiraLink", "Tests", "Unit.wlt"}]]'

   or through the Wolfram MCP TestReport tool.

   These tests exercise request construction rather than request execution, which
   is exactly where the pre-1.0 package went wrong: it built requests that
   silently dropped the method and the body. *)

BeginTestSection["JiraLink unit tests"];

Needs["JiraLink`"];

(* Shorthands into the package's private context. *)
$p = "JiraLink`Private`";
normalizeURL   = Symbol[$p <> "jiraNormalizeURL"];
queryValue     = Symbol[$p <> "jiraQueryValue"];
normalizeQuery = Symbol[$p <> "jiraNormalizeQuery"];
extractItems   = Symbol[$p <> "jiraExtractItems"];
statusFailure  = Symbol[$p <> "jiraStatusFailure"];
timeSpent      = Symbol[$p <> "jiraTimeSpent"];
fromJSON       = Symbol[$p <> "jiraFromJSON"];
projectOf      = Symbol[$p <> "projectKeyFromIssueKey"];

(* A connection built by hand: no network, no credential lookup. *)
conn = JiraConnect["https://jira.example.com/jira",
  "Token" -> "SECRET-TOKEN", "Default" -> False];

connNoCtx = JiraConnect["https://jira.example.com",
  "Token" -> "T", "Default" -> False];

connBasic = JiraConnect["https://jira.example.com",
  "Username" -> "fred", "Password" -> "pw", "Default" -> False];


(* ::Section:: *)
(* Loading *)

VerificationTest[
  StringQ[$JiraLinkVersion],
  True,
  TestID -> "version-string"
];

VerificationTest[
  (* Loading must not have established a connection or read any credential. *)
  $JiraConnection,
  None,
  TestID -> "load-has-no-side-effects"
];


(* ::Section:: *)
(* Base URL normalization *)

VerificationTest[
  normalizeURL["https://jira.example.com/jira"],
  <|"Scheme" -> "https", "Domain" -> "jira.example.com", "Port" -> None,
    "ContextPath" -> {"jira"}|>,
  TestID -> "url-with-context-path"
];

VerificationTest[
  normalizeURL["https://jira.example.com/jira/"],
  <|"Scheme" -> "https", "Domain" -> "jira.example.com", "Port" -> None,
    "ContextPath" -> {"jira"}|>,
  TestID -> "url-trailing-slash"
];

VerificationTest[
  (* No scheme: default to https rather than letting URLParse put the host in
     the path, and rather than falling back to cleartext http. *)
  normalizeURL["jira.example.com"],
  <|"Scheme" -> "https", "Domain" -> "jira.example.com", "Port" -> None,
    "ContextPath" -> {}|>,
  TestID -> "url-bare-hostname"
];

VerificationTest[
  normalizeURL["http://jira.example.com:8080/jira"],
  <|"Scheme" -> "http", "Domain" -> "jira.example.com", "Port" -> 8080,
    "ContextPath" -> {"jira"}|>,
  TestID -> "url-explicit-port"
];


(* ::Section:: *)
(* Request construction: the core regression

   Every one of these would have failed against the pre-1.0 default code path,
   which built HTTPRequest[url, <||>] and so always sent GET with no body. *)

VerificationTest[
  JiraRequestObject["GET", {"issue", "ABC-1"}, "Connection" -> conn]["URL"],
  "https://jira.example.com/jira/rest/api/2/issue/ABC-1",
  TestID -> "url-assembly-platform"
];

VerificationTest[
  JiraRequestObject["GET", {"search"}, "Connection" -> connNoCtx]["URL"],
  "https://jira.example.com/rest/api/2/search",
  TestID -> "url-assembly-no-context-path"
];

VerificationTest[
  JiraRequestObject["GET", {"board", "42"}, "API" -> "agile", "Connection" -> conn]["URL"],
  "https://jira.example.com/jira/rest/agile/1.0/board/42",
  TestID -> "url-assembly-agile"
];

VerificationTest[
  JiraRequestObject["GET", {"tokens"}, "API" -> "pat", "Connection" -> conn]["URL"],
  "https://jira.example.com/jira/rest/pat/latest/tokens",
  TestID -> "url-assembly-pat"
];

VerificationTest[
  JiraRequestObject["POST", {"issue"}, "Connection" -> conn]["Method"],
  "POST",
  TestID -> "method-reaches-the-wire"
];

VerificationTest[
  JiraRequestObject["DELETE", {"issue", "ABC-1"}, "Connection" -> conn]["Method"],
  "DELETE",
  TestID -> "delete-method-reaches-the-wire"
];

VerificationTest[
  ImportString[
    JiraRequestObject["POST", {"issue"},
      "Body" -> <|"fields" -> <|"summary" -> "hello"|>|>,
      "Connection" -> conn]["Body"],
    "RawJSON"],
  <|"fields" -> <|"summary" -> "hello"|>|>,
  TestID -> "body-reaches-the-wire"
];

VerificationTest[
  Lookup[JiraRequestObject["GET", {"myself"}, "Connection" -> conn]["Headers"], "authorization"],
  "Bearer SECRET-TOKEN",
  TestID -> "bearer-authorization-header"
];

VerificationTest[
  Lookup[JiraRequestObject["GET", {"myself"}, "Connection" -> connBasic]["Headers"], "authorization"],
  "Basic " <> BaseEncode[StringToByteArray["fred:pw", "UTF-8"], "Base64"],
  TestID -> "basic-authorization-header"
];

VerificationTest[
  (* Multipart requests are refused by Jira unless this header is present. *)
  Lookup[
    JiraRequestObject["POST", {"issue", "ABC-1", "attachments"},
      "Multipart" -> {"file" -> File["x.txt"]}, "Connection" -> conn]["Headers"],
    "x-atlassian-token"],
  "no-check",
  TestID -> "xsrf-header-on-multipart"
];

VerificationTest[
  JiraRequestObject["GET", {"search"},
    "Parameters" -> {"jql" -> "project = ABC", "maxResults" -> 100},
    "Connection" -> connNoCtx]["URL"],
  "https://jira.example.com/rest/api/2/search?jql=project+%3D+ABC&maxResults=100",
  TestID -> "query-parameters-encoded"
];


(* ::Section:: *)
(* Query parameter normalization *)

VerificationTest[
  {queryValue[True], queryValue[False]},
  {"true", "false"},
  TestID -> "booleans-become-json-booleans"
];

VerificationTest[
  queryValue[{"summary", "status"}],
  "summary,status",
  TestID -> "lists-become-comma-separated"
];

VerificationTest[
  normalizeQuery[{"a" -> 1, "b" -> None, "c" -> True}],
  {"a" -> "1", "c" -> "true"},
  TestID -> "none-valued-parameters-dropped"
];


(* ::Section:: *)
(* JQL escaping *)

VerificationTest[
  JiraJQLEscape["simple"],
  "\"simple\"",
  TestID -> "jql-plain"
];

VerificationTest[
  JiraJQLEscape["has \"quotes\""],
  "\"has \\\"quotes\\\"\"",
  TestID -> "jql-embedded-quotes"
];

VerificationTest[
  JiraJQLEscape["back\\slash"],
  "\"back\\\\slash\"",
  TestID -> "jql-backslash"
];

VerificationTest[
  JiraJQLEscape[{"a", "b"}],
  "(\"a\", \"b\")",
  TestID -> "jql-list-becomes-in-clause"
];


(* ::Section:: *)
(* Issue keys *)

VerificationTest[
  JiraIssueKeyQ /@ {"ABC-1", "AB_C-123", "abc-1", "ABC", "ABC-1x", "ABC-1 "},
  {True, True, False, False, False, False},
  TestID -> "issue-key-predicate-is-anchored"
];

VerificationTest[
  projectOf["WALOCURATE-12616"],
  "WALOCURATE",
  TestID -> "project-key-from-issue-key"
];


(* ::Section:: *)
(* Pagination envelope shapes *)

VerificationTest[
  extractItems[<|"startAt" -> 0, "values" -> {1, 2, 3}|>, "values"],
  {1, 2, 3},
  TestID -> "envelope-values"
];

VerificationTest[
  extractItems[<|"startAt" -> 0, "issues" -> {1, 2}|>, Automatic],
  {1, 2},
  TestID -> "envelope-array-key-detected"
];

VerificationTest[
  (* /project and friends answer with a bare array and no envelope at all. *)
  extractItems[{1, 2, 3}, Automatic],
  {1, 2, 3},
  TestID -> "bare-array-passthrough"
];

VerificationTest[
  extractItems[<|"issueLinkTypes" -> {"a"}|>, "issueLinkTypes"],
  {"a"},
  TestID -> "wrapped-bare-array"
];


(* ::Section:: *)
(* Failure objects *)

VerificationTest[
  FailureQ[statusFailure[404, "GET", "https://x/y", "{}"]],
  True,
  TestID -> "failure-is-a-failure"
];

VerificationTest[
  statusFailure[404, "GET", "https://x/y", "{}"][[1]],
  "JiraNotFoundError",
  TestID -> "404-tagged-not-found"
];

VerificationTest[
  statusFailure[401, "GET", "https://x/y", "{}"][[1]],
  "JiraAuthenticationError",
  TestID -> "401-tagged-authentication"
];

VerificationTest[
  statusFailure[429, "GET", "https://x/y", "{}"][[1]],
  "JiraRateLimitError",
  TestID -> "429-tagged-rate-limit"
];

VerificationTest[
  statusFailure[400, "POST", "https://x/y",
    "{\"errorMessages\":[\"boom\"],\"errors\":{\"summary\":\"required\"}}"]["Errors"],
  <|"summary" -> "required"|>,
  TestID -> "jira-error-envelope-preserved"
];

VerificationTest[
  statusFailure[400, "POST", "https://x/y",
    "{\"errorMessages\":[\"boom\"],\"errors\":{}}"]["ErrorMessages"],
  {"boom"},
  TestID -> "jira-error-messages-preserved"
];


(* ::Section:: *)
(* JSON handling *)

VerificationTest[
  fromJSON[""],
  Null,
  TestID -> "empty-body-is-null-not-an-error"
];

VerificationTest[
  fromJSON["{\"a\":1}"],
  <|"a" -> 1|>,
  TestID -> "json-becomes-association"
];

VerificationTest[
  FailureQ[fromJSON["not json at all"]],
  True,
  TestID -> "unparseable-json-is-a-failure"
];


(* ::Section:: *)
(* Durations *)

VerificationTest[
  timeSpent["3h 30m"],
  <|"timeSpent" -> "3h 30m"|>,
  TestID -> "duration-string-passthrough"
];

VerificationTest[
  timeSpent[Quantity[90, "Minutes"]],
  <|"timeSpentSeconds" -> 5400|>,
  TestID -> "quantity-converted-to-seconds"
];


(* ::Section:: *)
(* Connection objects *)

VerificationTest[
  JiraConnectionQ[conn],
  True,
  TestID -> "connection-predicate"
];

VerificationTest[
  Normal[conn]["AuthType"],
  "Bearer",
  TestID -> "token-option-selects-bearer-auth"
];

VerificationTest[
  (* The summary box must not leak the token. *)
  StringFreeQ[ToString[conn], "SECRET-TOKEN"],
  True,
  TestID -> "connection-display-masks-secret"
];


(* ::Section:: *)
(* Pre-1.0 compatibility *)

VerificationTest[
  (* The old two-argument form still builds a request, and the association it
     was given still becomes the body. *)
  ImportString[
    JiraRequestObject["POST", {"issue"},
      "Body" -> <|"fields" -> <|"summary" -> "s"|>|>,
      "Connection" -> conn]["Body"],
    "RawJSON"]["fields"]["summary"],
  "s",
  TestID -> "legacy-body-shape"
];

VerificationTest[
  (* All seven pre-1.0 symbols still exist and carry usage messages. *)
  AllTrue[
    {"JiraApiExecute", "JiraIssueData", "JiraCreateIssue", "JiraDeleteIssue",
     "JiraJqlSearch", "JiraFindIssues", "JiraIssueOpen"},
    (* MessageName holds its first argument, so the symbol must be forced. *)
    StringQ[MessageName[Evaluate[Symbol["JiraLink`" <> #]], "usage"]] &],
  True,
  TestID -> "legacy-symbols-retained"
];

VerificationTest[
  (* StartAt used to default to 1, which skipped the first result. *)
  OptionValue[JiraJqlSearch, "StartAt"],
  0,
  TestID -> "startat-off-by-one-fixed"
];

VerificationTest[
  (* MaxItems must be the System symbol, not a shadowing package-local one. *)
  Context[MaxItems],
  "System`",
  TestID -> "no-maxitems-shadowing"
];

EndTestSection[];
