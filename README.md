# JiraLink

A Wolfram Language client for the Jira REST API, targeting **Jira Server / Data
Center 9.x** (platform API v2, the Agile API, Jira Service Management, and
personal access tokens).

Jira Cloud's API v3 is not supported, and deliberately so: Data Center does not
serve it, and the two disagree about how users are identified — Data Center uses
`name`/`key`, Cloud uses `accountId`.

## Installing

```wolfram
PacletDirectoryLoad["/path/to/this/repository"];
Needs["JiraLink`"]
```

Loading has no side effects: it does not prompt, and it does not read any
credential. That means the package works from `wolframscript` and from tests.

The older route still works too — put the repository root on `$Path` and
evaluate ``Needs["JiraLink`"]``.

## Connecting

Personal access tokens are the recommended credential on Data Center. Create one
from your Jira profile under *Personal Access Tokens*, then:

```wolfram
(* store it once, in the operating system's secure credential storage *)
JiraStoreCredential["https://jira.example.com", <|"Token" -> "your-token"|>];

(* thereafter *)
JiraConnect["https://jira.example.com"];
JiraAuthenticationTest[]
```

Credentials are resolved on the first request, in this order: options given on
the call, then secure storage, then the environment variables `JIRA_URL`,
`JIRA_TOKEN`, `JIRA_USER` and `JIRA_PASSWORD`. Basic authentication still works
on Data Center and is used automatically when a username and password are what
is available.

Include the context path in the base URL if your server has one — pass
`https://jira.example.com/jira` rather than `https://jira.example.com`. Use
`https` and no explicit port number.

If you used a version of this package before 1.0, your credential is in an
encrypted file. Import it once:

```wolfram
JiraImportLegacyCredentials["the password you chose"]
```

## Using it

```wolfram
JiraIssue["ABC-123"]                          (* the whole record            *)
JiraIssueData["ABC-123", "summary"]           (* one field                   *)
JiraIssueData["ABC-123", {"summary", "status"}]

JiraSearch["project = ABC AND assignee = currentUser()"]   (* all pages       *)
JiraSearch["project = ABC", MaxItems -> 20]                (* first 20        *)
JiraFindIssues["project = ABC", "CoreProperties"]

issue = JiraCreateIssue["ABC", "Something is broken", "Bug",
  <|"description" -> "Steps to reproduce ...",
    "labels"      -> {"triage"}|>];

JiraAddComment[issue["key"], "Looking into it."];
JiraTransitionIssue[issue["key"], "In Progress"];   (* by name, not by id     *)
JiraAssignIssue[issue["key"], "fred"];
JiraAddWorklog[issue["key"], Quantity[90, "Minutes"]];
JiraAddAttachment[issue["key"], "/path/to/log.txt"];
JiraDeleteIssue[issue["key"]];
```

Boards and sprints:

```wolfram
JiraBoards["ABC"]
JiraBoardSprints[42, "State" -> "active"]
JiraSprintIssues[7]
JiraMoveIssuesToSprint[7, {"ABC-1", "ABC-2"}]
```

Anything not wrapped explicitly is reachable directly:

```wolfram
JiraApiExecute["GET",  {"issue", "ABC-1", "comment"}, "Parameters" -> {"expand" -> "renderedBody"}]
JiraApiExecute["POST", {"issue"}, "Body" -> <|"fields" -> <|...|>|>]
JiraApiExecute["GET",  {"board", "42", "sprint"}, "API" -> "agile"]
```

`"API"` selects among `"platform"` (the default), `"agile"`, `"servicedesk"`,
`"pat"` and `"auth"`.

## Errors

Failed requests return `Failure` objects rather than raw data, tagged so that
callers can branch on them:

```wolfram
f = JiraIssue["NOSUCH-1"];
FailureQ[f]           (* True                    *)
f["StatusCode"]       (* 404                     *)
f["ErrorMessages"]    (* Jira's own message list *)
f["Errors"]           (* per-field errors        *)
```

Tags are `JiraHTTPError`, `JiraAuthenticationError`, `JiraNotFoundError`,
`JiraRateLimitError`, `JiraConnectionError` and `JiraParseError`. Rate-limited
requests are retried automatically, honouring Jira's `Retry-After` header.

## Pagination

Functions that return lists follow pagination to the end by default. Use
`MaxItems -> n` to stop early. Jira caps page sizes per endpoint and silently
clamps oversized requests, so the paginator believes the page size it is given
back rather than the one it asked for.

## Testing

```wolfram
TestReport["JiraLink/Tests/Unit.wlt"]          (* offline, no credentials *)
```

`Tests/Integration.wlt` talks to a real server and is skipped unless `JIRA_URL`
is set; its write tests additionally need `JIRA_TEST_PROJECT` and clean up after
themselves.

## Upgrading from before 1.0

The seven original functions keep their names and argument patterns. Three
changes are visible:

- `JiraFindIssues` and `JiraJqlSearch` return associations rather than nested
  lists of rules. Code that did `"key" /. result` should now do `result["key"]`.
- `JiraCreateIssue` no longer forces `priority` to `"Major"`. Pass
  `"Priority" -> "Major"` to keep the old behaviour.
- Errors arrive as `Failure` objects instead of `$Failed`, `Null` or
  `Missing["NotAvailable"]`.

Writes and JQL search did not previously work at all — the default request path
discarded the HTTP method and body, and the search path sent no usable
credential — so any code depending on those was already failing.

## References

- [Jira 9.12 platform REST API](https://docs.atlassian.com/software/jira/docs/api/REST/9.12.0/)
- [Jira Software (Agile) 9.12 REST API](https://docs.atlassian.com/jira-software/REST/9.12.0/)
- [Using personal access tokens](https://confluence.atlassian.com/enterprise/using-personal-access-tokens-1026032365.html)
