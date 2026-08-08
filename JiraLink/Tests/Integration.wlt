(* ::Package:: *)

(* Integration.wlt -- tests that talk to a real Jira server.

   These are opt-in. Nothing runs unless both of the following are set:

       JIRA_URL           base URL, e.g. https://jira.example.com
       JIRA_TOKEN         a personal access token
                          (or JIRA_USER + JIRA_PASSWORD for basic auth)

   and, for the tests that create and destroy issues,

       JIRA_TEST_PROJECT  the key of a project you are content to write into

   Without JIRA_TEST_PROJECT the write tests are skipped and only read-only
   checks run. The write tests clean up after themselves: every issue they
   create is deleted again in the same section.

   Run with

       JIRA_URL=... JIRA_TOKEN=... JIRA_TEST_PROJECT=SCRATCH \
         wolframscript -code 'PacletDirectoryLoad["<repo>"]; \
           TestReport["<repo>/JiraLink/Tests/Integration.wlt"]'
*)

BeginTestSection["JiraLink integration tests"];

Needs["JiraLink`"];

$url     = Environment["JIRA_URL"];
$project = Environment["JIRA_TEST_PROJECT"];

$live    = StringQ[$url] && $url =!= "";
$canWrite = $live && StringQ[$project] && $project =!= "";

If[!$live,
  Print["JIRA_URL is not set: skipping all integration tests."];
];
If[$live && !$canWrite,
  Print["JIRA_TEST_PROJECT is not set: skipping the write tests."];
];

If[$live, JiraConnect[$url]];


(* ::Section:: *)
(* Connectivity and authentication *)

VerificationTest[
  If[!$live, True, AssociationQ[JiraServerInfo[]]],
  True,
  TestID -> "server-info-reachable"
];

VerificationTest[
  (* This is the check that the pre-1.0 JQL search could never pass, because it
     sent no usable credential. *)
  If[!$live, True, StringQ[Lookup[JiraAuthenticationTest[], "name", None]]],
  True,
  TestID -> "authenticated-as-a-real-user"
];

VerificationTest[
  If[!$live, True, ListQ[JiraProjects[]]],
  True,
  TestID -> "projects-listed"
];

VerificationTest[
  If[!$live, True, ListQ[JiraFields[]]],
  True,
  TestID -> "fields-listed"
];


(* ::Section:: *)
(* Search and pagination *)

VerificationTest[
  If[!$live, True,
    With[{r = JiraSearch["order by created DESC", MaxItems -> 3]},
      ListQ[r] && Length[r] <= 3]],
  True,
  TestID -> "search-respects-maxitems"
];

VerificationTest[
  If[!$live, True, IntegerQ[JiraIssueCount["order by created DESC"]]],
  True,
  TestID -> "issue-count-returns-an-integer"
];

VerificationTest[
  (* A deliberately malformed query must come back as a Failure, not as data. *)
  If[!$live, True, FailureQ[JiraSearch["this is not valid jql at all ((("]]],
  True,
  TestID -> "bad-jql-gives-a-failure"
];

VerificationTest[
  If[!$live, True,
    With[{f = JiraIssue["NOSUCHPROJECT-999999"]},
      FailureQ[f] && f["StatusCode"] === 404]],
  True,
  TestID -> "missing-issue-gives-404-failure"
];


(* ::Section:: *)
(* Write round trip

   Creates an issue, reads it back, comments on it, then deletes it. This is the
   sequence that silently did nothing before: the default request path dropped
   the method and the body, so the POST was really a GET. *)

$created = None;

VerificationTest[
  If[!$canWrite, True,
    Module[{res},
      res = JiraCreateIssue[$project, "JiraLink integration smoke test", "Task"];
      $created = If[AssociationQ[res], Lookup[res, "key", None], None];
      StringQ[$created]
    ]],
  True,
  TestID -> "issue-actually-created"
];

VerificationTest[
  If[!$canWrite || !StringQ[$created], True,
    JiraIssueData[$created, "summary"] === "JiraLink integration smoke test"],
  True,
  TestID -> "created-issue-reads-back"
];

VerificationTest[
  If[!$canWrite || !StringQ[$created], True,
    AssociationQ[JiraAddComment[$created, "comment from the JiraLink test suite"]]],
  True,
  TestID -> "comment-added"
];

VerificationTest[
  If[!$canWrite || !StringQ[$created], True,
    Length[JiraComments[$created]] >= 1],
  True,
  TestID -> "comment-reads-back"
];

VerificationTest[
  If[!$canWrite || !StringQ[$created], True,
    ListQ[JiraTransitions[$created]]],
  True,
  TestID -> "transitions-listed"
];

VerificationTest[
  If[!$canWrite || !StringQ[$created], True,
    JiraDeleteIssue[$created] === Null],
  True,
  TestID -> "issue-actually-deleted"
];

VerificationTest[
  If[!$canWrite || !StringQ[$created], True,
    With[{f = JiraIssue[$created]},
      FailureQ[f] && f["StatusCode"] === 404]],
  True,
  TestID -> "deleted-issue-is-gone"
];

EndTestSection[];
