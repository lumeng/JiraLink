(* ::Package:: *)

(* :Title: JiraLink *)
(* :Context: JiraLink` *)
(* :Author: Meng LU <lumeng.dev@gmail.com> *)
(* :Package Version: 1.0.0 *)
(* :Copyright: (c) 2016-2026 Meng LU <lumeng.dev@gmail.com> *)
(* :Keywords: JIRA, Atlassian, REST, API, JiraLink *)

(* :Discussion:

   A Wolfram Language client for the Jira REST API as shipped with Jira
   Server / Data Center 9.x.  That means platform API v2 (/rest/api/2),
   the Agile API (/rest/agile/1.0), Jira Service Management
   (/rest/servicedeskapi), and personal access tokens (/rest/pat/latest).

   Cloud's API v3 is deliberately not supported: Data Center does not
   serve it, and the two disagree about how users are identified
   (DC uses `name`/`key`, Cloud uses `accountId`).

   Loading this package has no side effects.  Credentials are resolved
   lazily, on the first request, so the package can be used from
   wolframscript and from tests.

   Layering, low to high:

       Common.wl     shared constants, messages, option plumbing
       Auth.wl       credential resolution and storage
       Request.wl    the single path to the wire
       Paginate.wl   the startAt/maxResults/isLast envelope walker
       ...           one file per Jira resource family
       Compat.wl     the pre-1.0 API, reimplemented on the above
*)

BeginPackage["JiraLink`"];

(* Loading is idempotent: drop any definitions from a previous load so that
   re-reading the package during development does not merge old and new
   downvalues. *)
Unprotect @@ Names["JiraLink`*"];
ClearAll @@ Names["JiraLink`*"];


(* ::Section:: *)
(* Package information *)

$JiraLinkVersion::usage = "$JiraLinkVersion is the version string of the loaded JiraLink package.";

$JiraLinkDirectory::usage = "$JiraLinkDirectory is the directory containing the JiraLink package.";


(* ::Section:: *)
(* Connection and authentication *)

JiraConnect::usage = "JiraConnect[url] establishes a connection to the Jira instance at url, \
resolving credentials from the options given, from secure storage, or from the environment, \
and sets $JiraConnection.\nJiraConnect[] reconnects using the stored default instance.\n\
JiraConnect[url, \"Token\" -> token] authenticates with a personal access token.\n\
JiraConnect[url, Authentication -> <|\"Username\" -> u, \"Password\" -> p|>] uses HTTP basic \
authentication.";

JiraDisconnect::usage = "JiraDisconnect[] clears $JiraConnection. It does not remove stored credentials.";

$JiraConnection::usage = "$JiraConnection is the connection used by JiraLink functions when no \
\"Connection\" option is given. Set it with JiraConnect.";

$JiraDefaultURL::usage = "$JiraDefaultURL is the Jira base URL used by JiraConnect[] when called \
with no argument.";

JiraConnectionObject::usage = "JiraConnectionObject[...] represents a connection to a Jira instance, \
as returned by JiraConnect. Its displayed form masks the stored credential; Normal gives the \
underlying association.";

JiraConnectionQ::usage = "JiraConnectionQ[expr] gives True if expr is a valid Jira connection object.";

JiraStoreCredential::usage = "JiraStoreCredential[url] prompts for a personal access token in a \
masked field and saves it in the operating system's secure credential storage, under the key \
\"JiraLink:<domain>\". The token is never echoed, so it does not end up in the notebook, in the \
input history, or in the saved file.\n\
JiraStoreCredential[url, \"Basic\"] prompts for a username and password instead.\n\
JiraStoreCredential[url, <|\"Token\" -> token|>] stores a credential given explicitly; prefer the \
prompting form in a notebook.\n\
JiraStoreCredential[url, <|\"Username\" -> u, \"Password\" -> p|>] stores a basic-auth credential.";

JiraDeleteCredential::usage = "JiraDeleteCredential[url] removes the stored credential for url from \
secure credential storage.";

JiraCredentialQ::usage = "JiraCredentialQ[url] gives True if a credential for url is present in \
secure credential storage.";

JiraStoredCredentials::usage = "JiraStoredCredentials[] lists the Jira instances for which a \
credential is held in secure credential storage.";

JiraAuthenticationTest::usage = "JiraAuthenticationTest[] verifies the current connection by \
requesting the authenticated user's own record, and returns it. It gives a Failure if \
authentication does not succeed.";

JiraImportLegacyCredentials::usage = "JiraImportLegacyCredentials[password] reads the pre-1.0 \
encrypted configuration file ~/.jiralinkconfig.m using password, and copies the credential it \
contains into secure credential storage. This is a one-time migration step.";

JiraTokens::usage = "JiraTokens[] lists the personal access tokens belonging to the authenticated user.";

JiraCreateToken::usage = "JiraCreateToken[name] creates a personal access token named name and \
returns it. The raw token value is returned only once, at creation.\n\
JiraCreateToken[name, days] creates a token that expires after the given number of days.";

JiraDeleteToken::usage = "JiraDeleteToken[id] revokes the personal access token with the given id.";

JiraSessionLogin::usage = "JiraSessionLogin[username, password] creates a cookie-based session \
via /rest/auth/1/session.";

JiraSessionLogout::usage = "JiraSessionLogout[] invalidates the current cookie-based session.";

JiraSessionInfo::usage = "JiraSessionInfo[] returns information about the current cookie-based session.";


(* ::Section:: *)
(* Generic access *)

JiraApiExecute::usage = "JiraApiExecute[method, {segment1, segment2, ...}] performs an arbitrary \
Jira REST call and returns the decoded response.\n\
JiraApiExecute[method, path, \"Body\" -> body] sends body as JSON.\n\
JiraApiExecute[method, path, \"API\" -> \"agile\"] targets the Agile API instead of the platform API; \
other values are \"platform\", \"servicedesk\", \"pat\" and \"auth\".\n\
JiraApiExecute[resource, params] is the pre-1.0 form and is still supported.";

JiraRequestObject::usage = "JiraRequestObject[method, path] returns the HTTPRequest that \
JiraApiExecute would send, without sending it. Useful for debugging and for tests.";

$JiraRequestLog::usage = "$JiraRequestLog is a list of the most recent Jira requests, kept when \
$JiraLogRequests is True.";

$JiraLogRequests::usage = "$JiraLogRequests determines whether requests are recorded in $JiraRequestLog. \
The default is False.";


(* ::Section:: *)
(* Issues *)

JiraIssue::usage = "JiraIssue[key] returns the complete record of the issue with the given key, \
including its metadata and all of its fields.";

JiraIssueData::usage = "JiraIssueData[key] returns the fields of the issue with the given key.\n\
JiraIssueData[key, field] returns a single field.\n\
JiraIssueData[key, {field1, field2, ...}] returns the given fields.";

JiraCreateIssue::usage = "JiraCreateIssue[project, summary] creates a task in project.\n\
JiraCreateIssue[project, summary, issueType] creates an issue of the given type.\n\
JiraCreateIssue[project, summary, issueType, fields] additionally sets the given fields.\n\
JiraCreateIssue[parentKey, summary, \"Subtask\", fields] creates a subtask of parentKey.";

JiraCreateIssues::usage = "JiraCreateIssues[{spec1, spec2, ...}] creates several issues in one \
request, where each spec is an association of Jira fields.";

JiraUpdateIssue::usage = "JiraUpdateIssue[key, fields] updates the given fields of an issue.\n\
JiraUpdateIssue[key, fields, \"Update\" -> ops] additionally applies the given update operations.";

JiraDeleteIssue::usage = "JiraDeleteIssue[key] deletes an issue. Use \"DeleteSubtasks\" -> True to \
delete its subtasks along with it.";

JiraAssignIssue::usage = "JiraAssignIssue[key, username] assigns an issue to a user. Use \
Automatic for the project's default assignee, and None to leave it unassigned.";

JiraTransitions::usage = "JiraTransitions[key] lists the workflow transitions currently available \
on an issue.";

JiraTransitionIssue::usage = "JiraTransitionIssue[key, transition] moves an issue through the named \
transition, given either as its name or as its numeric id.\n\
JiraTransitionIssue[key, transition, fields] also sets the given fields as part of the transition.";

JiraComments::usage = "JiraComments[key] returns the comments on an issue.";

JiraComment::usage = "JiraComment[key, id] returns a single comment on an issue.";

JiraAddComment::usage = "JiraAddComment[key, text] adds a comment to an issue.";

JiraUpdateComment::usage = "JiraUpdateComment[key, id, text] edits an existing comment.";

JiraDeleteComment::usage = "JiraDeleteComment[key, id] deletes a comment.";

JiraWorklogs::usage = "JiraWorklogs[key] returns the work log entries of an issue.";

JiraAddWorklog::usage = "JiraAddWorklog[key, timeSpent] logs work against an issue, where timeSpent \
is a Jira duration string such as \"3h 30m\" or a Quantity of time.";

JiraUpdateWorklog::usage = "JiraUpdateWorklog[key, id, data] updates a work log entry.";

JiraDeleteWorklog::usage = "JiraDeleteWorklog[key, id] deletes a work log entry.";

JiraAddAttachment::usage = "JiraAddAttachment[key, file] attaches a file to an issue.\n\
JiraAddAttachment[key, {file1, file2, ...}] attaches several files.";

JiraAttachment::usage = "JiraAttachment[id] returns the metadata of an attachment.";

JiraAttachmentDownload::usage = "JiraAttachmentDownload[id, file] downloads an attachment to file.";

JiraDeleteAttachment::usage = "JiraDeleteAttachment[id] deletes an attachment.";

JiraAttachmentSettings::usage = "JiraAttachmentSettings[] reports whether attachments are enabled \
on the server and what the maximum upload size is.";

JiraRemoteLinks::usage = "JiraRemoteLinks[key] lists the remote links of an issue.";

JiraAddRemoteLink::usage = "JiraAddRemoteLink[key, url, title] adds a remote link to an issue.";

JiraDeleteRemoteLink::usage = "JiraDeleteRemoteLink[key, id] removes a remote link from an issue.";

JiraCreateIssueLink::usage = "JiraCreateIssueLink[inwardKey, type, outwardKey] links two issues \
with the named link type, for example \"Blocks\" or \"Relates\".";

JiraIssueLink::usage = "JiraIssueLink[id] returns a single issue link.";

JiraDeleteIssueLink::usage = "JiraDeleteIssueLink[id] deletes an issue link.";

JiraVotes::usage = "JiraVotes[key] returns the votes on an issue.";

JiraAddVote::usage = "JiraAddVote[key] casts the authenticated user's vote for an issue.";

JiraDeleteVote::usage = "JiraDeleteVote[key] withdraws the authenticated user's vote from an issue.";

JiraWatchers::usage = "JiraWatchers[key] returns the watchers of an issue.";

JiraAddWatcher::usage = "JiraAddWatcher[key, username] adds a watcher to an issue.";

JiraDeleteWatcher::usage = "JiraDeleteWatcher[key, username] removes a watcher from an issue.";

JiraNotify::usage = "JiraNotify[key, subject, body] sends a notification about an issue to its \
watchers. Use the \"To\" option to direct it at particular users, groups or roles.";

JiraIssueChangelog::usage = "JiraIssueChangelog[key] returns the change history of an issue.";

JiraIssueProperties::usage = "JiraIssueProperties[key] lists the property keys stored on an issue.";

JiraIssueProperty::usage = "JiraIssueProperty[key, propertyKey] returns a property stored on an issue.";

JiraSetIssueProperty::usage = "JiraSetIssueProperty[key, propertyKey, value] stores a property on an issue.";

JiraDeleteIssueProperty::usage = "JiraDeleteIssueProperty[key, propertyKey] deletes a property from an issue.";

JiraEditMeta::usage = "JiraEditMeta[key] reports which fields of an issue may be edited, and what \
values they accept.";

JiraCreateMeta::usage = "JiraCreateMeta[project] lists the issue types available when creating an \
issue in a project.\n\
JiraCreateMeta[project, issueTypeId] lists the fields available for one issue type.\n\
Jira 9 removed the server-wide form of this endpoint, so a project must always be given.";

JiraIssueOpen::usage = "JiraIssueOpen[key] opens an issue in the default web browser.";

JiraIssueURL::usage = "JiraIssueURL[key] gives the browser URL of an issue.";

JiraIssueKeyQ::usage = "JiraIssueKeyQ[string] gives True if string has the form of a Jira issue key.";

JiraArchiveIssue::usage = "JiraArchiveIssue[key] archives an issue. Requires Jira Data Center.";

JiraRestoreIssue::usage = "JiraRestoreIssue[key] restores an archived issue. Requires Jira Data Center.";


(* ::Section:: *)
(* Search *)

JiraSearch::usage = "JiraSearch[jql] runs a JQL query and returns the matching issues, following \
pagination as far as MaxItems allows.\n\
JiraSearch[jql, fields] returns only the given fields of each issue.";

JiraJqlSearch::usage = "JiraJqlSearch[jql] runs a JQL query and returns the raw search response, \
including the pagination envelope.";

JiraFindIssues::usage = "JiraFindIssues[jql] returns the keys of the issues matching a JQL query.\n\
JiraFindIssues[jql, \"Properties\"] returns all fields of each issue, keyed by issue key.\n\
JiraFindIssues[jql, \"CoreProperties\"] does the same but omits custom fields.\n\
JiraFindIssues[jql, field] returns one field of each issue.\n\
JiraFindIssues[jql, {field1, field2, ...}] returns the given fields of each issue.";

JiraIssueCount::usage = "JiraIssueCount[jql] gives the number of issues matching a JQL query, \
without retrieving them.";

JiraIssuePicker::usage = "JiraIssuePicker[query] suggests issues matching a partial key or summary.";

JiraJQLEscape::usage = "JiraJQLEscape[string] quotes and escapes string so that it can be used \
safely as a literal value in a JQL query.";

JiraValidateJQL::usage = "JiraValidateJQL[jql] checks a JQL query for errors without running it.";


(* ::Section:: *)
(* Projects, versions and components *)

JiraProjects::usage = "JiraProjects[] lists the projects visible to the authenticated user.";

JiraProject::usage = "JiraProject[key] returns the record of a project.";

JiraCreateProject::usage = "JiraCreateProject[data] creates a project. Requires administrator rights.";

JiraUpdateProject::usage = "JiraUpdateProject[key, data] updates a project. Requires administrator rights.";

JiraDeleteProject::usage = "JiraDeleteProject[key] deletes a project. Requires administrator rights.";

JiraProjectVersions::usage = "JiraProjectVersions[key] lists the versions of a project.";

JiraProjectComponents::usage = "JiraProjectComponents[key] lists the components of a project.";

JiraProjectStatuses::usage = "JiraProjectStatuses[key] lists the issue types of a project together \
with the statuses available to each.";

JiraProjectRoles::usage = "JiraProjectRoles[key] lists the roles defined on a project.\n\
JiraProjectRoles[key, id] returns the members of one role.";

JiraProjectCategories::usage = "JiraProjectCategories[] lists the project categories defined on the server.";

JiraProjectProperties::usage = "JiraProjectProperties[key] lists the property keys stored on a project.";

JiraVersion::usage = "JiraVersion[id] returns the record of a version.";

JiraCreateVersion::usage = "JiraCreateVersion[project, name] creates a version in a project.";

JiraUpdateVersion::usage = "JiraUpdateVersion[id, data] updates a version.";

JiraDeleteVersion::usage = "JiraDeleteVersion[id] deletes a version.";

JiraVersionIssueCounts::usage = "JiraVersionIssueCounts[id] gives the number of issues that reference \
a version as their fix version or affected version.";

JiraVersionUnresolvedCount::usage = "JiraVersionUnresolvedCount[id] gives the number of unresolved \
issues assigned to a version.";

JiraComponent::usage = "JiraComponent[id] returns the record of a component.";

JiraCreateComponent::usage = "JiraCreateComponent[project, name] creates a component in a project.";

JiraUpdateComponent::usage = "JiraUpdateComponent[id, data] updates a component.";

JiraDeleteComponent::usage = "JiraDeleteComponent[id] deletes a component.";

JiraComponentIssueCount::usage = "JiraComponentIssueCount[id] gives the number of issues assigned \
to a component.";


(* ::Section:: *)
(* Users and groups *)

JiraMyself::usage = "JiraMyself[] returns the record of the authenticated user.";

JiraUser::usage = "JiraUser[username] returns the record of a user. Jira Data Center identifies \
users by name, not by the account id used in Jira Cloud.";

JiraUserSearch::usage = "JiraUserSearch[query] finds users whose name or display name matches query.";

JiraAssignableUsers::usage = "JiraAssignableUsers[project] lists the users to whom issues in a \
project may be assigned.\nJiraAssignableUsers[project, query] restricts the list by a search string.";

JiraUserPicker::usage = "JiraUserPicker[query] suggests users matching a partial name, in the form \
used by Jira's own user pickers.";

JiraUserGroups::usage = "JiraUserGroups[username] lists the groups a user belongs to.";

JiraCreateUser::usage = "JiraCreateUser[data] creates a user. Requires administrator rights.";

JiraUpdateUser::usage = "JiraUpdateUser[username, data] updates a user. Requires administrator rights.";

JiraDeleteUser::usage = "JiraDeleteUser[username] deletes a user. Requires administrator rights.";

JiraGroupMembers::usage = "JiraGroupMembers[group] lists the members of a group.";

JiraCreateGroup::usage = "JiraCreateGroup[name] creates a group. Requires administrator rights.";

JiraDeleteGroup::usage = "JiraDeleteGroup[name] deletes a group. Requires administrator rights.";

JiraAddGroupUser::usage = "JiraAddGroupUser[group, username] adds a user to a group.";

JiraRemoveGroupUser::usage = "JiraRemoveGroupUser[group, username] removes a user from a group.";

JiraGroupPicker::usage = "JiraGroupPicker[query] suggests groups matching a partial name.";


(* ::Section:: *)
(* Fields and other metadata *)

JiraFields::usage = "JiraFields[] lists all fields defined on the server, both system and custom.";

JiraCustomFields::usage = "JiraCustomFields[] lists only the custom fields defined on the server.";

JiraCustomFieldOption::usage = "JiraCustomFieldOption[id] returns a single custom field option. \
Jira Data Center offers no endpoint for listing or editing the options of a custom field; that \
family of endpoints exists only in Jira Cloud.";

JiraCreateCustomField::usage = "JiraCreateCustomField[data] creates a custom field. Requires \
administrator rights.";

JiraIssueTypes::usage = "JiraIssueTypes[] lists the issue types defined on the server.";

JiraIssueType::usage = "JiraIssueType[id] returns a single issue type.";

JiraPriorities::usage = "JiraPriorities[] lists the issue priorities defined on the server.";

JiraResolutions::usage = "JiraResolutions[] lists the resolutions defined on the server.";

JiraStatuses::usage = "JiraStatuses[] lists the issue statuses defined on the server.";

JiraStatusCategories::usage = "JiraStatusCategories[] lists the status categories defined on the server.";

JiraIssueLinkTypes::usage = "JiraIssueLinkTypes[] lists the issue link types defined on the server.";

JiraSecuritySchemes::usage = "JiraSecuritySchemes[] lists the issue security schemes defined on the server.";


(* ::Section:: *)
(* Server metadata, filters and dashboards *)

JiraServerInfo::usage = "JiraServerInfo[] returns the version and deployment information of the \
Jira server.";

JiraConfiguration::usage = "JiraConfiguration[] reports which optional Jira features are enabled.";

JiraMyPermissions::usage = "JiraMyPermissions[] reports the authenticated user's global permissions.\n\
JiraMyPermissions[project] reports their permissions within a project.";

JiraPermissions::usage = "JiraPermissions[] lists all permissions defined on the server.";

JiraApplicationRoles::usage = "JiraApplicationRoles[] lists the application roles defined on the server.";

JiraFilters::usage = "JiraFilters[] lists the authenticated user's favourite filters.";

JiraFilter::usage = "JiraFilter[id] returns a saved filter.";

JiraCreateFilter::usage = "JiraCreateFilter[name, jql] saves a new filter.";

JiraUpdateFilter::usage = "JiraUpdateFilter[id, data] updates a saved filter.";

JiraDeleteFilter::usage = "JiraDeleteFilter[id] deletes a saved filter.";

JiraFilterPermissions::usage = "JiraFilterPermissions[id] lists the share permissions of a filter.";

JiraDashboards::usage = "JiraDashboards[] lists the dashboards visible to the authenticated user.";

JiraDashboard::usage = "JiraDashboard[id] returns a single dashboard.";


(* ::Section:: *)
(* Agile: boards, sprints and epics *)

JiraBoards::usage = "JiraBoards[] lists the agile boards visible to the authenticated user.\n\
JiraBoards[project] lists the boards of a project.";

JiraBoard::usage = "JiraBoard[id] returns a single agile board.";

JiraCreateBoard::usage = "JiraCreateBoard[name, type, filterId] creates an agile board, where type \
is \"scrum\" or \"kanban\".";

JiraDeleteBoard::usage = "JiraDeleteBoard[id] deletes an agile board.";

JiraBoardConfiguration::usage = "JiraBoardConfiguration[id] returns the configuration of a board, \
including its columns and estimation settings.";

JiraBoardIssues::usage = "JiraBoardIssues[id] lists the issues on a board.";

JiraBoardBacklog::usage = "JiraBoardBacklog[id] lists the issues in a board's backlog.";

JiraBoardEpics::usage = "JiraBoardEpics[id] lists the epics on a board.";

JiraBoardSprints::usage = "JiraBoardSprints[id] lists the sprints of a board. Use the \"State\" \
option to restrict to \"future\", \"active\" or \"closed\" sprints.";

JiraBoardProjects::usage = "JiraBoardProjects[id] lists the projects associated with a board.";

JiraBoardVersions::usage = "JiraBoardVersions[id] lists the versions associated with a board.";

JiraSprint::usage = "JiraSprint[id] returns a single sprint.";

JiraCreateSprint::usage = "JiraCreateSprint[boardId, name] creates a sprint on a board.";

JiraUpdateSprint::usage = "JiraUpdateSprint[id, data] updates a sprint. Setting \"state\" to \
\"active\" starts it and \"closed\" completes it.";

JiraDeleteSprint::usage = "JiraDeleteSprint[id] deletes a sprint.";

JiraSprintIssues::usage = "JiraSprintIssues[id] lists the issues in a sprint.";

JiraMoveIssuesToSprint::usage = "JiraMoveIssuesToSprint[id, {key1, key2, ...}] moves issues into a sprint.";

JiraMoveIssuesToBacklog::usage = "JiraMoveIssuesToBacklog[{key1, key2, ...}] moves issues to the backlog.";

JiraEpic::usage = "JiraEpic[key] returns a single epic.";

JiraUpdateEpic::usage = "JiraUpdateEpic[key, data] updates an epic.";

JiraEpicIssues::usage = "JiraEpicIssues[key] lists the issues belonging to an epic.\n\
JiraEpicIssues[None] lists the issues on a board that belong to no epic.";

JiraMoveIssuesToEpic::usage = "JiraMoveIssuesToEpic[key, {issue1, issue2, ...}] moves issues into an epic.";

JiraRankEpic::usage = "JiraRankEpic[key, \"Before\" -> otherKey] ranks one epic relative to another.";

JiraRankIssues::usage = "JiraRankIssues[{key1, key2, ...}, \"Before\" -> otherKey] ranks issues \
relative to another issue.";


(* ::Section:: *)
(* Jira Service Management *)

JiraServiceDeskInfo::usage = "JiraServiceDeskInfo[] returns version information for Jira Service \
Management, or a Failure if it is not installed.";

JiraServiceDesks::usage = "JiraServiceDesks[] lists the service desks on the server.";

JiraServiceDesk::usage = "JiraServiceDesk[id] returns a single service desk.";

JiraRequestTypes::usage = "JiraRequestTypes[serviceDeskId] lists the request types of a service desk.";

JiraCustomerRequests::usage = "JiraCustomerRequests[] lists customer requests visible to the \
authenticated user.";

JiraCustomerRequest::usage = "JiraCustomerRequest[id] returns a single customer request.";

JiraCreateCustomerRequest::usage = "JiraCreateCustomerRequest[serviceDeskId, requestTypeId, fields] \
raises a customer request.";

JiraRequestComments::usage = "JiraRequestComments[id] lists the comments on a customer request.";

JiraAddRequestComment::usage = "JiraAddRequestComment[id, text] comments on a customer request.";

JiraRequestTransitions::usage = "JiraRequestTransitions[id] lists the transitions available on a \
customer request.";

JiraTransitionRequest::usage = "JiraTransitionRequest[id, transitionId] transitions a customer request.";

JiraRequestParticipants::usage = "JiraRequestParticipants[id] lists the participants of a customer request.";

JiraRequestSLA::usage = "JiraRequestSLA[id] reports the service level agreement status of a customer request.";

JiraOrganizations::usage = "JiraOrganizations[] lists the organizations defined in Jira Service Management.";


(* ::Section:: *)
(* Administration *)

JiraWorkflows::usage = "JiraWorkflows[] lists the workflows defined on the server. Requires \
administrator rights.";

JiraWorkflowScheme::usage = "JiraWorkflowScheme[id] returns a workflow scheme. Requires \
administrator rights.";

JiraNotificationScheme::usage = "JiraNotificationScheme[] lists the notification schemes.\n\
JiraNotificationScheme[id] returns one notification scheme.";

JiraPermissionScheme::usage = "JiraPermissionScheme[] lists the permission schemes.\n\
JiraPermissionScheme[id] returns one permission scheme.";

JiraPrioritySchemes::usage = "JiraPrioritySchemes[] lists the priority schemes.";

JiraScreens::usage = "JiraScreens[] lists the screens defined on the server.";

JiraScreenAvailableFields::usage = "JiraScreenAvailableFields[id] lists the fields that may be \
added to a screen.";

JiraSecurityLevel::usage = "JiraSecurityLevel[id] returns an issue security level.";

JiraAuditRecords::usage = "JiraAuditRecords[] returns records from the audit log. Requires \
administrator rights.";

JiraApplicationProperties::usage = "JiraApplicationProperties[] lists the server's advanced settings.";

JiraSetApplicationProperty::usage = "JiraSetApplicationProperty[id, value] changes an advanced \
setting. Requires administrator rights.";

JiraReindex::usage = "JiraReindex[] starts a reindex. Requires administrator rights.";

JiraReindexStatus::usage = "JiraReindexStatus[] reports the progress of a running reindex.";

JiraReindexIssues::usage = "JiraReindexIssues[{key1, key2, ...}] reindexes particular issues.";

JiraIndexSnapshot::usage = "JiraIndexSnapshot[] lists index snapshots. Requires Jira Data Center.";

JiraClusterNodes::usage = "JiraClusterNodes[] lists the nodes of a Jira Data Center cluster.";

JiraReleaseWebSudo::usage = "JiraReleaseWebSudo[] releases the current WebSudo session.";

JiraUpgradeStatus::usage = "JiraUpgradeStatus[] reports the status of pending upgrade tasks.";

JiraRunUpgrade::usage = "JiraRunUpgrade[] runs pending upgrade tasks. Requires administrator rights.";


(* ::Section:: *)
(* Options shared across the package *)

(* Options are string-keyed ("Connection", "Parameters", "Fields", ...), following
   the convention the package already used, with two exceptions that reuse existing
   System` symbols rather than shadowing them:

     MaxItems       how many results to take from a paginated endpoint (default All)
     TimeConstraint how long to wait for a response

   Declaring either of those here would create JiraLink`MaxItems and
   JiraLink`TimeConstraint shadowing the System` symbols of the same name, so they
   are deliberately left undeclared and used unqualified. *)


Begin["`Private`"];

$JiraLinkDirectory = DirectoryName[$InputFileName];

$JiraLinkVersion = "1.0.0";

(* The submodules are read inside JiraLink`Private`, so they share one private
   context and can see the public symbols declared above without qualification. *)
Scan[
  Get[FileNameJoin[{$JiraLinkDirectory, #}]] &,
  {
    "Common.wl",
    "Auth.wl",
    "Request.wl",
    "Paginate.wl",
    "Issues.wl",
    "Search.wl",
    "Projects.wl",
    "Users.wl",
    "Fields.wl",
    "Meta.wl",
    "Agile.wl",
    "ServiceDesk.wl",
    "Admin.wl",
    "Compat.wl"
  }
];

End[]; (* `Private` *)

EndPackage[];
