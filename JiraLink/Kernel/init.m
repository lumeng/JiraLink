(* JiraLink application entry point.

   Loaded either by the paclet manager (via PacletInfo.wl, whose Kernel
   extension names this directory and the context JiraLink`) or the old way, by
   putting the repository root on $Path and evaluating Needs["JiraLink`"].

   An explicit file name is used rather than a context name so that both routes
   work regardless of $Path, and so that the load no longer depends on the
   filesystem being case-insensitive -- the pre-1.0 init.m asked for the context
   JiraLink`Kernel`JiraLink` while the file on disk was called JIRALink.m. *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "JiraLink.wl"}]]
