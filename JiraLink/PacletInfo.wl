(* ::Package:: *)

(* Paclet metadata for JiraLink.

   Load during development with

       PacletDirectoryLoad["/path/to/repository-root"];
       Needs["JiraLink`"]

   The legacy `Needs` route (put the repository root on $Path) still works,
   because Kernel/init.m is retained.
*)

PacletObject[<|
  "Name"            -> "JiraLink",
  "Version"         -> "1.0.0",
  "WolframVersion"  -> "12.0+",
  "Description"     -> "Wolfram Language client for the Jira REST API (Server / Data Center).",
  "Creator"         -> "Meng LU <lumeng.dev@gmail.com>",
  "License"         -> "MIT",
  "Keywords"        -> {"JIRA", "Atlassian", "REST", "API", "issue tracker", "Agile"},
  "Extensions"      -> {
    {"Kernel",
      "Root"    -> "Kernel",
      "Context" -> {"JiraLink`"}
    },
    {"Documentation",
      "Language" -> "English"
    }
  }
|>]
