# Creates a new ConnectTask object

This function generates an R6 class for a ConnectTask object. The object
then has methods to add dependency and dependent tasks. Once tasks are
added to a ConnectDag they may be executed.

## Usage

``` r
connect_task(
  guid,
  trigger_rule = trigger_options,
  server = Sys.getenv(paste0(prefix, "_SERVER"), NA_character_),
  api_key = Sys.getenv(paste0(prefix, "_API_KEY"), NA_character_),
  prefix = "CONNECT",
  simulated = FALSE
)
```

## Arguments

- guid:

  A scalar character of the guid for the content deployed to Posit
  Connect

- trigger_rule:

  A scalar character that defines state of dependency (upstream) tasks
  must be in to execute. See details.

- server:

  The URL for accessing Posit Connect. Defaults to environment variable
  CONNECT_SERVER

- api_key:

  The API Key to authenticate to Posit Connect with. Defaults to
  environment variable CONNECT_API_KEY

- prefix:

  The prefix used to determine environment variables

- simulated:

  A simulated connect task does not validate with a connect server nor
  executes any connect jobs

## Value

A ConnectTask R6 Environment

## Details

You can make other tasks dependency tasks by using
[set_upstream](https://timeddilation.github.io/connectapi.dag/reference/set_upstream.md).
To make other tasks dependents, use
[set_downstream](https://timeddilation.github.io/connectapi.dag/reference/set_downstream.md).

Trigger rules allow you to define conditions of upstream tasks required
in order to execute the task. By default, \`all_success\` is used,
requiring all upstream tasks to complete evaluation successfully in
order to execute. If a task does not meet its trigger rule requirement,
the task will be skipped.

All task trigger rules evaluate their immediate upstream tasks. For
tasks further upstream of the immediate upstream tasks and their
statuses are not considered.

The following trigger rules are available:

- "all_success" All upstream tasks executed successfully.

- "all_failed" All upstream tasks failed during execution.

- "all_skipped" All upstream tasks skipped execution.

- "all_done" All upstream tasks completed evaluation. This includes
  skipped tasks.

- "one_success" At least one upstream task executed successfully.

- "one_failed" At least one upstream task failed during execution.

- "one_done" At least one upstream task completed evaluation. This
  includes skipped tasks.

- "none_failed" No upstream tasks failed. All other upstream tasks
  completed evaluation.

- "none_skipped" No upstream tasks skipped. All other upstream tasks
  completed evaluation.

- "always" Task will always run regardless of upstream task statuses.

## Examples

``` r
task0 <- connect_task("task1", trigger_rule = "all_done", simulated = TRUE)
task0
#> ConnectTask: 
#>   GUID: simulated_task1 
#>   Name: task1 
#>   Trigger Rule: all_done 
#>   App Mode: simulation 
#>   Status: Pending 
#>   Upstream Tasks: 0 
#>   Downstream Tasks: 0 
```
