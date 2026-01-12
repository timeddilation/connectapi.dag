# Run a single ConnectTask

Executes a ConnectTask, causing the content on Connect to re-render.
This is not normally called directly, the ConnectDAG will use this
function instead.

## Usage

``` r
task_run(env, verbose = FALSE)
```

## Arguments

- env:

  a ConnectTask R6 environment created by
  [connect_task](https://timeddilation.github.io/connectapi.dag/reference/connect_task.md)

- verbose:

  A boolean, when TRUE prints messages to console as the task executes

## Examples

``` r
task0 <- connect_task("task0", simulated = TRUE)
task_run(task0)
task0
#> ConnectTask: 
#>   GUID: simulated_task0 
#>   Name: task0 
#>   Trigger Rule: all_success 
#>   App Mode: simulation 
#>   Status: Succeeded 
#>   Upstream Tasks: 0 
#>   Downstream Tasks: 0 
```
