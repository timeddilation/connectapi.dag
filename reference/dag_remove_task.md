# Remove a task from a DAG

Removes a specified ConnectTask environment from a ConnectDAG
environment.

## Usage

``` r
dag_remove_task(env, task)
```

## Arguments

- env:

  A ConnectDAG R6 environment created by
  [connect_dag](https://timeddilation.github.io/connectapi.dag/reference/connect_dag.md)

- task:

  A connectTask R6 environment created by
  [connect_task](https://timeddilation.github.io/connectapi.dag/reference/connect_task.md)

## Examples

``` r
task0 <- connect_task("task0", simulated = TRUE)
task1 <- connect_task("task1", simulated = TRUE)

my_dag <-
  connect_dag() |>
  dag_add_tasks(task0, task1)

my_dag |> dag_remove_task(task1)
my_dag$tasks
#> [[1]]
#> ConnectTask: 
#>   GUID: simulated_task0 
#>   Name: task0 
#>   Trigger Rule: all_success 
#>   App Mode: simulation 
#>   Status: Pending 
#>   Upstream Tasks: 0 
#>   Downstream Tasks: 0 
#> 
```
