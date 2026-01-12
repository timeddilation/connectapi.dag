# Add Tasks to a DAG

Adds ConnectTask environments to an existing ConnectDAG

## Usage

``` r
dag_add_tasks(env, ...)
```

## Arguments

- env:

  A ConnectDAG R6 environment created by
  [connect_dag](https://timeddilation.github.io/connectapi.dag/reference/connect_dag.md)

- ...:

  Any number of ConnectTask R6 environments created by
  [connect_task](https://timeddilation.github.io/connectapi.dag/reference/connect_task.md)

## Examples

``` r
task0 <- connect_task("task0", simulated = TRUE)
task1 <- connect_task("task1", simulated = TRUE)
task0 |> set_downstream(task1)

my_dag <-
  connect_dag() |>
  dag_add_tasks(task0, task1)

my_dag
#> ConnectDAG: 
#>   Name: new_dag 
#>   Is Valid: TRUE 
#>   Pin Name: new_dag 
#>   Tasks: 2 
#>                 guid  name  status trigger_rule exec_order
#>      simulated_task0 task0 Pending  all_success          1
#>      simulated_task1 task1 Pending  all_success          2
```
