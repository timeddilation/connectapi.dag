# Return DAG tasks as a data.frame

Creates a data.frame of the tasks added to the DAG. Includes the
execution order of the tasks if the DAG is valid.

## Usage

``` r
dag_as_df(env)
```

## Arguments

- env:

  A ConnectDAG R6 environment created by
  [connect_dag](https://timeddilation.github.io/connectapi.dag/reference/connect_dag.md)

## Value

A data.frame

## Examples

``` r
task0 <- connect_task("task0", simulated = TRUE)
task1 <- connect_task("task1", simulated = TRUE)
task0 |> set_downstream(task1)

my_dag <- connect_dag(name = "my_dag", task0, task1)
dag_as_df(my_dag)
#>              guid  name  status trigger_rule exec_order
#> 1 simulated_task0 task0 Pending  all_success          1
#> 2 simulated_task1 task1 Pending  all_success          2
```
