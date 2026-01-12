# Run orchestrated ConnectTasks

Executes Posit Connect Tasks sequentially, ensuring dependency tasks run
before dependents.

## Usage

``` r
dag_run(env, verbose = FALSE)
```

## Arguments

- env:

  A ConnectDAG R6 environment created by
  [connect_dag](https://timeddilation.github.io/connectapi.dag/reference/connect_dag.md)

- verbose:

  A boolean, when TRUE prints messages to console as tasks execute

## Examples

``` r
task0 <- connect_task("task0", simulated = TRUE)
task1 <- connect_task("task1", simulated = TRUE)
task0 |> set_downstream(task1)

my_dag <- connect_dag(name = "my_dag", task0, task1)
dag_run(my_dag)
my_dag
#> ConnectDAG: 
#>   Name: my_dag 
#>   Is Valid: TRUE 
#>   Pin Name: my_dag 
#>   Tasks: 2 
#>                 guid  name    status trigger_rule exec_order
#>      simulated_task0 task0 Succeeded  all_success          1
#>      simulated_task1 task1 Succeeded  all_success          2
```
