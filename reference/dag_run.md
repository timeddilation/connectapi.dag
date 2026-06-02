# Run orchestrated ConnectTasks

Executes Posit Connect Tasks, ensuring dependency tasks run before
dependents. Independent tasks may run concurrently when the DAG's
\`max_concurrent\` is greater than 1 (see
[dag_set_max_concurrent](https://timeddilation.github.io/connectapi.dag/reference/dag_set_max_concurrent.md)).

## Usage

``` r
dag_run(env, verbose = FALSE, max_concurrent = NULL)
```

## Arguments

- env:

  A ConnectDAG R6 environment created by
  [connect_dag](https://timeddilation.github.io/connectapi.dag/reference/connect_dag.md)

- verbose:

  A boolean, when TRUE prints messages to console as tasks execute

- max_concurrent:

  An optional override for the DAG's \`max_concurrent\` field, applied
  to this run only

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
