# Reset a DAG to its initial state

After a DAG has run its tasks, and is in a completed state, use this
function to set the state back to an initial state to run through a
clean DAG.

## Usage

``` r
dag_reset(env)
```

## Arguments

- env:

  A ConnectDAG R6 environment created by
  [connect_dag](https://timeddilation.github.io/connectapi.dag/reference/connect_dag.md)

## Details

After a DAG has run, all of the DAG tasks also update their internal
states. Resetting a DAG will also reset every task to its initial state.
Then, the DAG itself is reset to an initial state. This ensures when
[dag_run](https://timeddilation.github.io/connectapi.dag/reference/dag_run.md)
is used, all tasks are executed as expected. This is extremely important
when using persistently saved DAGs, such as with
[dag_write_connect_pin](https://timeddilation.github.io/connectapi.dag/reference/dag_write_connect_pin.md)

## Examples

``` r
task0 <- connect_task("task0", simulated = TRUE)
task1 <- connect_task("task1", simulated = TRUE)
task0 |> set_downstream(task1)

my_dag <- connect_dag(name = "my_dag", task0, task1)
dag_run(my_dag)

dag_reset(my_dag)
my_dag
#> ConnectDAG: 
#>   Name: my_dag 
#>   Is Valid: TRUE 
#>   Pin Name: my_dag 
#>   Tasks: 2 
#>                 guid  name  status trigger_rule exec_order
#>      simulated_task0 task0 Pending  all_success          1
#>      simulated_task1 task1 Pending  all_success          2
```
