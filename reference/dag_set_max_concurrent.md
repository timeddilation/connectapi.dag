# Set the maximum number of tasks a DAG runs concurrently

Controls how many tasks the DAG may run at the same time when executed.
The default is 1, which runs tasks sequentially. Because the rendering
work happens on the Posit Connect server, raising this lets independent
branches of the DAG render simultaneously, finishing the whole DAG
faster. The setting is stored on the DAG, so it persists when the DAG is
pinned and re-run from a scheduled job.

## Usage

``` r
dag_set_max_concurrent(env, n)
```

## Arguments

- env:

  A ConnectDAG R6 environment created by
  [connect_dag](https://timeddilation.github.io/connectapi.dag/reference/connect_dag.md)

- n:

  A positive integer giving the maximum number of simultaneously running
  tasks

## Examples

``` r
task0 <- connect_task("task0", simulated = TRUE)
task1 <- connect_task("task1", simulated = TRUE)
task2 <- connect_task("task2", simulated = TRUE)
task0 |> set_downstream(task1, task2)

my_dag <-
  connect_dag(name = "my_dag", task0, task1, task2) |>
  dag_set_max_concurrent(2)
```
