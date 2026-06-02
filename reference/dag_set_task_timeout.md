# Set the per-task timeout for a DAG

Sets the maximum number of seconds any single task may run before the
scheduler marks it as Failed and continues with the rest of the DAG.
This guards against a render that never finishes on Posit Connect. Pass
\`NA\` to disable the per-task timeout (the default).

## Usage

``` r
dag_set_task_timeout(env, seconds)
```

## Arguments

- env:

  A ConnectDAG R6 environment created by
  [connect_dag](https://timeddilation.github.io/connectapi.dag/reference/connect_dag.md)

- seconds:

  A positive number of seconds, or NA to disable the timeout

## Examples

``` r
my_dag <- connect_dag(name = "my_dag")
dag_set_task_timeout(my_dag, 600)
```
