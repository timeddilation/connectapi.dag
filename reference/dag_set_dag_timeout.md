# Set the overall timeout for a DAG run

Sets the maximum number of seconds an entire DAG run may take. When the
limit is exceeded, any still-running tasks are marked Failed and the run
stops, guaranteeing the scheduler terminates. Pass \`NA\` to disable the
global timeout (the default).

## Usage

``` r
dag_set_dag_timeout(env, seconds)
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
dag_set_dag_timeout(my_dag, 3600)
```
