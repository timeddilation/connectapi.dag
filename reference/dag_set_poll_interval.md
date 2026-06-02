# Set the scheduler poll interval for a DAG

Sets how many seconds the scheduler sleeps between poll cycles while
tasks are running. Lower values make the DAG notice finished tasks
sooner at the cost of more frequent requests to Posit Connect. Defaults
to 1 second.

## Usage

``` r
dag_set_poll_interval(env, seconds)
```

## Arguments

- env:

  A ConnectDAG R6 environment created by
  [connect_dag](https://timeddilation.github.io/connectapi.dag/reference/connect_dag.md)

- seconds:

  A positive number of seconds

## Examples

``` r
my_dag <- connect_dag(name = "my_dag")
dag_set_poll_interval(my_dag, 5)
```
