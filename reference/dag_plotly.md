# Visualize a ConnectDAG graph with plotly

Create an interactive plotly visual of the DAG. This function is not
exported. It is called internally by the ConnectDAG class using the
\`plot\` method.

## Usage

``` r
dag_plotly(connect_dag)
```

## Arguments

- connect_dag:

  A ConnectDAG R6 environment

## Value

A plotly graph

## Examples

``` r
task0 <- connect_task("task0", simulated = TRUE)
task1 <- connect_task("task1", simulated = TRUE)
task0 |> set_downstream(task1)

my_dag <- connect_dag(task0, task1)
plot(my_dag)
```
