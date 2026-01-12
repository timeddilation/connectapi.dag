# Validates a ConnectDAG is complete and can be executed

A valid ConnectDAG has all tasks linked in a single network (no
islands), and is a \[graph\] DAG. A DAG that is not valid will generate
a warning message with the reason.

## Usage

``` r
dag_validate(env, verbose = TRUE)
```

## Arguments

- env:

  A ConnectDAG R6 environment

- verbose:

  A logical, when TRUE (the default) a message will print to the console
  if valid

## Value

A boolean indicating if it is valid or not

## Examples

``` r
task1 <- connect_task("task1", simulated = TRUE)
task2 <- connect_task("task2", simulated = TRUE)
task1 |> set_downstream(task2)

my_dag <- connect_dag(task1, task2)
dag_validate(my_dag)
#> DAG is valid!
my_dag
#> ConnectDAG: 
#>   Name: new_dag 
#>   Is Valid: TRUE 
#>   Pin Name: new_dag 
#>   Tasks: 2 
#>                 guid  name  status trigger_rule exec_order
#>      simulated_task1 task1 Pending  all_success          1
#>      simulated_task2 task2 Pending  all_success          2
```
