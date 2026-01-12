# Create a DAG to orchestrate Connect Tasks

A Connect Directed Acyclic Graph (DAG) is a sequence of tasks to execute
in Posit Connect. Use this function to link tasks together into a single
DAG. DAGs may then be executed, which will orchestrate connect tasks.

## Usage

``` r
connect_dag(..., name = "new_dag")
```

## Arguments

- ...:

  Connect Tasks to add to the graph

- name:

  A personalized name for the DAG

## Value

A ConnectDAG R6 environment

## Examples

``` r
task0 <- connect_task("task0", simulated = TRUE)
task1 <- connect_task("task1", simulated = TRUE)
task0 |> set_downstream(task1)

my_dag <- connect_dag(name = "my_dag", task0, task1)
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
