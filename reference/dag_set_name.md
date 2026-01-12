# Set the name of a DAG

DAG names are used as identifiers for the DAG when using pins.

## Usage

``` r
dag_set_name(env, name)
```

## Arguments

- env:

  A ConnectDAG R6 environment created by
  [connect_dag](https://timeddilation.github.io/connectapi.dag/reference/connect_dag.md)

- name:

  A scalar character of the name you want to set for the DAG

## Examples

``` r
my_dag <-
  connect_dag() |>
  dag_set_name("My Awesome DAG")
#> Warning: Pins only allows alphanumeric characters, dashes, and underscores in application names. My-Awesome-DAG will be used instead when writing to a pin board. Use dag_set_pin_name() to overwrite this name.

my_dag
#> No tasks are linked in the graph.
#> ConnectDAG: 
#>   Name: My Awesome DAG 
#>   Is Valid: FALSE 
#>   Pin Name: My-Awesome-DAG 
#>   Tasks: 0 
#>     [1] guid         name         status       trigger_rule exec_order  
#>     <0 rows> (or 0-length row.names)
```
