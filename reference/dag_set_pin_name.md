# Sets the Pin name for Connect Board

When using this package's built-in features for the \`pins\` package,
the ConnectDAG's pin_name is used to write to a Connect Board. This
function allows you to overwrite the default name given for the Pin
Name. By default the pin name will be the ConnectDAG's name, but will
substitute any invalid pin name characters with a hyphen. Note that
using \`dag_set_name()\` will also change the pin name.

## Usage

``` r
dag_set_pin_name(env, pin_name)
```

## Arguments

- env:

  A ConnectDAG R6 environment created by
  [connect_dag](https://timeddilation.github.io/connectapi.dag/reference/connect_dag.md)

- pin_name:

  A scalar character you want to use for the pin name

## Examples

``` r
my_dag <- connect_dag(name = "my_dag")
my_dag |> dag_set_pin_name("pinned_dag_name")

my_dag
#> No tasks are linked in the graph.
#> ConnectDAG: 
#>   Name: my_dag 
#>   Is Valid: FALSE 
#>   Pin Name: pinned_dag_name 
#>   Tasks: 0 
#>     [1] guid         name         status       trigger_rule exec_order  
#>     <0 rows> (or 0-length row.names)
```
