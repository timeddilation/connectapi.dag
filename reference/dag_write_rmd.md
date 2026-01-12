# Write a deployable Rmd file for a ConnectDAG

Create an Rmd file you can deploy to Posit Connect to run your pinned
DAG. You may have to edit some aspects of this file, if for instance you
have different \`board_connect()\` parameters than the default.

## Usage

``` r
dag_write_rmd(
  env,
  pin_name = env$pin_name,
  file = paste0(env$pin_name, "_job", ".Rmd"),
  ...
)
```

## Arguments

- env:

  A ConnectDAG R6 environment created by
  [connect_dag](https://timeddilation.github.io/connectapi.dag/reference/connect_dag.md)

- pin_name:

  A scalar character of the name of the pin on the Connect Board

- file:

  The name of the file that will be generated

- ...:

  Arguments passed to
  [board_connect](https://pins.rstudio.com/reference/board_connect.html)

## Details

Before attempting to write the Rmd file, this function checks if the pin
actually exists. Parameters passed to \`...\` will pass down to
\`pins::board_connect()\` to be able to check. Beyond that, this
function does not use the connection to the board.

The generated Rmd file contains 3 code chunks: \_setup\_, \_run-dag\_,
and \_save\_. First, an environment set up loads the necessary packages
and reads the ConnectDAG environment from the pin. Then, it runs the
dag, resetting the state of the DAG before running. Lastly, it saves the
new version of the DAG back to the Connect Pin.

In the \_setup\_ chunk, a connection to the board is made using the pins
package. You may need to modify this code to work in your environment if
you are using anything other than the defaults for
\`pins::board_connect()\`.

In the \_run-dag\_ chunk, you should not need to modify anything. This
will simply reset all task statuses and then execute the DAG tasks.

In the \_save\_ chunk, when overwriting the DAG to the Connect Pin, it
is versioned. You may also need to modify the parameters of
\`connectapi.dag::dag_write_connect_pin()\` if using anything other than
the defaults. The DAG also contains the state information of all the
tasks in the DAG after the DAG finishes executing. These versions keep a
working history of each DAG's run, allowing you to load previous
versions of the DAG.

## Examples

``` r
if (FALSE) { # \dontrun{
task0 <- connect_task("task0", simulated = TRUE)
task1 <- connect_task("task1", simulated = TRUE)
task0 |> set_downstream(task1)

my_dag <- connect_dag(name = "my_dag", task0, task1)
dag_write_connect_pin(my_dag)

dag_write_rmd(my_dag)
} # }
```
