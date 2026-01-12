# Save a DAG as a Pin to Posit Connect

Uses the \`pins\` package to save the DAG to a Connect Board. Pinned
DAGs can be easily loaded and run by scheduled jobs.

## Usage

``` r
dag_write_connect_pin(
  env,
  additional_tags = NA_character_,
  pin_title = paste(env$name, "connectapi.dag"),
  pin_description = NA_character_,
  ...
)
```

## Arguments

- env:

  A ConnectDAG R6 environment created by
  [connect_dag](https://timeddilation.github.io/connectapi.dag/reference/connect_dag.md)

- additional_tags:

  A character vector of tags to include on the pin. "DAG" is always
  applied.

- pin_title:

  Passed to pins::pin_write(title)

- pin_description:

  Passed to pins::pin_write(description)

- ...:

  arguments passed to
  [board_connect](https://pins.rstudio.com/reference/board_connect.html)

## Details

Once you have created a DAG in a local R environment, you will need to
save the ConnectDAG R6 environment in a deployed environment (Posit
Connect) if you want to schedule it in Posit Connect to run.

Once the dag is saved, you can quickly create a deploy-able and
schedule-able Rmd file using
[dag_write_rmd](https://timeddilation.github.io/connectapi.dag/reference/dag_write_rmd.md).

After the DAG runs, it can be saved again using this function. By
default, this function versions the pin that is written. This allows you
to load specific instances of DAG runs, and evaluate how it ran. The
versions of the pin serve as an effective log of all DAG run attempts.

Since the DAG contains the ConnectTask environments as well, you can see
a full snapshot of a DAG run, and all of the task statuses that resulted
at the end of the DAG execution.

## Examples

``` r
if (FALSE) { # \dontrun{
task0 <- connect_task("task0", simulated = TRUE)
task1 <- connect_task("task1", simulated = TRUE)
task0 |> set_downstream(task1)

my_dag <- connect_dag(name = "my_dag", task0, task1)
dag_write_connect_pin(my_dag)
} # }
```
