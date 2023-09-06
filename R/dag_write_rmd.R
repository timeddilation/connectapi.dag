#' Write a deployable Rmd file for a ConnectDAG
#'
#' @description
#' Create an Rmd file you can deploy to Posit Connect to run your pinned DAG.
#' You may have to edit some aspects of this file, if for instance you have different
#' `board_connect()` parameters than the default.
#'
#' @details
#' Before attempting to write the Rmd file, this function checks if the pin actually exists.
#' Parameters passed to `...` will pass down to `pins::board_connect()` to be able to check.
#' Beyond that, this function does not use the connection to the board.
#'
#' The generated Rmd file contains 3 code chunks: _setup_, _run-dag_, and _save_.
#' First, an environment set up loads the necessary packages and reads the
#' ConnectDAG environment from the pin.
#' Then, it runs the dag, resetting the state of the DAG before running.
#' Lastly, it saves the new version of the DAG back to the Connect Pin.
#'
#' In the _setup_ chunk, a connection to the board is made using the pins package.
#' You may need to modify this code to work in your environment if you are using
#' anything other than the defaults for `pins::board_connect()`.
#'
#' In the _run-dag_ chunk, you should not need to modify anything.
#' This will simply reset all task statuses and then execute the DAG tasks.
#'
#' In the _save_ chunk, when overwriting the DAG to the Connect Pin, it is versioned.
#' You may also need to modify the parameters of `connectapi.dag::dag_write_connect_pin()`
#' if using anything other than the defaults.
#' The DAG also contains the state information of all the tasks
#' in the DAG after the DAG finishes executing.
#' These versions keep a working history of each DAG's run,
#' allowing you to load previous versions of the DAG.
#'
#'
#' @param env A ConnectDAG R6 environment created by \link[connectapi.dag]{connect_dag}
#' @param pin_name A scalar character of the name of the pin on the Connect Board
#' @param file The name of the file that will be generated
#' @param ... Arguments passed to \link[pins]{board_connect}
#'
#' @examples
#' \dontrun{
#' task0 <- connect_task("task0", simulated = TRUE)
#' task1 <- connect_task("task1", simulated = TRUE)
#' task0 |> set_downstream(task1)
#'
#' my_dag <- connect_dag(name = "my_dag", task0, task1)
#' dag_write_connect_pin(my_dag)
#'
#' dag_write_rmd(my_dag)
#' }
#'
#' @importFrom glue glue
#' @importFrom pins pin_exists
#' @importFrom readr write_lines
#' @export

dag_write_rmd <- function(
  env,
  pin_name = env$pin_name,
  file = paste0(env$pin_name, ".Rmd"),
  ...
) {
  stopifnot(
    inherits(env, "ConnectDAG"),
    is.character(pin_name),
    length(pin_name) == 1
  )

  pin_available <- pins::pin_exists(
    pins::board_connect(...),
    pin_name
  )

  if (!pin_available) {
    stop(paste0(
      "No pin found for this DAG.",
      "Make sure to save the pin first using `dag_write_connect_pin()`"
    ))
  }

  rmd_title <- env$name
  rmd_dag_pin_name <- env$pin_name

  rmd_header <- glue::glue(
    "
    ---
    title: {{rmd_title}}
    output: html_document
    ---
    ", .open = "{{", .close = "}}"
  )

  rmd_dag_setup <- glue::glue(
    "
    ```{r setup}
    library(connectapi.dag)
    library(pins)

    dag_env <- pin_read(
      board = board_connect(),
      name = \"{{rmd_dag_pin_name}}\"
    )
    ```
    ", .open = "{{", .close = "}}"
  )

  rmd_dag_run <- glue::glue(
    "
    ```{r run_dag}
    dag_env |>
      dag_reset() |>
      dag_run()
    ```
    ", .open = "{{", .close = "}}"
  )

  rmd_dag_save <- glue::glue(
    "
    ```{r save}
    dag_write_connect_pin(dag_env)
    ```
    ", .open = "{{", .close = "}}"
  )

  rmd_txt <- c(
    rmd_header,
    rmd_dag_setup,
    rmd_dag_run,
    rmd_dag_save
  ) |> paste0(collapse = "\n\n")

  readr::write_lines(rmd_txt, file)
}
