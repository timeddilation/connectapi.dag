#' Save a DAG as a Pin to Posit Connect
#'
#' @description
#' Uses the `pins` package to save the DAG to a Connect Board.
#' Pinned DAGs can be easily loaded and run by scheduled jobs.
#'
#' @details
#' Once you have created a DAG in a local R environment, you will need
#' to save the ConnectDAG R6 environment in a deployed environment (Posit Connect)
#' if you want to schedule it in Posit Connect to run.
#'
#' Once the dag is saved, you can quickly create a deploy-able and schedule-able
#' Rmd file using \link[connectapi.dag]{dag_write_rmd}.
#'
#' After the DAG runs, it can be saved again using this function.
#' By default, this function versions the pin that is written.
#' This allows you to load specific instances of DAG runs, and evaluate how it ran.
#' The versions of the pin serve as an effective log of all DAG run attempts.
#'
#' Since the DAG contains the ConnectTask environments as well,
#' you can see a full snapshot of a DAG run, and all of the task statuses
#' that resulted at the end of the DAG execution.
#'
#' @param env A ConnectDAG R6 environment created by \link[connectapi.dag]{connect_dag}
#' @param additional_tags A character vector of tags to include on the pin. "DAG" is always applied.
#' @param pin_title Passed to pins::pin_write(title)
#' @param pin_description Passed to pins::pin_write(description)
#' @param ... arguments passed to \link[pins]{board_connect}
#'
#' @examples
#' \dontrun{
#' task0 <- connect_task("task0", simulated = TRUE)
#' task1 <- connect_task("task1", simulated = TRUE)
#' task0 |> set_downstream(task1)
#'
#' my_dag <- connect_dag(name = "my_dag", task0, task1)
#' dag_write_connect_pin(my_dag)
#' }
#'
#' @importFrom pins board_connect
#' @importFrom pins pin_write
#' @export

dag_write_connect_pin <- function(
    env,
    additional_tags = NA_character_,
    pin_title = paste(env$name, "connectapi.dag"),
    pin_description = NA_character_,
    ...
) {
  stopifnot(
    inherits(env, "ConnectDAG"),
    is.character(additional_tags),
    is.character(pin_title),
    length(pin_title) == 1,
    is.character(pin_description),
    length(pin_description) == 1
  )

  dag_tags <- "DAG"
  if (!is.na(additional_tags)) {
    dag_tags <- c(dag_tags, additional_tags)
  }

  connect_board <- pins::board_connect(...)

  pins::pin_write(
    connect_board,
    env,
    name = env$pin_name,
    type = "rds",
    title = paste(env$pin_name, "connectapi.dag"),
    description = pin_description,
    versioned = TRUE,
    tags = dag_tags
  )
}
