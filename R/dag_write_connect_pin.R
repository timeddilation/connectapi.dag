#' Save a DAG as a Pin to Posit Connect
#'
#' Uses the `pins` package to save the DAG to a Connect Board.
#' Pinned DAGs can be easily loaded and run by scheduled jobs.
#' After the DAG runs, it is saved again and versioned.
#' This allows you to load specific instances of DAG runs, and evaluate how it ran.
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
    pin_title = paste(env$pin_name, "connectapi.dag"),
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
