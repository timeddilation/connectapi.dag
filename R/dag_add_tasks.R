#' Add Tasks to a DAG
#'
#' Adds ConnectTask environments to an existing ConnectDAG
#'
#' @param env A ConnectDAG R6 environment created by \link[connectapi.dag]{connect_dag}
#' @param ... Any number of ConnectTask R6 environments created by \link[connectapi.dag]{connect_task}
#'
#' @examples
#' task0 <- connect_task("task0", simulated = TRUE)
#' task1 <- connect_task("task1", simulated = TRUE)
#' task0 |> set_downstream(task1)
#'
#' my_dag <-
#'   connect_dag() |>
#'   dag_add_tasks(task0, task1)
#'
#' run_dag(my_dag)
#' @export

dag_add_tasks <- function(env, ...) {
  stopifnot(inherits(env, "ConnectDAG"))

  for (task in list(...)) {
    env$add_task(task)
  }

  return(env)
}
