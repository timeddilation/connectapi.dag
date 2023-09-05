#' Remove a task from a DAG
#'
#' Removes a specified ConnectTask environment from a ConnectDAG environment.
#'
#' @param env A ConnectDAG R6 environment created by \link[connectapi.dag]{connect_dag}
#' @param task A connectTask R6 environment created by \link[connectapi.dag]{connect_task}
#'
#' @examples
#' task0 <- connect_task("task0", simulated = TRUE)
#' task1 <- connect_task("task1", simulated = TRUE)
#'
#' my_dag <-
#'   connect_dag() |>
#'   dag_add_tasks(task0, task1)
#'
#' my_dag |> dag_remove_task(task1)
#' my_dag$dag_tasks
#' @export

dag_remove_task <- function(env, task) {
  stopifnot(inherits(env, "ConnectDAG"))
  env$remove_task(task)
}
