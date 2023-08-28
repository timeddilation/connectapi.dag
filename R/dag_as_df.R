#' Return DAG tasks as a data.frame
#'
#' Creates a data.frame of the tasks added to the DAG.
#' Includes the execution order of the tasks if the DAG is valid.
#'
#' @param object A ConnectDAG R6 environment created by \link[connectapi.dag]{connect_dag}
#'
#' @return A data.frame
#'
#' @examples
#' task0 <- connect_task("task0", simulated = TRUE)
#' task1 <- connect_task("task1", simulated = TRUE)
#' task0 |> set_downstream(task1)
#'
#' my_dag <- connect_dag(name = "my_dag", task0, task1)
#' dag_as_df(my_dag)
#' @export

dag_as_df <- function(object) {
  stopifnot(inherits(object, "ConnectDAG"))
  object$tasks_as_df()
}
