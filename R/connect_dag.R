#' Create a DAG to orchestrate Connect Tasks
#'
#' A Connect Directed Acyclic Graph (DAG) is a sequence of tasks to execute in Posit Connect.
#' Use this function to link tasks together into a single DAG.
#' DAGs may then be executed, which will orchestrate connect tasks.
#'
#' @param ... Connect Tasks to add to the graph
#' @param name A personalized name for the DAG
#'
#' @return A ConnectDAG R6 environment
#'
#' @examples
#' task0 <- connect_task("task0", simulated = TRUE)
#' task1 <- connect_task("task1", simulated = TRUE)
#' task0 |> set_downstream(task1)
#'
#' my_dag <- connect_dag(name = "my_dag", task0, task1)
#' my_dag
#'
#' @export
connect_dag <- function(..., name = "new_dag") {
  ConnectDAG$new(name, ...)
}
