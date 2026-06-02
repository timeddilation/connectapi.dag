#' Run orchestrated ConnectTasks
#'
#' Executes Posit Connect Tasks, ensuring dependency tasks run before dependents.
#' Independent tasks may run concurrently when the DAG's `max_concurrent` is
#' greater than 1 (see \link[connectapi.dag]{dag_set_max_concurrent}).
#'
#' @param env A ConnectDAG R6 environment created by \link[connectapi.dag]{connect_dag}
#' @param verbose A boolean, when TRUE prints messages to console as tasks execute
#' @param max_concurrent An optional override for the DAG's `max_concurrent` field, applied to this run only
#'
#' @examples
#' task0 <- connect_task("task0", simulated = TRUE)
#' task1 <- connect_task("task1", simulated = TRUE)
#' task0 |> set_downstream(task1)
#'
#' my_dag <- connect_dag(name = "my_dag", task0, task1)
#' dag_run(my_dag)
#' my_dag
#' @export

dag_run <- function(env, verbose = FALSE, max_concurrent = NULL) {
  stopifnot(inherits(env, "ConnectDAG"), is.logical(verbose))
  env$execute(verbose, max_concurrent)
}
