#' Run orchestrated ConnectTasks
#'
#' Executes Posit Connect Tasks sequentially,
#' ensuring dependency tasks run before dependents.
#'
#' @param env A ConnectDAG R6 environment created by \link[connectapi.dag]{connect_dag}
#' @param verbose A boolean, when TRUE prints messages to console as tasks execute
#'
#' @examples
#' task0 <- connect_task("task0", simulated = TRUE)
#' task1 <- connect_task("task1", simulated = TRUE)
#' task0 |> set_downstream(task1)
#'
#' my_dag <- connect_dag(name = "my_dag", task0, task1)
#' run_dag(my_dag)
#' @export

run_dag <- function(env, verbose = FALSE) {
  stopifnot(inherits(env, "ConnectDAG"), is.logical(verbose))
  env$run_dag(verbose)
}
