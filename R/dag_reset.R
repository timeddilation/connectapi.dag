#' Reset a DAG to its initial state
#'
#' After a DAG has run its tasks, and is in a completed state,
#' use this function to set the state back to an initial state to run through a clean DAG.
#'
#' @param env A ConnectDAG R6 environment created by \link[connectapi.dag]{connect_dag}
#' @examples
#' task0 <- connect_task("task0", simulated = TRUE)
#' task1 <- connect_task("task1", simulated = TRUE)
#' task0 |> set_downstream(task1)
#'
#' my_dag <- connect_dag(name = "my_dag", task0, task1)
#' run_dag(my_dag)
#'
#' dag_reset(my_dag)
#' @export

dag_reset <- function(env) {
  stopifnot(inherits(env, "ConnectDAG"))
  env$dag_reset()

  return(env)
}
