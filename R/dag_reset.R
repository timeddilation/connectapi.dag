#' Reset a DAG to its initial state
#'
#' @description
#' After a DAG has run its tasks, and is in a completed state,
#' use this function to set the state back to an initial state to run through a clean DAG.
#'
#' @details
#' After a DAG has run, all of the DAG tasks also update their internal states.
#' Resetting a DAG will also reset every task to its initial state.
#' Then, the DAG itself is reset to an initial state.
#' This ensures when \link[connectapi.dag]{dag_run} is used, all tasks are executed as expected.
#' This is extremely important when using persistently saved DAGs, such as with \link[connectapi.dag]{dag_write_connect_pin}
#'
#'
#' @param env A ConnectDAG R6 environment created by \link[connectapi.dag]{connect_dag}
#' @examples
#' task0 <- connect_task("task0", simulated = TRUE)
#' task1 <- connect_task("task1", simulated = TRUE)
#' task0 |> set_downstream(task1)
#'
#' my_dag <- connect_dag(name = "my_dag", task0, task1)
#' dag_run(my_dag)
#'
#' dag_reset(my_dag)
#' my_dag
#' @export

dag_reset <- function(env) {
  stopifnot(inherits(env, "ConnectDAG"))
  env$reset()
}
