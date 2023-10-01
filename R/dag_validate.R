#' Validates a ConnectDAG is complete and can be executed
#'
#' A valid ConnectDAG has all tasks linked in a single network (no islands),
#' and is a [graph] DAG.
#' A DAG that is not valid will generate a warning message with the reason.
#'
#' @param env A ConnectDAG R6 environment
#' @param verbose A logical, when TRUE (the default) a message will print to the console if valid
#'
#' @return A boolean indicating if it is valid or not
#'
#' @examples
#' task1 <- connect_task("task1", simulated = TRUE)
#' task2 <- connect_task("task2", simulated = TRUE)
#' task1 |> set_downstream(task2)
#'
#' my_dag <- connect_dag(task1, task2)
#' dag_validate(my_dag)
#' my_dag
#' @export

dag_validate <- function(env, verbose = TRUE) {
  stopifnot(inherits(env, "ConnectDAG"))
  env$evaluate_validity(verbose)
}
