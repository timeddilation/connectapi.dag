#' Set the name of a DAG
#'
#' DAG names are used as identifiers for the DAG when using pins.
#'
#' @param env A ConnectDAG R6 environment created by \link[connectapi.dag]{connect_dag}
#' @param name A scalar character of the name you want to set for the DAG
#'
#' @examples
#' my_dag <-
#'   connect_dag() |>
#'   dag_set_name("My Awesome DAG")
#'
#' print(my_dag)
#' @export

dag_set_name <- function(env, name) {
  stopifnot(inherits(env, "ConnectDAG"))
  env$set_name(name)
}
