#' Sets the Pin name for Connect Board
#'
#' When using this package's built-in features for the `pins` package,
#' the ConnectDAG's pin_name is used to write to a Connect Board.
#' This function allows you to overwrite the default name given for the Pin Name.
#' By default the pin name will be the ConnectDAG's name,
#' but will substitute any invalid pin name characters with a hyphen.
#' Note that using `dag_set_name()` will also change the pin name.
#'
#' @param env A ConnectDAG R6 environment created by \link[connectapi.dag]{connect_dag}
#' @param pin_name A scalar character you want to use for the pin name
#'
#' @examples
#' my_dag <- connect_dag(name = "my_dag")
#' my_dag |> dag_set_pin_name("pinned_dag_name")
#'
#' my_dag
#' @export

dag_set_pin_name <- function(env, pin_name) {
  stopifnot(inherits(env, "ConnectDAG"))
  env$set_connect_pin_name(pin_name)
}
