#' Run a single ConnectTask
#'
#' Executes a ConnectTask, causing the content on Connect to re-render.
#' This is not normally called directly, the ConnectDAG will use this function instead.
#'
#' @param env a ConnectTask R6 environment created by \link[connectapi.dag]{connect_task}
#' @param verbose A boolean, when TRUE prints messages to console as the task executes
#'
#' @examples
#' task0 <- connect_task("task0", simulated = TRUE)
#' run_task(task0)
#' @export

run_task <- function(env, verbose = FALSE) {
  stopifnot(inherits(env, "ConnectTask"), is.logical(verbose))
  env$execute_task(verbose)
}
