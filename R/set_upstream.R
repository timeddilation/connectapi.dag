#' Sets dependency task(s) for a given ConnecTask
#'
#' Tasks upstream from a given Connect Task must complete evaluation before the task runs.
#' When a task is linked as an upstream task, the upstream task also creates a downstream link to the provided task.
#'
#' @param env A ConnectTask environment generated by \link[connectapi.dag]{connect_task}
#'     that you want to set upstream/dependency tasks for.
#' @param ... Any ConnectTasks to make upstream of `env`
#'
#' @return Does not return anything, it modified to ConnecTask Environment in-place
#'
#' @examples
#' task0 <- connect_task("task0", simulated = TRUE)
#' task1 <- connect_task("task1", simulated = TRUE)
#' task2 <- connect_task("task2", simulated = TRUE)
#'
#' task2 |> set_upstream(task0, task1)
#' plot(task2)
#' @export

set_upstream <- function(env, ...) {
  stopifnot(inherits(env, "ConnectTask"))
  env$set_upstream(...)
}
