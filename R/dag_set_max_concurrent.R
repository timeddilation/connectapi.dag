#' Set the maximum number of tasks a DAG runs concurrently
#'
#' Controls how many tasks the DAG may run at the same time when executed.
#' The default is 1, which runs tasks sequentially. Because the rendering work
#' happens on the Posit Connect server, raising this lets independent branches
#' of the DAG render simultaneously, finishing the whole DAG faster. The setting
#' is stored on the DAG, so it persists when the DAG is pinned and re-run from a
#' scheduled job.
#'
#' @param env A ConnectDAG R6 environment created by \link[connectapi.dag]{connect_dag}
#' @param n A positive integer giving the maximum number of simultaneously running tasks
#'
#' @examples
#' task0 <- connect_task("task0", simulated = TRUE)
#' task1 <- connect_task("task1", simulated = TRUE)
#' task2 <- connect_task("task2", simulated = TRUE)
#' task0 |> set_downstream(task1, task2)
#'
#' my_dag <-
#'   connect_dag(name = "my_dag", task0, task1, task2) |>
#'   dag_set_max_concurrent(2)
#' @export

dag_set_max_concurrent <- function(env, n) {
  stopifnot(inherits(env, "ConnectDAG"))
  env$set_max_concurrent(n)
}

#' Set the per-task timeout for a DAG
#'
#' Sets the maximum number of seconds any single task may run before the
#' scheduler marks it as Failed and continues with the rest of the DAG. This
#' guards against a render that never finishes on Posit Connect. Pass `NA` to
#' disable the per-task timeout (the default).
#'
#' @param env A ConnectDAG R6 environment created by \link[connectapi.dag]{connect_dag}
#' @param seconds A positive number of seconds, or NA to disable the timeout
#'
#' @examples
#' my_dag <- connect_dag(name = "my_dag")
#' dag_set_task_timeout(my_dag, 600)
#' @export

dag_set_task_timeout <- function(env, seconds) {
  stopifnot(inherits(env, "ConnectDAG"))
  env$set_task_timeout(seconds)
}

#' Set the overall timeout for a DAG run
#'
#' Sets the maximum number of seconds an entire DAG run may take. When the limit
#' is exceeded, any still-running tasks are marked Failed and the run stops,
#' guaranteeing the scheduler terminates. Pass `NA` to disable the global timeout
#' (the default).
#'
#' @param env A ConnectDAG R6 environment created by \link[connectapi.dag]{connect_dag}
#' @param seconds A positive number of seconds, or NA to disable the timeout
#'
#' @examples
#' my_dag <- connect_dag(name = "my_dag")
#' dag_set_dag_timeout(my_dag, 3600)
#' @export

dag_set_dag_timeout <- function(env, seconds) {
  stopifnot(inherits(env, "ConnectDAG"))
  env$set_dag_timeout(seconds)
}

#' Set the scheduler poll interval for a DAG
#'
#' Sets how many seconds the scheduler sleeps between poll cycles while tasks are
#' running. Lower values make the DAG notice finished tasks sooner at the cost of
#' more frequent requests to Posit Connect. Defaults to 1 second.
#'
#' @param env A ConnectDAG R6 environment created by \link[connectapi.dag]{connect_dag}
#' @param seconds A positive number of seconds
#'
#' @examples
#' my_dag <- connect_dag(name = "my_dag")
#' dag_set_poll_interval(my_dag, 5)
#' @export

dag_set_poll_interval <- function(env, seconds) {
  stopifnot(inherits(env, "ConnectDAG"))
  env$set_poll_interval(seconds)
}
