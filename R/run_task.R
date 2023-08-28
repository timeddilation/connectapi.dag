run_task <- function(con_task) {
  stopifnot(inherits(con_task, "ConnectTask"))
  con_task$execute_task()
}
