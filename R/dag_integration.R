#' DAG functions integration validation
#'
#' Validates functional workflow for declaring dags

dag_integration <- function() {
  task0 <- connect_task("task0", simulated = TRUE)
  task1 <- connect_task("task1", simulated = TRUE)
  task2 <- connect_task("task2", simulated = TRUE)
  task3 <- connect_task("task3", simulated = TRUE)

  task1 |>
    set_upstream(task0) |>
    set_downstream(task2)

  task3 |> set_upstream(task0)

  my_dag <-
    connect_dag() |>
    dag_set_name("Validation-DAG") |>
    dag_add_tasks(task0, task1, task2, task3) |>
    dag_validate(verbose = FALSE)

  return(my_dag)
}
