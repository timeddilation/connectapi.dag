test_that("tasks execute with a valid DAG", {
  task0 <- sim_task("task0", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)

  task1 |>
    set_upstream(task0) |>
    set_downstream(task2)

  dag0 <- connect_dag(task0, task1, task2)

  run_dag(dag0)

  expect_true(dag0$is_complete)

  dag0_task_statuses <-
    lapply(dag0$dag_tasks, {\(task) task$task_status}) |>
    unlist()
  expect_true(all(dag0_task_statuses == "Succeeded"))
})

test_that("a DAG does not execute if already executed", {
  task0 <- sim_task("task0", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)

  task1 |>
    set_upstream(task0) |>
    set_downstream(task2)

  dag0 <- connect_dag(task0, task1, task2)

  run_dag(dag0)

  expect_error(run_dag(dag0))

  dag0_task_statuses <-
    lapply(dag0$dag_tasks, {\(task) task$task_status}) |>
    unlist()
  expect_true(all(dag0_task_statuses == "Succeeded"))
})

test_that("tasks do not execute with an invalid DAG", {
  task0 <- sim_task("task0", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)

  dag0 <- connect_dag(task0, task1, task2)

  run_dag(dag0) |>
    suppressWarnings() |>
    suppressMessages() |>
    expect_error()

  expect_false(dag0$is_complete)

  dag0_task_statuses <-
    lapply(dag0$dag_tasks, {\(task) task$task_status}) |>
    unlist()
  expect_true(all(dag0_task_statuses == "Pending"))
})
