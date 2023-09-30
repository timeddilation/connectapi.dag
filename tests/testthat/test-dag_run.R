test_that("tasks execute with a valid DAG", {
  task0 <- sim_task("task0", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)

  task1 |>
    set_upstream(task0) |>
    set_downstream(task2)

  dag0 <- connect_dag(task0, task1, task2)

  expect_true(is.na(dag0$run_id))
  expect_true(is.na(dag0$run_start))
  expect_true(is.na(dag0$run_end))
  expect_false(dag0$is_complete)

  dag_run(dag0)

  expect_true(inherits(dag0$run_id, "character"))
  expect_true(inherits(dag0$run_start, "POSIXct"))
  expect_true(inherits(dag0$run_end, "POSIXct"))
  expect_true(dag0$is_complete)

  dag0_task_statuses <-
    vapply(dag0$tasks, {\(task) task$status}, character(1))
  expect_true(all(dag0_task_statuses == "Succeeded"))
})

test_that("verbose execution of valid DAG prints output", {
  task0 <- sim_task("task0", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)

  task1 |>
    set_upstream(task0) |>
    set_downstream(task2)

  dag0 <- connect_dag(task0, task1, task2)

  expect_output(dag_run(dag0, verbose = TRUE)) |>
    suppressMessages()

  expect_true(dag0$is_complete)
})

test_that("a DAG does not execute if already executed", {
  task0 <- sim_task("task0", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)

  task1 |>
    set_upstream(task0) |>
    set_downstream(task2)

  dag0 <- connect_dag(task0, task1, task2)

  dag_run(dag0)

  expect_error(dag_run(dag0))

  dag0_task_statuses <-
    vapply(dag0$tasks, {\(task) task$status}, character(1))
  expect_true(all(dag0_task_statuses == "Succeeded"))
})

test_that("tasks do not execute with an invalid DAG", {
  task0 <- sim_task("task0", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)

  dag0 <- connect_dag(task0, task1, task2)

  dag_run(dag0) |>
    suppressWarnings() |>
    suppressMessages() |>
    expect_error()

  expect_false(dag0$is_complete)

  dag0_task_statuses <-
    vapply(dag0$tasks, {\(task) task$status}, character(1))
  expect_true(all(dag0_task_statuses == "Pending"))
})
