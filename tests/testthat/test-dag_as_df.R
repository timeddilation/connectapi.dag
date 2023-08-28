test_that("the expected df is returned", {
  task0 <- sim_task("task0", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)

  task1 |>
    set_upstream(task0) |>
    set_downstream(task2)

  dag0 <- connect_dag(task0, task1, task2)
  df <- dag_as_df(dag0)

  expect_true(inherits(df, "data.frame"))
  expect_equal(nrow(df), 3)
  expect_identical(names(df), c("task_name", "task_guid", "task_status", "trigger_rule", "exec_order"))
  expect_true(all(is.integer(df[, "exec_order"])))
})

test_that("an empty DAG has no rows", {
  dag0 <- connect_dag(name = "dag0")
  df <- dag_as_df(dag0) |> suppressMessages()

  expect_true(inherits(df, "data.frame"))
  expect_equal(nrow(df), 0)
  expect_identical(names(df), c("task_name", "task_guid", "task_status", "trigger_rule", "exec_order"))
})

test_that("an invalid DAG still shows df with no exec order", {
  task0 <- sim_task("task0", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)

  dag0 <- connect_dag(task0, task1, task2)
  df <- dag_as_df(dag0) |> suppressMessages() |> suppressWarnings()

  expect_true(inherits(df, "data.frame"))
  expect_equal(nrow(df), 3)
  expect_identical(names(df), c("task_name", "task_guid", "task_status", "trigger_rule", "exec_order"))
  expect_true(all(is.na(df[, "exec_order"])))
})
