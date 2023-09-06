test_that("must be a dag", {
  expect_error(dag_reset("foo"))

  dag0 <- connect_dag(name = "foo")
  dag_reset(dag0)

  expect_true(inherits(dag0, "ConnectDAG"))
})

test_that("all tasks reset to pending, and is_complete is FALSE", {
  task0 <- sim_task("task0", fail_prob = 0) # will complete
  task1 <- sim_task("task1", fail_prob = 1) # will fail
  task2 <- sim_task("task2", fail_prob = 0) # will skip

  task1 |>
    set_upstream(task0) |>
    set_downstream(task2)

  dag0 <- connect_dag(task0, task1, task2)
  dag_run(dag0)

  expect_true(dag0$is_complete)

  dag_reset(dag0)
  dag_task_statuses <- dag_as_df(dag0)$task_status

  expect_true(all(dag_as_df(dag0)$task_status == "Pending"))
  expect_false(dag0$is_complete)
})
