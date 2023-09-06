test_that("must be a dag", {
  expect_error(dag_reset("foo"))

  dag0 <- connect_dag(name = "foo")
  dag_reset(dag0)

  expect_true(inherits(dag0, "ConnectDAG"))
})

test_that("all tasks reset to pending and envs are cleared, and is_complete is FALSE", {
  task0 <- sim_task("task0", fail_prob = 0) # will complete
  task1 <- sim_task("task1", fail_prob = 1) # will fail
  task2 <- sim_task("task2", fail_prob = 0) # will skip

  task1 |>
    set_upstream(task0) |>
    set_downstream(task2)

  dag0 <- connect_dag(task0, task1, task2)
  dag_run(dag0)

  expect_true(dag0$is_complete)

  # actual test of reset functionality
  dag_reset(dag0)
  ## DAG is flagged as incomplete
  expect_false(dag0$is_complete)

  ## tasks are all in Pending Status
  dag_task_statuses <- dag_as_df(dag0)$task_status
  expect_true(all(dag_as_df(dag0)$task_status == "Pending"))

  ## no tasks have a task_variant
  task_variants <-
    lapply(dag0$dag_tasks, {\(task) task$task_variant}) |>
    unlist()
  expect_true(all(is.na(task_variants)))
  ## no tasks have a task_rendering
  task_renderings <-
    lapply(dag0$dag_tasks, {\(task) task$task_rendering}) |>
    unlist()
  expect_true(all(is.na(task_renderings)))
})
