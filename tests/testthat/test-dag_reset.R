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
  expect_true(is.na(dag0$run_id))
  expect_true(is.na(dag0$run_start))
  expect_true(is.na(dag0$run_end))
  expect_false(dag0$is_complete)

  ## tasks are all in Pending Status
  dag_task_statuses <- dag_as_df(dag0)$status
  expect_true(all(dag_as_df(dag0)$status == "Pending"))

  ## no tasks have a task_variant
  connect_variants <- sapply(dag0$tasks, {\(task) task$connect_variant})
  expect_true(all(is.na(connect_variants)))

  ## no tasks have a task_rendering
  connect_renderings <- sapply(dag0$tasks, {\(task) task$connect_rendering})
  expect_true(all(is.na(connect_renderings)))

  ## poll state is cleared on every task
  expect_true(all(vapply(dag0$tasks, {\(task) task$poll_first}, integer(1)) == 0L))
  expect_true(all(vapply(dag0$tasks, {\(task) task$poll_error_count}, integer(1)) == 0L))
  expect_true(all(vapply(dag0$tasks, {\(task) is.na(task$poll_output)}, logical(1))))
  expect_true(all(vapply(dag0$tasks, {\(task) is.na(task$poll_task_id)}, logical(1))))
})

test_that("reset returns a Running task to Pending", {
  task0 <- sim_task("task0", fail_prob = 0, sim_duration = 5)
  task1 <- sim_task("task1", fail_prob = 0)
  task0 |> set_downstream(task1)

  dag0 <- connect_dag(task0, task1)

  # dispatch task0 directly so it is left in a Running state mid-flight
  task0$dispatch()
  expect_equal(task0$status, "Running")

  dag_reset(dag0)
  expect_equal(task0$status, "Pending")
  expect_true(is.na(task0$sim_will_fail))
  expect_true(is.na(task0$sim_polls_remaining))
})
