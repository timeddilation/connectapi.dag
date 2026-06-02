test_that("max_concurrent = 1 runs the DAG and succeeds (serial path)", {
  task0 <- sim_task("task0", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)

  task1 |>
    set_upstream(task0) |>
    set_downstream(task2)

  dag0 <- connect_dag(task0, task1, task2, max_concurrent = 1)
  dag_run(dag0)

  statuses <- vapply(dag0$tasks, {\(task) task$status}, character(1))
  expect_true(all(statuses == "Succeeded"))
})

test_that("a diamond runs both middle branches concurrently", {
  root <- sim_task("root", fail_prob = 0)
  left <- sim_task("left", fail_prob = 0, sim_duration = 2)
  right <- sim_task("right", fail_prob = 0, sim_duration = 2)
  join <- sim_task("join", fail_prob = 0)

  root |> set_downstream(left, right)
  join |> set_upstream(left, right)

  dag0 <- connect_dag(root, left, right, join, max_concurrent = 2)
  dag0$set_poll_interval(0.01)
  dag_run(dag0)

  statuses <- vapply(dag0$tasks, {\(task) task$status}, character(1))
  expect_true(all(statuses == "Succeeded"))
})

test_that("max_concurrent caps the number of simultaneously running tasks", {
  tracker <- new.env()
  tracker$current <- 0L
  tracker$max <- 0L

  TrackingTask <- R6::R6Class(
    "TrackingTask",
    inherit = SimTask,
    public = list(
      tracker = NULL,
      initialize = function(guid, tracker, sim_duration = 2L) {
        super$initialize(guid, "all_success", fail_prob = 0, sim_duration = sim_duration)
        self$tracker <- tracker
      },
      dispatch = function(verbose = FALSE) {
        super$dispatch(verbose)
        if (self$status == "Running") {
          self$tracker$current <- self$tracker$current + 1L
          self$tracker$max <- max(self$tracker$max, self$tracker$current)
        }
        invisible(self)
      },
      poll_once = function(wait = 0, verbose = FALSE, error_threshold = 3L) {
        was_running <- self$status == "Running"
        super$poll_once(wait, verbose, error_threshold)
        if (was_running && self$status != "Running") {
          self$tracker$current <- self$tracker$current - 1L
        }
        invisible(self)
      }
    )
  )

  root <- sim_task("root", fail_prob = 0)
  children <- lapply(
    1:5,
    function(i) TrackingTask$new(paste0("child", i), tracker, sim_duration = 2L)
  )
  do.call(root$set_downstream, children)

  dag0 <- connect_dag(root, max_concurrent = 2)
  do.call(dag0$add_tasks, children)
  dag0$set_poll_interval(0.01)

  dag_run(dag0)

  expect_equal(tracker$max, 2L)
  child_statuses <- vapply(children, {\(task) task$status}, character(1))
  expect_true(all(child_statuses == "Succeeded"))
})

test_that("a downstream waits for all upstreams to finish before being evaluated", {
  fast <- sim_task("fast", fail_prob = 0)
  slow <- sim_task("slow", fail_prob = 1, sim_duration = 3)
  # all_success: must see slow's final (Failed) status, not evaluate early
  downstream <- sim_task("downstream", trigger_rule = "all_success", fail_prob = 0)
  downstream |> set_upstream(fast, slow)

  dag0 <- connect_dag(fast, slow, downstream, max_concurrent = 3)
  dag0$set_poll_interval(0.01)
  dag_run(dag0)

  expect_equal(fast$status, "Succeeded")
  expect_equal(slow$status, "Failed")
  expect_equal(downstream$status, "Skipped")
})

test_that("trigger rules resolve identically under concurrent execution", {
  # upstream A succeeds, upstream B fails; verify each rule's downstream outcome
  cases <- list(
    list(rule = "all_success",  expect = "Skipped"),
    list(rule = "all_failed",   expect = "Skipped"),
    list(rule = "all_done",     expect = "Succeeded"),
    list(rule = "one_success",  expect = "Succeeded"),
    list(rule = "one_failed",   expect = "Succeeded"),
    list(rule = "one_done",     expect = "Succeeded"),
    list(rule = "none_failed",  expect = "Skipped"),
    list(rule = "none_skipped", expect = "Succeeded"),
    list(rule = "always",       expect = "Succeeded")
  )

  for (case in cases) {
    a <- sim_task("a", fail_prob = 0, sim_duration = 1)
    b <- sim_task("b", fail_prob = 1, sim_duration = 2)
    d <- sim_task("d", trigger_rule = case$rule, fail_prob = 0)
    d |> set_upstream(a, b)

    dag0 <- connect_dag(a, b, d, max_concurrent = 3)
    dag0$set_poll_interval(0.01)
    dag_run(dag0)

    expect_equal(d$status, case$expect, info = case$rule)
  }
})

test_that("all_skipped resolves through a skip cascade", {
  grandparent <- sim_task("grandparent", fail_prob = 1)
  a <- sim_task("a", trigger_rule = "all_success", fail_prob = 0)
  b <- sim_task("b", trigger_rule = "all_success", fail_prob = 0)
  d <- sim_task("d", trigger_rule = "all_skipped", fail_prob = 0)

  grandparent |> set_downstream(a, b)
  d |> set_upstream(a, b)

  dag0 <- connect_dag(grandparent, a, b, d, max_concurrent = 3)
  dag0$set_poll_interval(0.01)
  dag_run(dag0)

  expect_equal(grandparent$status, "Failed")
  expect_equal(a$status, "Skipped")
  expect_equal(b$status, "Skipped")
  expect_equal(d$status, "Succeeded")
})

test_that("a chain of skips terminates without hanging", {
  root <- sim_task("root", fail_prob = 1)
  a <- sim_task("a", trigger_rule = "all_success", fail_prob = 0)
  b <- sim_task("b", trigger_rule = "all_success", fail_prob = 0)
  c <- sim_task("c", trigger_rule = "all_success", fail_prob = 0)

  root |> set_downstream(a)
  a |> set_downstream(b)
  b |> set_downstream(c)

  dag0 <- connect_dag(root, a, b, c, max_concurrent = 2)
  dag_run(dag0)

  expect_true(dag0$is_complete)
  expect_equal(c$status, "Skipped")
})

test_that("per-task timeout fails a task that never finishes", {
  slow <- sim_task("slow", fail_prob = 0, sim_duration = 1e6)
  downstream <- sim_task("downstream", trigger_rule = "all_done", fail_prob = 0)
  slow |> set_downstream(downstream)

  dag0 <- connect_dag(slow, downstream, max_concurrent = 2)
  dag0$set_poll_interval(0.05)
  dag0$set_task_timeout(0.3)
  dag_run(dag0)

  expect_equal(slow$status, "Failed")
  expect_equal(downstream$status, "Succeeded")
  expect_true(dag0$is_complete)
})

test_that("global DAG timeout halts a run that never finishes", {
  slow <- sim_task("slow", fail_prob = 0, sim_duration = 1e6)
  downstream <- sim_task("downstream", trigger_rule = "all_done", fail_prob = 0)
  slow |> set_downstream(downstream)

  dag0 <- connect_dag(slow, downstream, max_concurrent = 2)
  dag0$set_poll_interval(0.05)
  dag0$set_dag_timeout(0.3)
  dag_run(dag0) |> suppressMessages()

  expect_equal(slow$status, "Failed")
  expect_true(dag0$is_complete)
})
