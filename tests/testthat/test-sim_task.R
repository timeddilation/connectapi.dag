test_that("returns a SimTask object", {
  sim_task0 <- sim_task("foo")

  expect_true(inherits(sim_task0, "SimTask"))
  expect_true(inherits(sim_task0, "ConnectTask"))
})

test_that("only valid trigger_rule can be supplied", {
  sim_task0 <- sim_task("foo")
  expect_equal(sim_task0$trigger_rule, "all_success")

  sim_task1 <- sim_task("bar", trigger_rule = "all_done")
  expect_equal(sim_task1$trigger_rule, "all_done")

  expect_error(sim_task("foobar", trigger_rule = "foobar"))
})

test_that("fail_prob = 0 always succeeds", {
  sim_task0 <- sim_task("foo", fail_prob = 0)
  task_run(sim_task0)

  expect_equal(sim_task0$task_status, "Succeeded")
})

test_that("fail_prob = 1 always fails", {
  sim_task0 <- sim_task("foo", fail_prob = 1)
  task_run(sim_task0)

  expect_equal(sim_task0$task_status, "Failed")
})

test_that("fail_prob between 0 and 1 will always either succeed or fail", {
  sim_task0 <- sim_task("foo", fail_prob = 0.5)
  task_run(sim_task0)

  expect_true(sim_task0$task_status %in% c("Succeeded", "Failed"))
})

test_that("non-verbose task execution does not print message", {
  sim_task0 <- sim_task("foo", fail_prob = 0)

  expect_no_message(task_run(sim_task0, verbose = FALSE))
})

test_that("verbose task execution prints success message", {
  sim_task0 <- sim_task("foo", fail_prob = 0)

  expect_message(
    task_run(sim_task0, verbose = TRUE),
    regexp = "Task Succeeded"
  ) |> suppressMessages()
})

test_that("verbose task execution prints failed message", {
  sim_task0 <- sim_task("foo", fail_prob = 1)

  expect_message(
    task_run(sim_task0, verbose = TRUE),
    regexp = "Task Failed"
  ) |> suppressMessages()
})

test_that("verbose task execution prints skipped message", {
  sim_task0 <- sim_task("foo", fail_prob = 1)
  sim_task1 <- sim_task("bar", fail_prob = 0)

  sim_task0 |> set_downstream(sim_task1)

  task_run(sim_task0)

  expect_message(
    task_run(sim_task1, verbose = TRUE),
    regexp = "Task Skipped"
  ) |> suppressMessages()
})
