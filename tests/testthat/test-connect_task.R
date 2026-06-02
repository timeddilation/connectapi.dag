test_that("connect_task generates ConnectTask object", {
  expect_true(inherits(connect_task("foo", simulated = TRUE), "ConnectTask"))

  expect_output(
    print(connect_task("foo", simulated = TRUE)),
    regexp = paste0(
      "^ConnectTask.*", "GUID: .*foo.*", "Name: foo.*",
      "Trigger Rule: all_success.*",
      "App Mode: simulation.*",
      "Status: Pending.*",
      "Upstream Tasks: [0-9]*.*",
      "Downstream Tasks: [0-9]*.*"
    )
  )
})

test_that("only valid trigger_rule can be supplied", {
  task0 <- connect_task("foo", simulated = TRUE)
  expect_true(inherits(task0, "ConnectTask"))
  expect_equal(task0$trigger_rule, "all_success")

  task1 <- connect_task("bar", trigger_rule = "all_done", simulated = TRUE)
  expect_true(inherits(task1, "ConnectTask"))
  expect_equal(task1$trigger_rule, "all_done")

  expect_error(connect_task("foo", trigger_rule = "bar", simulated = TRUE))
})

test_that("dispatch sets a task Running before it finishes", {
  task0 <- sim_task("foo", fail_prob = 0, sim_duration = 1)

  expect_equal(task0$status, "Pending")
  task0$dispatch()
  expect_equal(task0$status, "Running")
})

test_that("execute blocks until a dispatched task reaches a terminal status", {
  task0 <- sim_task("foo", fail_prob = 0, sim_duration = 3)
  task0$execute()

  expect_equal(task0$status, "Succeeded")
})

test_that("a task that cannot be skipped is Skipped by execute", {
  task0 <- sim_task("foo", fail_prob = 1)
  task1 <- sim_task("bar", fail_prob = 0)
  task0 |> set_downstream(task1)

  task0$execute()
  task1$execute()

  expect_equal(task0$status, "Failed")
  expect_equal(task1$status, "Skipped")
})
