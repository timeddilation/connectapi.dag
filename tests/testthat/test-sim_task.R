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
