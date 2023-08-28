test_that("connect_task generates ConnectTask object", {
  expect_true(inherits(connect_task("foo", simulated = TRUE), "ConnectTask"))
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
