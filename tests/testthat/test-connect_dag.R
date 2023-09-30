test_that("dag intiializes with provided tasks", {
  task0 <- sim_task("task0", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)

  dag0 <- connect_dag(task0, task1)

  expect_true(inherits(dag0, "ConnectDAG"))
  expect_equal(length(dag0$tasks), 2)
  expect_output(
    suppressMessages(print(dag0)),
    regexp = paste0(
      "^ConnectDAG.*",
      "Name: .*",
      "Is Valid: (TRUE|FALSE).*",
      "Pin Name: .*",
      "Tasks: [0-9]*.*"
    )
  )
})

test_that("dag initializes with only a name and no tasks", {
  dag0 <- connect_dag(name = "foo")

  expect_true(inherits(dag0, "ConnectDAG"))
  expect_equal(length(dag0$tasks), 0)
  expect_equal(dag0$name, "foo")

  dag0$evaluate_validity() |>
    suppressMessages()
  expect_false(dag0$is_valid)
})

test_that("dag fails to initialize when provided tasks are not ConnecTask objects", {
  expect_error(connect_dag("foo"))
})
