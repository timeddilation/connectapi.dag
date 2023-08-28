test_that("only ConnectTask may be added downstream", {
  task0 <- connect_task("task0", simulated = T)
  task1 <- connect_task("task1", simulated = T)

  expect_invisible(task0 |> set_downstream(task1))
  expect_error(task0 |> set_downstream("foo"))
  expect_error(task0 |> set_downstream(FALSE))
})

test_that("tasks are added downstream", {
  task0 <- connect_task("task0", simulated = T)
  task1 <- connect_task("task1", simulated = T)

  task0 |> set_downstream(task1)

  expect_equal(length(task0$downstream_tasks), 1)
  expect_equal(task0$downstream_tasks[[1]]$task_name, "task1")
})

test_that("multiple tasks can be added at once", {
  task0 <- connect_task("task0", simulated = T)
  task1 <- connect_task("task1", simulated = T)
  task2 <- connect_task("task2", simulated = T)

  task0 |> set_downstream(task1, task2)

  expect_equal(length(task0$downstream_tasks), 2)
})

test_that("tasks are not added upstream", {
  task0 <- connect_task("task0", simulated = T)
  task1 <- connect_task("task1", simulated = T)

  task0 |> set_downstream(task1)

  expect_equal(length(task0$upstream_tasks), 0)
})

test_that("added downstream task references task as upstream", {
  task0 <- connect_task("task0", simulated = T)
  task1 <- connect_task("task1", simulated = T)

  task0 |> set_downstream(task1)

  expect_equal(length(task1$downstream_tasks), 0)
  expect_equal(length(task1$upstream_tasks), 1)
  expect_equal(task1$upstream_tasks[[1]]$task_name, "task0")
})
