test_that("tasks are added after dag is initialized", {
  dag0 <- connect_dag()

  task0 <- sim_task("task0", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)

  dag0 |>
    dag_add_tasks(task0, task1)

  expect_equal(length(dag0$tasks), 2)
})

test_that("env is a ConnectDAG", {
  obj <- TRUE
  task0 <- sim_task("task0", fail_prob = 0)

  expect_error(obj |> dag_add_tasks(task0))
})

test_that("Added tasks are a ConnectTask", {
  dag0 <- connect_dag()

  task0 <- sim_task("task0", fail_prob = 0)
  obj <- TRUE

  expect_error(dag0 |> dag_add_tasks(task0, obj))
})

test_that("Same task added more than once only links once", {
  dag0 <- connect_dag()

  task0 <- sim_task("task0", fail_prob = 0)

  dag0 |>
    dag_add_tasks(task0, task0)

  expect_equal(length(dag0$tasks), 1)
})
