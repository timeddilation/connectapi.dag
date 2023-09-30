test_that("specified task is removed", {
  task0 <- sim_task("task0", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)

  dag0 <- connect_dag(task0, task1, task2)
  dag_remove_task(dag0, task1)

  expect_length(dag0$tasks, 2)

  task_names <- dag0$task_attrs("name")
  expect_true(all(task_names %in% c("task0", "task2")))
})

test_that("attempt to remove a task not added raises warning", {
  task0 <- sim_task("task0", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)

  dag0 <- connect_dag(task0, task1)

  expect_warning(dag_remove_task(dag0, task2))
  expect_length(dag0$tasks, 2)
})

test_that("only a ConnectDAG can be passed to env param", {
  task0 <- sim_task("task0", fail_prob = 0)

  dag0 <- connect_dag(task0)

  expect_error(dag_remove_task("dag0", task0))
})

test_that("only a ConnectTask can be passed to task param", {
  task0 <- sim_task("task0", fail_prob = 0)

  dag0 <- connect_dag(task0)

  expect_error(dag_remove_task(dag0, "task0"))
})

test_that("removing a task invalidates DAG until re-checked", {
  #TODO
  expect_true(TRUE)
})
