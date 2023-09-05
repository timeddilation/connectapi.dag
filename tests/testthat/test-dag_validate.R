test_that("a simple DAG is valid", {
  task0 <- connect_task("task0", simulated = TRUE)
  task1 <- connect_task("task1", simulated = TRUE)

  task0 |> set_downstream(task1)

  dag0 <-
    connect_dag(task0, task1) |>
    dag_validate() |>
    suppressMessages()

  expect_true(dag0$is_valid)
})

test_that("a complex DAG is valid", {
  task0 <- connect_task("task0", simulated = TRUE)
  task1 <- connect_task("task1", simulated = TRUE)
  task2 <- connect_task("task2", simulated = TRUE)
  task3 <- connect_task("task3", simulated = TRUE)
  task4 <- connect_task("task4", trigger_rule = "one_success", simulated = TRUE)
  task5 <- connect_task("task5", simulated = TRUE)
  task6 <- connect_task("task6", simulated = TRUE)
  task7 <- connect_task("task7", simulated = TRUE)
  task8 <- connect_task("task8", simulated = TRUE)
  task9 <- connect_task("task9", simulated = TRUE)
  # make a new dag with those tasks
  dag0 <- connect_dag(task0, task1, task2, task3, task4,
                      task5, task6, task7, task8, task9)

  task1 |>
    set_downstream(task2, task3) |>
    set_upstream(task0)
  task4 |>
    set_upstream(task1, task7, task8) |>
    set_downstream(task6)
  task5 |>
    set_upstream(task6)
  task9 |>
    set_upstream(task3, task6)

  dag_validate(dag0) |>
    suppressMessages()

  expect_true(dag0$is_valid)
})

test_that("circular dependencies is an invalid DAG", {
  task0 <- connect_task("task0", simulated = TRUE)
  task1 <- connect_task("task1", simulated = TRUE)
  task2 <- connect_task("task2", simulated = TRUE)

  task0 |> set_downstream(task1)
  task1 |> set_downstream(task2)
  task2 |> set_downstream(task0)

  dag0 <-
    connect_dag(task0, task1, task2) |>
    dag_validate() |>
    suppressMessages() |>
    suppressWarnings()

  expect_false(dag0$is_valid)
})

test_that("disconnected tasks is an invalid DAG", {
  task0 <- connect_task("task0", simulated = TRUE)
  task1 <- connect_task("task1", simulated = TRUE)

  dag0 <-
    connect_dag(task0, task1) |>
    dag_validate() |>
    suppressMessages() |>
    suppressWarnings()

  expect_false(dag0$is_valid)
})

test_that("one or more islands is an invalid DAG", {
  task0 <- connect_task("task0", simulated = TRUE)
  task1 <- connect_task("task1", simulated = TRUE)
  task2 <- connect_task("task2", simulated = TRUE)
  task3 <- connect_task("task3", simulated = TRUE)

  task0 |> set_downstream(task1)
  task2 |> set_downstream(task3)

  dag0 <-
    connect_dag(task0, task1, task2, task3) |>
    dag_validate() |>
    suppressMessages() |>
    suppressWarnings()

  expect_false(dag0$is_valid)
})

test_that("all tasks with a link to another task are provided to the dag", {
  task0 <- connect_task("task0", simulated = TRUE)
  task1 <- connect_task("task1", simulated = TRUE)

  task0 |> set_downstream(task1)

  dag0 <-
    connect_dag(task0) |>
    dag_validate() |>
    suppressMessages() |>
    suppressWarnings()

  expect_false(dag0$is_valid)
})
