test_that("must be a ConnectDAG", {
  expect_error(dag_plotly("foo"))

  task0 <- connect_task("task0", simulated = TRUE)
  task1 <- connect_task("task1", simulated = TRUE)
  task2 <- connect_task("task2", simulated = TRUE)

  task1 |>
    set_upstream(task0) |>
    set_downstream(task2)

  dag0 <-
    connect_dag(task0, task1, task2) |>
    dag_validate() |>
    suppressMessages()

  expect_true(dag0$is_valid)
  expect_no_warning(dag_plotly(dag0))
  expect_true(inherits(dag_plotly(dag0), "plotly"))
})

test_that("DAG with no graph generated will silently be created", {
  task0 <- connect_task("task0", simulated = TRUE)
  task1 <- connect_task("task1", simulated = TRUE)
  task2 <- connect_task("task2", simulated = TRUE)

  task1 |>
    set_upstream(task0) |>
    set_downstream(task2)

  dag0 <- connect_dag(task0, task1, task2)

  expect_true(is.na(dag0$dag_graph))
  expect_true(inherits(dag_plotly(dag0), "plotly"))
  expect_true(inherits(dag0$dag_graph, "igraph"))
})

test_that("ConnectDAG plot method returns null when no graph is available", {
  dag0 <- connect_dag()

  expect_null(
    dag0$plot() |>
      suppressWarnings() |>
      suppressMessages()
  )
})

test_that("ConnectDAG plot method returns plotly when graph is available", {
  task0 <- connect_task("task0", simulated = TRUE)
  task1 <- connect_task("task1", simulated = TRUE)
  task2 <- connect_task("task2", simulated = TRUE)

  task1 |>
    set_upstream(task0) |>
    set_downstream(task2)

  dag0 <-
    connect_dag(task0, task1, task2) |>
    dag_validate() |>
    suppressMessages()

  expect_true(inherits(dag0$plot(), "plotly"))
})
