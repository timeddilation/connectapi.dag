test_that("DAG environment is in expected state", {
  dag0 <- dag_integration()

  expect_true(dag0$is_valid)
  expect_s3_class(dag0$dag_graph, "igraph")
  expect_length(dag0$tasks, 4)
  expect_false(dag0$is_complete)

  dag_task_statuses <- 
    vapply(dag0$tasks, {\(task) task$status}, character(1))
  expect_true(all(dag_task_statuses == "Pending"))
})
