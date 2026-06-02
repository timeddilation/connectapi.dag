test_that("dummy test, cannot unit test without auth to a connect server", {
  expect_equal(2 * 2, 4)
})

test_that("a DAG survives an RDS round-trip with its concurrency settings", {
  task0 <- sim_task("task0", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task0 |> set_downstream(task1)

  dag0 <- connect_dag(task0, task1, name = "round_trip", max_concurrent = 3)
  dag0$set_task_timeout(120)
  dag_run(dag0)

  rds_path <- tempfile(fileext = ".rds")
  on.exit(unlink(rds_path), add = TRUE)
  saveRDS(dag0, rds_path)
  restored <- readRDS(rds_path)

  expect_equal(restored$max_concurrent, 3L)
  expect_equal(restored$task_timeout, 120)
  expect_true(restored$is_complete)

  # a restored DAG resets cleanly and can run again
  dag_reset(restored)
  expect_false(restored$is_complete)
  expect_equal(restored$max_concurrent, 3L)
  expect_true(all(vapply(restored$tasks, {\(task) task$poll_first}, integer(1)) == 0L))

  dag_run(restored)
  statuses <- vapply(restored$tasks, {\(task) task$status}, character(1))
  expect_true(all(statuses == "Succeeded"))
})
