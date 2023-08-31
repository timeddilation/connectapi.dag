test_that("setting a name changes the name", {
  dag0 <-
    connect_dag(name = "foo") |>
    dag_set_name("bar")

  expect_equal(dag0$name, "bar")
})

test_that("env is a ConnectDag", {
  expect_error(dag_set_name("foo", "bar"))
})

test_that("name must be character", {
  expect_error(dag_set_name(connect_dag(), TRUE))
})

test_that("name must be scalar", {
  expect_error(dag_set_name(connect_dag(), c("foo", "bar")))
})
