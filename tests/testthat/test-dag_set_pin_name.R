test_that("using set pin name only changes the pin name", {
  dag0 <- connect_dag(name = "foo")
  dag_set_pin_name(dag0, "bar")

  expect_equal(dag0$name, "foo")
  expect_equal(dag0$pin_name, "bar")
})

test_that("warns when pin name is less than 3 characters", {
  dag0 <- connect_dag(name = "foo")

  expect_warning(dag_set_pin_name(dag0, "ba"))
})

test_that("warns when pin name is greater than 64 character", {
  dag0 <- connect_dag(name = "foo")

  expect_warning(dag_set_pin_name(dag0, "5ab896b9-8096-4dc1-af5e-7732b1d694b4-8cec681d-374a-4bfe-8b26-1a691a607e5a"))
})

test_that("warns when pin name used illegal characters", {
  dag0 <- connect_dag(name = "foo")

  expect_warning(dag_set_pin_name(dag0, "foo bar"))
})

test_that("when pin name uses illegal characters, they are substitued with legal characters", {
  dag0 <- connect_dag(name = "foo")
  dag_set_pin_name(dag0, "foo bar") |>
    suppressWarnings()

  expect_equal(dag0$pin_name, "foo-bar")
})

test_that("env must be a dag", {
  task0 <- connect_task("task0", simulated = TRUE)

  expect_error(dag_set_pin_name(task0, "bar"))
})

test_that("pin_name must be a character", {
  dag0 <- connect_dag(name = "foo")

  expect_error(dag_set_pin_name(dag0, TRUE))
})

test_that("pin_name must be scalar", {
  dag0 <- connect_dag(name = "foo")

  expect_error(dag_set_pin_name(dag0, c("foo", "bar")))
})
