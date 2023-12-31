################################################################################
# no upstream tasks
################################################################################
test_that("task runs regardless of trigger rule with no upstream tasks", {
  task0 <- sim_task("task0", fail_prob = 0)
  task_run(task0)
  expect_equal(task0$status, "Succeeded")
})
################################################################################
# all_success
################################################################################
test_that("all_success trigger runs successfully", {
  task0 <- sim_task("task0", trigger_rule = "all_success", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)

  task0 |> set_upstream(task1, task2)

  task_run(task1)
  task_run(task2)
  task_run(task0)

  expect_equal(task1$status, "Succeeded")
  expect_equal(task2$status, "Succeeded")
  expect_equal(task0$status, "Succeeded")
})

test_that("all_success trigger skips when one upstream not successful", {
  task0 <- sim_task("task0", trigger_rule = "all_success", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 1)
  task2 <- sim_task("task2", fail_prob = 0)

  task0 |> set_upstream(task1, task2)

  task_run(task1)
  task_run(task2)
  task_run(task0)

  expect_equal(task1$status, "Failed")
  expect_equal(task2$status, "Succeeded")
  expect_equal(task0$status, "Skipped")
})
################################################################################
# all_failed
################################################################################
test_that("all_failed trigger runs successfully", {
  task0 <- sim_task("task0", trigger_rule = "all_failed", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 1)
  task2 <- sim_task("task2", fail_prob = 1)

  task0 |> set_upstream(task1, task2)

  task_run(task1)
  task_run(task2)
  task_run(task0)

  expect_equal(task1$status, "Failed")
  expect_equal(task2$status, "Failed")
  expect_equal(task0$status, "Succeeded")
})

test_that("all_failed trigger skips when on upstream task is not Failed", {
  task0 <- sim_task("task0", trigger_rule = "all_failed", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 1)
  task2 <- sim_task("task2", fail_prob = 0)

  task0 |> set_upstream(task1, task2)

  task_run(task1)
  task_run(task2)
  task_run(task0)

  expect_equal(task1$status, "Failed")
  expect_equal(task2$status, "Succeeded")
  expect_equal(task0$status, "Skipped")
})
################################################################################
# all_done
################################################################################
test_that("all_done trigger runs for all terminal upstream states", {
  task0 <- sim_task("task0", trigger_rule = "all_done", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 1)
  task2 <- sim_task("task2", fail_prob = 0)
  task3 <- sim_task("task3", fail_prob = 0)
  task4 <- sim_task("task4", fail_prob = 1)

  task0 |> set_upstream(task1, task2, task3)
  task3 |> set_upstream(task4)

  task_run(task4)
  task_run(task3)
  task_run(task1)
  task_run(task2)
  task_run(task0)

  expect_equal(task4$status, "Failed")
  expect_equal(task3$status, "Skipped")
  expect_equal(task1$status, "Failed")
  expect_equal(task2$status, "Succeeded")
  expect_equal(task0$status, "Succeeded")
})

test_that("all_done trigger skips when upstream is not terminal", {
  task0 <- sim_task("task0", trigger_rule = "all_failed", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)

  task0 |> set_upstream(task1, task2)

  task_run(task1)
  task_run(task0)

  expect_equal(task1$status, "Succeeded")
  expect_equal(task2$status, "Pending")
  expect_equal(task0$status, "Skipped")
})
################################################################################
# all_skipped
################################################################################
test_that("all_skipped trigger runs successfully", {
  task0 <- sim_task("task0", trigger_rule = "all_skipped", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)
  task3 <- sim_task("task3", fail_prob = 1)

  task0 |> set_upstream(task1, task2)
  task3 |> set_downstream(task1, task2)

  task_run(task3)
  task_run(task2)
  task_run(task1)
  task_run(task0)

  expect_equal(task3$status, "Failed")
  expect_equal(task1$status, "Skipped")
  expect_equal(task2$status, "Skipped")
  expect_equal(task0$status, "Succeeded")
})

test_that("all_skipped trigger skips if any upstream not skipped", {
  task0 <- sim_task("task0", trigger_rule = "all_skipped", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)
  task3 <- sim_task("task3", fail_prob = 1)

  task0 |> set_upstream(task1, task2)
  task3 |> set_downstream(task1)

  task_run(task3)
  task_run(task2)
  task_run(task1)
  task_run(task0)

  expect_equal(task3$status, "Failed")
  expect_equal(task1$status, "Skipped")
  expect_equal(task2$status, "Succeeded")
  expect_equal(task0$status, "Skipped")
})
################################################################################
# one_success
################################################################################
test_that("one_success trigger runs successfully", {
  task0 <- sim_task("task0", trigger_rule = "one_success", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 1)
  task2 <- sim_task("task2", fail_prob = 0)

  task0 |> set_upstream(task1, task2)

  task_run(task1)
  task_run(task2)
  task_run(task0)

  expect_equal(task1$status, "Failed")
  expect_equal(task2$status, "Succeeded")
  expect_equal(task0$status, "Succeeded")
})

test_that("one_success trigger skips if none upstream are successful", {
  task0 <- sim_task("task0", trigger_rule = "one_success", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 1)
  task2 <- sim_task("task2", fail_prob = 1)

  task0 |> set_upstream(task1, task2)

  task_run(task1)
  task_run(task2)
  task_run(task0)

  expect_equal(task1$status, "Failed")
  expect_equal(task2$status, "Failed")
  expect_equal(task0$status, "Skipped")
})
################################################################################
# one_failed
################################################################################
test_that("one_failed trigger runs successfully", {
  task0 <- sim_task("task0", trigger_rule = "one_failed", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 1)
  task2 <- sim_task("task2", fail_prob = 0)

  task0 |> set_upstream(task1, task2)

  task_run(task1)
  task_run(task2)
  task_run(task0)

  expect_equal(task1$status, "Failed")
  expect_equal(task2$status, "Succeeded")
  expect_equal(task0$status, "Succeeded")
})

test_that("one_failed trigger skips if none upstream are failed", {
  task0 <- sim_task("task0", trigger_rule = "one_failed", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)

  task0 |> set_upstream(task1, task2)

  task_run(task1)
  task_run(task2)
  task_run(task0)

  expect_equal(task1$status, "Succeeded")
  expect_equal(task2$status, "Succeeded")
  expect_equal(task0$status, "Skipped")
})
################################################################################
# one_done
################################################################################
test_that("one_done trigger runs successfully when upstream succeeded", {
  task0 <- sim_task("task0", trigger_rule = "one_done", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)

  task0 |> set_upstream(task1, task2)

  task_run(task2)
  task_run(task0)

  expect_equal(task1$status, "Pending")
  expect_equal(task2$status, "Succeeded")
  expect_equal(task0$status, "Succeeded")
})

test_that("one_done trigger runs successfully when upstream failed", {
  task0 <- sim_task("task0", trigger_rule = "one_done", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 1)
  task2 <- sim_task("task2", fail_prob = 1)

  task0 |> set_upstream(task1, task2)

  task_run(task2)
  task_run(task0)

  expect_equal(task1$status, "Pending")
  expect_equal(task2$status, "Failed")
  expect_equal(task0$status, "Succeeded")
})

test_that("one_done trigger runs successfully when upstream skipped", {
  task0 <- sim_task("task0", trigger_rule = "one_done", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)
  task3 <- sim_task("task3", fail_prob = 1)

  task0 |> set_upstream(task1, task2)
  task2 |> set_upstream(task3)

  task_run(task3)
  task_run(task2)
  task_run(task0)

  expect_equal(task3$status, "Failed")
  expect_equal(task1$status, "Pending")
  expect_equal(task2$status, "Skipped")
  expect_equal(task0$status, "Succeeded")
})

test_that("one_done trigger skips if none upstream are done", {
  task0 <- sim_task("task0", trigger_rule = "one_done", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)

  task0 |> set_upstream(task1, task2)

  task_run(task0)

  expect_equal(task1$status, "Pending")
  expect_equal(task2$status, "Pending")
  expect_equal(task0$status, "Skipped")
})
################################################################################
# none_failed
################################################################################
test_that("none_failed trigger runs successfully when upstream succeeded or skipped", {
  task0 <- sim_task("task0", trigger_rule = "none_failed", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)
  task3 <- sim_task("task3", fail_prob = 1)

  task0 |> set_upstream(task1, task2)
  task2 |> set_upstream(task3)

  task_run(task3)
  task_run(task2)
  task_run(task1)
  task_run(task0)

  expect_equal(task3$status, "Failed")
  expect_equal(task2$status, "Skipped")
  expect_equal(task1$status, "Succeeded")
  expect_equal(task0$status, "Succeeded")
})

test_that("none_failed trigger skips when one upstream failed", {
  task0 <- sim_task("task0", trigger_rule = "none_failed", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 1)

  task0 |> set_upstream(task1, task2)

  task_run(task2)
  task_run(task1)
  task_run(task0)

  expect_equal(task2$status, "Failed")
  expect_equal(task1$status, "Succeeded")
  expect_equal(task0$status, "Skipped")
})
################################################################################
# none_skipped
################################################################################
test_that("none_skipped trigger runs successfully when upstream succeeded or failed", {
  task0 <- sim_task("task0", trigger_rule = "none_skipped", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 1)

  task0 |> set_upstream(task1, task2)

  task_run(task2)
  task_run(task1)
  task_run(task0)

  expect_equal(task2$status, "Failed")
  expect_equal(task1$status, "Succeeded")
  expect_equal(task0$status, "Succeeded")
})

test_that("none_skipped trigger skips when one upstream skipped", {
  task0 <- sim_task("task0", trigger_rule = "none_skipped", fail_prob = 0)
  task1 <- sim_task("task1", fail_prob = 0)
  task2 <- sim_task("task2", fail_prob = 0)
  task3 <- sim_task("task3", fail_prob = 1)

  task0 |> set_upstream(task1, task2)
  task2 |> set_upstream(task3)

  task_run(task3)
  task_run(task2)
  task_run(task1)
  task_run(task0)

  expect_equal(task3$status, "Failed")
  expect_equal(task2$status, "Skipped")
  expect_equal(task1$status, "Succeeded")
  expect_equal(task0$status, "Skipped")
})
################################################################################
# always
################################################################################
test_that("always trigger runs successfully regardless of upstream task statuses", {
  task0 <- sim_task("task0", trigger_rule = "always", fail_prob = 0) # Expect Succeeded
  task1 <- sim_task("task1", fail_prob = 0) # Force Succeeded
  task2 <- sim_task("task2", fail_prob = 1) # Force Failed
  task3 <- sim_task("task3", fail_prob = 0) # Force Skipped
  task4 <- sim_task("task4", fail_prob = 1) # Force Failed

  task4 |> set_downstream(task3)
  # task0 will have 3 predecessors, one in each status: Succeeded, Skipped, and Failed
  task0 |> set_upstream(task1, task2, task3)

  dag0 <- connect_dag(task0, task1, task2, task3, task4)

  task_run(task4)
  task_run(task3)
  task_run(task2)
  task_run(task1)
  task_run(task0)

  expect_equal(task4$status, "Failed")
  expect_equal(task3$status, "Skipped")
  expect_equal(task2$status, "Failed")
  expect_equal(task1$status, "Succeeded")
  expect_equal(task0$status, "Succeeded")
})
