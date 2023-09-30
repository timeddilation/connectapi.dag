#' Class simulating a ConnectTask
#'
#' @name SimTask
#'
#' @section Usage:
#' \preformatted{
#' sim_task <- SimTask$new("task0", "always", 0)
#' sim_task$execute_task()
#' }
#'
#' @section Details:
#'
#' This class simulates a ConnectTask running in a ConnectDAG.
#' It is inherited from ConnectTask, with the only differences being to the
#' `new()` and `execute_task()` methods.
#' However, when using a SimTask, no attempt is made to validate the content
#' is published to Posit Connect.
#' Additionally, you may control the probability a task will fail.
#' This allows you to simulate the DAG under specific scenarios.
#' This class is used heavily in tests when it is desirable to force
#' an upstream task failure.
#'
#' @importFrom R6 R6Class
#'
#' @family R6 classes
#' @export

SimTask <- R6::R6Class(
  "SimTask",
  inherit = ConnectTask,
  public = list(
    #' @field fail_prob A numeric between 0 and 1, determining the chance of a failure
    fail_prob = 0.2,

    #' @description Initializes a new SimTask
    #' @param guid A scalar character of the guid for the content item. Not validated in Posit Connect.
    #' @param trigger_rule A scalar character that defines state of dependency (upstream) tasks must be in to execute
    #' @param fail_prob A numeric between 0 and 1, determining the chance of a failure
    initialize = function(guid, trigger_rule = "all_success", fail_prob = 0.2) {
      stopifnot(
        is.numeric(fail_prob),
        fail_prob >= 0,
        fail_prob <= 1
      )

      trigger_rule <- match.arg(trigger_rule, trigger_options)

      self$task_guid <- paste0("simulated_", guid)
      self$task_name <- guid
      self$task_status <- "Pending"
      self$trigger_rule <- trigger_rule
      self$fail_prob <- fail_prob
    },

    #' @description Simulates the execution of task, taking into account failure probability
    #' @param verbose Should the task print messages as it executes?
    execute_task = function(verbose = FALSE) {
      if (verbose) message(paste("Starting task", self$task_name))

      if (self$can_run()) {
        fail_run <- sample(c(T,F), 1, prob = c(self$fail_prob, 1-self$fail_prob))

        if (fail_run) {
          self$task_status <- "Failed"
          if (verbose) message("Task Failed")
        } else {
          self$task_status <- "Succeeded"
          if (verbose) message("Task Succeeded")
        }
      } else {
        self$task_status <- "Skipped"
        if (verbose) message("Task Skipped")
      }

      invisible(self)
    }
  )
)

#' Simulate a Connect Task
#'
#' Simulates a connect task evaluation, with an optional probability for the task to fail.
#' For a full list of available trigger rules, refer to \link[connectapi.dag]{connect_task}.
#'
#' @param guid A scalar character for the guid/name of the task to simulate
#' @param trigger_rule A scalar character that defines state of dependency (upstream) tasks must be in to execute
#' @param fail_prob A value between 0 and 1 for the probability of task failure
#'
#' @return A SimTask E6 environment
#'
#' @examples
#' sim_task0 <- sim_task("foo", trigger_rule = "always", fail_prob = 0)
#' task_run(sim_task0)
#'
#' @export
sim_task <- function(guid, trigger_rule = "all_success", fail_prob = 0.2) {
  trigger_rule <- match.arg(trigger_rule, trigger_options)
  stopifnot(is.character(guid))

  SimTask$new(guid, trigger_rule, fail_prob)
}
