#' Class simulating a ConnectTask
#'
#' @name SimTask
#'
#' @section Usage:
#' \preformatted{
#' sim_task <- SimTask$new("task0", "always", 0)
#' sim_task$execute()
#' }
#'
#' @section Details:
#'
#' This class simulates a ConnectTask running in a ConnectDAG.
#' It is inherited from ConnectTask, overriding the `new()`, `dispatch()`, and
#' `poll_once()` methods so no calls are made to Posit Connect.
#' However, when using a SimTask, no attempt is made to validate the content
#' is published to Posit Connect.
#' Additionally, you may control the probability a task will fail.
#' This allows you to simulate the DAG under specific scenarios.
#' You may also give a task a simulated duration (in scheduler poll cycles),
#' which is useful for exercising concurrent DAG execution in tests.
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
    #' @field sim_duration The number of scheduler poll cycles the task stays Running before finishing
    sim_duration = 0L,
    #' @field sim_polls_remaining The number of poll cycles left before this dispatched task finishes
    sim_polls_remaining = NA_integer_,
    #' @field sim_will_fail Whether the in-flight simulated render will end in failure, decided at dispatch
    sim_will_fail = NA,

    #' @description Initializes a new SimTask
    #' @param guid A scalar character of the guid for the content item. Not validated in Posit Connect.
    #' @param trigger_rule A scalar character that defines state of dependency (upstream) tasks must be in to execute
    #' @param fail_prob A numeric between 0 and 1, determining the chance of a failure
    #' @param sim_duration The number of scheduler poll cycles the task should stay Running before finishing
    initialize = function(guid, trigger_rule = "all_success", fail_prob = 0.2, sim_duration = 0L) {
      stopifnot(
        is.numeric(fail_prob),
        fail_prob >= 0,
        fail_prob <= 1,
        is.numeric(sim_duration),
        sim_duration >= 0
      )

      trigger_rule <- match.arg(trigger_rule, trigger_options)

      self$guid <- paste0("simulated_", guid)
      self$name <- guid
      self$status <- "Pending"
      self$trigger_rule <- trigger_rule
      self$app_mode <- "simulation"
      self$fail_prob <- fail_prob
      self$sim_duration <- as.integer(sim_duration)
    },

    #' @description Resets the simulated task to an initial state
    reset = function() {
      super$reset()
      self$sim_polls_remaining <- NA_integer_
      self$sim_will_fail <- NA

      invisible(self)
    },

    #' @description Simulates dispatching a render, deciding the eventual outcome up front
    #' @param verbose Should the task print messages as it executes?
    dispatch = function(verbose = FALSE) {
      if (verbose) message(paste("Starting task", self$name))

      self$sim_will_fail <-
        sample(c(TRUE, FALSE), 1, prob = c(self$fail_prob, 1 - self$fail_prob))
      self$sim_polls_remaining <- self$sim_duration
      self$dispatch_time <- Sys.time()
      self$status <- "Running"

      if (self$sim_duration == 0L) self$poll_once(verbose = verbose)

      invisible(self)
    },

    #' @description Simulates polling the render, finishing once the simulated duration elapses
    #' @param wait Ignored for simulated tasks; present for signature compatibility
    #' @param verbose Should the task print messages as it executes?
    #' @param error_threshold Ignored for simulated tasks; present for signature compatibility
    poll_once = function(wait = 0, verbose = FALSE, error_threshold = 3L) {
      self$sim_polls_remaining <- self$sim_polls_remaining - 1L

      if (self$sim_polls_remaining > 0L) return(invisible(self))

      if (self$sim_will_fail) {
        self$status <- "Failed"
        if (verbose) message("Task Failed")
      } else {
        self$status <- "Succeeded"
        if (verbose) message("Task Succeeded")
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
#' @param sim_duration The number of scheduler poll cycles the task should stay Running before finishing.
#'   Defaults to 0 (finishes immediately). Use a positive value to simulate a long-running render
#'   when testing concurrent DAG execution.
#'
#' @return A SimTask E6 environment
#'
#' @examples
#' sim_task0 <- sim_task("foo", trigger_rule = "always", fail_prob = 0)
#' task_run(sim_task0)
#'
#' @export
sim_task <- function(guid, trigger_rule = "all_success", fail_prob = 0.2, sim_duration = 0L) {
  trigger_rule <- match.arg(trigger_rule, trigger_options)
  stopifnot(is.character(guid))

  SimTask$new(guid, trigger_rule, fail_prob, sim_duration)
}
