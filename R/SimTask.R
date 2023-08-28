SimTask <- R6::R6Class(
  "SimTask",
  inherit = ConnectTask,


  public = list(
    fail_prob = 0.2,


    initialize = function(task_name, trigger_rule = "all_success", fail_prob = 0.2) {
      stopifnot(is.numeric(fail_prob))

      super$initialize(task_name, trigger_rule)
      self$fail_prob <- fail_prob
    },


    execute_task = function(verbose = FALSE) {
      if (verbose) message(paste("Starting task", self$task_name))

      if (self$can_run()) {
        # actually do something
        ### BEGIN STUB
        fail_run <- sample(c(T,F), 1, prob = c(self$fail_prob, 1-self$fail_prob))

        if (fail_run) {
          self$task_status <- "Failed"
          if (verbose) message("Task Failed")
        } else {
          self$task_status <- "Succeeded"
          if (verbose) message("Task Succeeded")
        }
        ### END STUB
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
#'
#' @param task_name A scalar character for the name of the task deployed to Posit Connect
#' @param trigger_rule A scalar character that defines state of dependency (upstream) tasks must be in to execute
#' @param fail_prob A value between 0 and 1 for the probability of task failure
#'
#' @export
sim_task <- function(task_name, trigger_rule = "all_success", fail_prob = 0.2) {
  stopifnot(is.character(task_name), is.character(trigger_rule),
            fail_prob >= 0, fail_prob <= 1)
  SimTask$new(task_name, trigger_rule, fail_prob)
}
