#' Creates a new ConnectTask object
#'
#' @description
#' This function generates an R6 class for a ConnectTask object.
#' The object then has methods to add dependency and dependent tasks.
#' Once tasks are added to a ConnectDag they may be executed.
#'
#' @details
#' You can make other tasks dependency tasks by using \link[connectapi.dag]{set_upstream}.
#' To make other tasks dependents, use \link[connectapi.dag]{set_downstream}.
#'
#' Trigger rules allow you to define conditions of upstream tasks required in order to execute the task.
#' By default, `all_success` is used, requiring all upstream tasks to complete evaluation successfully in order to execute.
#' If a task does not meet its trigger rule requirement, the task will be skipped.
#'
#' All task trigger rules evaluate their immediate upstream tasks.
#' For tasks further upstream of the immediate upstream tasks and their statuses are not considered.
#'
#' The following trigger rules are available:
#' \itemize{
#'  \item{"all_success"} {All upstream tasks executed successfully.}
#'  \item{"all_failed"} {All upstream tasks failed during execution.}
#'  \item{"all_skipped"} {All upstream tasks skipped execution.}
#'  \item{"all_done"} {All upstream tasks completed evaluation. This includes skipped tasks.}
#'  \item{"one_success"} {At least one upstream task executed successfully.}
#'  \item{"one_failed"} {At least one upstream task failed during execution.}
#'  \item{"one_done"} {At least one upstream task completed evaluation. This includes skipped tasks.}
#'  \item{"none_failed"} {No upstream tasks failed. All other upstream tasks completed evaluation.}
#'  \item{"none_skipped"} {No upstream tasks skipped. All other upstream tasks completed evaluation.}
#'  \item{"always"} {Task will always run regardless of upstream task statuses.}
#' }
#'
#' @param guid A scalar character of the guid for the content deployed to Posit Connect
#' @param trigger_rule A scalar character that defines state of dependency (upstream) tasks must be in to execute. See details.
#' @param server The URL for accessing Posit Connect. Defaults to environment variable CONNECT_SERVER
#' @param api_key The API Key to authenticate to Posit Connect with. Defaults to environment variable CONNECT_API_KEY
#' @param prefix The prefix used to determine environment variables
#' @param simulated A simulated connect task does not validate with a connect server nor executes any connect jobs
#'
#' @return A ConnectTask R6 Environment
#'
#' @examples
#' task0 <- connect_task("task1", trigger_rule = "all_done", simulated = TRUE)
#' task0
#' @export

connect_task <- function(
    guid,
    trigger_rule = trigger_options,
    server = Sys.getenv(paste0(prefix, "_SERVER"), NA_character_),
    api_key = Sys.getenv(paste0(prefix, "_API_KEY"), NA_character_),
    prefix = "CONNECT",
    simulated = FALSE
) {
  # validate params
  stopifnot(
    is.character(guid),
    is.character(trigger_rule),
    is.logical(simulated)
  )
  trigger_rule <- match.arg(trigger_rule, trigger_options)
  # if simulating, do not create a connection to the server
  if (simulated) return(SimTask$new(guid, trigger_rule, fail_prob = 0))
  # connect to server
  connect_server <-
    connectapi::connect(server = server, api_key = api_key) |>
    suppressMessages() |>
    suppressWarnings()

  stopifnot(
    inherits(connect_server, "R6"),
    inherits(connect_server, "Connect")
  )
  # create task
  return(ConnectTask$new(guid, trigger_rule, connect_server))
}
