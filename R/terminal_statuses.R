# Statuses a task can no longer transition out of.
# Used by ConnectTask$can_run() to evaluate trigger rules and by the
# ConnectDAG scheduler to decide when a task's downstreams may be evaluated.
terminal_statuses <- c("Succeeded", "Failed", "Skipped")
