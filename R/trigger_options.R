# TODO: none_failed_min_one_success
# TODO: always - there is not really a need for this one with how it currently works
#    Because all upstream tasks are evaluated, and un-connected tasks are not allowed to exist
trigger_options <- c("all_success", "all_failed", "all_done", "all_skipped",
                     "one_success", "one_failed", "one_done",
                     "none_failed", "none_skipped")
