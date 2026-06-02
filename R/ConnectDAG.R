#' Class representing a DAG of Connect Tasks
#'
#' @name ConnectDAG
#'
#' @section Usage:
#' \preformatted{
#' dag <- ConnectDAG$new(name = "dag")
#' }
#'
#' @section Details:
#'
#' This class requires ConnectTasks are added to it,
#' which are themselves linked in a dependency chain.
#' As tasks are added, each tasks' graph is unioned in the DAG's graph.
#' It can then be validated to ensure it is in fact a Directed Acyclic Graph.
#' Once validated, the DAG can orchestrate tasks in Posit Connect.
#'
#' @importFrom R6 R6Class
#' @importFrom uuid UUIDgenerate
#' @importFrom purrr pluck
#' @importFrom igraph V
#' @importFrom igraph union
#' @importFrom igraph topo_sort
#' @importFrom igraph is.connected
#' @importFrom igraph is.dag
#' @importFrom igraph as_data_frame
#' @importFrom igraph identical_graphs
#'
#' @family R6 classes
#' @export

ConnectDAG <- R6::R6Class(
  "ConnectDAG",
  public = list(
    #' @field name The user-defined name of this DAG. Useful for organizing multiple DAGs.
    name = NA_character_,
    #' @field pin_name The name of pin on Connect where this DAG is saved.
    pin_name = NA_character_,
    #' @field tasks A list of ConnectTasks this DAG is orchestrating.
    tasks = list(),
    #' @field dag_graph An igraph object of all linked tasks in this DAG
    dag_graph = NA,
    #' @field is_valid Indicates if the tasks' dependency chain forms a proper DAG. DO NOT MODIFY DIRECTLY!
    is_valid = FALSE,
    #' @field run_id A UUID created for an instance of a DAG run
    run_id = NA,
    #' @field run_start The time a DAG run started
    run_start = NA,
    #' @field run_end The time a DAG run ended
    run_end = NA,
    #' @field is_complete Indicates if all tasks in this DAG have been evaluated for execution.
    is_complete = FALSE,
    #' @field max_concurrent The maximum number of tasks allowed to run simultaneously. Defaults to 1 (sequential).
    max_concurrent = 1L,
    #' @field poll_interval The seconds the scheduler sleeps between poll cycles while tasks are running.
    poll_interval = 1,
    #' @field task_timeout The maximum seconds a single task may run before being failed. NA disables the timeout.
    task_timeout = NA_real_,
    #' @field dag_timeout The maximum seconds the entire DAG run may take before remaining tasks are failed. NA disables the timeout.
    dag_timeout = NA_real_,

    #' @description Initializes a new ConnectDAG
    #' @param name A personalized name for the DAG
    #' @param ... Connect Tasks to add to the graph
    #' @param max_concurrent The maximum number of tasks allowed to run simultaneously. Defaults to 1 (sequential).
    initialize = function(name = "new_dag", ..., max_concurrent = 1L) {
      self$set_name(name)
      self$set_max_concurrent(max_concurrent)
      dag_add_tasks(self, ...)
    },

    #' @description Displays summary of environment in console.
    print = function() {
      print_df <-
        self$tasks_as_df() |>
        print(row.names = FALSE) |>
        capture.output() |>
        {\(df_text) gsub("^", "    ", df_text)}() |>
        paste0(collapse = "\n")


      cat("ConnectDAG: \n")
      cat("  Name:", self$name, "\n")
      cat("  Is Valid:", self$is_valid, "\n")
      cat("  Pin Name:", self$pin_name, "\n")
      cat("  Tasks:", length(self$tasks), "\n")
      cat(print_df)

      invisible(self)
    },

    #' @description Adds a ConnectTask to this DAG
    #' @param task a ConnectTask R6 environment
    add_task = function(task) {
      stopifnot(inherits(task, "ConnectTask"))

      if (!task$guid %in% self$task_attrs("guid")) {
        self$tasks <- append(self$tasks, task)
      }

      invisible(self)
    },

    #' @description Adds any number of ConnectTasks to this DAG
    #' @param ... Any number of ConnectTask R6 environments
    add_tasks = function(...) {
      for (task in list(...)) {
        self$add_task(task)
      }

      invisible(self)
    },

    #' @description Removes a ConnectTask from this DAG
    #' @param task a ConnectTask R6 environment
    remove_task = function(task) {
      stopifnot(inherits(task, "ConnectTask"))

      task_id <- task$guid

      task_idx <-
        self$task_attrs("guid") |>
        {\(task_ids) task_ids == task_id}() |>
        which()

      if (length(task_idx) == 1) {
        self$tasks <- self$tasks[-task_idx]
      } else {
        warning(paste("Task", task_id, "is not part of DAG."))
      }

      invisible(self)
    },

    #' @description Sets the name of this DAG, if needed to change after initializing
    #' @param name A scalar character of the name
    set_name = function(name) {
      stopifnot(is.character(name), length(name) == 1)
      self$name <- name
      self$set_connect_pin_name()

      invisible(self)
    },

    #' @description Sets the name used when using \link[connectapi.dag]{dag_write_connect_pin}
    #' @param pin_name A scalar character of the name desired or required
    set_connect_pin_name = function(pin_name = self$name) {
      stopifnot(is.character(pin_name), length(pin_name) == 1)

      self$pin_name <- gsub("[^-_A-Za-z0-9]", "-", pin_name)

      if (self$pin_name != pin_name) {
        warning(paste(
          "Pins only allows alphanumeric characters, dashes, and underscores in application names.",
          self$pin_name, "will be used instead when writing to a pin board.",
          "Use dag_set_pin_name() to overwrite this name."
        ))
      }

      if (nchar(self$pin_name) < 3 | nchar(self$pin_name) > 64) {
        warning(paste(
          "Pins require applications names are between 3 and 64 characters.",
          "Attempting to write this to a pin board with the current name will results in an error.",
          "Use dag_set_pin_name() to overwrite this name."
        ))
      }

      invisible(self)
    },

    #' @description Sets the maximum number of tasks allowed to run simultaneously when the DAG executes
    #' @param n A positive integer. 1 runs the DAG sequentially; higher values allow concurrent task execution.
    set_max_concurrent = function(n) {
      stopifnot(is.numeric(n), length(n) == 1, n >= 1)
      self$max_concurrent <- as.integer(n)

      invisible(self)
    },

    #' @description Sets how long a single task may run before the scheduler fails it
    #' @param seconds A positive number of seconds, or NA to disable the per-task timeout
    set_task_timeout = function(seconds) {
      stopifnot(length(seconds) == 1, is.na(seconds) || (is.numeric(seconds) && seconds > 0))
      self$task_timeout <- as.numeric(seconds)

      invisible(self)
    },

    #' @description Sets the overall wall-clock limit for an entire DAG run
    #' @param seconds A positive number of seconds, or NA to disable the global timeout
    set_dag_timeout = function(seconds) {
      stopifnot(length(seconds) == 1, is.na(seconds) || (is.numeric(seconds) && seconds > 0))
      self$dag_timeout <- as.numeric(seconds)

      invisible(self)
    },

    #' @description Sets the seconds the scheduler sleeps between poll cycles while tasks are running
    #' @param seconds A positive number of seconds
    set_poll_interval = function(seconds) {
      stopifnot(is.numeric(seconds), length(seconds) == 1, seconds > 0)
      self$poll_interval <- as.numeric(seconds)

      invisible(self)
    },

    #' @description Prints a plotly graph of the DAG's graph
    #' @param plotly A logical, indicate to use a plotly visual or a static visual
    plot = function(plotly = TRUE) {
      private$validate_dag()
      if (is(self$dag_graph, "igraph"))
        if (plotly)
          dag_plotly(self)
        else
          plot(self$dag_graph, layout = igraph::layout_as_tree(self$dag_graph))

      else
        return(invisible(NULL))
    },

    #' @description Returns a character vector of DAG tasks' specified attribute
    #' @param task_attr The name of the character attribute to return
    task_attrs = function(task_attr = c("guid", "name", "status")) {
      task_attr <- match.arg(task_attr)
      vapply(self$tasks, {\(task) purrr::pluck(task, task_attr)}, character(1))
    },

    #' @description Returns a data.frame of all tasks added to this DAG
    #' @param revalidate_dag Should the DAG be validated before returning the data.frame?
    tasks_as_df = function(revalidate_dag = TRUE) {
      if (revalidate_dag) private$validate_dag()

      tasks_df <- data.frame(
        guid = character(),
        name = character(),
        status = character(),
        trigger_rule = character(),
        exec_order = integer()
      )

      if (length(self$tasks) > 0) {
        tasks_df <-
          do.call(rbind, lapply(self$tasks, {\(task) task$df_row()}))

        if (self$is_valid) {
          ordered_tasks <- data.frame(guid = private$task_exec_order())
          ordered_tasks$exec_order <- ordered_tasks |> row.names() |> as.integer()
          tasks_df <- merge(tasks_df, ordered_tasks, by = "guid")
          tasks_df <- tasks_df[order(tasks_df$exec_order),]
        } else {
          tasks_df$exec_order <- NA_integer_
        }
      }

      return(tasks_df)
    },

    #' @description Executes all tasks added to this DAG, honoring dependencies and trigger rules
    #' @param verbose Should it print messages as it executes tasks?
    #' @param max_concurrent An optional override for the DAG's `max_concurrent` field for this run only
    execute = function(verbose = FALSE, max_concurrent = NULL) {
      # Preflight checks
      ## check if this instance already attempted to execute
      if (self$is_complete) {
        stop("DAG has already been run, cannot run again.")
      }
      ## check if is a valid dag
      private$validate_dag()
      if (!self$is_valid) {
        stop("Not a valid DAG. Cannot execute tasks.")
      }

      cap <- if (is.null(max_concurrent)) self$max_concurrent else as.integer(max_concurrent)
      stopifnot(is.numeric(cap), length(cap) == 1, cap >= 1)

      # Execution Logic
      self$run_id <- uuid::UUIDgenerate()
      self$run_start <- Sys.time()

      private$run_scheduler(cap, verbose)

      self$run_end <- Sys.time()
      self$is_complete <- TRUE
      if (verbose) print(self$tasks_as_df())

      invisible(self)
    },

    #' @description Resets the DAG to an initial state, allowing it to run again
    reset = function() {
      for (task in self$tasks) {
        task$reset()
      }

      self$is_complete <- FALSE
      self$run_start <- NA
      self$run_end <- NA
      self$run_id <- NA_character_

      invisible(self)
    },

    #' @description Determines if all added tasks form a valid DAG, setting the `is_valid` field
    #' @param verbose Should it print a message to the console of the result?
    evaluate_validity = function(verbose = TRUE) {
      stopifnot(is.logical(verbose))
      private$validate_dag()

      if (self$is_valid & verbose) {
        message("DAG is valid!")
      }

      if (!self$is_valid & verbose) {
        message("Oh no! DAG is invalid.")
      }

      invisible(self)
    }
  ),


  private = list(
    update_dag_graph = function() {
      # logical vector of actual graphs, not NAs
      task_graphs <- lapply(self$tasks, {\(task) task$task_graph})
      valid_task_graphs <- vapply(task_graphs, is, logical(1), "igraph")

      # first check if there are any linked tasks
      # if there are none, then we cannot generate a graph
      tasks_linked <- any(valid_task_graphs)

      if (!tasks_linked) {
        message("No tasks are linked in the graph.")
        return(invisible(NULL))
      }

      # stage a new graph
      new_graph <-
        do.call(igraph::union, task_graphs[valid_task_graphs])

      # if a graph already exists, are there any changes to apply?
      # if not, don't do anything
      # TODO: not even sure why this is necessary, maybe it's not?
      if (is(self$dag_graph, "igraph")) {
        if (igraph::identical_graphs(new_graph, self$dag_graph, attrs = FALSE))
          return(invisible(NULL))
      }

      self$dag_graph <- new_graph
      return(invisible(NULL))
    },


    validate_dag = function() {
      private$update_dag_graph()
      ### Validation Step 1
      # no graph generated means it's invalid
      if (!is(self$dag_graph, "igraph")) {
        self$is_valid <- FALSE
        return(invisible(NULL))
      }

      ### Validation Step 2-3
      task_list_guids <- self$task_attrs("guid")
      task_node_guids <- names(igraph::V(self$dag_graph))

      # check if all nodes in the graph have a task in the dag task list
      missing_tasks <- any(!task_node_guids %in% task_list_guids)
      if (missing_tasks) {
        warning(paste(
          "A task is linked as either upstream or downstream of a provided task,",
          "but was not added explictely to the DAG: ",
          paste0(task_node_guids[!task_node_guids %in% task_list_guids], collapse = ", ")
        ))
      }

      # check if all tasks in the dag task list appear as nodes in the graph
      unlinked_tasks <- any(!task_list_guids %in% task_node_guids)
      if (unlinked_tasks) {
        warning(paste(
          "A task is added to the DAG,",
          "but not set as upstream or downstream of other tasks: ",
          paste0(task_list_guids[!task_list_guids %in% task_node_guids], collapse = ", ")
        ))
      }

      ### Validation Step 4
      # a valid DAG is both a DAG and connected
      is_dag <- igraph::is.dag(self$dag_graph)
      is_connected <- igraph::is.connected(self$dag_graph)

      if (!is_dag) {
        warning("Circular dependency detected. Not a valid DAG.")
      }
      if (!is_connected) {
        warning("DAG is not connected. All tasks must be connected to a single network.")
      }

      ### Finalize Validation
      # set valid state, inform through warning consequences if invalid
      self$is_valid <- all(c(is_dag, is_connected, !missing_tasks, !unlinked_tasks))

      if (!self$is_valid) {
        warning("DAG is not valid. It cannot be executed. See warning messages.")
      }

      invisible(self)
    },

    # returns the order in which tasks should execute
    task_exec_order = function() {
      self$dag_graph |>
        igraph::topo_sort() |>
        names()
    },


    # Cooperative scheduler driving concurrent task execution.
    #
    # The expensive work (rendering content) happens on the Connect server, so a
    # single R session can orchestrate many simultaneous renders by dispatching
    # them and polling each without blocking. Each cycle:
    #   1. poll every Running task once (non-blocking), failing any that exceed
    #      the per-task timeout;
    #   2. find Pending tasks whose immediate upstreams are all terminal, in topo
    #      order, and either Skip them (trigger rule unmet) or dispatch them up to
    #      the concurrency cap;
    #   3. sleep one poll_interval while tasks are genuinely running, otherwise
    #      loop immediately so chains of skips resolve without delay.
    # The loop ends when no task is Pending or Running. Termination is guaranteed
    # because the graph is a validated DAG and both timeouts force terminality.
    run_scheduler = function(cap, verbose = FALSE) {
      ordered_guids <- private$task_exec_order()

      repeat {
        status_of <- function(task) task$status
        statuses <- vapply(self$tasks, status_of, character(1))

        running <- self$tasks[statuses == "Running"]
        pending <- self$tasks[statuses == "Pending"]

        if (length(running) == 0 && length(pending) == 0) break

        # Global timeout guard
        if (!is.na(self$dag_timeout) &&
            as.numeric(difftime(Sys.time(), self$run_start, units = "secs")) > self$dag_timeout) {
          for (task in running) task$status <- "Failed"
          if (verbose) message("DAG timeout exceeded. Remaining running tasks marked Failed.")
          break
        }

        # 1. Poll every running task once, enforcing the per-task timeout
        for (task in running) {
          task$poll_once(wait = 0, verbose = verbose)
          if (task$status == "Running" && !is.na(self$task_timeout) &&
              as.numeric(difftime(Sys.time(), task$dispatch_time, units = "secs")) > self$task_timeout) {
            task$status <- "Failed"
            if (verbose) message(paste0("[", task$name, "] Task timeout exceeded. Marked Failed."))
          }
        }

        # 2. Find Pending tasks whose immediate upstreams are all terminal
        eligible <- Filter(
          function(task) {
            ups <- unlist(task$linked_tasks_attrs("upstream_tasks", "status"))
            length(ups) == 0 || all(ups %in% terminal_statuses)
          },
          self$tasks[vapply(self$tasks, status_of, character(1)) == "Pending"]
        )

        if (length(eligible) > 0) {
          eligible_guids <- vapply(eligible, {\(task) task$guid}, character(1))
          eligible <- eligible[order(match(eligible_guids, ordered_guids))]
        }

        # 3. Evaluate trigger rules; skip or dispatch up to the concurrency cap
        for (task in eligible) {
          if (!task$can_run()) {
            task$status <- "Skipped"
            if (verbose) message(paste0("[", task$name, "] Task Skipped"))
            next
          }

          n_running <- sum(vapply(self$tasks, status_of, character(1)) == "Running")
          if (n_running >= cap) break

          task$dispatch(verbose)
        }

        # 4. Sleep only while something is genuinely rendering on the server
        any_running <- any(vapply(self$tasks, status_of, character(1)) == "Running")
        if (any_running) Sys.sleep(self$poll_interval)
      }

      invisible(self)
    }
  )
)
