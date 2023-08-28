ConnectDAG <- R6::R6Class(
  "ConnectDAG",


  public = list(
    name = NA_character_,
    dag_tasks = list(),
    dag_graph = NA,
    is_valid = FALSE,
    is_complete = FALSE,


    initialize = function(name, ...) {
      stopifnot(is.character(name))
      self$name <- name

      for (task in list(...)) {
        self$add_task(task)
      }
    },


    add_task = function(task) {
      stopifnot(is(task, "ConnectTask"))
      self$dag_tasks <- append(self$dag_tasks, task)

      invisible(self)
    },


    plot = function() {
      private$validate_dag()
      if (is(self$dag_graph, "igraph"))
        plotly_dag_graph(self)
      else
        return(invisible(NULL))
    },


    tasks_as_df = function(revalidate_dag = TRUE) {
      if (revalidate_dag) private$validate_dag()

      tasks_df <- data.frame(
        task_name = character(),
        task_id = character(),
        task_status = character(),
        trigger_rule = character(),
        exec_order = integer()
      )

      if (length(self$dag_tasks) > 0) {
        tasks_df <-
          do.call(rbind, lapply(self$dag_tasks, {\(task) task$df_row()}))

        if (self$is_valid) {
          ordered_tasks <- data.frame(task_name = private$task_exec_order())
          ordered_tasks$exec_order <- ordered_tasks |> row.names() |> as.integer()
          tasks_df <- merge(tasks_df, ordered_tasks, by = "task_name")
          tasks_df <- tasks_df[order(tasks_df$exec_order),]
        } else {
          tasks_df$exec_order <- NA_integer_
        }
      }

      return(tasks_df)
    },


    run_dag = function(verbose = FALSE) {
      # check if this instance already attempted to execute
      if (self$is_complete) {
        stop("DAG has already been run, cannot run again.")
      }
      # check if is a valid dag
      private$validate_dag()
      if (!self$is_valid) {
        stop("Not a valid DAG. Cannot execute tasks.")
      }

      for (task in private$task_exec_order()) {
        private$run_dag_task(task, verbose)
      }

      self$is_complete <- TRUE
      if (verbose) print(self$tasks_as_df())

      invisible(self)
    },


    evaluate_validity = function() {
      private$validate_dag()
      return(self$is_valid)
    }
  ),


  private = list(
    update_dag_graph = function() {
      # logical vector of actual graphs, not NAs
      task_graphs <- lapply(self$dag_tasks, {\(task) task$task_graph})

      valid_task_graphs <-
        lapply(task_graphs, is, "igraph") |>
        unlist()

      # first check if there are any linked tasks
      # if there are none, then we cannot generate a graph
      tasks_linked <-
        any(valid_task_graphs)

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
      task_list_names <-
        lapply(self$dag_tasks, {\(task) task$task_name}) |>
        unlist()
      task_node_names <- names(V(self$dag_graph))

      # check if all nodes in the graph have a task in the dag task list
      missing_tasks <- any(!task_node_names %in% task_list_names)
      if (missing_tasks) {
        warning(paste(
          "A task is linked as either upstream or downstream of a provided task,",
          "but was not added explictely to the DAG: ",
          paste0(task_node_names[!task_node_names %in% task_list_names], collapse = ", ")
        ))
      }

      # check if all tasks in the dag task list appear as nodes in the graph
      unlinked_tasks <- any(!task_list_names %in% task_node_names)
      if (unlinked_tasks) {
        warning(paste(
          "A task is added to the DAG,",
          "but not set as upstream or downstream of other tasks: ",
          paste0(task_list_names[!task_list_names %in% task_node_names], collapse = ", ")
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

      return(invisible(NULL))
    },

    # returns the order in which tasks should execute
    task_exec_order = function() {
      self$dag_graph |>
        igraph::topo_sort() |>
        names()
    },


    # Runs a DAG task
    run_dag_task = function(task_name, verbose = FALSE) {
      dag_task <- which(lapply(self$dag_tasks, {\(task) task$task_name}) == task_name)
      self$dag_tasks[[dag_task]]$execute_task(verbose)
    }
  )
)
