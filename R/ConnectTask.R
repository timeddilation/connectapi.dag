ConnectTask <- R6::R6Class(
  "ConnectTask",
  public = list(
    task_guid = NA_character_,
    connect_server = NA,
    task_content_item = NA, # connectapi::content_item()
    task_variant = NA, # connectapi::get_variant_default()
    task_rendering = NA, # connectapi::variant_render()
    task_name = NA_character_,
    # possible statuses: Pending, Succeeded, Failed, Skipped
    task_status = NA_character_,
    trigger_rule = NA_character_,
    upstream_tasks = list(),
    downstream_tasks = list(),
    task_graph = NULL,


    initialize = function(guid, trigger_rule = "all_success", connect_server) {
      trigger_rule <- match.arg(trigger_rule, trigger_options)

      self$task_guid <- guid
      self$task_status <- "Pending"
      self$trigger_rule <- trigger_rule
      self$connect_server <- connect_server

      tryCatch(
        self$task_content_item <-
          connectapi::content_item(self$connect_server, self$task_guid),
        error = function(e) stop(paste(
          "Could not locate a content with guid", self$task_guid,
          "on server", self$connect_server$server,
          "\n", e
        ))
      )

      self$task_name <-
        self$task_content_item$content$title

      # TODO: See if there is a way to validate the content item here
      # right now, I do not know if the content can be rendered until you attempt to call render
      # It is possible to specify an API, Shiny App, etc as a task in the DAG
      # This should fail validation. Only items that render should be allowed
    },


    reset = function() {
      self$task_status <- "Pending"
      self$task_content_item <- NA
      self$task_variant <- NA
      self$task_rendering <- NA

      invisible(self)
    },


    df_row = function() {
      data.frame(
        task_name = self$task_name,
        task_guid = self$task_guid,
        task_status = self$task_status,
        trigger_rule = self$trigger_rule
      )
    },


    set_downstream = function(...) {
      for (task in list(...)) {
        private$link_task(task, "downstream_tasks")
      }

      invisible(self)
    },


    set_upstream = function(...) {
      for (task in list(...)) {
        private$link_task(task, "upstream_tasks")
      }

      invisible(self)
    },


    plot = function() {
      if (inherits(self$task_graph, "igraph"))
        plot(self$task_graph, layout = layout_as_tree(self$task_graph))
      else {
        message("No upstream or downstream tasks to plot")
        return(invisible(NULL))
      }
    },


    linked_task_names = function(link = c("both", "upstream_tasks", "downstream_tasks")) {
      link <- match.arg(link)

      if (link == "both") {
        task_names <- c(
          self$linked_task_names("upstream_tasks"),
          self$linked_task_names("downstream_tasks")
        )
      } else {
        task_names <-
          lapply(get(link, envir = self), {\(task) task$task_name}) |>
          unlist()
      }

      return(task_names)
    },


    execute_task = function(verbose = FALSE) {
      if (verbose) message(paste("Starting task", self$task_name))

      if (self$can_run()) {
        self$task_variant <-
          connectapi::get_variant_default(self$task_content_item) |>
          suppressWarnings()

        tryCatch(
          self$task_rendering <- connectapi::variant_render(self$task_variant),
          error = function(e) stop(paste(
            self$task_guid,
            "is not a content item that can render.",
            "Make sure you do not specify an API, Application, or other content item without a render call.",
            e
          ))
        )

        self$poll_task(verbose)
      } else {
        self$task_status <- "Skipped"
        if (verbose) message("Task Skipped")
      }

      invisible(self)
    },


    poll_task = function(verbose = FALSE) {
      res <- tryCatch(
        connectapi::poll_task(self$task_rendering),
        error = function(e) print(e)
      )

      task_successful <- !any(class(res) == "error")

      if (task_successful) {
        self$task_status <- "Succeeded"
        if (verbose) message("Task Succeeded")
      } else {
        self$task_status <- "Failed"
        if (verbose) message("Task Failed")
      }

      invisible(self)
    },


    upstream_task_statuses = function() {
      lapply(self$upstream_tasks, {\(task) task$task_status}) |>
        unlist()
    },


    can_run = function() {
      if (length(self$upstream_tasks) == 0) return(TRUE)
      terminal_statuses <- c("Succeeded", "Failed", "Skipped")

      switch (self$trigger_rule,
        all_success = all(self$upstream_task_statuses() == "Succeeded"),
        all_failed = all(self$upstream_task_statuses() == "Failed"),
        all_done = all(self$upstream_task_statuses() %in% terminal_statuses),
        all_skipped = all(self$upstream_task_statuses() == "Skipped"),

        one_success = any(self$upstream_task_statuses() == "Succeeded"),
        one_failed = any(self$upstream_task_statuses() == "Failed"),
        one_done = any(self$upstream_task_statuses() %in% terminal_statuses),

        none_failed = all(self$upstream_task_statuses() %in% c("Succeeded", "Skipped")),
        none_skipped = all(self$upstream_task_statuses() %in% c("Succeeded", "Failed")),
        always = TRUE
      )
    }
  ),


  private = list(
    link_task = function(task, link = c("upstream_tasks", "downstream_tasks")) {
      stopifnot(inherits(task, "ConnectTask"))
      link <- match.arg(link)

      if (task$task_name %in% self$linked_task_names()) {
        # Task already linked, cannot link task again
        return(invisible(NULL))
      }

      if (link == "upstream_tasks") {
        self$upstream_tasks <- append(self$upstream_tasks, task)
        task$set_downstream(self)
      } else {
        self$downstream_tasks <- append(self$downstream_tasks, task)
        task$set_upstream(self)
      }

      private$update_task_graph()
    },


    update_task_graph = function() {
      downstream_task_names <- self$linked_task_names("downstream_tasks")
      upstream_task_names <- self$linked_task_names("upstream_tasks")

      downstream_tasks_df <-
        if(!is.null(downstream_task_names)) {
          data.frame(
            source = self$task_name,
            target = downstream_task_names
          )
        } else NULL

      upstream_tasks_df <-
        if(!is.null(upstream_task_names)) {
          data.frame(
            source = upstream_task_names,
            target = self$task_name
          )
        } else NULL

      graph_df <- rbind(downstream_tasks_df, upstream_tasks_df)

      self$task_graph <- igraph::graph_from_data_frame(graph_df)
    }
  )
)
