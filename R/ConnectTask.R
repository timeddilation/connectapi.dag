#' Class representing a Task published to Connect
#'
#' @name ConnectTask
#'
#' @section Usage:
#' \preformatted{
#' task <- ConnectTask$new(
#'   guid = "1f6c9a82-0177-47fa-a27c-be090b39dca7",
#'   trigger_rule = "always",
#'   connect_server = connectapi::connect()
#' )
#' task$execute()
#' }
#'
#' @section Details:
#'
#' This class validates a published content item on Posit Connect,
#' and allows the user to render the item programatically.
#' Additionally, other tasks may be set as dependencies or dependents
#' for the purposes of creating a DAG.
#'
#' @importFrom R6 R6Class
#' @importFrom connectapi content_item
#' @importFrom connectapi get_variant_default
#' @importFrom connectapi variant_render
#' @importFrom connectapi poll_task
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph layout_as_tree
#' @importFrom purrr pluck
#'
#' @family R6 classes
#' @export

ConnectTask <- R6::R6Class(
  "ConnectTask",
  public = list(
    #' @field guid The guid of the content item published to Connect
    guid = NA_character_,
    #' @field name The name of the ContentItem on Connect
    name = NA_character_,
    #' @field status The status of this task. Possible statuses: Pending, Succeeded, Failed, Skipped
    status = NA_character_,
    #' @field trigger_rule The rule for when to run this task. See \link[connectapi.dag]{connect_task} for details
    trigger_rule = NA_character_,
    #' @field upstream_tasks A list of ConnectTask R6 envs that are dependencies for this task
    upstream_tasks = list(),
    #' @field downstream_tasks A list of ConnectTask R6 envs that are dependents of this task
    downstream_tasks = list(),
    #' @field task_graph An igraph object of this task, and its immediate upstream and downstream tasks
    task_graph = NULL,
    #' @field connect_server An Connect R6 env generated from connectapi::connect()
    connect_server = NA,
    #' @field connect_content_item A ContentItem R6 env generated from connectapi::content_item()
    connect_content_item = NA,
    #' @field connect_variant A TaskVariant R6 env generated from connectapi::get_variant_default()
    connect_variant = NA,
    #' @field connect_rendering A VariantRender R6 env generated from connectapi::variant_render()
    connect_rendering = NA,
    #' @field app_mode The type of content being rendered on Posit Connect
    app_mode = NA_character_,

    #' @description Initializes a new ConnectTask
    #' @param guid A scalar character of the guid for the content deployed to Posit Connect
    #' @param trigger_rule A scalar character that defines state of dependency (upstream) tasks must be in to execute
    #' @param connect_server A \link[connectapi]{connect} environment with a connection to the Connect server
    initialize = function(guid, trigger_rule = "all_success", connect_server) {
      trigger_rule <- match.arg(trigger_rule, trigger_options)
      stopifnot(inherits(connect_server, "Connect"))

      self$guid <- guid
      self$status <- "Pending"
      self$trigger_rule <- trigger_rule
      self$connect_server <- connect_server

      tryCatch(
        self$connect_content_item <-
          connectapi::content_item(self$connect_server, self$guid),
        error = function(e) stop(paste(
          "Could not locate a content with guid", self$guid,
          "on server", self$connect_server$server,
          "\n", e
        ))
      )

      self$app_mode <- self$content$app_mode
      known_valid_app_modes <- c("jupyter-static", "rmd-static", "quarto-static")

      if (!self$app_mode %in% known_valid_app_modes) {
        warning(paste0(
          "This content's app_mode `", self$app_mode, "` ",
          "is not in the known list of apps that can be rendered. ",
          "You may not be able to run this task. ",
          "If this warning is being raised in a valid scenario, ",
          "please submit an issue on github. ",
          "https://github.com/timeddilation/connectapi.dag/issues", "\n"
        ))
      }

      self$name <-
        self$connect_content_item$content$title
    },

    #' @description Displays summary of environment in console.
    print = function() {
      cat("ConnectTask: \n")
      cat("  GUID:", self$guid, "\n")
      cat("  Name:", self$name, "\n")
      cat("  Trigger Rule:", self$trigger_rule, "\n")
      cat("  App Mode:", self$app_mode, "\n")
      cat("  Status:", self$status, "\n")
      cat("  Upstream Tasks:", length(self$upstream_tasks), "\n")
      cat("  Downstream Tasks:", length(self$downstream_tasks), "\n")
    },

    #' @description Resets the task to an initial state, read to be executed
    reset = function() {
      self$status <- "Pending"
      self$connect_variant <- NA
      self$connect_rendering <- NA

      invisible(self)
    },

    #' @description
        #' Makes a single-row data.frame of this Task's details.
        #' Used by the ConnectDAG to create a data.frame of all linked tasks.
    df_row = function() {
      data.frame(
        guid = self$guid,
        name = self$name,
        status = self$status,
        trigger_rule = self$trigger_rule
      )
    },

    #' @description Sets dependent tasks on this task
    #' @param ... Any number of ConnectTask R6 environments
    set_downstream = function(...) {
      for (task in list(...)) {
        self$link_task(task, "downstream_tasks")
      }

      invisible(self)
    },

    #' @description Sets dependency tasks on this task
    #' @param ... Any number of ConnectTask R6 environments
    set_upstream = function(...) {
      for (task in list(...)) {
        self$link_task(task, "upstream_tasks")
      }

      invisible(self)
    },

    #' @description
        #' Adds a single ConnectTask link either upstream or downstream.
        #' Should not be used directly.
        #' Called internally by the set_upstream and set_downstream methods.
    #' @param task A ConnectTask R6 environment
    #' @param link The type of link to create
    link_task = function(task, link = c("upstream_tasks", "downstream_tasks")) {
      stopifnot(inherits(task, "ConnectTask"))
      link <- match.arg(link)

      if (task$guid %in% self$linked_tasks_attrs("both", "guid")) {
        # Task already linked, cannot link task again
        return(invisible(NULL))
      }

      if (task$guid == self$guid) {
        warning("Cannot reference self as an upstream or downstream task.")
        return(invisible(NULL))
      }

      if (link == "upstream_tasks") {
        self$upstream_tasks <- append(self$upstream_tasks, task)
        task$set_downstream(self)
      } else {
        self$downstream_tasks <- append(self$downstream_tasks, task)
        task$set_upstream(self)
      }

      self$update_task_graph()
    },

    #' @description Re-generates this task's `task_graph` after a task is linked
    update_task_graph = function() {
      downstream_task_guids <-
        self$linked_tasks_attrs("downstream_tasks", "guid") |>
        unlist()

      downstream_tasks_df <-
        if(!is.null(downstream_task_guids)) {
          data.frame(
            source = self$guid,
            target = downstream_task_guids
          )
        } else NULL

      upstream_task_guids <-
        self$linked_tasks_attrs("upstream_tasks", "guid") |>
        unlist()

      upstream_tasks_df <-
        if(!is.null(upstream_task_guids)) {
          data.frame(
            source = upstream_task_guids,
            target = self$guid
          )
        } else NULL

      graph_df <- rbind(downstream_tasks_df, upstream_tasks_df)

      self$task_graph <- igraph::graph_from_data_frame(graph_df)
    },

    #' @description Visualizes this task's `task_graph`
    plot = function() {
      if (inherits(self$task_graph, "igraph"))
        plot(self$task_graph, layout = igraph::layout_as_tree(self$task_graph))
      else {
        message("No upstream or downstream tasks to plot")
        return(invisible(NULL))
      }
    },

    #' @description Returns a list of the linked tasks' attributes
    #' @param link They type of link to use to search for tasks
    #' @param task_attr The name of the attribute to return
    linked_tasks_attrs = function(
      link = c("both", "upstream_tasks", "downstream_tasks"),
      task_attr = c("guid", "name", "status")
    ) {
      link <- match.arg(link)
      task_attr <- match.arg(task_attr)

      if (link == "both") {
        task_attrs <- c(
          self$linked_tasks_attrs("upstream_tasks", task_attr),
          self$linked_tasks_attrs("downstream_tasks", task_attr)
        )
      } else {
        task_attrs <-
          lapply(get(link, envir = self), {\(task) purrr::pluck(task, task_attr)})
      }

      return(task_attrs)
    },

    #' @description Executes a ConnectTask on a remote Connect Server
    #' @param verbose Should the task print messages as it executes?
    execute = function(verbose = FALSE) {
      if (verbose) message(paste("Starting task", self$name))

      if (self$can_run()) {
        self$connect_variant <-
          connectapi::get_variant_default(self$connect_content_item) |>
          suppressWarnings()

        tryCatch(
          self$connect_rendering <- connectapi::variant_render(self$connect_variant),
          error = function(e) stop(paste(
            self$guid,
            "is not a content item that can render.",
            "Make sure you do not specify an API, Application, or other content item without a render call.",
            e
          ))
        )

        self$poll_task(verbose)
      } else {
        self$status <- "Skipped"
        if (verbose) message("Task Skipped")
      }

      invisible(self)
    },

    #' @description A wrapper around connectapi::poll_task for this task's execution
    #' @param verbose Should the task print messages as it executes?
    poll_task = function(verbose = FALSE) {
      res <- tryCatch(
        connectapi::poll_task(self$connect_rendering),
        error = function(e) print(e)
      )

      task_successful <- !any(class(res) == "error")

      if (task_successful) {
        self$status <- "Succeeded"
        if (verbose) message("Task Succeeded")
      } else {
        self$status <- "Failed"
        if (verbose) message("Task Failed")
      }

      invisible(self)
    },

    #' @description Returns a logical indicating if this task can run based on the `trigger_rule`
    can_run = function() {
      if (length(self$upstream_tasks) == 0) return(TRUE)
      terminal_statuses <- c("Succeeded", "Failed", "Skipped")

      upstream_statuses <-
        self$linked_tasks_attrs("upstream_tasks", "status") |>
        unlist()

      switch (self$trigger_rule,
        all_success = all(upstream_statuses == "Succeeded"),
        all_failed = all(upstream_statuses == "Failed"),
        all_done = all(upstream_statuses %in% terminal_statuses),
        all_skipped = all(upstream_statuses == "Skipped"),

        one_success = any(upstream_statuses == "Succeeded"),
        one_failed = any(upstream_statuses == "Failed"),
        one_done = any(upstream_statuses %in% terminal_statuses),

        none_failed = all(upstream_statuses %in% c("Succeeded", "Skipped")),
        none_skipped = all(upstream_statuses %in% c("Succeeded", "Failed")),
        always = TRUE
      )
    }
  )
)
