#' Visualize a ConnectDAG graph with plotly
#'
#' Create an interactive plotly visual of the DAG.
#' This function is not exported.
#' It is called internally by the ConnectDAG class using the `plot` method.
#'
#' @param connect_dag A ConnectDAG R6 environment
#'
#' @return A plotly graph
#'
#' @examples
#' task0 <- connect_task("task0", simulated = TRUE)
#' task1 <- connect_task("task1", simulated = TRUE)
#' task0 |> set_downstream(task1)
#'
#' my_dag <- connect_dag(task0, task1)
#' plot(my_dag)
#'
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom plotly add_text
#' @importFrom plotly add_annotations
#' @importFrom plotly layout
#' @importFrom igraph as_data_frame
#' @importFrom igraph layout_as_tree
#' @importFrom stats setNames

dag_plotly <- function(connect_dag) {
  stopifnot(inherits(connect_dag, "ConnectDAG"))

  suppressMessages(dag_validate(connect_dag))

  dag_graph <- connect_dag$dag_graph
  tasks_df <- connect_dag$tasks_as_df(revalidate_dag = FALSE)

  plot_nodes_df <-
    dag_graph |>
    igraph::as_data_frame(what = "vertices") |>
    cbind(igraph::layout_as_tree(dag_graph)) |>
    stats::setNames(c("task_name", "posx", "posy")) |>
    merge(tasks_df, by = "task_name")

  plot_nodes_df$plotly_text <- paste0(
    "Task: ", plot_nodes_df$task_name, "<br>",
    "Status: ", plot_nodes_df$task_status, "<br>",
    "Trigger Rule: ", plot_nodes_df$trigger_rule
  )

  plot_edges_df <-
    dag_graph |>
    igraph::as_data_frame(what = "edges") |>
    merge(plot_nodes_df[, c("task_name", "posx", "posy")], by.x = "from", by.y = "task_name") |>
    merge(plot_nodes_df[, c("task_name", "posx", "posy")], by.x = "to", by.y = "task_name") |>
    stats::setNames(c("to", "from", "from_x", "from_y", "to_x", "to_y"))

  pp <- plotly::plot_ly(plot_nodes_df, type = 'scatter', mode = 'markers') |>
    plotly::add_trace(
      data = plot_nodes_df[plot_nodes_df$task_status == "Pending",],
      x = ~posx,
      y = ~posy,
      color = ~task_status,
      text = ~plotly_text,
      hoverinfo = "text",
      marker = list(
        size = 48,
        color = "lightgray"
      )
    ) |>
    plotly::add_trace(
      data = plot_nodes_df[plot_nodes_df$task_status == "Succeeded",],
      x = ~posx,
      y = ~posy,
      color = ~task_status,
      text = ~plotly_text,
      hoverinfo = "text",
      marker = list(
        size = 48,
        color = "#87D873"
      )
    ) |>
    plotly::add_trace(
      data = plot_nodes_df[plot_nodes_df$task_status == "Failed",],
      x = ~posx,
      y = ~posy,
      color = ~task_status,
      text = ~plotly_text,
      hoverinfo = "text",
      marker = list(
        size = 48,
        color = "#C94948"
      )
    ) |>
    plotly::add_trace(
      data = plot_nodes_df[plot_nodes_df$task_status == "Skipped",],
      x = ~posx,
      y = ~posy,
      color = ~task_status,
      text = ~plotly_text,
      hoverinfo = "text",
      marker = list(
        size = 48,
        color = "#B5B091"
      )
    ) |>
    plotly::add_text(~posx, ~posy, text = ~task_name, showlegend = FALSE, data = plot_nodes_df) |>
    plotly::add_annotations(
      data = plot_edges_df,
      x = ~to_x,
      y = ~to_y,
      xref = "x", yref = "y",
      axref = "x", ayref = "y",
      ax = ~from_x,
      ay = ~from_y,
      text = "",
      showarrow = TRUE,
      standoff = 32,
      startstandoff = 32
    ) |>
    plotly::layout(
      xaxis = list(showgrid = F, zeroline = F, showticklabels = F, title = ""),
      yaxis = list(showgrid = F, zeroline = F, showticklabels = F, title = ""),
      legend = list(text = "Task Status")
    )

  suppressWarnings(print(pp))
}
