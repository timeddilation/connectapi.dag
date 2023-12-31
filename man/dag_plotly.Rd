% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dag_plotly.R
\name{dag_plotly}
\alias{dag_plotly}
\title{Visualize a ConnectDAG graph with plotly}
\usage{
dag_plotly(connect_dag)
}
\arguments{
\item{connect_dag}{A ConnectDAG R6 environment}
}
\value{
A plotly graph
}
\description{
Create an interactive plotly visual of the DAG.
This function is not exported.
It is called internally by the ConnectDAG class using the `plot` method.
}
\examples{
task0 <- connect_task("task0", simulated = TRUE)
task1 <- connect_task("task1", simulated = TRUE)
task0 |> set_downstream(task1)

my_dag <- connect_dag(task0, task1)
plot(my_dag)

}
