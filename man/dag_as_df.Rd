% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dag_as_df.R
\name{dag_as_df}
\alias{dag_as_df}
\title{Return DAG tasks as a data.frame}
\usage{
dag_as_df(env)
}
\arguments{
\item{env}{A ConnectDAG R6 environment created by \link[connectapi.dag]{connect_dag}}
}
\value{
A data.frame
}
\description{
Creates a data.frame of the tasks added to the DAG.
Includes the execution order of the tasks if the DAG is valid.
}
\examples{
task0 <- connect_task("task0", simulated = TRUE)
task1 <- connect_task("task1", simulated = TRUE)
task0 |> set_downstream(task1)

my_dag <- connect_dag(name = "my_dag", task0, task1)
dag_as_df(my_dag)
}
