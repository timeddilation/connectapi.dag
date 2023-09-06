
# connectapi.dag

This package allows for the creation of DAGs, inspired by Apache
Airflow, for content items published to [Posit
Connect](https://docs.posit.co/rsc/). It leverages the existing
[connectapi](https://github.com/rstudio/connectapi) package to make API
calls to your Posit Server, and adds syntax similar to Airflow’s python
package for defining your DAGs.

A DAG is a collection of linked tasks, organized with dependencies and
relationships that describe the rules for how and when they should run.

The main benefit of using this package is to not have to manage so many
rendering schedules in Posit Connect. You may often find yourself having
jobs dependent on another job, and creating one or two hour offsets for
their schedules to ensure the completion of one before the other. This
can lead to many issues in data pipelines, especially when a dependency
task fails you may not want dependent tasks to run at all.

## Disclaimer

Because this package relies entirely the `connectapi` to interact with
your Posit Connect Server, all of the same disclaimers that package
states are applicable here as well. The `connectapi` package is itself
experimental.

## Installation

TODO: No instruction yet.

## Getting Started

Before starting here, it is highly recommended you familiarize yourself
with [connectapi](https://github.com/rstudio/connectapi) briefly. The
most important aspect to understand is how the client is created. By
default, this package looks for environment variables containing the
secrets required to connect to your Posit Connect server.

    CONNECT_SERVER = https://connect.example.com
    CONNECT_API_KEY = your-api-key

Once you have the basics figured out with `connectapi`, you can start
defining **tasks** and **DAGs**.

## Tasks

The most fundamental part of creating a DAG is to define the tasks you
want included in the DAG. The tasks are created by specifying the GUID
of the content item published to your Posit Connect server, with an
option for the *trigger_rule* to follow. By default, the *trigger_rule*
is **all_success**, meaning all predecessor tasks completed
successfully. If a task has no predecessors, it will always run. The
`connect_task` function returns an R6 environment that manages the state
of the task.

``` r
library(connectapi.dag)

task0 <- connect_task("4af62803-c9aa-45f4-8336-0ea7bbdd9334")
task1 <- connect_task("711b8c7a-97ef-4343-9e55-0d4d426ebf59")
task2 <- connect_task("eda729a2-e3f1-4063-b606-e52d01e9aa23")
```

### Task Dependencies

Two functions are provided to create dependencies: `set_upstream()` and
`set_downstream()`. For each of these functions, they may accept any
number of tasks to set as dependencies.

For example, the following will make *task1* and *task2* dependent on
*task0* completing.

``` r
task0 |>
  set_downstream(task1, task2)
```

## DAGs

Once all tasks are defined, you may add them to a DAG R6 environment.
This object will validate the tasks are in fact linked as a DAG. You may
create task dependencies before or after creating the DAG.

``` r
my_dag <- connect_dag(task1, task2, task3)
```

With the dag object created, you can check the validity of the DAG. If
any issues are found, a message will return in the console for why it is
invalid.

``` r
my_dag$evaluate_validity()
```

You may also plot the DAG. By default, the `plot()` method on the DAG
will create a plotly graph. You may plot the DAG even when the DAG is
invalid. This can be useful when troubleshooting circular references,
islands, or other issues in the DAG.

``` r
plot(my_dag)
```

## Running DAGs

Once you have a valid DAG, you can run it simply with the `dag_run`
function.

``` r
dag_run(my_dag)
```

After it has ran, you can `plot()` the DAG again to show task statuses.
Tasks have 3 possible statuses after DAG runs; *Succeeded*, *Failed*,
and *Skipped*. A task will be skipped if it’s *trigger_rule* requirement
is not met.

If you ever run into issues executing a DAG, it may be useful to plot
the DAG to see where the issue arrised.

## Future Plans

Currently, to deploy a DAG, you would need to publish an RMD that
defines all of the tasks and the DAG. The result of the DAG execution is
not saved anywhere, and it’s difficult to and manage changes on.

In the future, the plan is to use the `pins` package to deploy the DAGs
to a pin on your Posit Connect server. A deployment of the DAG would
then version the DAG on the Pin. An RMD would still be required to pull
the DAG from the pin and run it.

Following this, the job that runs the DAG would clone the DAG, and save
the state of the completed DAG to a pin. This would allow you to load
that DAG into a new R environment if it ran into any issues, and
re-execute the failed/skipped tasks.

Finally, a Shiny application may be built to list all published DAGs,
plot them, re-run them, and provide other administration features.
