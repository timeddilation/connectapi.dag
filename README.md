
<!-- badges: start -->

[![R build
status](https://github.com/timeddilation/connectapi.dag/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/timeddilation/connectapi.dag/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

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

This package is not currently available on CRAN.

To install the development version:

``` r
devtools::install_github("timeddilation/connectapi.dag")
```

## Getting Started

Before starting here, it is highly recommended you familiarize yourself
with [connectapi](https://github.com/rstudio/connectapi) briefly. The
most important aspect to understand is how the client is created. By
default, this package looks for environment variables containing the
secrets required to connect to your Posit Connect server.

    CONNECT_SERVER = https://connect.example.com
    CONNECT_API_KEY = your-api-key

It is highly recommended you use these exact environment variables for
authentication, as they are used by both `connectapi` and `pins`.

Once you have the basics figured out with `connectapi`, and you can
create a connect client using `connectapi::connect()` you can start
defining **tasks** and **DAGs**.

Secondly, it is suggested to familiarize yourself with
[pins](https://github.com/rstudio/pins-r). Specifically, this package
uses `pins::board_connect()` under the hood for managing DAG deployments
to Posit Connect. Although, it is not required to use this package’s
built-in features for DAG deployments, as you can do whatever you want
with the DAG environment once it has been defined.

In order to use the defaults with above environment variables, `pins` V
\>= 1.1.0 is required.

## Overview

Create and deploy DAGs to orchestrate scheduled jobs on Posit Connect in
4 steps:

- Define Tasks and dependency chains
- Define a DAG as a collection of tasks
- Store the DAG in persistent storage using pins
- Schedule the DAG to run, and remove schedules from individual tasks

``` r
library(connectapi.dag)

# Define Tasks
# Use `simulated = TRUE` to skip Posit Validation
# Real Connect Tasks must be defined with `simulated = FALSE` (the default)
task0 <- connect_task("guid-0", simulated = TRUE)
task1 <- connect_task("guid-1", simulated = TRUE)
task2 <- connect_task("guid-2", simulated = TRUE)
task3 <- connect_task("guid-3", simulated = TRUE)
task4 <- connect_task("guid-4", trigger_rule = "one_success", simulated = TRUE)

# define dependency chain
task1 |>
  set_upstream(task0) |>
  set_downstream(task2, task3)

task4 |>
  set_upstream(task2, task3)

# create the DAG
my_dag <-
  connect_dag() |>
  dag_add_tasks(task0, task1, task2, task3, task4) |>
  dag_set_name("simulated_dag") |>
  dag_validate()

# save the DAG to Posit Connect as a pin
dag_write_connect_pin(my_dag)

# create a deployment Rmd to read the DAG from storage and execute
dag_write_rmd(my_dag)
```

Open the created Rmd file and click to deploy to Connect!

## Details

### Tasks

The most fundamental part of creating a DAG is to define the tasks you
want included in the DAG. The tasks are created by specifying the GUID
of the content item published to your Posit Connect server, with an
option for the *trigger_rule* to follow. By default, the *trigger_rule*
is **all_success**, meaning all predecessor tasks completed
successfully. View the man page of the `connect_task` function for a
list of all available trigger rules.

If a task has no predecessors, it will always run. The `connect_task`
function returns an R6 environment that manages the state of the task.

``` r
task0 <- connect_task("4af62803-c9aa-45f4-8336-0ea7bbdd9334")
task1 <- connect_task("711b8c7a-97ef-4343-9e55-0d4d426ebf59")
task2 <- connect_task("eda729a2-e3f1-4063-b606-e52d01e9aa23")
```

#### Task Dependencies

Two functions are provided to create dependencies: `set_upstream()` and
`set_downstream()`. For each of these functions, they may accept any
number of tasks to set as dependencies.

For example, the following will make *task1* and *task2* dependent on
*task0* completing.

``` r
task0 |>
  set_downstream(task1, task2)
```

You can visualize a task’s immediate dependencies and dependents by
plotting it:

``` r
plot(task0)
```

#### Simulated Tasks

For simple happy-path testing, you may use `simulated = TRUE` parameter
in the `connect_task()` function. Simulated tasks created this way will
always run successfully and skip validation with Posit Connect.

To test what happens in failure scenarios, you may instead use the
`sim_task()` function. This function also creates a simulated
ConnectTask environment, but allows you to specify a probability of
failure.

This gives you a great way to visualize what happens under failure
scenarios.

``` r
sim_task0 <- sim_task("guid-0", fail_prob = 0)
sim_task1 <- sim_task("guid-1", fail_prob = 0)
sim_task2 <- sim_task("guid-2", fail_prob = 0.2)
sim_task3 <- sim_task("guid-3", fail_prob = 1)
sim_task4 <- sim_task("guid-4", trigger_rule = "one_success", fail_prob = 0)

# define dependency chain
sim_task1 |>
  set_upstream(sim_task0) |>
  set_downstream(sim_task2, sim_task3)

sim_task4 |>
  set_upstream(sim_task2, sim_task3)

sim_dag <-
  connect_dag(sim_task0, sim_task1, sim_task2, sim_task3, sim_task4) |>
  dag_validate() |>
  dag_run()

plot(sim_dag)
```

### DAGs

Once all tasks are defined, you may add them to a DAG R6 environment.
This object will validate the tasks are in fact linked as a DAG. You may
create task dependencies before or after creating the DAG.

``` r
my_dag <- connect_dag(task0, task1, task2, name = "my_dag")
```

Note: All tasks that are linked in the dependency chain must be added to
the DAG. The DAG will not validate if it detects a task has a dependency
link, and that task is not added to the DAG.

With the DAG object created, you can check the validity of the DAG. If
any issues are found, a message will return in the console for why it is
invalid. It is recommended you always run `dag_validate` on a DAG before
storing it or trying to execute it. When a DAG is executed, it will
check validity and raise an error if the DAG is invalid.

``` r
dag_validate(my_dag)
```

You may also plot the DAG. By default, the `plot()` method on the DAG
will create a plotly graph. You may plot the DAG even when the DAG is
invalid. This can be useful when troubleshooting circular references,
islands, or other issues in the DAG.

``` r
plot(my_dag)
```

### Running DAGs

Once you have a valid DAG, you can run it simply with the `dag_run`
function. Calling this from a local environment will execute the tasks
in the DAG. However, this does not yet schedule the DAG as a whole. Keep
reading below for deployment options.

``` r
dag_run(my_dag)
```

After it has ran, you can `plot()` the DAG again to show task statuses.
Tasks have 3 possible statuses after DAG runs; *Succeeded*, *Failed*,
and *Skipped*. A task will be skipped if it’s *trigger_rule* requirement
is not met.

If you ever run into issues executing a DAG, it may be useful to plot
the DAG to see where the issue arrised.

### Storing DAGs

Once a DAG is defined, you will want to save it to persistent storage so
you can later retrieve it to run it again. You can use any method you
want for this portion, as you can simply save the DAG environment with
`saveRDS`. For instance, if you want to use S3 for your persistent
storage and versioning, you can use the `paws.storage` package instead.
Or, you can save the DAG as an RDS object in your project’s repository,
and include it in the bundle you publish to Posit Connect.

However, since this package is explicitly for Posit Connect, integration
to use Posit Connect persistent storage with the `pins` package is
already built-in.

The `dag_write_connect_pin()` function handles most of the work for you.
The DAG environment has a `pin_name` attribute, which can be changed
with the `dag_set_pin_name()` function.

``` r
dag_write_connect_pin(my_dag)
```

### Scheduling DAGs

Scheduling a DAG works the same way as scheduling any other job on Posit
Connect. You must publish a bundle that can be rendered (Rmd, quarto,
jupyter, etc.) that will read the DAG environment and call the
`run_dag()` method on it.

This package simplifies the process if you’re also using pins on Connect
for persistent storage. After saving the DAG to Connect using pins, you
can use `dag_write_rmd()` function to generate the Rmd file needed to
execute and schedule the DAG.

``` r
dag_write_rmd(my_dag)
```

Note that this function requires the original DAG environment. The DAG
contains the name of the pin on Posit Connect. This function will first
validate the pin exists on your Posit Connect Server. Once validated, it
will compose the entire job that you can then publish and schedule.

Once published and scheduled, you can remove any schedules you have on
existing tasks within your DAG.

Part of the defaults for this job is to save the DAG again, overwriting
and versioning the existing DAG. It is imperative the DAG’s `pin_name`
is identical to the pin it is reading. When this job reads the DAG, it
will first reset the DAG using `dag_reset()`, which resets all task
statuses and the DAG’s internal state.

This allows you to have a log of all previous DAG runs, and which tasks
succeeded, failed, or skipped. It is planned for the future to introduce
functions and tools to read previous versions of the DAGs and view all
the tasks states for prior runs to allow investigation of issues, and
possibly the ability to re-run specific tasks.

## Other Notes

Tasks that the DAG runs follow any settings you have created for those
tasks. For instance, email on failure will still occur if the task
render fails. The DAG job itself does not raise errors when a task
fails, as it expects some tasks might fail.

## Future Plans

A Shiny application may be built to list all published DAGs, view the
history of DAG executions, plot them, re-run them, and provide other
administration features. This shiny app may be ran locally, in
workbench, or as a deployed app on Posit Connect.

Currently, tasks in a DAG cannot be run concurrently, even when it
should be possible to be running multiple tasks at the same time. This
is a major optimization that can be made, but requires a lot of thought
and planning to execute correctly and give the user proper control of
that behavior. When this tool becomes more mature, this kind of feature
will be added.
