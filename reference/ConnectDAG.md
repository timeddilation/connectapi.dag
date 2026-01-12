# Class representing a DAG of Connect Tasks

Class representing a DAG of Connect Tasks

Class representing a DAG of Connect Tasks

## Usage

    dag <- ConnectDAG$new(name = "dag")

## Details

This class requires ConnectTasks are added to it, which are themselves
linked in a dependency chain. As tasks are added, each tasks' graph is
unioned in the DAG's graph. It can then be validated to ensure it is in
fact a Directed Acyclic Graph. Once validated, the DAG can orchestrate
tasks in Posit Connect.

## See also

Other R6 classes:
[`ConnectTask`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.md),
[`SimTask`](https://timeddilation.github.io/connectapi.dag/reference/SimTask.md)

## Public fields

- `name`:

  The user-defined name of this DAG. Useful for organizing multiple
  DAGs.

- `pin_name`:

  The name of pin on Connect where this DAG is saved.

- `tasks`:

  A list of ConnectTasks this DAG is orchestrating.

- `dag_graph`:

  An igraph object of all linked tasks in this DAG

- `is_valid`:

  Indicates if the tasks' dependency chain forms a proper DAG. DO NOT
  MODIFY DIRECTLY!

- `run_id`:

  A UUID created for an instance of a DAG run

- `run_start`:

  The time a DAG run started

- `run_end`:

  The time a DAG run ended

- `is_complete`:

  Indicates if all tasks in this DAG have been evaluated for execution.

## Methods

### Public methods

- [`ConnectDAG$new()`](#method-ConnectDAG-new)

- [`ConnectDAG$print()`](#method-ConnectDAG-print)

- [`ConnectDAG$add_task()`](#method-ConnectDAG-add_task)

- [`ConnectDAG$add_tasks()`](#method-ConnectDAG-add_tasks)

- [`ConnectDAG$remove_task()`](#method-ConnectDAG-remove_task)

- [`ConnectDAG$set_name()`](#method-ConnectDAG-set_name)

- [`ConnectDAG$set_connect_pin_name()`](#method-ConnectDAG-set_connect_pin_name)

- [`ConnectDAG$plot()`](#method-ConnectDAG-plot)

- [`ConnectDAG$task_attrs()`](#method-ConnectDAG-task_attrs)

- [`ConnectDAG$tasks_as_df()`](#method-ConnectDAG-tasks_as_df)

- [`ConnectDAG$execute()`](#method-ConnectDAG-execute)

- [`ConnectDAG$reset()`](#method-ConnectDAG-reset)

- [`ConnectDAG$evaluate_validity()`](#method-ConnectDAG-evaluate_validity)

- [`ConnectDAG$clone()`](#method-ConnectDAG-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes a new ConnectDAG

#### Usage

    ConnectDAG$new(name = "new_dag", ...)

#### Arguments

- `name`:

  A personalized name for the DAG

- `...`:

  Connect Tasks to add to the graph

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Displays summary of environment in console.

#### Usage

    ConnectDAG$print()

------------------------------------------------------------------------

### Method `add_task()`

Adds a ConnectTask to this DAG

#### Usage

    ConnectDAG$add_task(task)

#### Arguments

- `task`:

  a ConnectTask R6 environment

------------------------------------------------------------------------

### Method `add_tasks()`

Adds any number of ConnectTasks to this DAG

#### Usage

    ConnectDAG$add_tasks(...)

#### Arguments

- `...`:

  Any number of ConnectTask R6 environments

------------------------------------------------------------------------

### Method `remove_task()`

Removes a ConnectTask from this DAG

#### Usage

    ConnectDAG$remove_task(task)

#### Arguments

- `task`:

  a ConnectTask R6 environment

------------------------------------------------------------------------

### Method `set_name()`

Sets the name of this DAG, if needed to change after initializing

#### Usage

    ConnectDAG$set_name(name)

#### Arguments

- `name`:

  A scalar character of the name

------------------------------------------------------------------------

### Method `set_connect_pin_name()`

Sets the name used when using
[dag_write_connect_pin](https://timeddilation.github.io/connectapi.dag/reference/dag_write_connect_pin.md)

#### Usage

    ConnectDAG$set_connect_pin_name(pin_name = self$name)

#### Arguments

- `pin_name`:

  A scalar character of the name desired or required

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Prints a plotly graph of the DAG's graph

#### Usage

    ConnectDAG$plot(plotly = TRUE)

#### Arguments

- `plotly`:

  A logical, indicate to use a plotly visual or a static visual

------------------------------------------------------------------------

### Method `task_attrs()`

Returns a character vector of DAG tasks' specified attribute

#### Usage

    ConnectDAG$task_attrs(task_attr = c("guid", "name", "status"))

#### Arguments

- `task_attr`:

  The name of the character attribute to return

------------------------------------------------------------------------

### Method `tasks_as_df()`

Returns a data.frame of all tasks added to this DAG

#### Usage

    ConnectDAG$tasks_as_df(revalidate_dag = TRUE)

#### Arguments

- `revalidate_dag`:

  Should the DAG be validated before returning the data.frame?

------------------------------------------------------------------------

### Method `execute()`

Executes all tasks, in order, that are added to this DAG

#### Usage

    ConnectDAG$execute(verbose = FALSE)

#### Arguments

- `verbose`:

  Should it print messages as it executes tasks?

------------------------------------------------------------------------

### Method `reset()`

Resets the DAG to an initial state, allowing it to run again

#### Usage

    ConnectDAG$reset()

------------------------------------------------------------------------

### Method `evaluate_validity()`

Determines if all added tasks form a valid DAG, setting the \`is_valid\`
field

#### Usage

    ConnectDAG$evaluate_validity(verbose = TRUE)

#### Arguments

- `verbose`:

  Should it print a message to the console of the result?

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ConnectDAG$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
