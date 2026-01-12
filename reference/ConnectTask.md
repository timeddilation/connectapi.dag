# Class representing a Task published to Connect

Class representing a Task published to Connect

Class representing a Task published to Connect

## Usage

    task <- ConnectTask$new(
      guid = "1f6c9a82-0177-47fa-a27c-be090b39dca7",
      trigger_rule = "always",
      connect_server = connectapi::connect()
    )
    task$execute()

## Details

This class validates a published content item on Posit Connect, and
allows the user to render the item programatically. Additionally, other
tasks may be set as dependencies or dependents for the purposes of
creating a DAG.

## See also

Other R6 classes:
[`ConnectDAG`](https://timeddilation.github.io/connectapi.dag/reference/ConnectDAG.md),
[`SimTask`](https://timeddilation.github.io/connectapi.dag/reference/SimTask.md)

## Public fields

- `guid`:

  The guid of the content item published to Connect

- `name`:

  The name of the ContentItem on Connect

- `status`:

  The status of this task. Possible statuses: Pending, Succeeded,
  Failed, Skipped

- `trigger_rule`:

  The rule for when to run this task. See
  [connect_task](https://timeddilation.github.io/connectapi.dag/reference/connect_task.md)
  for details

- `upstream_tasks`:

  A list of ConnectTask R6 envs that are dependencies for this task

- `downstream_tasks`:

  A list of ConnectTask R6 envs that are dependents of this task

- `task_graph`:

  An igraph object of this task, and its immediate upstream and
  downstream tasks

- `connect_server`:

  An Connect R6 env generated from connectapi::connect()

- `connect_content_item`:

  A ContentItem R6 env generated from connectapi::content_item()

- `connect_variant`:

  A TaskVariant R6 env generated from connectapi::get_variant_default()

- `connect_rendering`:

  A VariantRender R6 env generated from connectapi::variant_render()

- `app_mode`:

  The type of content being rendered on Posit Connect

## Methods

### Public methods

- [`ConnectTask$new()`](#method-ConnectTask-new)

- [`ConnectTask$print()`](#method-ConnectTask-print)

- [`ConnectTask$reset()`](#method-ConnectTask-reset)

- [`ConnectTask$df_row()`](#method-ConnectTask-df_row)

- [`ConnectTask$set_downstream()`](#method-ConnectTask-set_downstream)

- [`ConnectTask$set_upstream()`](#method-ConnectTask-set_upstream)

- [`ConnectTask$link_task()`](#method-ConnectTask-link_task)

- [`ConnectTask$update_task_graph()`](#method-ConnectTask-update_task_graph)

- [`ConnectTask$plot()`](#method-ConnectTask-plot)

- [`ConnectTask$linked_tasks_attrs()`](#method-ConnectTask-linked_tasks_attrs)

- [`ConnectTask$execute()`](#method-ConnectTask-execute)

- [`ConnectTask$poll_task()`](#method-ConnectTask-poll_task)

- [`ConnectTask$can_run()`](#method-ConnectTask-can_run)

- [`ConnectTask$clone()`](#method-ConnectTask-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes a new ConnectTask

#### Usage

    ConnectTask$new(guid, trigger_rule = "all_success", connect_server)

#### Arguments

- `guid`:

  A scalar character of the guid for the content deployed to Posit
  Connect

- `trigger_rule`:

  A scalar character that defines state of dependency (upstream) tasks
  must be in to execute

- `connect_server`:

  A
  [connect](https://posit-dev.github.io/connectapi/reference/connect.html)
  environment with a connection to the Connect server

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Displays summary of environment in console.

#### Usage

    ConnectTask$print()

------------------------------------------------------------------------

### Method `reset()`

Resets the task to an initial state, read to be executed

#### Usage

    ConnectTask$reset()

------------------------------------------------------------------------

### Method `df_row()`

Makes a single-row data.frame of this Task's details. Used by the
ConnectDAG to create a data.frame of all linked tasks.

#### Usage

    ConnectTask$df_row()

------------------------------------------------------------------------

### Method [`set_downstream()`](https://timeddilation.github.io/connectapi.dag/reference/set_downstream.md)

Sets dependent tasks on this task

#### Usage

    ConnectTask$set_downstream(...)

#### Arguments

- `...`:

  Any number of ConnectTask R6 environments

------------------------------------------------------------------------

### Method [`set_upstream()`](https://timeddilation.github.io/connectapi.dag/reference/set_upstream.md)

Sets dependency tasks on this task

#### Usage

    ConnectTask$set_upstream(...)

#### Arguments

- `...`:

  Any number of ConnectTask R6 environments

------------------------------------------------------------------------

### Method `link_task()`

Adds a single ConnectTask link either upstream or downstream. Should not
be used directly. Called internally by the set_upstream and
set_downstream methods.

#### Usage

    ConnectTask$link_task(task, link = c("upstream_tasks", "downstream_tasks"))

#### Arguments

- `task`:

  A ConnectTask R6 environment

- `link`:

  The type of link to create

------------------------------------------------------------------------

### Method `update_task_graph()`

Re-generates this task's \`task_graph\` after a task is linked

#### Usage

    ConnectTask$update_task_graph()

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Visualizes this task's \`task_graph\`

#### Usage

    ConnectTask$plot()

------------------------------------------------------------------------

### Method `linked_tasks_attrs()`

Returns a list of the linked tasks' attributes

#### Usage

    ConnectTask$linked_tasks_attrs(
      link = c("both", "upstream_tasks", "downstream_tasks"),
      task_attr = c("guid", "name", "status")
    )

#### Arguments

- `link`:

  They type of link to use to search for tasks

- `task_attr`:

  The name of the attribute to return

------------------------------------------------------------------------

### Method `execute()`

Executes a ConnectTask on a remote Connect Server

#### Usage

    ConnectTask$execute(verbose = FALSE)

#### Arguments

- `verbose`:

  Should the task print messages as it executes?

------------------------------------------------------------------------

### Method `poll_task()`

A wrapper around connectapi::poll_task for this task's execution

#### Usage

    ConnectTask$poll_task(verbose = FALSE)

#### Arguments

- `verbose`:

  Should the task print messages as it executes?

------------------------------------------------------------------------

### Method `can_run()`

Returns a logical indicating if this task can run based on the
\`trigger_rule\`

#### Usage

    ConnectTask$can_run()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ConnectTask$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
