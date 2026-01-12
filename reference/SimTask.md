# Class simulating a ConnectTask

Class simulating a ConnectTask

Class simulating a ConnectTask

## Usage

    sim_task <- SimTask$new("task0", "always", 0)
    sim_task$execute()

## Details

This class simulates a ConnectTask running in a ConnectDAG. It is
inherited from ConnectTask, with the only differences being to the
\`new()\` and \`execute_task()\` methods. However, when using a SimTask,
no attempt is made to validate the content is published to Posit
Connect. Additionally, you may control the probability a task will fail.
This allows you to simulate the DAG under specific scenarios. This class
is used heavily in tests when it is desirable to force an upstream task
failure.

## See also

Other R6 classes:
[`ConnectDAG`](https://timeddilation.github.io/connectapi.dag/reference/ConnectDAG.md),
[`ConnectTask`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.md)

## Super class

[`connectapi.dag::ConnectTask`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.md)
-\> `SimTask`

## Public fields

- `fail_prob`:

  A numeric between 0 and 1, determining the chance of a failure

## Methods

### Public methods

- [`SimTask$new()`](#method-SimTask-new)

- [`SimTask$execute()`](#method-SimTask-execute)

- [`SimTask$clone()`](#method-SimTask-clone)

Inherited methods

- [`connectapi.dag::ConnectTask$can_run()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-can_run)
- [`connectapi.dag::ConnectTask$df_row()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-df_row)
- [`connectapi.dag::ConnectTask$link_task()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-link_task)
- [`connectapi.dag::ConnectTask$linked_tasks_attrs()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-linked_tasks_attrs)
- [`connectapi.dag::ConnectTask$plot()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-plot)
- [`connectapi.dag::ConnectTask$poll_task()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-poll_task)
- [`connectapi.dag::ConnectTask$print()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-print)
- [`connectapi.dag::ConnectTask$reset()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-reset)
- [`connectapi.dag::ConnectTask$set_downstream()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-set_downstream)
- [`connectapi.dag::ConnectTask$set_upstream()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-set_upstream)
- [`connectapi.dag::ConnectTask$update_task_graph()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-update_task_graph)

------------------------------------------------------------------------

### Method `new()`

Initializes a new SimTask

#### Usage

    SimTask$new(guid, trigger_rule = "all_success", fail_prob = 0.2)

#### Arguments

- `guid`:

  A scalar character of the guid for the content item. Not validated in
  Posit Connect.

- `trigger_rule`:

  A scalar character that defines state of dependency (upstream) tasks
  must be in to execute

- `fail_prob`:

  A numeric between 0 and 1, determining the chance of a failure

------------------------------------------------------------------------

### Method `execute()`

Simulates the execution of task, taking into account failure probability

#### Usage

    SimTask$execute(verbose = FALSE)

#### Arguments

- `verbose`:

  Should the task print messages as it executes?

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SimTask$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
