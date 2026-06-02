# Class simulating a ConnectTask

Class simulating a ConnectTask

Class simulating a ConnectTask

## Usage


    sim_task <- SimTask$new("task0", "always", 0)
    sim_task$execute()

## Details

This class simulates a ConnectTask running in a ConnectDAG. It is
inherited from ConnectTask, overriding the \`new()\`, \`dispatch()\`,
and \`poll_once()\` methods so no calls are made to Posit Connect.
However, when using a SimTask, no attempt is made to validate the
content is published to Posit Connect. Additionally, you may control the
probability a task will fail. This allows you to simulate the DAG under
specific scenarios. You may also give a task a simulated duration (in
scheduler poll cycles), which is useful for exercising concurrent DAG
execution in tests. This class is used heavily in tests when it is
desirable to force an upstream task failure.

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

- `sim_duration`:

  The number of scheduler poll cycles the task stays Running before
  finishing

- `sim_polls_remaining`:

  The number of poll cycles left before this dispatched task finishes

- `sim_will_fail`:

  Whether the in-flight simulated render will end in failure, decided at
  dispatch

## Methods

### Public methods

- [`SimTask$new()`](#method-SimTask-new)

- [`SimTask$reset()`](#method-SimTask-reset)

- [`SimTask$dispatch()`](#method-SimTask-dispatch)

- [`SimTask$poll_once()`](#method-SimTask-poll_once)

- [`SimTask$clone()`](#method-SimTask-clone)

Inherited methods

- [`connectapi.dag::ConnectTask$can_run()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-can_run)
- [`connectapi.dag::ConnectTask$df_row()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-df_row)
- [`connectapi.dag::ConnectTask$execute()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-execute)
- [`connectapi.dag::ConnectTask$link_task()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-link_task)
- [`connectapi.dag::ConnectTask$linked_tasks_attrs()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-linked_tasks_attrs)
- [`connectapi.dag::ConnectTask$plot()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-plot)
- [`connectapi.dag::ConnectTask$print()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-print)
- [`connectapi.dag::ConnectTask$set_downstream()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-set_downstream)
- [`connectapi.dag::ConnectTask$set_upstream()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-set_upstream)
- [`connectapi.dag::ConnectTask$update_task_graph()`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.html#method-update_task_graph)

------------------------------------------------------------------------

### Method `new()`

Initializes a new SimTask

#### Usage

    SimTask$new(
      guid,
      trigger_rule = "all_success",
      fail_prob = 0.2,
      sim_duration = 0L
    )

#### Arguments

- `guid`:

  A scalar character of the guid for the content item. Not validated in
  Posit Connect.

- `trigger_rule`:

  A scalar character that defines state of dependency (upstream) tasks
  must be in to execute

- `fail_prob`:

  A numeric between 0 and 1, determining the chance of a failure

- `sim_duration`:

  The number of scheduler poll cycles the task should stay Running
  before finishing

------------------------------------------------------------------------

### Method `reset()`

Resets the simulated task to an initial state

#### Usage

    SimTask$reset()

------------------------------------------------------------------------

### Method `dispatch()`

Simulates dispatching a render, deciding the eventual outcome up front

#### Usage

    SimTask$dispatch(verbose = FALSE)

#### Arguments

- `verbose`:

  Should the task print messages as it executes?

------------------------------------------------------------------------

### Method `poll_once()`

Simulates polling the render, finishing once the simulated duration
elapses

#### Usage

    SimTask$poll_once(wait = 0, verbose = FALSE, error_threshold = 3L)

#### Arguments

- `wait`:

  Ignored for simulated tasks; present for signature compatibility

- `verbose`:

  Should the task print messages as it executes?

- `error_threshold`:

  Ignored for simulated tasks; present for signature compatibility

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SimTask$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
