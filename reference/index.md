# Package index

## All functions

- [`ConnectDAG`](https://timeddilation.github.io/connectapi.dag/reference/ConnectDAG.md)
  : Class representing a DAG of Connect Tasks
- [`ConnectTask`](https://timeddilation.github.io/connectapi.dag/reference/ConnectTask.md)
  : Class representing a Task published to Connect
- [`SimTask`](https://timeddilation.github.io/connectapi.dag/reference/SimTask.md)
  : Class simulating a ConnectTask
- [`connect_dag()`](https://timeddilation.github.io/connectapi.dag/reference/connect_dag.md)
  : Create a DAG to orchestrate Connect Tasks
- [`connect_task()`](https://timeddilation.github.io/connectapi.dag/reference/connect_task.md)
  : Creates a new ConnectTask object
- [`dag_add_tasks()`](https://timeddilation.github.io/connectapi.dag/reference/dag_add_tasks.md)
  : Add Tasks to a DAG
- [`dag_as_df()`](https://timeddilation.github.io/connectapi.dag/reference/dag_as_df.md)
  : Return DAG tasks as a data.frame
- [`dag_integration()`](https://timeddilation.github.io/connectapi.dag/reference/dag_integration.md)
  : DAG functions integration validation
- [`dag_plotly()`](https://timeddilation.github.io/connectapi.dag/reference/dag_plotly.md)
  : Visualize a ConnectDAG graph with plotly
- [`dag_remove_task()`](https://timeddilation.github.io/connectapi.dag/reference/dag_remove_task.md)
  : Remove a task from a DAG
- [`dag_reset()`](https://timeddilation.github.io/connectapi.dag/reference/dag_reset.md)
  : Reset a DAG to its initial state
- [`dag_run()`](https://timeddilation.github.io/connectapi.dag/reference/dag_run.md)
  : Run orchestrated ConnectTasks
- [`dag_set_dag_timeout()`](https://timeddilation.github.io/connectapi.dag/reference/dag_set_dag_timeout.md)
  : Set the overall timeout for a DAG run
- [`dag_set_max_concurrent()`](https://timeddilation.github.io/connectapi.dag/reference/dag_set_max_concurrent.md)
  : Set the maximum number of tasks a DAG runs concurrently
- [`dag_set_name()`](https://timeddilation.github.io/connectapi.dag/reference/dag_set_name.md)
  : Set the name of a DAG
- [`dag_set_pin_name()`](https://timeddilation.github.io/connectapi.dag/reference/dag_set_pin_name.md)
  : Sets the Pin name for Connect Board
- [`dag_set_poll_interval()`](https://timeddilation.github.io/connectapi.dag/reference/dag_set_poll_interval.md)
  : Set the scheduler poll interval for a DAG
- [`dag_set_task_timeout()`](https://timeddilation.github.io/connectapi.dag/reference/dag_set_task_timeout.md)
  : Set the per-task timeout for a DAG
- [`dag_validate()`](https://timeddilation.github.io/connectapi.dag/reference/dag_validate.md)
  : Validates a ConnectDAG is complete and can be executed
- [`dag_write_connect_pin()`](https://timeddilation.github.io/connectapi.dag/reference/dag_write_connect_pin.md)
  : Save a DAG as a Pin to Posit Connect
- [`dag_write_rmd()`](https://timeddilation.github.io/connectapi.dag/reference/dag_write_rmd.md)
  : Write a deployable Rmd file for a ConnectDAG
- [`set_downstream()`](https://timeddilation.github.io/connectapi.dag/reference/set_downstream.md)
  : Sets dependent task(s) for a given ConnecTask
- [`set_upstream()`](https://timeddilation.github.io/connectapi.dag/reference/set_upstream.md)
  : Sets dependency task(s) for a given ConnecTask
- [`sim_task()`](https://timeddilation.github.io/connectapi.dag/reference/sim_task.md)
  : Simulate a Connect Task
- [`task_run()`](https://timeddilation.github.io/connectapi.dag/reference/task_run.md)
  : Run a single ConnectTask
