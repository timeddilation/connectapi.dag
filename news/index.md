# Changelog

## connectapi.dag 0.2.0

### New features

- DAGs can now run tasks **concurrently**. Because rendering happens on
  the Posit Connect server, the DAG dispatches independent tasks and
  polls them without blocking, so branches that do not depend on each
  other render at the same time. Concurrency is opt-in: `max_concurrent`
  defaults to `1` (sequential, identical to previous behavior). Raise it
  with
  [`dag_set_max_concurrent()`](https://timeddilation.github.io/connectapi.dag/reference/dag_set_max_concurrent.md),
  the `connect_dag(max_concurrent = )` argument, or the new
  `dag_run(max_concurrent = )` override. The setting is stored on the
  DAG, so it persists through pins and applies to scheduled runs.
- New per-task and global timeouts guarantee a run terminates even if a
  render never finishes on Connect:
  [`dag_set_task_timeout()`](https://timeddilation.github.io/connectapi.dag/reference/dag_set_task_timeout.md)
  and
  [`dag_set_dag_timeout()`](https://timeddilation.github.io/connectapi.dag/reference/dag_set_dag_timeout.md).
  A timed-out task is marked `Failed` and the run proceeds. Both are
  disabled (`NA`) by default.
- New
  [`dag_set_poll_interval()`](https://timeddilation.github.io/connectapi.dag/reference/dag_set_poll_interval.md)
  controls how often the scheduler polls running tasks (default 1
  second).
- [`sim_task()`](https://timeddilation.github.io/connectapi.dag/reference/sim_task.md)
  gained a `sim_duration` argument (in scheduler poll cycles) to
  simulate long-running renders when testing concurrent execution.
- Tasks have a new `Running` status, shown in amber when plotting a DAG.

### Behavior changes

- A task whose render cannot be started (e.g. content that cannot
  render) is now marked `Failed` instead of aborting the entire DAG run,
  so downstream trigger rules such as `all_done`/`one_failed` still
  fire. The error message is captured in the task’s `poll_output`.
- In verbose mode, render log lines are prefixed with the task name
  (`[task] ...`) so output from concurrently running tasks stays
  readable.
- `ConnectTask$execute()` has been split into `dispatch()` (start the
  render, non-blocking) and `poll_once()` (check progress,
  non-blocking). `execute()` is retained as a blocking convenience used
  by
  [`task_run()`](https://timeddilation.github.io/connectapi.dag/reference/task_run.md).
