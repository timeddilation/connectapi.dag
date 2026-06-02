# Simulate a Connect Task

Simulates a connect task evaluation, with an optional probability for
the task to fail. For a full list of available trigger rules, refer to
[connect_task](https://timeddilation.github.io/connectapi.dag/reference/connect_task.md).

## Usage

``` r
sim_task(
  guid,
  trigger_rule = "all_success",
  fail_prob = 0.2,
  sim_duration = 0L
)
```

## Arguments

- guid:

  A scalar character for the guid/name of the task to simulate

- trigger_rule:

  A scalar character that defines state of dependency (upstream) tasks
  must be in to execute

- fail_prob:

  A value between 0 and 1 for the probability of task failure

- sim_duration:

  The number of scheduler poll cycles the task should stay Running
  before finishing. Defaults to 0 (finishes immediately). Use a positive
  value to simulate a long-running render when testing concurrent DAG
  execution.

## Value

A SimTask E6 environment

## Examples

``` r
sim_task0 <- sim_task("foo", trigger_rule = "always", fail_prob = 0)
task_run(sim_task0)
```
