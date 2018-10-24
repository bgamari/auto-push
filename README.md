# auto-push

## The Algorithm

Say we have a branch (call it `master`), with the following history,

```
... ── A ── B ⇐ master
```

The algorithm begins with the following state,

 * a queue of /merge requests/ currently being tested or blocked,
   `requests = []`
 * a commit hash, `branch_head = B`, the current state of `master`
 * a commit hash, `future_head = B`, which will point to the future state of
   the branch, assuming all of the requests in `requests` pass.

When we receive a request to merge a branch, call it `feature-1`,

```
.. ── A ── B  ⇐ master
      ╰── C   ⇐ feature-1
```

we will first rebase `feature-1` on top of `branch_head`, producing rebased
branch `feature-1'`,

```
... ── A ── B  ⇐ master
           ╰── C'  ⇐ feature-1'
```

We will now update `future_head := C'`, add an entry for this request it to
`requests`, and fire off a CI job to build and test `feature-1'`. When
`merge-1`'s CI job passes, we will merge it to `master`. From this point
forth we will handle future merge requests under the optimistic assumption
that this build will succeed and consequently that `feature-1'` will be
merge to `master`.

Now say we receive another request, this time to merge `feature-2`,

```
.. ── A ── B       ⇐ master
      ╰── D ── E   ⇐ feature-2
```

Again we will rebase `feature-2` on top of `branch_head` (now `C'`),
producing a branch headed by `E'`,

```
.. ── A ── B                 ⇐ master
           ╰── C'            ⇐ feature-1'
               ╰── D' ── E'  ⇐ feature-2'
```

We will then update `future_head := E'`, add an entry to `requests`, and
fire off another CI job for `feature-2'`. Since `merge-2` chronologically
succeeded `merge-1`, we say that `merge-2` /depends on/ `merge-1`.
`feature-2'` will be merged to `master` when its CI job and that of
`merge-1` passes.  If `merge-2`'s CI job passes before we hear back about
`merge-1`, we will block before proceeding.

If `merge-1` fails, we will pop its entry off of the head of `requests`,
cancel `merge-2`'s build if it hasn't already finished, and rebase `merge-2`
on top of `branch_head`. Likewise we would do the same for any of its
dependents.

## Data Model

**Merge Request**

Consists of:

- MR ID
- Original commit hash (as pushed)
- Current commit hash (as rebased)
- Dependencies: a set of other MR's that this MR directly depends on.
  Transitive dependencies are handled automatically as build results cascade.
- Run state - one of:
  - `Runnable`: merge request is waiting to be picked up by a worker 
  - `Running`: currently being processed by a worker
  - `Passed`: build has finished and passes (dependencies nonwithstanding)
  - `Failed`: build has failed
- Merge base - one of:
  - `None`: the original merge request, not rebased yet
  - `Deps`: rebased optimistically (onto its dependencies)
  - `Master`: rebased conservatively (onto master)
- Merged - yes or no. Merged MRs don't need to be touched.

+-------------------+-----------+------------+---------+
|                   | run state | merge base | deps    |
+-------------------+-----------+------------+---------+
| New               | runnable  | none       | \*      |
+-------------------+-----------+------------+---------+
| ReRunnable        | runnable  | deps       | failed  |
+-------------------+-----------+------------+---------+
| Running           | running   | deps       | running |
+-------------------+-----------+------------+---------+
| RunningDepsPassed | running   | deps       | passed  |
+-------------------+-----------+------------+---------+
| ReRunning         | running   | master     | failed  |
+-------------------+-----------+------------+---------+
| PassedTentatively | passed    | deps       | running |
+-------------------+-----------+------------+---------+
| Passed            | passed    | deps       | passed  |
+-------------------+-----------+------------+---------+
| PassedReRun       | passed    | master     | failed  |
+-------------------+-----------+------------+---------+
| Failed            | failed    | \*         | \*      |
+-------------------+-----------+------------+---------+

## Main Loop

- Get all actionable merge requests. Actionable requests are all requests that
  haven't been merged yet.
- For each actionable merge request:
    - If it is runnable:
        - If current merge base is "none":
            - Rebase onto "deps" & mark as such
        - If current merge base is "deps":
            - Check dependencies
                - If dependencies "passed" or "running":
                    - Mark "running"
                    - Kick off a build
                - Otherwise:
                    - Schedule for re-running
                        - Keep run state at "runnable"
                        - Rebase onto "master"
        - If current merge base is "master":
            - Mark "running"
            - Kick off a build
    - If it is running:
        - If current merge base is "deps":
            - Check dependencies
            - If dependencies "failed":
                - Abort build
                - Rebase onto "master"
                - Mark as "runnable"
            - Otherwise:
                - Check build status
                - If build status is failed:
                    - Rebase onto "master"
                    - Mark as "runnable"
        - Check build status
        - If build status is failed:
            - Mark as "failed"
    - If it is "failed":
        - Do nothing.
    - If it is "passed":
        - If current merge base is "deps":
            - Check dependencies.
            - If dependencies "failed":
                - Rebase onto "master"
                - Mark as "runnable"
            - If dependencies "passed":
                - Merge into "master"
                - Mark merged
            - If dependencies "running":
                - Do nothing (wait for deps)
        - If current merge base is "master":
            - Merge into "master"
            - Mark merged
