CREATE TABLE IF NOT EXISTS merge_requests
    ( id INTEGER PRIMARY KEY
    , parent INTEGER NULL UNIQUE

    , status TEXT NOT NULL DEFAULT 'Runnable'

    -- 1 = rebased
    -- -1 = rebase failed
    -- 0 = not rebased yet
    , rebased INTEGER NOT NULL DEFAULT 0

    -- upstream branch to merge into
    , branch TEXT NOT NULL

    -- SHA of the original commit base
    , orig_base TEXT NULL

    -- SHA of the original MR head commit
    , orig_head TEXT NOT NULL UNIQUE

    -- SHA of the MR head commit as currently merged
    , current_base TEXT NULL

    -- SHA of the MR head commit as currently merged
    , current_head TEXT NOT NULL

    -- 1 = merged
    -- -1 = merge failed
    -- 0 = not merged yet
    , merged INTEGER DEFAULT 0

    -- external ID of the build, as chosen by the build driver
    , build_id TEXT NULL


    , FOREIGN KEY (parent) REFERENCES merge_request (id)
        ON DELETE NO ACTION ON UPDATE NO ACTION
    );

-- The jobs table is rather simple: it just contains the IDs of merge requests
-- that require attention. The `id` field is auto-incremented, so later jobs
-- receive higher values, and thus sorting by ID produces the correct queue
-- ordering.
CREATE TABLE IF NOT EXISTS jobs
    ( id INTEGER PRIMARY KEY

    -- Which merge request to look at
    , merge_request INTEGER NOT NULL

    -- Set this to call dibs on a job while working on it
    , worker TEXT NULL


    , FOREIGN KEY (merge_request) REFERENCES merge_requests (id)
        ON DELETE NO ACTION ON UPDATE NO ACTION
    );
