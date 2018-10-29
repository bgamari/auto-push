CREATE TABLE IF NOT EXISTS merge_requests
    ( id INTEGER PRIMARY KEY
    , parent INTEGER NULL UNIQUE
    , status TEXT NOT NULL DEFAULT 'Runnable'
    , rebased INTEGER NOT NULL DEFAULT 0
    , branch TEXT NOT NULL
    , orig_base TEXT NULL
    , orig_head TEXT NOT NULL UNIQUE
    , current_head TEXT NOT NULL
    , merged INTEGER DEFAULT 0
    , build_id TEXT NULL
    );
