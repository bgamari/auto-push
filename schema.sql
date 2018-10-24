CREATE TABLE IF NOT EXISTS merge_requests
    ( id INTEGER PRIMARY KEY
    , parent INTEGER NULL
    , status TEXT NOT NULL DEFAULT 'Runnable'
    , rebased TEXT NOT NULL DEFAULT 'none'
    , branch TEXT NOT NULL
    , orig_base TEXT NOT NULL
    , orig_head TEXT NOT NULL
    , current_head TEXT NOT NULL
    , merged INTEGER DEFAULT 0
    );
