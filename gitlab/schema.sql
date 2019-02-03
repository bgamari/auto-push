CREATE TABLE IF NOT EXISTS gitlab_merge_requests
    ( id INTEGER PRIMARY KEY
    , merge_request INTEGER NOT NULL UNIQUE
    , last_comment_status INTEGER NOT NULL
    , original_head TEXT NOT NULL

    , FOREIGN KEY (merge_request) REFERENCES merge_request (id)
        ON DELETE NO ACTION
    );

