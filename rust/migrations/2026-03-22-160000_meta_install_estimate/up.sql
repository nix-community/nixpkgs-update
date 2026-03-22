CREATE TABLE package_tracking_meta (
    key TEXT PRIMARY KEY NOT NULL,
    value TEXT NOT NULL
);

INSERT INTO package_tracking_meta (key, value) VALUES ('failure_event_version', '1');

ALTER TABLE packages ADD COLUMN install_estimate BIGINT;
