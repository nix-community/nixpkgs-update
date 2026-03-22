ALTER TABLE packages ADD COLUMN last_update_failure_kind TEXT;
ALTER TABLE packages ADD COLUMN last_update_failure_message TEXT;
ALTER TABLE packages ADD COLUMN last_update_failure_at DATETIME;
ALTER TABLE packages ADD COLUMN declared_maintainer_count BIGINT;
ALTER TABLE packages ADD COLUMN last_rebuild_path_count BIGINT;
