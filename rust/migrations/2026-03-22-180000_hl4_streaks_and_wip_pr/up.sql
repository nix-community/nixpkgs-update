ALTER TABLE packages ADD COLUMN consecutive_failures INTEGER NOT NULL DEFAULT 0;
ALTER TABLE packages ADD COLUMN last_success_at DATETIME;
ALTER TABLE packages ADD COLUMN failed_update_pr INTEGER;
ALTER TABLE packages ADD COLUMN failed_update_pr_opened_at DATETIME;
ALTER TABLE packages ADD COLUMN escalated_at DATETIME;
