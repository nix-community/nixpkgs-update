ALTER TABLE packages DROP COLUMN consecutive_failures;
ALTER TABLE packages DROP COLUMN last_success_at;
ALTER TABLE packages DROP COLUMN failed_update_pr;
ALTER TABLE packages DROP COLUMN failed_update_pr_opened_at;
ALTER TABLE packages DROP COLUMN escalated_at;
