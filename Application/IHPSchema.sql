-- this file has been manually copy/pasted from ./build/ihp-lib/ in order to allow docker to 
-- access it and run the db migration
-- Provides all the default settings for a IHP database in development mode
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Used by IHP.Job
CREATE TYPE JOB_STATUS AS ENUM ('job_status_not_started', 'job_status_running', 'job_status_failed', 'job_status_succeeded', 'job_status_retry');
