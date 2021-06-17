-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE conf_reports (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    cluster_id UUID NOT NULL,
    report TEXT NOT NULL,
    namespace TEXT NOT NULL,
    resource_name TEXT NOT NULL,
    resource_kind TEXT NOT NULL,
    warning_count INT NOT NULL,
    danger_count INT NOT NULL,
    pass_count INT NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE TABLE poll_cluster_jobs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,
    last_error TEXT DEFAULT NULL,
    attempts_count INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    locked_by UUID DEFAULT NULL,
    cluster_id UUID DEFAULT NULL
);
CREATE INDEX poll_cluster_jobs_cluster_id_index ON poll_cluster_jobs (cluster_id);
CREATE TABLE cluster_connections (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL UNIQUE,
    url TEXT NOT NULL UNIQUE,
    sa_token TEXT NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE conf_reports ADD CONSTRAINT conf_reports_ref_cluster_id FOREIGN KEY (cluster_id) REFERENCES cluster_connections (id) ON DELETE CASCADE;
ALTER TABLE poll_cluster_jobs ADD CONSTRAINT poll_cluster_jobs_ref_cluster_id FOREIGN KEY (cluster_id) REFERENCES cluster_connections (id) ON DELETE CASCADE;
