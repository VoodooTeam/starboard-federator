

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


SET SESSION AUTHORIZATION DEFAULT;

ALTER TABLE public.cluster_connections DISABLE TRIGGER ALL;



ALTER TABLE public.cluster_connections ENABLE TRIGGER ALL;


ALTER TABLE public.conf_reports DISABLE TRIGGER ALL;



ALTER TABLE public.conf_reports ENABLE TRIGGER ALL;


ALTER TABLE public.poll_cluster_jobs DISABLE TRIGGER ALL;



ALTER TABLE public.poll_cluster_jobs ENABLE TRIGGER ALL;


