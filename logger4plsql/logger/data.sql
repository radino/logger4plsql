rem to avoid unnessecary cursors
alter session set cursor_sharing=force;

@@./data/t_appender.sql
@@./data/t_log_level.sql
@@./data/t_logger.sql
@@./data/t_param.sql
COMMIT;
