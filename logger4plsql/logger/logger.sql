spool logger.log
@@./../params.sql

connect &logger_connect
@@./contexts.sql
@@./tables.sql
@@./sequences.sql
@@./views.sql
@@./data.sql
@@./packages.sql
--@@./grants.sql
spool off
exit
