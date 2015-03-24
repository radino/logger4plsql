spool drop.log
@@./../params.sql

connect &logger_connect

@@./packages/drop.sql
@@./views/drop.sql
@@./tables/drop.sql
@@./sequences/drop.sql

spool off
exit
