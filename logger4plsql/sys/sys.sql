set define on
spool sys.log
@@./../params.sql

connect &&sys_connect
@@./users.sql
@@./roles.sql
@@./grants.sql
spool off
exit