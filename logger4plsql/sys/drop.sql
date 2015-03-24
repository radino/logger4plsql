set define on
spool drop.log
@@./../params.sql

connect &&sys_connect
@@./grants/revoke.sql
@@./roles/drop.sql
@@./users/drop.sql
@@./contexts/drop.sql

spool off
exit