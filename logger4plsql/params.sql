set echo on
define YES=""
define NO="--"


rem Database
rem ========

rem database connection string
rem examples: 
rem   tnsnames: orcl, 
rem   easy connect: 192.168.1.1:1521/orcl
rem   easy connect with domain: 192.168.1.1:1521/orcl.acme.com
rem   tnsnames full:  (DESCRIPTION=(ADDRESS=(PROTOCOL=TCP)(HOST=192.168.1.1)(PORT=1521))(CONNECT_DATA=(SERVER=DEDICATED)(SERVICE_NAME=orcl)))
define database=clstr11

rem Roles and users settings
rem ========================

rem Username for logger
rem Logging system will be installed into this schema
define logger_user=logger

rem A role which contains all neccessary privileges for logger_user
rem see script sys/grants/logger_role.sql
rem 
define logger_role=logger_role

rem Quotas and tablespaces
rem ======================

rem You can separate indexes and tables into different tablespaces if you want
define table_tablespace_small=USERS
define table_tablespace_large=USERS
define index_tablespace_small=USERS
define index_tablespace_large=USERS

rem You can define quotas for the tablespaces
define table_small_quota=10M
define table_large_quota=1G
define index_small_quota=10M
define index_large_quota=1G

rem Connection parameters 
rem =====================

rem Password for SYS
define sys_password=oracle

rem Connection string for sys account.
rem String must be accepted by sqlplus.
define sys_connect="sys/&sys_password@&database as sysdba"

rem Password for logger user
define logger_password=logger

rem Connection string for logger account.
rem String must be accepted by sqlplus.
define logger_connect="&logger_user/&logger_password@&database"

