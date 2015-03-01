CREATE USER &logger_user
IDENTIFIED BY &logger_password
DEFAULT TABLESPACE &table_tablespace_large
QUOTA &table_small_quota ON &table_tablespace_small
QUOTA &table_large_quota ON &table_tablespace_large
QUOTA &index_small_quota ON &index_tablespace_small
QUOTA &index_large_quota ON &index_tablespace_large;


