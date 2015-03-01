set NLS_LANG=AMERICAN_AMERICA.US7ASCII

cd sys
sqlplus /nolog @sys.sql
cd ..


cd logger
sqlplus /nolog @logger.sql
cd ..


