#!/bin/bash
NLS_LANG=AMERICAN_AMERICA.US7ASCII
export NLS_LANG;

cd sys
sqlplus /nolog <<EOF
@sys.sql
EOF
cd ..

cd logger 
sqlplus /nolog <<EOF
@logger.sql
EOF
cd ..

