#!/bin/bash
NLS_LANG=AMERICAN_AMERICA.US7ASCII
export NLS_LANG;

cd logger 
sqlplus /nolog <<EOF
@drop.sql
EOF
cd ..


cd sys
sqlplus /nolog <<EOF
@drop.sql
EOF
cd ..



