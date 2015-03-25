rem using execute immediate because substitution variable cannot start a command.
begin 
 &drop_schema dbms_output.put_line('killing connected sessions');
 &drop_schema  for i in (select * from v$session where username = upper('&logger_user')) loop
 &drop_schema    execute immediate 'alter system kill session ''' || i.sid || ',' || i.serial# || ''' immediate'; 
 &drop_schema    dbms_output.put_line('killing connected sessions: ' || i.sid || ',' || i.serial#);
 &drop_schema   end loop;

 &drop_schema execute immediate 'drop user &logger_user cascade';
 &drop_schema dbms_output.put_line('user &logger_user dropped');
 null;
end;
/

