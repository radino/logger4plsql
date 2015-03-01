insert into T_LOG_LEVEL (LOG_LEVEL, SEVERITY, DESCR)
values ('ALL', -99999, 'The ALL has the lowest possible rank and is intended to turn on all logging.');
insert into T_LOG_LEVEL (LOG_LEVEL, SEVERITY, DESCR)
values ('TRACE', 10000, 'The TRACE Level designates finer-grained informational events than the DEBUG');
insert into T_LOG_LEVEL (LOG_LEVEL, SEVERITY, DESCR)
values ('DEBUG', 20000, 'The DEBUG Level designates fine-grained informational events that are most useful to debug an application.');
insert into T_LOG_LEVEL (LOG_LEVEL, SEVERITY, DESCR)
values ('INFO', 30000, 'The INFO level designates informational messages that highlight the progress of the application at coarse-grained level.');
insert into T_LOG_LEVEL (LOG_LEVEL, SEVERITY, DESCR)
values ('WARN', 40000, 'The WARN level designates potentially harmful situations.');
insert into T_LOG_LEVEL (LOG_LEVEL, SEVERITY, DESCR)
values ('ERROR', 50000, 'The ERROR level designates error events that might still allow the application to continue running.');
insert into T_LOG_LEVEL (LOG_LEVEL, SEVERITY, DESCR)
values ('FATAL', 60000, 'The FATAL level designates very severe error events that will presumably lead the application to abort.');
insert into T_LOG_LEVEL (LOG_LEVEL, SEVERITY, DESCR)
values ('OFF', 99999, 'The OFF has the highest possible rank and is intended to turn off logging.');
