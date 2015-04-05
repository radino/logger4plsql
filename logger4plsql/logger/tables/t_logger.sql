create table t_logger
(
  logger     varchar2(255) not null,
  log_level  number(5),
  appenders  number(10) default 0 not null,
  additivity number(1) default 1 not null,
  backtrace  number(1) default 0 not null,
  callstack  number(1) default 0 not null,
  constraint pk_logger primary key (logger)
)
organization index
tablespace &index_tablespace_small;

alter table t_logger
  add constraint chk_logger_additivity check (additivity in (0,1));

comment on table t_logger is 'Settings for logger'

comment on column t_logger.logger
  is 'Logger name';

comment on column t_logger.log_level
  is 'Log level for logger is represented by severity
-99999:The ALL has the lowest possible rank and is intended to turn on all logging.
10000:The TRACE Level designates finer-grained informational events than the DEBUG
20000:The DEBUG Level designates fine-grained informational events that are most useful to debug an application.
30000:The INFO level designates informational messages that highlight the progress of the application at coarse-grained level.
40000:The WARN level designates potentially harmful situations.
50000:The ERROR level designates error events that might still allow the application to continue running.
60000:The FATAL level designates very severe error events that will presumably lead the application to abort.
99999:The OFF has the highest possible rank and is intended to turn off logging.';

comment on column t_logger.appenders
  is 'Appenders for logger. Binary coded (with OR)';
comment on column t_logger.additivity
  is 'Additivity flag (cumulation of appenders) for logger';
comment on column t_logger.backtrace
  is 'Backtrace flag (logging backtrace) for logger';
comment on column t_logger.callstack
  is 'Callstack flag (logging callstack) for logger';


