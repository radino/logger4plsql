create table t_logger
(
  logger     varchar2(255) not null,
  log_level  varchar2(32),
  appenders  number(10) default 0 not null,
  additivity number(1) default 1 not null,
  constraint pk_logger primary key (logger)
)
organization index
tablespace &index_tablespace_small;

alter table t_logger
  add constraint fk_logger_log_level foreign key (log_level)
  references t_log_level (log_level);

alter table t_logger
  add constraint chk_logger_additivity check (additivity in (0,1));

comment on table t_logger is 'Settings for logger'

comment on column t_logger.logger
  is 'Logger name';
comment on column t_logger.log_level
  is 'Log level for logger';
comment on column t_logger.appenders
  is 'Appenders for logger. Binary coded (with OR)';
comment on column t_logger.additivity
  is 'Additivity flag (cumulation of appenders) for logger';
