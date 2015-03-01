create table t_log
(
  id         number not null,
  logger     varchar2(255) not null,
  message    varchar2(4000),
  log_date   timestamp(6) default systimestamp not null,
  call_stack varchar2(2000),
  backtrace  varchar2(4000),
  log_level  varchar2(32) not null
) tablespace &table_tablespace_large
;

comment on table t_log
  is 'Logging table';

comment on column t_log.id
  is 'Primary key';
comment on column t_log.logger
  is 'Logger name';
comment on column t_log.message
  is 'Logged message';
comment on column t_log.log_date
  is 'Log date';
comment on column t_log.call_stack
  is 'Call stack';
comment on column t_log.backtrace
  is 'Backtrace';
comment on column t_log.log_level
  is 'Log level';
  
alter table t_log
  add constraint pk_log primary key (id)
  using index tablespace &index_tablespace_large;
