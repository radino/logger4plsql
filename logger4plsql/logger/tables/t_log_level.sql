create table t_log_level
(
  log_level varchar2(32) not null,
  severity  number(5) not null,
  descr     varchar2(200) not null,
  constraint pk_log_level primary key (log_level)
)
organization index
tablespace &index_tablespace_small;

comment on table t_log_level is 'Log levels'

comment on column t_log_level.log_level
  is 'Log level (primary key)';
comment on column t_log_level.severity
  is 'Severity code. The less severity imply more logging';
comment on column t_log_level.descr
  is 'Log level description';
