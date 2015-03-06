create table t_appender
(
  appender          varchar2(30) not null,
  code              number(38) not null,
  base_context_name varchar2(28) not null,
  constraint pk_appender primary key (appender)
)
organization index
tablespace &index_tablespace_small;

alter table t_appender
  add constraint chk_appender_upper
  check (upper(base_context_name)=base_context_name and upper(appender)=appender);

comment on table t_appender
  is 'Appenders';

comment on column t_appender.appender
  is 'Appender name';
comment on column t_appender.code
  is 'Appender code: 2^n';
comment on column t_appender.base_context_name
  is 'Basename for the appender''s contexts. Global context has a suffix _G. Local context has a suffix _L.';
