create table t_app_appender
(
  app             varchar2(30) not null,
  appender_code   number(10) not null,
  parameter_name  varchar2(30) not null,
  parameter_value varchar2(4000),
  constraint pk_app_appender primary key (app, appender_code, parameter_name)
)
organization index
tablespace &index_tablespace_small
overflow tablespace &table_tablespace_small;

alter table t_app_appender
  add constraint fk_app_appender foreign key (appender_code)
  references t_appender (code);

alter table t_app_appender
  add constraint fk_t_app_appender_app foreign key (app)
  references t_app (app);

alter table t_app_appender
  add constraint chk_app_appender_upper
  check (parameter_name=upper(parameter_name));

comment on table t_app_appender
  is 'Appender settings for application';
comment on column t_app_appender.app
  is 'Application code';
comment on column t_app_appender.appender_code
  is 'Appender code';
comment on column t_app_appender.parameter_name
  is 'Parameter name for (application, appender)';
comment on column t_app_appender.parameter_value
  is 'Parameter value';

