create table t_app_appender
(
  app             varchar2(30) not null,
  appender        varchar2(30) not null,
  parameter_name  varchar2(30) not null,
  parameter_value varchar2(4000),
  constraint pk_app_appender primary key (app, appender, parameter_name)
)
organization index
tablespace &index_tablespace_small
overflow tablespace &table_tablespace_small;

alter table t_app_appender
  add constraint fk_app_appender foreign key (appender)
  references t_appender (appender);

alter table t_app_appender
  add constraint chck_app_appender_upper
  check (upper(app)=app and upper(appender)=appender and parameter_name=upper(parameter_name));

comment on table t_app_appender
  is 'Appender settings for application';
comment on column t_app_appender.app
  is 'Application code';
comment on column t_app_appender.appender
  is 'Appender name';
comment on column t_app_appender.parameter_name
  is 'Parameter name for (application, appender)';
comment on column t_app_appender.parameter_value
  is 'Parameter value';

