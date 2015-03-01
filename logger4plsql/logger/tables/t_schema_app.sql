create table t_schema_app
(
  schema varchar2(30) not null,
  app      varchar2(30) not null,
  constraint pk_schema_app primary key (schema,app)
) 
organization index
tablespace &index_tablespace_small;

alter table t_schema_app
  add constraint chck_schema_app_upper
  check (upper(app)=app and upper(schema)=schema);

comment on table t_schema_app is 'Schema - application mapping';

comment on column t_schema_app.schema
  is 'Schema name';
comment on column t_schema_app.app
  is 'Application to which schema belongs';
