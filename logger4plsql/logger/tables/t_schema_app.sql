create table t_schema_app
(
  schema varchar2(30) not null,
  app      varchar2(30) not null,
  constraint pk_schema_app primary key (schema,app)
) 
organization index
tablespace &index_tablespace_small;

alter table t_schema_app
  add constraint chk_schema_app_upper
  check (upper(schema)=schema);

alter table t_schema_app
  add constraint fk_schema_app_app foreign key (app)
  references t_app (app);

comment on table t_schema_app is 'Schema - application mapping';

comment on column t_schema_app.schema
  is 'Schema name';
comment on column t_schema_app.app
  is 'Application to which schema belongs';
