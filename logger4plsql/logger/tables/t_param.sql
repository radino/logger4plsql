create table t_param
(
  app         varchar2(30),
  param_name  varchar2(30) not null,
  param_value varchar2(4000) not null,
  constraint pk_param primary key (app, param_name)
)
organization index 
tablespace &index_tablespace_small
overflow 
tablespace &table_tablespace_small
;

alter table t_param
  add constraint chk_param_upper
  check (upper(param_name)=param_name);

alter table t_param
  add constraint fk_param_app foreign key (app)
  references t_app (app);

comment on table t_param is 'Parameters for logging system'

comment on column t_param.app
  is 'Application to which parameter belongs';
comment on column t_param.param_name
  is 'Parameter name';
comment on column t_param.param_value
  is 'Parameter value';
