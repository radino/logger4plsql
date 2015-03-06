create table t_app
(
  app      varchar2(30) not null,
  app_desc varchar2(200) not null,
  constraint pk_app primary key (app)
)
organization index
  tablespace &index_tablespace_small;

alter table t_app
  add constraint chk_app_upper
  check (upper(app)=app);

comment on table t_app
  is 'table ';
comment on column t_app.app
  is 'application name';
comment on column t_app.app_desc
  is 'application description';
