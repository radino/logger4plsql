CREATE OR REPLACE VIEW V_GLOBAL_APPENDER_PARAM AS
SELECT a.appender,
       CAST(regexp_substr(gc.attribute, '(.*)(#)', 1, 1, NULL, 1) AS VARCHAR2(30)) app,
       CAST(regexp_substr(gc.attribute, '(#)(.*)', 1, 1, NULL, 2) AS VARCHAR2(30)) param_name,
       gc.VALUE param_value
  FROM t_appender a
 INNER JOIN global_context gc ON (a.base_context_name || '_G' = gc.namespace)
 WHERE gc.attribute NOT IN ('INITIALIZED', 'DEFAULT#CODE');
