CREATE OR REPLACE VIEW V_SESSION_APPENDER_PARAM AS
SELECT a.appender,
       CAST(regexp_substr(sc.attribute, '(.*)(#)', 1, 1, NULL, 1) AS VARCHAR2(30)) app,
       CAST(regexp_substr(sc.attribute, '(#)(.*)', 1, 1, NULL, 2) AS VARCHAR2(30)) param_name,
       sc.VALUE param_value
  FROM t_appender a
 INNER JOIN session_context sc ON (a.base_context_name || '_L' = sc.namespace)
 WHERE sc.attribute NOT IN ('INITIALIZED', 'DEFAULT#CODE');
