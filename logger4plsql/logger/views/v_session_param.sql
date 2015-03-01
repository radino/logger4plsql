CREATE OR REPLACE VIEW V_SESSION_PARAM AS
SELECT CAST (regexp_substr(sc.attribute, '(.*)(#)', 1, 1, NULL, 1) AS VARCHAR2(30)) app,
       CAST (regexp_substr(sc.attribute, '(#)(.*)', 1, 1, NULL, 2) AS VARCHAR2(30)) param_name,
       sc.VALUE param_value
  FROM session_context sc
 WHERE sc.attribute NOT IN ('INITIALIZED')
   AND sc.namespace = 'CTX_LOGGER_PARAMS_L';
