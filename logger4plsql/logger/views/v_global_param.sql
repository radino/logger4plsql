CREATE OR REPLACE VIEW V_GLOBAL_PARAM AS
SELECT CAST(regexp_substr(gc.attribute, '(.*)(#)', 1, 1, NULL, 1) AS VARCHAR2(30)) app,
       CAST(regexp_substr(gc.attribute, '(#)(.*)', 1, 1, NULL, 2) AS VARCHAR2(30)) param_name,
       gc.VALUE param_value
  FROM global_context gc
 WHERE gc.attribute NOT IN ('INITIALIZED')
   AND gc.namespace = 'CTX_LOGGER_PARAMS_G';
