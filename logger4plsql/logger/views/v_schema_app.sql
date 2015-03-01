CREATE OR REPLACE VIEW V_SCHEMA_APP AS
SELECT CAST(sa.value AS VARCHAR2(30)) application,
       CAST(sa.attribute AS VARCHAR2(30)) schema
  FROM global_context sa
 WHERE sa.namespace = 'CTX_LOGGER_USER_APP_G'
 AND sa.attribute <> 'INITIALIZED';
