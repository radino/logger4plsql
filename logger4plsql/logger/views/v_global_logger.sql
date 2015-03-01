CREATE OR REPLACE VIEW V_GLOBAL_LOGGER AS
SELECT CAST(logger_name.VALUE AS VARCHAR2(30)) AS logger,
       CAST(logger_level.VALUE AS VARCHAR2(32)) AS log_level,
       CAST(logger_appenders.VALUE AS NUMBER) AS appenders,
       CAST(logger_additivity.VALUE AS NUMBER) AS additivity
  FROM global_context logger_name
  LEFT OUTER JOIN global_context logger_level ON (logger_level.namespace = 'CTX_LOGGER_LEV_G' AND
                                                 logger_name.attribute = logger_level.attribute)
  LEFT OUTER JOIN global_context logger_appenders ON (logger_appenders.namespace = 'CTX_LOGGER_APP_G' AND
                                                     logger_name.attribute = logger_appenders.attribute)
  LEFT OUTER JOIN global_context logger_additivity ON (logger_additivity.namespace = 'CTX_LOGGER_ADD_G' AND
                                                      logger_name.attribute = logger_additivity.attribute)
 WHERE logger_name.namespace = 'CTX_LOGGER_NAME_G'
   AND logger_name.attribute <> 'INITIALIZED';
