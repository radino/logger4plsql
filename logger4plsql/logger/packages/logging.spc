CREATE OR REPLACE PACKAGE logging IS
  /**
  * Implementation of log4j for PL/SQL.
  * Licence: MIT License (@see license.txt)
  * {*} web                 http://radino.eu
  * {*} email, gtalk(XMPP)  radoslav.golian@gmail.com, rgolian@gmail.com
  * {*} facebook            http://www.facebook.com/radoslav.golian
  * {*} twitter             http://twitter.com/radoslavgolian
  * (*) project page        https://github.com/radino/logger4plsql/
  * @author Radoslav Golian
  */

  /** Type for parameter names. */
  TYPE param_names_type IS TABLE OF user_objects.object_name%TYPE;

  /** Type for parameter values. */
  TYPE param_values_type IS TABLE OF VARCHAR2(32767);

  /** Type for log message */
  SUBTYPE message_type IS VARCHAR2(32767);

  /** Subtype for attribute name. */
  SUBTYPE ctx_attribute_type IS global_context.attribute%TYPE;

  /** Subtype for context namespace. */
  SUBTYPE ctx_namespace_type IS global_context.namespace%TYPE;

  /** Subtype for context aribute value. */
  SUBTYPE ctx_value_type IS global_context.VALUE%TYPE;

  /** Type for logger name hash. */
  SUBTYPE hash_type IS NUMBER; -- dbms_utility.get_hash_value;

  /** Type for appender settings. */
  TYPE appender_params_type IS TABLE OF t_app_appender.parameter_value%TYPE INDEX BY t_app_appender.parameter_name%TYPE;

  /** Type for context "boolean" */
  SUBTYPE ctx_boolean IS VARCHAR2(1);

  /** Constant for TRUE in a application context - application context can not handle booleans */
  c_true CONSTANT ctx_boolean := 'T';

  /** Constant for FALSE in a application context - application context can not handle booleans */
  c_false CONSTANT ctx_boolean := 'F';

  /** Constant indicating whether current database version is 11.2 or greater. */
  ver_ge_11_2 CONSTANT BOOLEAN :=
    $IF (dbms_db_version.version > 11) -- greater then 11
     OR (dbms_db_version.version = 11 AND dbms_db_version.release >= 2) -- or equal to 11.2
    $THEN TRUE
    $ELSE FALSE
    $END;

  /** Type for settings of all appenders. */
  TYPE appenders_params_type IS TABLE OF appender_params_type INDEX BY PLS_INTEGER; -- t_appender.code;

  /** Type for logger parameters */
  TYPE logger_type IS RECORD(
    always_from_ctx    ctx_boolean,
    logger             t_logger.logger%TYPE,
    log_level_severity t_log_level.severity%TYPE,
    enabled_appenders  t_logger.appenders%TYPE,
    app                t_app_appender.app%TYPE,
    appenders_params   appenders_params_type -- currently not used
  );

  /** Log level ALL. */
  c_all_level CONSTANT t_log_level.log_level%TYPE := 'ALL';

  /** Log level TRACE. */
  c_trace_level CONSTANT t_log_level.log_level%TYPE := 'TRACE';

  /** Log level DEBUG. */
  c_debug_level CONSTANT t_log_level.log_level%TYPE := 'DEBUG';

  /** Log level INFO. */
  c_info_level CONSTANT t_log_level.log_level%TYPE := 'INFO';

  /** Log level WARN. */
  c_warn_level CONSTANT t_log_level.log_level%TYPE := 'WARN';

  /** Log level ERROR. */
  c_error_level CONSTANT t_log_level.log_level%TYPE := 'ERROR';

  /** Log level FATAL. */
  c_fatal_level CONSTANT t_log_level.log_level%TYPE := 'FATAL';

  /** Log level OFF. */
  c_off_level CONSTANT t_log_level.log_level%TYPE := 'OFF';

  /** TABLE appender. */
  c_table_appender CONSTANT t_appender.appender%TYPE := 'TABLE';

  /** DBMS_OUTPUT appender. */
  c_dbms_output_appender CONSTANT t_appender.appender%TYPE := 'DBMS_OUTPUT';

  /** SMTP appender. */
  c_smtp_appender CONSTANT t_appender.appender%TYPE := 'SMTP';

  /**
  * Function returns bitwise OR of given numbers.
  * @param x_n1 First operand.
  * @param x_n2 Second operand.
  * @return Bitwise OR of given numbers.
  */
  FUNCTION bit_or(x_n1 IN NUMBER,
                  x_n2 IN NUMBER) RETURN NUMBER;

  /**
  * Function returns bitwise exclusive OR of given numbers.
  * @param x_n1 First operand.
  * @param x_n2 Second operand.
  * @return Bitwise exclusive OR of given numbers.
  */
  FUNCTION bit_xor(x_n1 IN NUMBER,
                   x_n2 IN NUMBER) RETURN NUMBER;

  /**
  * Funtion creates a XML string containing given parameters.
  * @param x_names Parameter names.
  * @param x_values Parameter values.
  * @return XML string containing given parameters.
  */
  FUNCTION serialize_to_xml(x_names  IN param_names_type,
                            x_values IN param_values_type) RETURN VARCHAR2;

  /**
  * Funtion creates a JSON string containing given parameters.
  * @param x_names Parameter names.
  * @param x_values Parameter values.
  * @return a JSON string containing given parameters.
  */
  FUNCTION serialize_to_json(x_names  IN param_names_type,
                             x_values IN param_values_type) RETURN VARCHAR2;

  /**
  * Returns a logger configuration for given method.
  * @param x_method Name of method.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  FUNCTION get_logger(x_method          IN VARCHAR2 DEFAULT NULL,
                      x_always_from_ctx IN ctx_boolean DEFAULT c_false) RETURN logger_type;

  /**
  * Returns the root logger configuration.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  FUNCTION get_root_logger(x_always_from_ctx IN ctx_boolean DEFAULT c_false) RETURN logger_type;

  /**
  * Returns the application logger configuration.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  FUNCTION get_app_logger(x_always_from_ctx IN ctx_boolean DEFAULT c_false) RETURN logger_type;

  /**
  * Returns the schema logger configuration.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  FUNCTION get_schema_logger(x_always_from_ctx IN ctx_boolean DEFAULT c_false) RETURN logger_type;

  /**
  * Procedure adds given schema to given application.
  * @param x_app Application name.
  * @param x_schema Schema name.
  */
  PROCEDURE add_schema_to_app(x_app    IN t_schema_app.app%TYPE,
                              x_schema IN t_schema_app.SCHEMA%TYPE DEFAULT user());

  /**
  * Procedure removes given schema from given application.
  * @param x_app Application name.
  * @param x_schema Schema name.
  */
  PROCEDURE remove_schema_from_app(x_app    IN t_schema_app.app%TYPE,
                                   x_schema IN t_schema_app.SCHEMA%TYPE DEFAULT user());

  /**
  * Procedure sets global parameter value for given app and parameter name.
  * @param x_app Application.
  * @param x_param_name Parameter name.
  * @param x_param_value Parameter value.
  */
  PROCEDURE set_global_parameter(x_app         IN t_param.app%TYPE,
                                 x_param_name  IN t_param.param_name%TYPE,
                                 x_param_value IN t_param.param_value%TYPE);

  /**
  * Procedure sets session parameter value for given app and parameter name.
  * @param x_app Application.
  * @param x_param_name Parameter name.
  * @param x_param_value Parameter value.
  */
  PROCEDURE set_session_parameter(x_app         IN t_param.app%TYPE,
                                  x_param_name  IN t_param.param_name%TYPE,
                                  x_param_value IN t_param.param_value%TYPE);

  /**
  * Procedure sets global layout for given appender and given application.
  * @param x_app Application name.
  * @param x_appender Appender name.
  * @param x_layout Layout.
  */
  PROCEDURE set_global_layout(x_app      IN t_app_appender.app%TYPE,
                              x_appender IN t_app_appender.appender%TYPE,
                              x_layout   IN t_app_appender.parameter_value%TYPE);

  /**
  * Procedure sets session layout for given appender and given application.
  * @param x_app Application name.
  * @param x_appender Appender name.
  * @param x_layout Layout.
  */
  PROCEDURE set_session_layout(x_app      IN t_app_appender.app%TYPE,
                               x_appender IN t_app_appender.appender%TYPE,
                               x_layout   IN t_app_appender.parameter_value%TYPE);

  /**
  * Procedure sets global value for given parameter name, appender and application.
  * @param x_app Application name.
  * @param x_appender Appender name.
  * @param x_parameter_name Parameter name.
  * @param x_parameter_value Parameter value.
  */
  PROCEDURE set_global_appender_param(x_app             IN t_app_appender.app%TYPE,
                                      x_appender        IN t_app_appender.appender%TYPE,
                                      x_parameter_name  IN t_app_appender.parameter_name%TYPE,
                                      x_parameter_value IN t_app_appender.parameter_value%TYPE);

  /**
  * Procedure sets session value for given parameter name, appender and application.
  * @param x_app Application name.
  * @param x_appender Appender name.
  * @param x_parameter_name Parameter name.
  * @param x_parameter_value Parameter value.
  */
  PROCEDURE set_session_appender_param(x_app             IN t_app_appender.app%TYPE,
                                       x_appender        IN t_app_appender.appender%TYPE,
                                       x_parameter_name  IN t_app_appender.parameter_name%TYPE,
                                       x_parameter_value IN t_app_appender.parameter_value%TYPE);

  /**
  * Procedure sets global log level for given logger.
  * @param x_logger Logger name.
  * @param x_log_level Log level.
  */
  PROCEDURE set_global_level(x_logger_name IN t_logger.logger%TYPE,
                             x_log_level   IN t_logger.log_level%TYPE);

  /**
  * Procedure sets session log level for given logger.
  * @param x_logger Logger name.
  * @param x_log_level Log level.
  */
  PROCEDURE set_session_level(x_logger_name IN t_logger.logger%TYPE,
                              x_log_level   IN t_logger.log_level%TYPE);

  /**
  * Procedure adds given global appenders to given logger and sets global additivity flag for the logger.
  * @param x_logger_name Logger name.
  * @param x_appender Binary coded Appender list.
  * @param x_additivity Additivity flag.
  */
  PROCEDURE add_global_appender(x_logger_name IN t_logger.logger%TYPE,
                                x_appender    IN t_appender.appender%TYPE,
                                x_additivity  IN BOOLEAN DEFAULT TRUE);

  /**
  * Procedure adds given session appenders to given logger and sets session additivity flag for the logger.
  * @param x_logger_name Logger name.
  * @param x_appender Binary coded Appender list.
  * @param x_additivity Additivity flag.
  */
  PROCEDURE add_session_appender(x_logger_name IN t_logger.logger%TYPE,
                                 x_appender    IN t_appender.appender%TYPE,
                                 x_additivity  IN BOOLEAN DEFAULT TRUE);

  /**
  * Procedure sets global additivity flag for given logger.
  * @param x_logger_name Loger name.
  * @param x_additivity Additivity flag.
  */
  PROCEDURE set_global_additivity(x_logger_name IN t_logger.logger%TYPE,
                                  x_additivity  IN BOOLEAN);

  /**
  * Procedure sets session additivity flag for given logger.
  * @param x_logger_name Loger name.
  * @param x_additivity Additivity flag.
  */
  PROCEDURE set_session_additivity(x_logger_name IN t_logger.logger%TYPE,
                                   x_additivity  IN BOOLEAN);

  /**
  * Procedure removes givne global appenders from given logger.
  * @param x_logger_name Logger name.
  * @param x_appender Binary coded Appender list.
  */
  PROCEDURE remove_global_appender(x_logger_name IN t_logger.logger%TYPE,
                                   x_appender    IN t_appender.appender%TYPE);

  /**
  * Procedure removes given session appenders from given logger.
  * @param x_logger_name Logger name.
  * @param x_appender Binary coded Appender list.
  */
  PROCEDURE remove_session_appender(x_logger_name IN t_logger.logger%TYPE,
                                    x_appender    IN t_appender.appender%TYPE);

  /**
  * Procedure logs message with TRACE log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  PROCEDURE trace(x_logger         IN OUT NOCOPY logger_type,
                  x_message        IN message_type DEFAULT SQLERRM,
                  x_log_backtrace  IN BOOLEAN DEFAULT FALSE,
                  x_log_call_stack IN BOOLEAN DEFAULT TRUE);

  /**
  * Procedure logs message with INFO log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  PROCEDURE info(x_logger         IN OUT NOCOPY logger_type,
                 x_message        IN message_type DEFAULT SQLERRM,
                 x_log_backtrace  IN BOOLEAN DEFAULT FALSE,
                 x_log_call_stack IN BOOLEAN DEFAULT TRUE);

  /**
  * Procedure logs message with DEBUG log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  PROCEDURE debug(x_logger         IN OUT NOCOPY logger_type,
                  x_message        IN message_type DEFAULT SQLERRM,
                  x_log_backtrace  IN BOOLEAN DEFAULT FALSE,
                  x_log_call_stack IN BOOLEAN DEFAULT TRUE);

  /**
  * Procedure logs message with WARN log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  PROCEDURE warn(x_logger         IN OUT NOCOPY logger_type,
                 x_message        IN message_type DEFAULT SQLERRM,
                 x_log_backtrace  IN BOOLEAN DEFAULT TRUE,
                 x_log_call_stack IN BOOLEAN DEFAULT TRUE);

  /**
  * Procedure logs message with ERROR log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  PROCEDURE error(x_logger         IN OUT NOCOPY logger_type,
                  x_message        IN message_type DEFAULT SQLERRM,
                  x_log_backtrace  IN BOOLEAN DEFAULT TRUE,
                  x_log_call_stack IN BOOLEAN DEFAULT TRUE);

  /**
  * Procedure logs message with FATAL log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  PROCEDURE fatal(x_logger         IN OUT NOCOPY logger_type,
                  x_message        IN message_type DEFAULT SQLERRM,
                  x_log_backtrace  IN BOOLEAN DEFAULT TRUE,
                  x_log_call_stack IN BOOLEAN DEFAULT TRUE);

  /**
  * Function checks whether TRACE log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is TRACE or lower.
  * {*} FALSE if log level for given logger is higher than TRACE.
  */
  FUNCTION is_trace_enabled(x_logger IN OUT NOCOPY logger_type) RETURN BOOLEAN;

  /**
  * Function checks whether DEBUG log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is DEBUG or lower.
  * {*} FALSE if log level for given logger is higher than DEBUG.
  */
  FUNCTION is_debug_enabled(x_logger IN OUT NOCOPY logger_type) RETURN BOOLEAN;

  /**
  * Function checks whether INFO log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is INFO or lower.
  * {*} FALSE if log level for given logger is higher than INFO.
  */
  FUNCTION is_info_enabled(x_logger IN OUT NOCOPY logger_type) RETURN BOOLEAN;

  /**
  * Function checks whether WARN log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is WARN or lower.
  * {*} FALSE if log level for given logger is higher than WARN.
  */
  FUNCTION is_warn_enabled(x_logger IN OUT NOCOPY logger_type) RETURN BOOLEAN;

  /**
  * Function checks whether ERROR log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is ERROR or lower.
  * {*} FALSE if log level for given logger is higher than ERROR.
  */
  FUNCTION is_error_enabled(x_logger IN OUT NOCOPY logger_type) RETURN BOOLEAN;

  /**
  * Function checks, whether FATAL log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is FATAL or lower.
  * {*} FALSE if log level for given logger is higher than FATAL.
  */
  FUNCTION is_fatal_enabled(x_logger IN OUT NOCOPY logger_type) RETURN BOOLEAN;

  /**
  * Procedure sets flag, which indicates that session setting (session context) will be used for logging.
  * @param x_usage Flag, which indicates that session setting (session context) will be used for logging.
  * {*} TRUE Session settings will be used (session context)
  * {*} FALSE Global settings will be used (global context)
  */
  PROCEDURE set_session_ctx_usage(x_usage IN BOOLEAN);

  /** Procedure purges all global contexts used by logging */
  PROCEDURE purge_global_contexts;

  /** Procedure purges all session contexts used by logging */
  PROCEDURE purge_session_contexts;

  /** Procedure copies global setting to session settings */
  PROCEDURE copy_global_to_session;

  /**
  * Procedure sets given attribute of given context to given value.
  * For internal use only. Please do not use.
  * @param x_namespace Name of context
  * @param x_attribute Attribute
  * @param x_value Value of attribute
  * @raises e_internal_use_only Can not be called from another schema.
  */
  PROCEDURE set_context(x_namespace IN ctx_namespace_type,
                        x_attribute IN ctx_attribute_type,
                        x_value     IN ctx_value_type);
END logging;
/
