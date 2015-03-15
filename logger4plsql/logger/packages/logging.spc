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
  * @usage The package may be compiled with these flags:
  *  {*} debug      If set on TRUE, the package is compiled with internal debugging
  *  {*} unit_test  If set on TRUE, all the methods are defined as public to support unit testing.
  * e.g. These settings are used for development of the logging functionality:
  * ALTER PACKAGE logging COMPILE PLSQL_CCFLAGS = 'debug:TRUE, unit_test:TRUE' REUSE SETTINGS
  * and these settings should be used for production
  * ALTER PACKAGE logging COMPILE PLSQL_CCFLAGS = 'debug:FALSE, unit_test:FALSE' REUSE SETTINGS
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
  ver_lt_11_2 CONSTANT BOOLEAN :=
    $IF (dbms_db_version.version < 11) -- less than 11
     OR (dbms_db_version.version = 11 AND dbms_db_version.release < 2) -- or less than 11.2
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
  
  /** Type used for serialization of loggers parameters */
  TYPE logger_settings_type IS RECORD (
     enabled_appenders  t_logger.appenders%TYPE,
     log_level          t_logger.log_level%TYPE,
     additivity         t_logger.additivity%TYPE       
  );
  
  /** Collection type of loggers used for serialization of loggers parameters */
  TYPE logger_settings_col_type IS TABLE OF logger_settings_type INDEX BY t_logger.logger%TYPE;
  
  /** Type for serialized logging settings. */
  TYPE serialized_settings_type IS RECORD (
     loggers          logger_settings_col_type,
     app_params       appender_params_type,
     appenders_params appenders_params_type
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

  -- these elements are defined only if internal debugging is set to TRUE
  $IF $$debug $THEN
     g_internal_log_level t_log_level.log_level%TYPE := c_trace_level;
     g_internal_appenders PLS_INTEGER := 2;
  $END

  -- these elements are public only when unit testing precompiler option is set to TRUE
  $IF $$unit_test $THEN
    -- types and variables
    SUBTYPE visibility_type IS PLS_INTEGER RANGE 1 .. 2;
    c_global_flag CONSTANT visibility_type := 1;
    c_session_flag CONSTANT visibility_type := 2;

    -- methods
    -- internal debugger private methods
    $IF $$debug $THEN
    PROCEDURE internal_log(x_level    IN t_log_level.log_level%TYPE,
                         x_logger   IN t_logger.logger%TYPE,
                         x_message  IN message_type,
                         x_appender IN PLS_INTEGER DEFAULT g_internal_appenders);
    PROCEDURE init_log_level_severities;
    $END
    FUNCTION bool_to_int(x_boolean IN BOOLEAN) RETURN NUMBER;
    PROCEDURE clear_all_context_rac_aware(x_namespace IN ctx_namespace_type);
    FUNCTION dequeue_from_cyclic_buffer RETURN VARCHAR2;
    PROCEDURE enqueue_into_cyclic_buffer(x_app IN VARCHAR2, x_message IN message_type);
    FUNCTION format_call_stack RETURN VARCHAR2;
    FUNCTION format_message(x_message     IN message_type,
                          x_layout      IN t_app_appender.parameter_value%TYPE,
                          x_logger_name IN t_logger.logger%TYPE,
                          x_level       IN t_logger.log_level%TYPE) RETURN VARCHAR2;
    FUNCTION get_app(x_schema IN t_schema_app.schema%TYPE) RETURN t_schema_app.app%TYPE;
    FUNCTION get_appender_code(x_appender IN t_appender.appender%TYPE) RETURN t_appender.code%TYPE;
    FUNCTION get_appender_param(x_app            IN t_app_appender.app%TYPE,
                                x_appender       IN t_app_appender.appender%TYPE,
                                x_parameter_name IN t_app_appender.parameter_name%TYPE,
                                x_visibility     IN visibility_type DEFAULT c_global_flag) RETURN t_app_appender.parameter_value%TYPE;  
    FUNCTION get_appenders(x_logger_name  IN t_logger.logger%TYPE,
                               x_app_ctx_name IN ctx_namespace_type,
                               x_add_ctx_name IN ctx_namespace_type) RETURN t_logger.appenders%TYPE;
    FUNCTION get_current_appender_param(x_app            IN t_app_appender.app%TYPE,
                                        x_appender       IN t_app_appender.appender%TYPE,
                                        x_parameter_name IN t_app_appender.parameter_name%TYPE) RETURN t_app_appender.parameter_value%TYPE;
    FUNCTION get_current_layout(x_app      IN t_app_appender.app%TYPE,
                                x_appender IN t_app_appender.appender%TYPE) RETURN t_app_appender.parameter_value%TYPE;
    FUNCTION get_current_parameter(x_app        IN t_param.app%TYPE,
                                 x_param_name IN t_param.param_name%TYPE) RETURN t_param.param_value%TYPE;
    FUNCTION get_current_used_appenders(x_logger_name IN t_logger.logger%TYPE) RETURN t_logger.appenders%TYPE;
    FUNCTION get_current_used_level(x_logger_name IN t_logger.logger%TYPE) RETURN t_logger.log_level%TYPE;
    FUNCTION get_layout(x_app        IN t_app_appender.app%TYPE,
                      x_appender   IN t_app_appender.appender%TYPE,
                      x_visibility IN visibility_type DEFAULT c_global_flag)
    RETURN t_app_appender.parameter_value%TYPE;
    FUNCTION get_level(x_logger_name IN t_logger.logger%TYPE,
                     x_ctx_name    IN VARCHAR2) RETURN t_logger.log_level%TYPE;
    FUNCTION get_level_severity(x_level IN t_log_level.log_level%TYPE) RETURN t_log_level.severity%TYPE;    
    FUNCTION get_nth_logger_name(x_logger_name IN t_logger.logger%TYPE,
                                 x_nth         IN PLS_INTEGER) RETURN t_logger.logger%TYPE;
    FUNCTION get_session_ctx_usage RETURN BOOLEAN;
    PROCEDURE init_appenders(x_visibility IN visibility_type DEFAULT c_global_flag);
    PROCEDURE init_email_cyclic_buffer(x_app IN VARCHAR2);
    PROCEDURE init_levels;
    PROCEDURE init_loggers(x_visibility IN visibility_type DEFAULT c_global_flag,
                           x_app        IN t_app.app%TYPE DEFAULT NULL);
    PROCEDURE init_params(x_visibility IN visibility_type DEFAULT c_global_flag,
                          x_app        IN t_param.app%%TYPE DEFAULT NULL);
    PROCEDURE init_session_identifier;
    PROCEDURE init_user_app;
    FUNCTION int_to_bool(x_number IN NUMBER) RETURN BOOLEAN;
    FUNCTION is_cyclic_buffer_empty RETURN BOOLEAN;
    FUNCTION is_initialized(x_ctx IN ctx_namespace_type) RETURN BOOLEAN;
    PROCEDURE log(x_level          IN t_log_level.log_level%TYPE,
                x_logger         IN OUT NOCOPY logger_type,
                x_message        IN message_type,
                x_log_backtrace  IN BOOLEAN,
                x_log_call_stack IN BOOLEAN);
    PROCEDURE log_smtp(x_app           IN t_app_appender.app%TYPE,
                     x_logger_name   IN t_logger.logger%TYPE,
                     x_level         IN t_log_level.log_level%TYPE,
                     x_message       IN message_type,
                     x_call_stack    IN BOOLEAN,
                     x_log_backtrace IN BOOLEAN);
    PROCEDURE log_stdout(x_app           IN t_app_appender.app%TYPE,
                       x_logger_name   IN t_logger.logger%TYPE,
                       x_level         IN t_log_level.log_level%TYPE,
                       x_message       IN message_type,
                       x_call_stack    IN BOOLEAN,
                       x_log_backtrace IN BOOLEAN);
    PROCEDURE log_table(x_app           IN t_app_appender.app%TYPE,
                      x_logger_name   IN t_logger.logger%TYPE,
                      x_level         IN t_log_level.log_level%TYPE,
                      x_message       IN message_type,
                      x_call_stack    IN BOOLEAN,
                      x_log_backtrace IN BOOLEAN);
    PROCEDURE parse_stack(o_logger     OUT VARCHAR2,
                          o_app        OUT VARCHAR2,
                          x_method     IN VARCHAR2 DEFAULT NULL,
                          x_call_stack IN VARCHAR2 DEFAULT dbms_utility.format_call_stack());
    FUNCTION serialize_settings RETURN ctx_value_type;7
    PROCEDURE set_context_rac_aware(x_namespace      IN ctx_namespace_type,
                                    x_attribute      IN ctx_attribute_type,
                                    x_value          IN ctx_value_type);
    PROCEDURE send_buffer(x_app IN t_app_appender.app%TYPE);
    PROCEDURE unimplemented;
    PROCEDURE use_requested_session_settings(x_app IN t_app.app%TYPE);
    PROCEDURE set_context(x_namespace IN ctx_namespace_type,
                          x_attribute IN ctx_attribute_type,
                          x_value     IN ctx_value_type);
  $END

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

  /** Procedure copies global setting to session settings.
  * @param x_app Application name. If set, copying will be done only for the given application.
  */
  PROCEDURE copy_global_to_session(x_app IN t_app.app%TYPE DEFAULT NULL);

  /**
  * Procedure adds an application to the configuration.
  * @param x_app Application name.
  * @param x_app_descr Application description.
  */
  PROCEDURE add_app(x_app IN t_app.app%TYPE,
                    x_app_descr IN t_app.app_desc%TYPE);
                    
  /**
  * Procedure removes the given application and all related configuration.
  * @param x_app Application name.
  */
  PROCEDURE remove_app(x_app IN t_app.app%TYPE);

  /** Procedure shows serialized settings. */
  PROCEDURE show_serialized_settings;

  /**
  * Procedure adds given appender to given logger and sets additivity flag for the logger in serialized settings.
  * @param x_logger_name Logger name.
  * @param x_appender Binary coded appender.
  * @param x_additivity Additivity flag.
  */
  PROCEDURE add_serialized_appender(x_logger_name IN t_logger.logger%TYPE,
                                    x_appender    IN t_appender.appender%TYPE,
                                    x_additivity  IN BOOLEAN DEFAULT TRUE);
                                    
  /**
  * Procedure removes given appender from given logger in serialized settings.
  * @param x_logger_name Logger name.
  * @param x_appender Binary coded appender.
  */
  PROCEDURE remove_serialized_appender(x_logger_name IN t_logger.logger%TYPE,
                                       x_appender    IN t_appender.appender%TYPE);
                                       
  /**
  * Procedure sets additivity flag for given logger in serialized settings.
  * @param x_logger_name Loger name.
  * @param x_additivity Additivity flag.
  */
  PROCEDURE set_serialized_additivity(x_logger_name IN t_logger.logger%TYPE,
                                      x_additivity  IN BOOLEAN);
                                      
  /**
  * Procedure sets session value for given parameter name, appender and application.
  * @param x_app Application name.
  * @param x_appender Appender name.
  * @param x_parameter_name Parameter name.
  * @param x_parameter_value Parameter value.
  */
  PROCEDURE set_serialized_appender_param(x_app             IN t_app_appender.app%TYPE,
                                          x_appender        IN t_app_appender.appender%TYPE,
                                          x_parameter_name  IN t_app_appender.parameter_name%TYPE,
                                          x_parameter_value IN t_app_appender.parameter_value%TYPE);
                                          
  /**
  * Procedure sets session layout for given appender and given application.
  * @param x_app Application name.
  * @param x_appender Appender name.
  * @param x_layout Layout.
  */
  PROCEDURE set_serialized_layout(x_app      IN t_app_appender.app%TYPE,
                                  x_appender IN t_app_appender.appender%TYPE,
                                  x_layout   IN t_app_appender.parameter_value%TYPE);
                                  
   /**
  * Procedure sets session log level for given logger.
  * @param x_logger Logger name.
  * @param x_log_level Log level.
  */
  PROCEDURE set_serialized_level(x_logger_name IN t_logger.logger%TYPE,
                                 x_log_level   IN t_logger.log_level%TYPE);

  /**
  * Procedure sets session parameter value for given app and parameter name.
  * @param x_app Application.
  * @param x_param_name Parameter name.
  * @param x_param_value Parameter value.
  */
  PROCEDURE set_serialized_parameter(x_app         IN t_param.app%TYPE,
                                     x_param_name  IN t_param.param_name%TYPE,
                                     x_param_value IN t_param.param_value%TYPE);

  /**
  * Procedure enables custom settings for given session.
  * @param x_instance Id of instance for the session (e.g. from gv$session.inst_id).
  * @param x_sessionid Audit session identifier (from gv$session.audsid)
  */
  PROCEDURE apply_settings_for_session(x_instance  IN PLS_INTEGER,
                                       x_sessionid IN NUMBER);

  /**
  * Procedure sets given attribute of given context to given value.
  * For internal use only. Please do not use.
  * @param x_namespace Name of context
  * @param x_attribute Attribute
  * @param x_value Value of attribute
  * @raises e_internal_use_only Can not be called from another schema.
  */
  PROCEDURE set_context_job(x_namespace IN ctx_namespace_type,
                            x_attribute IN ctx_attribute_type,
                            x_value     IN ctx_value_type);
                        
  /**
  * Procedure clears all contexts. 
  * For internal use only. Do not use.
  * @param x_namespace Name of context
  * @raises e_internal_use_only Can not be called from another schema.
  */
  PROCEDURE clear_all_context(x_namespace IN ctx_namespace_type);

END logging;
/

