create or replace package logging is
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
  type param_names_type is table of user_objects.object_name%type;

  /** Type for parameter values. */
  type param_values_type is table of varchar2(32767);

  /** Type for log message */
  subtype message_type is varchar2(32767);

  /** Subtype for attribute name. */
  subtype ctx_attribute_type is global_context.attribute%type;

  /** Subtype for context namespace. */
  subtype ctx_namespace_type is global_context.namespace%type;

  /** Subtype for context aribute value. */
  subtype ctx_value_type is global_context.value%type;

  /** Type for logger name hash. */
  subtype hash_type is number; -- dbms_utility.get_hash_value;

  /** Type for appender settings. */
  type appender_params_type is table of t_app_appender.parameter_value%type index by t_app_appender.parameter_name%type;

  /** Type for context "boolean" */
  subtype ctx_boolean is varchar2(1);

  /** Constant for TRUE in a application context - application context can not handle booleans */
  c_true constant ctx_boolean := 'T';

  /** Constant for FALSE in a application context - application context can not handle booleans */
  c_false constant ctx_boolean := 'F';

  /** Constant indicating whether current database version is 11.2 or greater. */
  ver_lt_11_2 constant boolean := $if (dbms_db_version.version < 11) -- less than 11
           or (dbms_db_version.version = 11 and dbms_db_version.release < 2) -- or less than 11.2
           $then true $else false $end;
                                 
  /** Type for serialization operation: e.g. set logger parameters, set appender parameter, etc. */
  subtype serialization_ops_type is varchar2(5);

  /** Operation for setting logger parameters */
  c_set_logger_op constant serialization_ops_type := 'SL';

  /** Operation for setting application parameters */
  c_set_app_param_op constant serialization_ops_type := 'SAP';

  /** Operation for setting application appender parameters */
  c_set_app_appender_param_op constant serialization_ops_type := 'SAAP';

  /** Item delimiter in serialized settings */
  c_ser_delim constant varchar2(3) := '#!';

  /** Type for settings of all appenders. */
  type appenders_params_type is table of appender_params_type index by pls_integer; -- t_appender.code;

  /** Type for logger parameters */
  type logger_type is record(
    always_from_ctx    ctx_boolean,
    logger             t_logger.logger%type,
    log_level_severity t_log_level.severity%type,
    enabled_appenders  t_logger.appenders%type,
    app                t_app_appender.app%type,
    appenders_params   appenders_params_type -- currently not used
    );

  /** Log level ALL. */
  c_all_level constant t_log_level.log_level%type := 'ALL';

  /** Log level TRACE. */
  c_trace_level constant t_log_level.log_level%type := 'TRACE';

  /** Log level DEBUG. */
  c_debug_level constant t_log_level.log_level%type := 'DEBUG';

  /** Log level INFO. */
  c_info_level constant t_log_level.log_level%type := 'INFO';

  /** Log level WARN. */
  c_warn_level constant t_log_level.log_level%type := 'WARN';

  /** Log level ERROR. */
  c_error_level constant t_log_level.log_level%type := 'ERROR';

  /** Log level FATAL. */
  c_fatal_level constant t_log_level.log_level%type := 'FATAL';

  /** Log level OFF. */
  c_off_level constant t_log_level.log_level%type := 'OFF';

  /** TABLE appender. */
  c_table_appender constant t_appender.code%type := 1;

  /** DBMS_OUTPUT appender. */
  c_dbms_output_appender constant t_appender.code%type := 2;

  /** SMTP appender. */
  c_smtp_appender constant t_appender.code%type := 4;

  -- these elements are defined only if internal debugging is set to TRUE
  $if $$debug $then
  g_internal_log_level t_log_level.log_level%type := c_trace_level;
  g_internal_appenders pls_integer := 2;
  $end

  -- these elements are public only when unit testing precompiler option is set to TRUE
  $if $$unit_test $then
  -- types and variables
  subtype visibility_type is pls_integer range 1 .. 2;
  c_global_flag  constant visibility_type := 1;
  c_session_flag constant visibility_type := 2;

  -- methods
  -- internal debugger private methods
  $if $$debug $then

  type logger_settings_type is record(
    enabled_appenders t_logger.appenders%type,
    log_level         t_logger.log_level%type,
    additivity        t_logger.additivity%type);
  type logger_settings_col_type is table of logger_settings_type index by t_logger.logger%type;
  type app_settings_type is record(
    app_params       appender_params_type,
    appenders_params appenders_params_type);
  type app_settings_col_type is table of app_settings_type index by t_app.app%type;
  type deserialized_settings_type is record(
    loggers      logger_settings_col_type,
    app_settings app_settings_col_type);

  procedure internal_log(x_level    in t_log_level.log_level%type,
                         x_logger   in t_logger.logger%type,
                         x_message  in message_type,
                         x_appender in pls_integer default g_internal_appenders);
  procedure init_log_level_severities;
  $end
  function bool_to_int(x_boolean in boolean) return number;
  procedure clear_all_context_rac_aware(x_namespace in ctx_namespace_type, x_visibility in visibility_type);
  function dequeue_from_cyclic_buffer return varchar2;
  procedure enqueue_into_cyclic_buffer(x_app in varchar2, x_message in message_type);
  function format_call_stack return varchar2;
  function format_message(x_message     in message_type,
                          x_layout      in t_app_appender.parameter_value%type,
                          x_logger_name in t_logger.logger%type,
                          x_level       in t_logger.log_level%type) return varchar2;
  function get_app(x_schema in t_schema_app.schema%type) return t_schema_app.app%type;
  function get_appender_param(x_app            in t_app_appender.app%type,
                              x_appender_code  in t_app_appender.appender_code%type,
                              x_parameter_name in t_app_appender.parameter_name%type,
                              x_visibility     in visibility_type default c_global_flag)
    return t_app_appender.parameter_value%type;
  function get_appenders(x_logger_name  in t_logger.logger%type,
                         x_app_ctx_name in ctx_namespace_type,
                         x_add_ctx_name in ctx_namespace_type) return t_logger.appenders%type;
  function get_current_appender_param(x_app            in t_app_appender.app%type,
                                      x_appender_code  in t_app_appender.appender_code%type,
                                      x_parameter_name in t_app_appender.parameter_name%type)
    return t_app_appender.parameter_value%type;
  function get_current_layout(x_app in t_app_appender.app%type, x_appender_code in t_app_appender.appender_code%type)
    return t_app_appender.parameter_value%type;
  function get_current_parameter(x_app in t_param.app%type, x_param_name in t_param.param_name%type)
    return t_param.param_value%type;
  function get_current_used_appenders(x_logger_name in t_logger.logger%type) return t_logger.appenders%type;
  function get_current_used_level(x_logger_name in t_logger.logger%type) return t_logger.log_level%type;
  function get_deserialized_settings(x_settings in ctx_value_type) return deserialized_settings_type;
  function get_layout(x_app           in t_app_appender.app%type,
                      x_appender_code in t_app_appender.appender_code%type,
                      x_visibility    in visibility_type default c_global_flag)
    return t_app_appender.parameter_value%type;
  function get_level(x_logger_name in t_logger.logger%type, x_ctx_name in varchar2) return t_logger.log_level%type;
  function get_level_severity(x_level in t_log_level.log_level%type) return t_log_level.severity%type;
  function get_nth_logger_name(x_logger_name in t_logger.logger%type, x_nth in pls_integer) return t_logger.logger%type;
  function get_session_ctx_usage return boolean;
  procedure init_appenders(x_visibility in visibility_type default c_global_flag);
  procedure init_email_cyclic_buffer(x_app in varchar2);
  procedure init_levels;
  procedure init_loggers(x_visibility in visibility_type default c_global_flag, x_app in t_app.app%type default null);
  procedure init_params(x_visibility in visibility_type default c_global_flag, x_app in t_param.app%type default null);
  procedure init_session_identifier;
  procedure init_user_app;
  function int_to_bool(x_number in number) return boolean;
  function is_cyclic_buffer_empty return boolean;
  function is_initialized(x_ctx in ctx_namespace_type) return boolean;
  procedure log(x_level          in t_log_level.log_level%type,
                x_logger         in out nocopy logger_type,
                x_message        in message_type,
                x_log_backtrace  in boolean,
                x_log_call_stack in boolean);
  procedure log_smtp(x_app           in t_app_appender.app%type,
                     x_logger_name   in t_logger.logger%type,
                     x_level         in t_log_level.log_level%type,
                     x_message       in message_type,
                     x_call_stack    in boolean,
                     x_log_backtrace in boolean);
  procedure log_stdout(x_app           in t_app_appender.app%type,
                       x_logger_name   in t_logger.logger%type,
                       x_level         in t_log_level.log_level%type,
                       x_message       in message_type,
                       x_call_stack    in boolean,
                       x_log_backtrace in boolean);
  procedure log_table(x_app           in t_app_appender.app%type,
                      x_logger_name   in t_logger.logger%type,
                      x_level         in t_log_level.log_level%type,
                      x_message       in message_type,
                      x_call_stack    in boolean,
                      x_log_backtrace in boolean);
  procedure parse_stack(o_logger     out varchar2,
                        o_app        out varchar2,
                        x_method     in varchar2 default null,
                        x_call_stack in varchar2 default dbms_utility.format_call_stack());
  function serialize_settings return ctx_value_type;
  procedure set_context_rac_aware(x_namespace  in ctx_namespace_type,
                                  x_attribute  in ctx_attribute_type,
                                  x_value      in ctx_value_type,
                                  x_visibility in visibility_type);
  procedure send_buffer(x_app in t_app_appender.app%type);
  procedure unimplemented;
  procedure use_requested_session_settings(x_app in t_app.app%type);
  procedure set_context(x_namespace in ctx_namespace_type,
                        x_attribute in ctx_attribute_type,
                        x_value     in ctx_value_type);
  $end

  /**
  * Function returns bitwise OR of given numbers.
  * @param x_n1 First operand.
  * @param x_n2 Second operand.
  * @return Bitwise OR of given numbers.
  */
  function bit_or(x_n1 in number, x_n2 in number) return number;

  /**
  * Function returns bitwise exclusive OR of given numbers.
  * @param x_n1 First operand.
  * @param x_n2 Second operand.
  * @return Bitwise exclusive OR of given numbers.
  */
  function bit_xor(x_n1 in number, x_n2 in number) return number;

  /**
  * Funtion creates a XML string containing given parameters.
  * @param x_names Parameter names.
  * @param x_values Parameter values.
  * @return XML string containing given parameters.
  */
  function serialize_to_xml(x_names in param_names_type, x_values in param_values_type) return varchar2;

  /**
  * Funtion creates a JSON string containing given parameters.
  * @param x_names Parameter names.
  * @param x_values Parameter values.
  * @return a JSON string containing given parameters.
  */
  function serialize_to_json(x_names in param_names_type, x_values in param_values_type) return varchar2;

  /**
  * Returns a logger configuration for given method.
  * @param x_method Name of method.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  function get_logger(x_method in varchar2 default null, x_always_from_ctx in ctx_boolean default c_false)
    return logger_type;

  /**
  * Returns the root logger configuration.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  function get_root_logger(x_always_from_ctx in ctx_boolean default c_false) return logger_type;

  /**
  * Returns the application logger configuration.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  function get_app_logger(x_always_from_ctx in ctx_boolean default c_false) return logger_type;

  /**
  * Returns the schema logger configuration.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  function get_schema_logger(x_always_from_ctx in ctx_boolean default c_false) return logger_type;

  /**
  * Procedure adds given schema to given application.
  * @param x_app Application name.
  * @param x_schema Schema name.
  */
  procedure add_schema_to_app(x_app in t_schema_app.app%type, x_schema in t_schema_app.schema%type default user());

  /**
  * Procedure removes given schema from given application.
  * @param x_app Application name.
  * @param x_schema Schema name.
  */
  procedure remove_schema_from_app(x_app in t_schema_app.app%type, x_schema in t_schema_app.schema%type default user());

  /**
  * Procedure sets global parameter value for given app and parameter name.
  * @param x_app Application.
  * @param x_param_name Parameter name.
  * @param x_param_value Parameter value.
  */
  procedure set_global_parameter(x_app         in t_param.app%type,
                                 x_param_name  in t_param.param_name%type,
                                 x_param_value in t_param.param_value%type);

  /**
  * Procedure sets session parameter value for given app and parameter name.
  * @param x_app Application.
  * @param x_param_name Parameter name.
  * @param x_param_value Parameter value.
  */
  procedure set_session_parameter(x_app         in t_param.app%type,
                                  x_param_name  in t_param.param_name%type,
                                  x_param_value in t_param.param_value%type);

  /**
  * Procedure sets global layout for given appender and given application.
  * @param x_app Application name.
  * @param x_appender_code Appender cpde.
  * @param x_layout Layout.
  */
  procedure set_global_layout(x_app           in t_app_appender.app%type,
                              x_appender_code in t_app_appender.appender_code%type,
                              x_layout        in t_app_appender.parameter_value%type);

  /**
  * Procedure sets session layout for given appender and given application.
  * @param x_app Application name.
  * @param x_appender_code Appender code.
  * @param x_layout Layout.
  */
  procedure set_session_layout(x_app           in t_app_appender.app%type,
                               x_appender_code in t_app_appender.appender_code%type,
                               x_layout        in t_app_appender.parameter_value%type);

  /**
  * Procedure sets global value for given parameter name, appender and application.
  * @param x_app Application name.
  * @param x_appender_code Appender code.
  * @param x_parameter_name Parameter name.
  * @param x_parameter_value Parameter value.
  */
  procedure set_global_appender_param(x_app             in t_app_appender.app%type,
                                      x_appender_code   in t_app_appender.appender_code%type,
                                      x_parameter_name  in t_app_appender.parameter_name%type,
                                      x_parameter_value in t_app_appender.parameter_value%type);

  /**
  * Procedure sets session value for given parameter name, appender and application.
  * @param x_app Application name.
  * @param x_appender_code Appender code.
  * @param x_parameter_name Parameter name.
  * @param x_parameter_value Parameter value.
  */
  procedure set_session_appender_param(x_app             in t_app_appender.app%type,
                                       x_appender_code   in t_app_appender.appender_code%type,
                                       x_parameter_name  in t_app_appender.parameter_name%type,
                                       x_parameter_value in t_app_appender.parameter_value%type);

  /**
  * Procedure sets global log level for given logger.
  * @param x_logger Logger name.
  * @param x_log_level Log level.
  */
  procedure set_global_level(x_logger_name in t_logger.logger%type, x_log_level in t_logger.log_level%type);

  /**
  * Procedure sets session log level for given logger.
  * @param x_logger Logger name.
  * @param x_log_level Log level.
  */
  procedure set_session_level(x_logger_name in t_logger.logger%type, x_log_level in t_logger.log_level%type);

  /**
  * Procedure adds given global appender to given logger and sets global additivity flag for the logger.
  * @param x_logger_name Logger name.
  * @param x_appender Appender code.
  * @param x_additivity Additivity flag.
  */
  procedure add_global_appender(x_logger_name   in t_logger.logger%type,
                                x_appender_code in t_appender.code%type,
                                x_additivity    in boolean default true);

  /**
  * Procedure adds given session appender to given logger and sets session additivity flag for the logger.
  * @param x_logger_name Logger name.
  * @param x_appender_code Binary coded Appender code.
  * @param x_additivity Additivity flag.
  */
  procedure add_session_appender(x_logger_name   in t_logger.logger%type,
                                 x_appender_code in t_appender.code%type,
                                 x_additivity    in boolean default true);

  /**
  * Procedure sets global additivity flag for given logger.
  * @param x_logger_name Loger name.
  * @param x_additivity Additivity flag.
  */
  procedure set_global_additivity(x_logger_name in t_logger.logger%type, x_additivity in boolean);

  /**
  * Procedure sets session additivity flag for given logger.
  * @param x_logger_name Loger name.
  * @param x_additivity Additivity flag.
  */
  procedure set_session_additivity(x_logger_name in t_logger.logger%type, x_additivity in boolean);

  /**
  * Procedure removes givne global appenders from given logger.
  * @param x_logger_name Logger name.
  * @param x_appender_code Appender code.
  */
  procedure remove_global_appender(x_logger_name in t_logger.logger%type, x_appender_code in t_appender.code%type);

  /**
  * Procedure removes given session appenders from given logger.
  * @param x_logger_name Logger name.
  * @param x_appender_code Appender code.
  */
  procedure remove_session_appender(x_logger_name in t_logger.logger%type, x_appender_code in t_appender.code%type);

  /**
  * Procedure logs message with TRACE log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  procedure trace(x_logger         in out nocopy logger_type,
                  x_message        in message_type default sqlerrm,
                  x_log_backtrace  in boolean default false,
                  x_log_call_stack in boolean default true);

  /**
  * Procedure logs message with INFO log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  procedure info(x_logger         in out nocopy logger_type,
                 x_message        in message_type default sqlerrm,
                 x_log_backtrace  in boolean default false,
                 x_log_call_stack in boolean default true);

  /**
  * Procedure logs message with DEBUG log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  procedure debug(x_logger         in out nocopy logger_type,
                  x_message        in message_type default sqlerrm,
                  x_log_backtrace  in boolean default false,
                  x_log_call_stack in boolean default true);

  /**
  * Procedure logs message with WARN log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  procedure warn(x_logger         in out nocopy logger_type,
                 x_message        in message_type default sqlerrm,
                 x_log_backtrace  in boolean default true,
                 x_log_call_stack in boolean default true);

  /**
  * Procedure logs message with ERROR log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  procedure error(x_logger         in out nocopy logger_type,
                  x_message        in message_type default sqlerrm,
                  x_log_backtrace  in boolean default true,
                  x_log_call_stack in boolean default true);

  /**
  * Procedure logs message with FATAL log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  procedure fatal(x_logger         in out nocopy logger_type,
                  x_message        in message_type default sqlerrm,
                  x_log_backtrace  in boolean default true,
                  x_log_call_stack in boolean default true);

  /**
  * Function checks whether TRACE log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is TRACE or lower.
  * {*} FALSE if log level for given logger is higher than TRACE.
  */
  function is_trace_enabled(x_logger in out nocopy logger_type) return boolean;

  /**
  * Function checks whether DEBUG log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is DEBUG or lower.
  * {*} FALSE if log level for given logger is higher than DEBUG.
  */
  function is_debug_enabled(x_logger in out nocopy logger_type) return boolean;

  /**
  * Function checks whether INFO log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is INFO or lower.
  * {*} FALSE if log level for given logger is higher than INFO.
  */
  function is_info_enabled(x_logger in out nocopy logger_type) return boolean;

  /**
  * Function checks whether WARN log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is WARN or lower.
  * {*} FALSE if log level for given logger is higher than WARN.
  */
  function is_warn_enabled(x_logger in out nocopy logger_type) return boolean;

  /**
  * Function checks whether ERROR log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is ERROR or lower.
  * {*} FALSE if log level for given logger is higher than ERROR.
  */
  function is_error_enabled(x_logger in out nocopy logger_type) return boolean;

  /**
  * Function checks, whether FATAL log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is FATAL or lower.
  * {*} FALSE if log level for given logger is higher than FATAL.
  */
  function is_fatal_enabled(x_logger in out nocopy logger_type) return boolean;

  /**
  * Procedure sets flag, which indicates that session setting (session context) will be used for logging.
  * @param x_usage Flag, which indicates that session setting (session context) will be used for logging.
  * {*} TRUE Session settings will be used (session context)
  * {*} FALSE Global settings will be used (global context)
  */
  procedure set_session_ctx_usage(x_usage in boolean);

  /** Procedure purges all global contexts used by logging */
  procedure purge_global_contexts;

  /** Procedure purges all session contexts used by logging */
  procedure purge_session_contexts;

  /** Procedure copies global setting to session settings.
  * @param x_app Application name. If set, copying will be done only for the given application.
  */
  procedure copy_global_to_session(x_app in t_app.app%type default null);

  /**
  * Procedure adds an application to the configuration.
  * @param x_app Application name.
  * @param x_app_descr Application description.
  */
  procedure add_app(x_app in t_app.app%type, x_app_descr in t_app.app_desc%type);

  /**
  * Procedure removes the given application and all related configuration.
  * @param x_app Application name.
  */
  procedure remove_app(x_app in t_app.app%type);

  /** Procedure shows serialized settings. */
  procedure show_serialized_settings;

  /**
  * Procedure clears all serialized settings.
  */
  procedure clear_serialized_settings;

  /**
  * Procedure adds given appender to given logger and sets additivity flag for the logger in serialized settings.
  * @param x_logger_name Logger name.
  * @param x_appender_code Appender code.
  * @param x_additivity Additivity flag.
  */
  procedure add_serialized_appender(x_logger_name   in t_logger.logger%type,
                                    x_appender_code in t_appender.code%type,
                                    x_additivity    in boolean default true);

  /**
  * Procedure removes given appender from given logger in serialized settings.
  * @param x_logger_name Logger name.
  * @param x_appender_code Appender code.
  */
  procedure remove_serialized_appender(x_logger_name in t_logger.logger%type, x_appender_code in t_appender.code%type);

  /**
  * Procedure sets additivity flag for given logger in serialized settings.
  * @param x_logger_name Loger name.
  * @param x_additivity Additivity flag.
  */
  procedure set_serialized_additivity(x_logger_name in t_logger.logger%type, x_additivity in boolean);

  /**
  * Procedure sets session value for given parameter name, appender and application.
  * @param x_app Application name.
  * @param x_appender_code Appender code.
  * @param x_parameter_name Parameter name.
  * @param x_parameter_value Parameter value.
  */
  procedure set_serialized_appender_param(x_app             in t_app_appender.app%type,
                                          x_appender_code   in t_app_appender.appender_code%type,
                                          x_parameter_name  in t_app_appender.parameter_name%type,
                                          x_parameter_value in t_app_appender.parameter_value%type);

  /**
  * Procedure sets session layout for given appender and given application.
  * @param x_app Application name.
  * @param x_appender_code Appender code.
  * @param x_layout Layout.
  */
  procedure set_serialized_layout(x_app           in t_app_appender.app%type,
                                  x_appender_code in t_app_appender.appender_code%type,
                                  x_layout        in t_app_appender.parameter_value%type);

  /**
  * Procedure sets session log level for given logger.
  * @param x_logger Logger name.
  * @param x_log_level Log level.
  */
  procedure set_serialized_level(x_logger_name in t_logger.logger%type, x_log_level in t_logger.log_level%type);

  /**
  * Procedure sets session parameter value for given app and parameter name.
  * @param x_app Application.
  * @param x_param_name Parameter name.
  * @param x_param_value Parameter value.
  */
  procedure set_serialized_parameter(x_app         in t_param.app%type,
                                     x_param_name  in t_param.param_name%type,
                                     x_param_value in t_param.param_value%type);

  /**
  * Procedure enables custom settings for given session.
  * @param x_instance Id of instance for the session (e.g. from gv$session.inst_id).
  * @param x_sessionid Audit session identifier (from gv$session.audsid)
  */
  procedure apply_settings_for_session(x_instance in pls_integer, x_sessionid in number);

  /**
  * Procedure sets given attribute of given context to given value.
  * For internal use only. Please do not use.
  * @param x_namespace Name of context
  * @param x_attribute Attribute
  * @param x_value Value of attribute
  * @raises e_internal_use_only Can not be called from another schema.
  */
  procedure set_context_job(x_namespace in ctx_namespace_type,
                            x_attribute in ctx_attribute_type,
                            x_value     in ctx_value_type);

  /**
  * Procedure clears all contexts. 
  * For internal use only. Do not use.
  * @param x_namespace Name of context
  * @raises e_internal_use_only Can not be called from another schema.
  */
  procedure clear_all_context(x_namespace in ctx_namespace_type);

end logging;
/

