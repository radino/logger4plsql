CREATE OR REPLACE PACKAGE BODY logging IS
  /*
  Copyright (c) 2010 Radoslav Golian <radoslav.golian@gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
  */

  /**
  * Implementation of log4j for PL/SQL.
  * Licence: MIT License (@see license.txt)
  * {*} web                 http://radino.eu
  * {*} email, gtalk(XMPP)  radoslav.golian@gmail.com, rgolian@gmail.com
  * {*} facebook            http://www.facebook.com/radoslav.golian
  * {*} twitter             http://twitter.com/radoslavgolian
  * (*) project page        http://sourceforge.net/projects/logger4plsql
  * @author Radoslav Golian
  */

  /** User using a logger. */
  c_user CONSTANT VARCHAR2(32) := upper(USER);

  /** SMTP appender: Type for a cyclic buffer item. */
  SUBTYPE mail_item_type IS VARCHAR2(4000);

  /** SMTP appender: Type for cyclic buffer. */
  TYPE mail_table_type IS TABLE OF mail_item_type;

  /** SMTP appender: Type for cyclic buffer structure (queue). */
  TYPE mail_cyclic_buffer_type IS RECORD(
    buff_size PLS_INTEGER, -- buffer size
    head      PLS_INTEGER, -- position of head in the queue
    tail      PLS_INTEGER, -- position of tail in the queue
    buffer    mail_table_type -- buffer containg logs
    );

  /** SMTP appender: Cyclic buffer global variable. */
  g_mail_buffer mail_cyclic_buffer_type;
  
  /** Hash for the root logger */
  g_root_logger_hash hash_type;

  /** Type for visibility: session or global */
  SUBTYPE visibility_type IS PLS_INTEGER RANGE 1 .. 2;

  /** Flag for visibility: global */
  c_global_flag CONSTANT visibility_type := 1;

  /** Flag for visibility: session */
  c_session_flag CONSTANT visibility_type := 2;

  /** Type for context list */
  TYPE context_list_type IS VARRAY(2) OF ctx_namespace_type;

  /** Names of global and session contexts containing additivity flag for loggers. */
  c_additivity_ctx CONSTANT context_list_type := context_list_type('CTX_LOGGER_ADD_G', 'CTX_LOGGER_ADD_L');

  /** Names of global and session contexts containing appenders set in loggers. */
  c_logger_appenders_ctx CONSTANT context_list_type := context_list_type('CTX_LOGGER_APP_G',
                                                                         'CTX_LOGGER_APP_L');

  /** Names of global and session contexts containing set log levels for loggers. */
  c_logger_levels_ctx CONSTANT context_list_type := context_list_type('CTX_LOGGER_LEV_G', 'CTX_LOGGER_LEV_L');

  /** Name of global context containing severities for log levels. */
  c_global_levels_ctx CONSTANT ctx_namespace_type := 'CTX_LOGGER_LEVELS_G';

  /** Names of global and session contexts containing logger names. */
  c_logger_names_ctx CONSTANT context_list_type := context_list_type('CTX_LOGGER_NAME_G', 'CTX_LOGGER_NAME_L');

  /** Name of global context containing schema-application mapping. */
  c_global_user_app_ctx CONSTANT ctx_namespace_type := 'CTX_LOGGER_USER_APP_G';

  /** Names of global and session context containing parameters for logging system. */
  c_parameters_ctx CONSTANT context_list_type := context_list_type('CTX_LOGGER_PARAMS_G',
                                                                   'CTX_LOGGER_PARAMS_L');

  /** Name of global context containing appenders' codes. */
  c_global_appenders_ctx CONSTANT ctx_namespace_type := 'CTX_LOGGER_APPENDERS_G';

  /** Root logger name. */
  c_root_logger_name CONSTANT ctx_namespace_type := '/';
  
  /** Key for flag indicating whether session context is in use. */
  c_session_usage_param CONSTANT ctx_attribute_type := 'USAGE';

  /** Key for layout parameter in context. */
  c_layout_param CONSTANT ctx_attribute_type := 'LAYOUT';

  /** Key for flag inticating whether a context containing this key was initialized or not. */
  c_init_param CONSTANT ctx_attribute_type := 'INITIALIZED';

  /** Key for default appenders' layout. */
  c_default_layout_param CONSTANT ctx_attribute_type := 'DEFAULT#DEFAULT_LAYOUT';

  /** Separator char in named hierarchy. */
  c_separator CONSTANT VARCHAR2(1) := '.';

  /** End of line character(s). */
  c_nl CONSTANT VARCHAR2(2) := chr(10);

  /** Space character. */
  c_space CONSTANT VARCHAR2(1) := chr(32);

  /** Lenght of end of line character(s). */
  c_nl_length CONSTANT PLS_INTEGER := length(c_nl);

  /** Schema where the logger is installed. */
  c_schema_name CONSTANT ctx_namespace_type := sys_context('USERENV', 'CURRENT_SCHEMA');

  /** Qualified name of this package. */
  c_package_name CONSTANT ctx_namespace_type := c_schema_name || '.' || $$PLSQL_UNIT;

  /** Procedure raises uniplemented feature exception.
  */
  PROCEDURE unimplemented IS
  BEGIN
    raise_application_error('-20999', 'Unimplemented feature');
  END unimplemented;

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
                        x_value     IN ctx_value_type) IS
  BEGIN
    IF c_user <> c_schema_name THEN
      raise_application_error(-20003, 'For internal use only');
    END IF;
    dbms_session.set_context(x_namespace, x_attribute, x_value);
  END set_context;

  /** Procedure sets context for all RAC instances.
  * @param x_namespace Name of context
  * @param x_attribute Attribute
  * @param x_value Value of attribute
  */
  PROCEDURE set_context_on_rac(x_namespace      IN ctx_namespace_type,
                               x_attribute      IN ctx_attribute_type,
                               x_value          IN ctx_value_type) IS
    l_job_number     BINARY_INTEGER;
    l_what           user_jobs.what%TYPE;
    l_instance_count NUMBER;
    l_instance_table dbms_utility.instance_table;
    e_invalid_instance EXCEPTION;
    PRAGMA EXCEPTION_INIT(e_invalid_instance, -23428);
    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN
    l_what := 'logging.set_context(
                    ''' || x_namespace || ''',
                    ''' || x_attribute || ''',
                    ''' || x_value || '''
                );';

    dbms_utility.active_instances(instance_table => l_instance_table, instance_count => l_instance_count);

    FOR i IN 1 .. l_instance_table.count LOOP
      BEGIN
        dbms_job.submit(job       => l_job_number,
                        what      => l_what,
                        next_date => SYSDATE,
                        INTERVAL  => NULL,
                        instance  => l_instance_table(i).inst_number);
        -- if there is no such instance ignore error (or it is not running)
      EXCEPTION
        WHEN e_invalid_instance THEN
          NULL;
      END;

    END LOOP;
    COMMIT;
  END set_context_on_rac;

  /**
  * Function returns bitwise OR of given numbers.
  * @param x_n1 First operand.
  * @param x_n2 Second operand.
  * @return Bitwise OR of given numbers.
  */
  FUNCTION bit_or(x_n1 IN NUMBER,
                  x_n2 IN NUMBER) RETURN NUMBER IS
  BEGIN
    RETURN x_n1 + x_n2 - bitand(x_n1, x_n2);
  END bit_or;

  /**
  * Function returns bitwise exclusive OR of given numbers.
  * @param x_n1 First operand.
  * @param x_n2 Second operand.
  * @return Bitwise exclusive OR of given numbers.
  */
  FUNCTION bit_xor(x_n1 IN NUMBER,
                   x_n2 IN NUMBER) RETURN NUMBER IS
  BEGIN
    -- would be nice to have bitwise shifts in PL/SQL: *2 => << 1
    RETURN x_n1 + x_n2 - 2 * bitand(x_n1, x_n2);
  END bit_xor;
  
  /* Function translates 3-valued BOOLEAN to NUMBER
  * @param x_boolean A boolean value
  * @return The translated boolean to integer
  *         {*} 0 when the given value is FALSE
  *         {*} 1 when the given value is TRUE
  *         {*} NULL, otherwise
  */
  FUNCTION bool_to_int(x_boolean IN BOOLEAN) RETURN NUMBER IS
  BEGIN
    RETURN CASE x_boolean WHEN TRUE THEN 1 WHEN FALSE THEN 0 END;
  END bool_to_int;

  /* Function translates an NUMBER to BOOLEAN
  * @param x_number A number value.
  * @return The translated number to boolean
  *         {*} FALSE when the given value is 0
  *         {*} TRUE when the given value is 1
  *         {*} NULL, otherwise
  */
  FUNCTION int_to_bool(x_number IN NUMBER) RETURN BOOLEAN IS
  BEGIN
    RETURN CASE x_number WHEN 1 THEN TRUE WHEN 0 THEN FALSE END;
  END int_to_bool; 

  /**
  * Funtion creates a XML string containing given parameters.
  * @param x_names Parameter names (has to be dense collection).
  * @param x_values Parameter values (has to be dense collection).
  * @return XML string containing given parameters.
  */
  FUNCTION serialize_to_xml(x_names  IN param_names_type,
                            x_values IN param_values_type) RETURN VARCHAR2 IS
    l_result VARCHAR2(4000);
  BEGIN
    IF x_names.FIRST IS NULL THEN
      RETURN NULL;
    END IF;
    l_result := '<params>';
    FOR i IN x_names.FIRST .. x_names.LAST LOOP
      IF x_values(i) IS NULL THEN
        l_result := l_result || '<' || x_names(i) || '/>';
      ELSE
        l_result := l_result || '<' || x_names(i) || '>' || x_values(i) || '</' || x_names(i) || '>';
      END IF;
    END LOOP;
    l_result := l_result || '</params>';
    RETURN l_result;
  END serialize_to_xml;

  /**
  * Funtion creates a JSON string containing given parameters.
  * @param x_names Parameter names (has to be dense collection).
  * @param x_values Parameter values (has to be dense collection).
  * @return a JSON string containing given parameters.
  */
  FUNCTION serialize_to_json(x_names  IN param_names_type,
                             x_values IN param_values_type) RETURN VARCHAR2 IS
    l_result VARCHAR2(4000);
    c_nl CONSTANT VARCHAR2(1) := chr(10);
  BEGIN
    IF x_names.FIRST IS NULL THEN
      RETURN NULL;
    END IF;

    FOR i IN x_names.FIRST .. x_names.LAST LOOP
      IF x_values(i) IS NULL THEN
        l_result := l_result || x_values(i) || ': ' || x_names(i) || c_nl;
      END IF;
    END LOOP;
    RETURN l_result;
  END serialize_to_json;

  /**
  * Function hashes given logger name.
  * @param x_logger Logger name.
  * @return Hash of given logger name.
  */
  FUNCTION hash_logger_name(x_logger_name IN t_logger.logger%TYPE) RETURN hash_type IS
  BEGIN
    RETURN dbms_utility.get_hash_value(x_logger_name, 1, 1073741824);
  END hash_logger_name;

  /**
  * Function returns application name for given schema.
  * @param x_schema Schema name.
  * @return Application name for given schema.
  */
  FUNCTION get_app(x_schema IN t_schema_app.schema%TYPE) RETURN t_schema_app.app%TYPE IS
  BEGIN
    RETURN sys_context(c_global_user_app_ctx, x_schema);
  END get_app;

  /**
  * Function returns call stack (without functions of this package).
  * @return Call stack.
  */
  FUNCTION format_call_stack RETURN VARCHAR2 IS
    l_header_end  PLS_INTEGER;
    l_logging_end PLS_INTEGER;
    l_call_stack  VARCHAR2(2000 CHAR);
    c_stack_body_offset CONSTANT PLS_INTEGER := 3;
  BEGIN
    l_call_stack := dbms_utility.format_call_stack();
    -- skip header
    l_header_end := instr(l_call_stack, c_nl, nth => c_stack_body_offset) + c_nl_length;

    l_logging_end := instr(l_call_stack, c_nl, instr(l_call_stack, c_package_name, -1, 1));

    RETURN substr(l_call_stack, 1, l_header_end) || substr(l_call_stack, l_logging_end + c_nl_length);
  END format_call_stack;

  /**
  * Procedure adds given schema to given application.
  * @param x_app Application name.
  * @param x_schema Schema name.
  */
  PROCEDURE add_schema_to_app(x_app    IN t_schema_app.app%TYPE,
                              x_schema IN t_schema_app.SCHEMA%TYPE DEFAULT USER) IS
    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN
    INSERT INTO t_schema_app
      (SCHEMA, app)
    VALUES
      (upper(x_schema), upper(x_app));

    $IF logging.ver_ge_11_2 $THEN
      set_context_on_rac(c_global_user_app_ctx, upper(x_schema), upper(x_app));
    $END

    dbms_session.set_context(c_global_user_app_ctx, upper(x_schema), upper(x_app));
    COMMIT;
  EXCEPTION
    WHEN dup_val_on_index THEN
      NULL;
  END add_schema_to_app;

  /**
  * Procedure removes given schema from given application.
  * @param x_app Application name.
  * @param x_schema Schema name.
  */
  PROCEDURE remove_schema_from_app(x_app    IN t_schema_app.app%TYPE,
                                   x_schema IN t_schema_app.SCHEMA%TYPE DEFAULT USER) IS
    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN
    DELETE FROM t_schema_app ua
     WHERE ua.SCHEMA = upper(x_schema)
       AND ua.app = upper(x_app);

    dbms_session.clear_context(c_global_user_app_ctx, upper(x_schema)); -- it's case insensitive, but for clarity..
    COMMIT;
  END remove_schema_from_app;

  /**
  * Procedure sets initialization flag or clears a given context.
  * @param x_ctx Context name.
  * @param x_initialized Intialization flag.
  * {*} TRUE Set context flag as initialized.
  * {*} FALSE Clear the context. Flag will be NULL.
  */
  PROCEDURE set_initialization(x_ctx         IN ctx_namespace_type,
                               x_initialized IN BOOLEAN) IS
  BEGIN
    IF x_initialized THEN
      -- $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bool_to_int',  'YES');
      -- $END
      dbms_session.set_context(x_ctx, c_init_param, bool_to_int(x_initialized));
    ELSE
      dbms_session.clear_context(x_ctx, c_init_param);
    END IF;
  END set_initialization;

  /**
  * Function returns whether is context initialized or not.
  * @param x_ctx Context name.
  * @return Flag indication whether is context initialized or not.
  */
  FUNCTION is_initialized(x_ctx IN ctx_namespace_type) RETURN BOOLEAN IS
  BEGIN
    IF sys_context(x_ctx, c_init_param) IS NOT NULL THEN
      RETURN TRUE;
    END IF;

    RETURN FALSE;
  END is_initialized;

  /**
  * Procedure initializes a context for log levels. Lazy initialization is used.
  */
  PROCEDURE init_levels IS
  BEGIN
    IF is_initialized(c_global_levels_ctx) THEN
      RETURN;
    END IF;

    FOR l_row IN (SELECT ll.log_level, ll.severity
                    FROM t_log_level ll) LOOP
      dbms_session.set_context(c_global_levels_ctx, l_row.log_level, l_row.severity);
    END LOOP;

    set_initialization(c_global_levels_ctx, TRUE);
  END init_levels;

  /**
  * Procedure initializes a global or session contexts for appenders.
  * Lazy initialization is used.
  * @param x_global Flag, whether global or session contexts should be initialized.
  * {*} c_global_flag Initialize global contexts
  * {*} c_session_flag Initialize session contexts
  */
  PROCEDURE init_appenders(x_visibility IN visibility_type DEFAULT c_global_flag) IS
    l_current_appender_ctx ctx_namespace_type;
    l_ctx_suffix           VARCHAR2(2);
  BEGIN
    IF x_visibility = c_global_flag AND is_initialized(c_global_appenders_ctx) THEN
      RETURN;
    END IF;

    l_ctx_suffix := CASE x_visibility WHEN c_global_flag THEN '_G' ELSE '_L' END;

    FOR l_row IN (SELECT a.appender, a.code, a.base_context_name
                    FROM t_appender a) LOOP

      l_current_appender_ctx := l_row.base_context_name || l_ctx_suffix;

      -- there is no session context for appenders
      IF x_visibility = c_global_flag THEN
        dbms_session.set_context(c_global_appenders_ctx, l_row.appender, l_row.base_context_name);
      END IF;

      dbms_session.set_context(l_current_appender_ctx, 'DEFAULT#CODE', l_row.code);

      FOR l_row2 IN (SELECT aa.app, aa.parameter_name, aa.parameter_value
                       FROM t_app_appender aa
                      WHERE aa.appender = l_row.appender) LOOP
        dbms_session.set_context(l_current_appender_ctx,
                                 l_row2.app || '#' || l_row2.parameter_name,
                                 l_row2.parameter_value);
      END LOOP;

      set_initialization(l_current_appender_ctx, TRUE);
    END LOOP;

    IF x_visibility = c_global_flag THEN
      set_initialization(c_global_appenders_ctx, TRUE);
    END IF;
  END init_appenders;

  /**
  * Procedure initializes a global context for parameters of logging system.
  * Lazy initialization is used.
  * @param x_visibility Flag, whether global or session contexts should be initialized.
  * {*} c_global_flag  Initialize global contexts
  * {*} c_session_flag Initialize session contexts
  */
  PROCEDURE init_params(x_visibility IN visibility_type DEFAULT c_global_flag) IS
  BEGIN
    IF is_initialized(c_parameters_ctx(x_visibility)) THEN
      RETURN;
    END IF;

    FOR l_row IN (SELECT p.app, p.param_name, p.param_value
                    FROM t_param p) LOOP
      dbms_session.set_context(c_parameters_ctx(x_visibility),
                               l_row.app || '#' || l_row.param_name,
                               l_row.param_value);
    END LOOP;

    set_initialization(c_parameters_ctx(x_visibility), TRUE);
  END init_params;

  /**
  * Procedure initializes a global contexts for loggers.
  * These contexts are initialized:
  * {*} context containing logger names
  * {*} context containing log levels for all loggers
  * {*} context containing appenders set for all loggers
  * {*} context contaiging additivity flag for all loggers
  * Lazy initialization is used.
  * @param x_visibility Flag, whether global or session contexts should be initialized.
  * {*} c_global_flag  Initialize global contexts
  * {*} c_session_flag Initialize session contexts
  */
  PROCEDURE init_loggers(x_visibility IN visibility_type DEFAULT c_global_flag) IS
    l_hlogger hash_type;
  BEGIN
    IF is_initialized(c_logger_names_ctx(x_visibility)) THEN
      RETURN;
    END IF;

    FOR l_row IN (SELECT l.logger, l.log_level, l.appenders, l.additivity
                    FROM t_logger l) LOOP
      l_hlogger := hash_logger_name(l_row.logger);
      dbms_session.set_context(c_logger_names_ctx(x_visibility), l_hlogger, l_row.logger);
      dbms_session.set_context(c_logger_levels_ctx(x_visibility), l_hlogger, l_row.log_level);
      dbms_session.set_context(c_logger_appenders_ctx(x_visibility), l_hlogger, l_row.appenders);
      dbms_session.set_context(c_additivity_ctx(x_visibility), l_hlogger, l_row.additivity);
    END LOOP;

    set_initialization(c_logger_names_ctx(x_visibility), TRUE);
  END init_loggers;

  /**
  * Procedure initializes a global context schema-application mapping.
  * Lazy initialization is used.
  */
  PROCEDURE init_user_app IS
  BEGIN
    IF is_initialized(c_global_user_app_ctx) THEN
      RETURN;
    END IF;

    FOR l_row IN (SELECT ua.SCHEMA, ua.app
                    FROM t_schema_app ua) LOOP
      dbms_session.set_context(c_global_user_app_ctx, l_row.SCHEMA, l_row.app);
    END LOOP;

    set_initialization(c_global_user_app_ctx, TRUE);
  END init_user_app;

  /**
  * Procedure sets flag, which indicates that session setting (session context) will be used for logging.
  * @param x_usage Flag, which indicates that session setting (session context) will be used for logging.
  * {*} TRUE Session settings will be used (local context)
  * {*} FALSE Global settings will be used (global context)
  */
  PROCEDURE set_session_ctx_usage(x_usage IN BOOLEAN) IS
  BEGIN
    -- $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bool_to_int',  'YES');
    -- $END
    dbms_session.set_context(c_parameters_ctx(c_session_flag),
                             c_session_usage_param,
                             bool_to_int(x_usage));
  END set_session_ctx_usage;

  /**
  * Procedure sets global layout for given appender and given application.
  * @param x_app Application name.
  * @param x_appender Appender name.
  * @param x_layout Layout.
  */
  PROCEDURE set_global_layout(x_app      IN t_app_appender.app%TYPE,
                              x_appender IN t_app_appender.appender%TYPE,
                              x_layout   IN t_app_appender.parameter_value%TYPE) IS
    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN
    -- $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('set_global_appender_param',  'YES');
    -- $END
    set_global_appender_param(x_app             => x_app,
                              x_appender        => x_appender,
                              x_parameter_name  => c_layout_param,
                              x_parameter_value => x_layout);
  END set_global_layout;

  /**
  * Procedure sets session layout for given appender and given application.
  * @param x_app Application name.
  * @param x_appender Appender name.
  * @param x_layout Layout.
  */
  PROCEDURE set_session_layout(x_app      IN t_app_appender.app%TYPE,
                               x_appender IN t_app_appender.appender%TYPE,
                               x_layout   IN t_app_appender.parameter_value%TYPE) IS
  BEGIN
    -- $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('set_session_appender_param','YES');
    -- $END
    set_session_appender_param(x_app             => x_app,
                               x_appender        => x_appender,
                               x_parameter_name  => c_layout_param,
                               x_parameter_value => x_layout);
  END set_session_layout;

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
                                      x_parameter_value IN t_app_appender.parameter_value%TYPE) IS
    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN
    MERGE INTO t_app_appender aa
    USING (SELECT NULL
             FROM dual) dummy
    ON (aa.app = x_app AND aa.appender = x_appender AND aa.parameter_name = x_parameter_name)
    WHEN MATCHED THEN
      UPDATE
         SET aa.parameter_value = x_parameter_value
    WHEN NOT MATCHED THEN
      INSERT
        (app, appender, parameter_name, parameter_value)
      VALUES
        (x_app, x_appender, x_parameter_name, x_parameter_value);

    $IF (dbms_db_version.version > 11) OR (dbms_db_version.version > 11 AND dbms_db_version.release >= 2) $THEN
      set_context_on_rac(c_global_user_app_ctx, upper(x_schema), upper(x_app), l_instances);
    $END

    $IF logging.ver_ge_11_2 $THEN
      set_context_on_rac(sys_context(c_global_appenders_ctx, x_appender) || '_G',
                         x_app || '#' || x_parameter_name,
                         x_parameter_value);
    $END

    dbms_session.set_context(sys_context(c_global_appenders_ctx, x_appender) || '_G',
                             x_app || '#' || x_parameter_name,
                             x_parameter_value);

    COMMIT;
  END set_global_appender_param;

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
                                       x_parameter_value IN t_app_appender.parameter_value%TYPE) IS
  BEGIN
    dbms_session.set_context(sys_context(c_global_appenders_ctx, x_appender) || '_L',
                             x_app || '#' || x_parameter_name,
                             x_parameter_value);
  END set_session_appender_param;

  /**
  * Function returns a flag whether session settings are used for logging.
  * @return Flag whether session settings are used for logging.
  */
  FUNCTION get_session_ctx_usage RETURN BOOLEAN IS
  BEGIN
    RETURN CASE sys_context(c_parameters_ctx(c_session_flag), c_session_usage_param) WHEN '1' THEN TRUE ELSE FALSE END;
  END get_session_ctx_usage;

  /**
  * Function returns appender code for given appender name.
  * @param x_appender Appender name.
  * @return Appender code.
  */
  FUNCTION get_appender_code(x_appender IN t_appender.appender%TYPE) RETURN t_appender.code%TYPE IS
  BEGIN
    RETURN sys_context(sys_context(c_global_appenders_ctx, x_appender) || '_G', 'DEFAULT#CODE');
  END get_appender_code;

  /**
  * Function returns global layout for given application and appender.
  * If layout is not set, default layout is returned.
  * @param x_app Application name.
  * @param x_appender Appender name.
  * @return Global layout for given application and appender.
  */
  FUNCTION get_global_layout(x_app      IN t_app_appender.app%TYPE,
                             x_appender IN t_app_appender.appender%TYPE)
    RETURN t_app_appender.parameter_value%TYPE IS
  BEGIN
    RETURN nvl(sys_context(sys_context(c_global_appenders_ctx, x_appender) || '_G', x_app || '#LAYOUT'),
               sys_context(c_parameters_ctx(c_global_flag), c_default_layout_param));
  END get_global_layout;

  /**
  * Function returns session layout for given application and appender.
  * If layout is not set, default layout is returned.
  * @param x_app Application name.
  * @param x_appender Appender name.
  * @return session layout for given application and appender.
  */
  FUNCTION get_session_layout(x_app      IN t_app_appender.app%TYPE,
                              x_appender IN t_app_appender.appender%TYPE)
    RETURN t_app_appender.parameter_value%TYPE IS
  BEGIN
    RETURN nvl(sys_context(sys_context(c_global_appenders_ctx, x_appender) || '_L', x_app || '#LAYOUT'),
               sys_context(c_parameters_ctx(c_global_flag), c_default_layout_param));
  END get_session_layout;

  /**
  * Function returns global parameter value for given application, appender and parameter.
  * @param x_app Application name.
  * @param x_appender Appender name.
  * @param x_parameter_name Parameter name.
  * @return Global parameter value for given application, appender and parameter.
  */
  FUNCTION get_global_appender_param(x_app            IN t_app_appender.app%TYPE,
                                     x_appender       IN t_app_appender.appender%TYPE,
                                     x_parameter_name IN t_app_appender.parameter_name%TYPE)
    RETURN t_app_appender.parameter_value%TYPE IS
  BEGIN
    RETURN sys_context(sys_context(c_global_appenders_ctx, x_appender) || '_G',
                       x_app || '#' || x_parameter_name);
  END get_global_appender_param;

  /**
  * Function returns global parameter value for given application, appender and parameter.
  * @param x_app Application name.
  * @param x_appender Appender name.
  * @param x_parameter_name Parameter name.
  * @return Global parameter value for given application, appender and parameter.
  */
  FUNCTION get_session_appender_param(x_app            IN t_app_appender.app%TYPE,
                                      x_appender       IN t_app_appender.appender%TYPE,
                                      x_parameter_name IN t_app_appender.parameter_name%TYPE)
    RETURN t_app_appender.parameter_value%TYPE IS
  BEGIN
    RETURN sys_context(sys_context(c_global_appenders_ctx, x_appender) || '_L',
                       x_app || '#' || x_parameter_name);
  END get_session_appender_param;

  /**
  * Function returns current (global or session based on get_session_ctx_usage) parameter
  * value for given application, appender and parameter.
  * @param x_app Application name.
  * @param x_appender Appender name.
  * @param x_parameter_name Parameter name.
  * @return Current parameter value for given application, appender and parameter.
  */
  FUNCTION get_current_appender_param(x_app            IN t_app_appender.app%TYPE,
                                      x_appender       IN t_app_appender.appender%TYPE,
                                      x_parameter_name IN t_app_appender.parameter_name%TYPE)
    RETURN t_app_appender.parameter_value%TYPE IS
  BEGIN

    IF get_session_ctx_usage() THEN
      RETURN get_session_appender_param(x_app, x_appender, x_parameter_name);
    END IF;

    RETURN get_global_appender_param(x_app, x_appender, x_parameter_name);
  END get_current_appender_param;

  /**
  * Function returns current (global or session based on get_session_ctx_usage) layout
  * for given application and appender.
  * @param x_app Application name.
  * @param x_appender Appender name.
  * @return Current layout for given application and appender.
  */
  FUNCTION get_current_layout(x_app      IN t_app_appender.app%TYPE,
                              x_appender IN t_app_appender.appender%TYPE)
    RETURN t_app_appender.parameter_value%TYPE IS
  BEGIN
    IF get_session_ctx_usage THEN
      RETURN get_session_layout(x_app, x_appender);
    END IF;

    RETURN get_global_layout(x_app, x_appender);
  END get_current_layout;

  /**
  * Function returns a nth descendant from given logger.
  * @param x_logger_name Loger name (Named hierarchy).
  * @param x_nth Level of descendant we want to obtain.
  * @return Name of descendant.
  */
  FUNCTION get_nth_logger_name(x_logger_name IN t_logger.logger%TYPE,
                               x_nth         IN PLS_INTEGER) RETURN t_logger.logger%TYPE IS
    l_logger_name t_logger.logger%TYPE;
  BEGIN
    l_logger_name := substr(x_logger_name, 1, instr(x_logger_name || c_separator, c_separator, -1, x_nth) - 1);
    RETURN l_logger_name;
  END get_nth_logger_name;

  /**
  * Function returns log level for given logger name and context.
  * @param x_logger_name Logger name.
  * @param x_ctx_name Context name.
  * @return Log level assigned for given logger on given context.
  */
  FUNCTION get_level(x_logger_name IN t_logger.logger%TYPE,
                     x_ctx_name    IN VARCHAR2) RETURN t_logger.log_level%TYPE IS
    i             PLS_INTEGER;
    l_logger_name t_logger.logger%TYPE;
    l_level       t_logger.log_level%TYPE;
    l_hlogger     hash_type;
  BEGIN
    i := 1;
    LOOP
      l_logger_name := get_nth_logger_name(x_logger_name, i);
      EXIT WHEN l_logger_name IS NULL;

      l_hlogger := hash_logger_name(l_logger_name);

      l_level := sys_context(x_ctx_name, l_hlogger);

      IF l_level IS NOT NULL THEN
        RETURN l_level;
      END IF;

      i := i + 1;
    END LOOP;

    l_hlogger := g_root_logger_hash;
    RETURN sys_context(x_ctx_name, l_hlogger);
  END get_level;

  /**
  * Function returns binary coded list of appenders for given logger.
  * @param x_logger_name Logger name.
  * @param x_ctx_name Name of a context containing appenders.
  * @param x_ctx_name Name of a context containing additivity flag.
  * @return Binary coded list of appenders for given logger.
  */
  FUNCTION get_appenders(x_logger_name  IN t_logger.logger%TYPE,
                         x_app_ctx_name IN ctx_namespace_type,
                         x_add_ctx_name IN ctx_namespace_type) RETURN t_logger.appenders%TYPE IS
    i             PLS_INTEGER;
    l_logger_name t_logger.logger%TYPE;
    l_hlogger     hash_type;
    l_appenders   t_logger.appenders%TYPE;
  BEGIN
    i           := 1;
    l_appenders := 0;
    LOOP
      l_logger_name := get_nth_logger_name(x_logger_name, i);
      EXIT WHEN l_logger_name IS NULL;

      l_hlogger   := hash_logger_name(l_logger_name);
      l_appenders := bit_or(l_appenders, nvl(sys_context(x_app_ctx_name, l_hlogger), 0));

      IF sys_context(x_add_ctx_name, l_hlogger) = 0 THEN
        RETURN l_appenders;
      END IF;

      i := i + 1;
    END LOOP;

    l_hlogger := g_root_logger_hash;
    RETURN bit_or(l_appenders, nvl(sys_context(x_app_ctx_name, l_hlogger), 0));
  END get_appenders;

  /**
  * Function returns binary coded list of global appenders for given logger.
  * @param x_logger_name Logger name.
  * @return Binary coded list of global appenders for given logger.
  */
  FUNCTION get_global_appenders(x_logger_name IN t_logger.logger%TYPE) RETURN t_logger.appenders%TYPE IS
  BEGIN
    RETURN get_appenders(x_logger_name,
                         c_logger_appenders_ctx(c_global_flag),
                         c_additivity_ctx(c_global_flag));
  END get_global_appenders;

  /**
  * Function returns binary coded list of session appenders for given logger.
  * @param x_logger_name Logger name.
  * @return Binary coded list of session appenders for given logger.
  */
  FUNCTION get_session_appenders(x_logger_name IN t_logger.logger%TYPE) RETURN t_logger.appenders%TYPE IS
  BEGIN
    RETURN get_appenders(x_logger_name,
                         c_logger_appenders_ctx(c_session_flag),
                         c_additivity_ctx(c_session_flag));
  END get_session_appenders;

  /**
  * Function returns binary coded list of currently used appenders (based on get_session_ctx_usage) for given logger.
  * @param x_logger_name Logger name.
  * @return Binary coded list of currently used appenders for given logger.
  */
  FUNCTION get_current_used_appenders(x_logger_name IN t_logger.logger%TYPE) RETURN t_logger.appenders%TYPE IS
  BEGIN

    IF get_session_ctx_usage() THEN
      RETURN get_session_appenders(x_logger_name);
    END IF;

    RETURN get_global_appenders(x_logger_name);
  END get_current_used_appenders;

  /**
  * Function returns global log level for given logger.
  * @param x_logger_name Logger name.
  * @return Global log level for given logger.
  */
  FUNCTION get_global_level(x_logger_name IN t_logger.logger%TYPE) RETURN t_logger.log_level%TYPE IS
  BEGIN
    RETURN get_level(x_logger_name, c_logger_levels_ctx(c_global_flag));
  END get_global_level;

  /**
  * Function returns session log level for given logger.
  * @param x_logger_name Logger name.
  * @return session log level for given logger.
  */
  FUNCTION get_session_level(x_logger_name IN t_logger.logger%TYPE) RETURN t_logger.log_level%TYPE IS
  BEGIN
    RETURN get_level(x_logger_name, c_logger_levels_ctx(c_session_flag));
  END get_session_level;

  /**
  * Function returns currently used (based on get_session_ctx_usage) log level for given logger.
  * @param x_logger_name Logger name.
  * @return Currently used log level for given logger.
  */
  FUNCTION get_current_used_level(x_logger_name IN t_logger.logger%TYPE) RETURN t_logger.log_level%TYPE IS
  BEGIN
    IF get_session_ctx_usage() THEN
      RETURN get_session_level(x_logger_name);
    END IF;

    RETURN get_global_level(x_logger_name);
  END get_current_used_level;

  /**
  * Procedure adds given global appenders to given logger and sets global additivity flag for the logger.
  * @param x_logger Logger name.
  * @param x_appender Binary coded Appender list.
  * @param x_additivity Additivity flag.
  */
  PROCEDURE add_global_appender(x_logger_name IN t_logger.logger%TYPE,
                                x_appender    IN t_appender.appender%TYPE,
                                x_additivity  IN BOOLEAN DEFAULT TRUE) IS
    PRAGMA AUTONOMOUS_TRANSACTION;
    l_app        t_schema_app.app%TYPE;
    l_code       t_appender.code%TYPE;
    l_appenders  t_logger.appenders%TYPE;
    l_hlogger    hash_type;
    l_additivity t_logger.additivity%TYPE;
  BEGIN
    l_app := get_app(c_user);

    IF l_app IS NULL AND c_user NOT IN ('SYS', c_schema_name) THEN
      raise_application_error(-20002, 'You have no privilege to set the appender.');
    END IF;

    BEGIN
      SELECT a.code
        INTO l_code
        FROM t_appender a
       WHERE a.appender = x_appender;
    EXCEPTION
      WHEN no_data_found THEN
        raise_application_error(-20001, 'No such appender');
    END;

    -- $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bool_to_int',  'YES');
    -- $END
    l_additivity := bool_to_int(x_additivity);
                
    UPDATE t_logger l
       SET l.appenders = bit_or(l.appenders, l_code), l.additivity = l_additivity
     WHERE l.logger = x_logger_name
    RETURNING appenders INTO l_appenders;

    IF SQL%NOTFOUND THEN
      l_appenders := l_code;

      INSERT INTO t_logger
        (logger, appenders, additivity)
      VALUES
        (x_logger_name, l_appenders, l_additivity);
    END IF;

    l_hlogger := hash_logger_name(x_logger_name);

    $IF logging.ver_ge_11_2 $THEN
      set_context_on_rac(c_logger_names_ctx(c_global_flag), l_hlogger, x_logger_name);
      set_context_on_rac(c_logger_appenders_ctx(c_global_flag), l_hlogger, l_appenders);
      set_context_on_rac(c_additivity_ctx(c_global_flag), l_hlogger, l_additivity);
    $END

    dbms_session.set_context(c_logger_names_ctx(c_global_flag), l_hlogger, x_logger_name);
    dbms_session.set_context(c_logger_appenders_ctx(c_global_flag), l_hlogger, l_appenders);
    dbms_session.set_context(c_additivity_ctx(c_global_flag), l_hlogger, l_additivity);

    COMMIT;
  END add_global_appender;

  /**
  * Procedure sets global additivity flag for given logger.
  * @param x_logger_name Loger name.
  * @param x_additivity Additivity flag.
  */
  PROCEDURE set_global_additivity(x_logger_name IN t_logger.logger%TYPE,
                                  x_additivity  IN BOOLEAN) IS
    PRAGMA AUTONOMOUS_TRANSACTION;
    l_app        t_schema_app.app%TYPE;
    l_appenders  t_logger.appenders%TYPE;
    l_dummy      VARCHAR2(1);
    l_hlogger    hash_type;
    l_additivity t_logger.additivity%TYPE;
  BEGIN
    l_app := get_app(c_user);

    SELECT NULL
      INTO l_dummy
      FROM t_schema_app ua
     WHERE ua.SCHEMA = c_user
       AND ua.app = l_app;

    -- $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bool_to_int',  'YES');
    -- $END
    l_additivity := bool_to_int(x_additivity);

    UPDATE t_logger l
       SET l.additivity = l_additivity
     WHERE l.logger = x_logger_name
    RETURNING appenders INTO l_appenders;

    IF SQL%NOTFOUND THEN
      l_appenders := 0;

      INSERT INTO t_logger
        (logger, appenders, additivity)
      VALUES
        (x_logger_name, l_appenders, l_additivity);
    END IF;

    l_hlogger := hash_logger_name(x_logger_name);

    $IF logging.ver_ge_11_2 $THEN
      set_context_on_rac(c_logger_names_ctx(c_global_flag), l_hlogger, x_logger_name);
      set_context_on_rac(c_logger_appenders_ctx(c_global_flag), l_hlogger, l_appenders);
      set_context_on_rac(c_additivity_ctx(c_global_flag), l_hlogger, l_additivity);
    $END

    dbms_session.set_context(c_logger_names_ctx(c_global_flag), l_hlogger, x_logger_name);
    dbms_session.set_context(c_logger_appenders_ctx(c_global_flag), l_hlogger, l_appenders);
    dbms_session.set_context(c_additivity_ctx(c_global_flag), l_hlogger, l_additivity);

    COMMIT;
  END set_global_additivity;

  /**
  * Procedure removes givne global appenders from given logger.
  * @param x_logger_name Logger name.
  * @param x_appender Binary coded Appender list.
  */
  PROCEDURE remove_global_appender(x_logger_name IN t_logger.logger%TYPE,
                                   x_appender    IN t_appender.appender%TYPE) IS
    PRAGMA AUTONOMOUS_TRANSACTION;
    l_app       t_schema_app.app%TYPE;
    l_code      t_appender.code%TYPE;
    l_appenders t_logger.appenders%TYPE;
    l_hlogger   hash_type;
  BEGIN
    l_app := get_app(c_user);

    IF l_app IS NULL AND c_user NOT IN ('SYS', c_schema_name) THEN
      raise_application_error(-20002, 'You have no privilege to set the appender.');
    END IF;

    BEGIN
      SELECT a.code
        INTO l_code
        FROM t_appender a
       WHERE a.appender = x_appender;
    EXCEPTION
      WHEN no_data_found THEN
        raise_application_error(-20001, 'No such appender');
    END;

    -- unset appenders
    UPDATE t_logger l
       SET l.appenders = bit_xor(l.appenders, l_code)
     WHERE l.logger = x_logger_name
       AND bitand(l.appenders, l_code) > 0
    RETURNING appenders INTO l_appenders;

    -- an appender with given code was set for given logger
    IF SQL%FOUND THEN
      l_hlogger := hash_logger_name(x_logger_name);

      $IF logging.ver_ge_11_2 $THEN
        set_context_on_rac(c_logger_names_ctx(c_global_flag), l_hlogger, x_logger_name);
        set_context_on_rac(c_logger_appenders_ctx(c_global_flag), l_hlogger, l_appenders);
      $END

      dbms_session.set_context(c_logger_names_ctx(c_global_flag), l_hlogger, x_logger_name);
      dbms_session.set_context(c_logger_appenders_ctx(c_global_flag), l_hlogger, l_appenders);
    END IF;

    COMMIT;
  END remove_global_appender;

  /**
  * Procedure adds given session (session) appenders to given logger and sets session additivity flag for the logger.
  * @param x_logger_name Logger name.
  * @param x_appender Binary coded Appender list.
  * @param x_additivity Additivity flag.
  */
  PROCEDURE add_session_appender(x_logger_name IN t_logger.logger%TYPE,
                                 x_appender    IN t_appender.appender%TYPE,
                                 x_additivity  IN BOOLEAN DEFAULT TRUE) IS
    l_appenders t_logger.appenders%TYPE;
    l_code      t_appender.code%TYPE;
    l_hlogger   hash_type;
  BEGIN
    BEGIN
      SELECT a.code
        INTO l_code
        FROM t_appender a
       WHERE a.appender = x_appender;
    EXCEPTION
      WHEN no_data_found THEN
        raise_application_error(-20001, 'No such appender');
    END;

    l_hlogger := hash_logger_name(x_logger_name);

    l_appenders := nvl(sys_context(c_logger_appenders_ctx(c_session_flag), l_hlogger), 0);
    l_appenders := bit_or(l_appenders, l_code);
    dbms_session.set_context(c_logger_names_ctx(c_session_flag), l_hlogger, x_logger_name);
    dbms_session.set_context(c_logger_appenders_ctx(c_session_flag), l_hlogger, l_appenders);
    -- $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bool_to_int',  'YES');
    -- $END
    dbms_session.set_context(c_additivity_ctx(c_session_flag),
                             l_hlogger,
                             bool_to_int(x_additivity));
  END add_session_appender;

  /**
  * Procedure sets session additivity flag for given logger.
  * @param x_logger_name Loger name.
  * @param x_additivity Additivity flag.
  */
  PROCEDURE set_session_additivity(x_logger_name IN t_logger.logger%TYPE,
                                   x_additivity  IN BOOLEAN) IS
    l_appenders t_logger.appenders%TYPE;
    l_hlogger   hash_type;
  BEGIN
    l_hlogger   := hash_logger_name(x_logger_name);
    l_appenders := nvl(sys_context(c_logger_appenders_ctx(c_session_flag), l_hlogger), 0);
    dbms_session.set_context(c_logger_names_ctx(c_session_flag), l_hlogger, x_logger_name);
    dbms_session.set_context(c_logger_appenders_ctx(c_session_flag), l_hlogger, l_appenders);
    -- $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bool_to_int',  'YES');
    -- $END
    dbms_session.set_context(c_additivity_ctx(c_session_flag),
                             l_hlogger,
                             bool_to_int(x_additivity));
  END set_session_additivity;

  /**
  * Procedure sets global parameter value for given app and parameter name.
  * @param x_app Application.
  * @param x_param_name Parameter name.
  * @param x_param_value Parameter value.
  */
  PROCEDURE set_global_parameter(x_app         IN t_param.app%TYPE,
                                 x_param_name  IN t_param.param_name%TYPE,
                                 x_param_value IN t_param.param_value%TYPE) IS
    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN
    MERGE INTO t_param p
    USING (SELECT NULL
             FROM dual) dummy
    ON (p.app = x_app AND p.param_name = x_param_name)
    WHEN MATCHED THEN
      UPDATE
         SET p.param_value = x_param_value
    WHEN NOT MATCHED THEN
      INSERT
        (app, param_name, param_value)
      VALUES
        (x_app, x_param_name, x_param_value);

    dbms_session.set_context(c_parameters_ctx(c_global_flag), x_app || '#' || x_param_name, x_param_value);

    $IF logging.ver_ge_11_2 $THEN
      set_context_on_rac(c_parameters_ctx(c_global_flag),
                         x_app || '#' || x_param_name,
                         x_param_value);
    $END

    COMMIT;
  END set_global_parameter;

  /**
  * Procedure sets session parameter value for given app and parameter name.
  * @param x_app Application.
  * @param x_param_name Parameter name.
  * @param x_param_value Parameter value.
  */
  PROCEDURE set_session_parameter(x_app         IN t_param.app%TYPE,
                                  x_param_name  IN t_param.param_name%TYPE,
                                  x_param_value IN t_param.param_value%TYPE) IS
  BEGIN
    dbms_session.set_context(c_parameters_ctx(c_session_flag), x_app || '#' || x_param_name, x_param_value);
  END set_session_parameter;

  /**
  * Function obtains session parameter value for given app and parameter name.
  * @param x_app Application.
  * @param x_param_name Parameter name.
  * @return Sessio parameter value for given application and parameter name.
  */
  FUNCTION get_session_parameter(x_app        IN t_param.app%TYPE,
                                 x_param_name IN t_param.param_name%TYPE) RETURN t_param.param_value%TYPE IS
  BEGIN
    RETURN sys_context(c_parameters_ctx(c_session_flag), x_app || '#' || x_param_name);
  END get_session_parameter;

  /**
  * Function obtains global parameter value for given app and parameter name.
  * @param x_app Application.
  * @param x_param_name Parameter name.
  * @return Global parameter value for given application and parameter name.
  */
  FUNCTION get_global_parameter(x_app        IN t_param.app%TYPE,
                                x_param_name IN t_param.param_name%TYPE) RETURN t_param.param_value%TYPE IS
  BEGIN
    RETURN sys_context(c_parameters_ctx(c_global_flag), x_app || '#' || x_param_name);
  END get_global_parameter;

  /**
  * Function obtains global parameter value for given app and parameter name.
  * @param x_app Application.
  * @param x_param_name Parameter name.
  * @return Global parameter value for given application and parameter name.
  */
  FUNCTION get_current_parameter(x_app        IN t_param.app%TYPE,
                                 x_param_name IN t_param.param_name%TYPE) RETURN t_param.param_value%TYPE IS
  BEGIN
    IF get_session_ctx_usage() THEN
      RETURN get_session_parameter(x_app, x_param_name);
    END IF;

    RETURN get_global_parameter(x_app, x_param_name);
  END get_current_parameter;

  /**
  * Procedure removes given session appenders from given logger.
  * @param x_logger_name Logger name.
  * @param x_appender Binary coded Appender list.
  */
  PROCEDURE remove_session_appender(x_logger_name IN t_logger.logger%TYPE,
                                    x_appender    IN t_appender.appender%TYPE) IS
    l_appenders t_logger.appenders%TYPE;
    l_code      t_appender.code%TYPE;
    l_hlogger   hash_type;
  BEGIN
    BEGIN
      SELECT a.code
        INTO l_code
        FROM t_appender a
       WHERE a.appender = x_appender;
    EXCEPTION
      WHEN no_data_found THEN
        raise_application_error(-20001, 'No such appender');
    END;

    l_hlogger   := hash_logger_name(x_logger_name);
    l_appenders := nvl(sys_context(c_logger_appenders_ctx(c_session_flag), l_hlogger), 0);

    IF bitand(l_appenders, l_code) > 0 THEN
      l_appenders := bit_xor(l_appenders, l_code);
      dbms_session.set_context(c_logger_appenders_ctx(c_session_flag), l_hlogger, l_appenders);
    END IF;
  END remove_session_appender;

  /**
  * Procedure sets global log level for given logger.
  * @param x_logger Logger name.
  * @param x_log_level Log level.
  */
  PROCEDURE set_global_level(x_logger_name IN t_logger.logger%TYPE,
                             x_log_level   IN t_logger.log_level%TYPE) IS
    PRAGMA AUTONOMOUS_TRANSACTION;
    l_app       t_schema_app.app%TYPE;
    l_dummy     VARCHAR2(1);
    l_hlogger   hash_type;
  BEGIN
    l_app := get_app(c_user);

    SELECT NULL
      INTO l_dummy
      FROM t_schema_app ua
     WHERE ua.SCHEMA = c_user
       AND ua.app = l_app;
       
    MERGE INTO t_logger l
    USING (SELECT NULL
             FROM dual) dummy
    ON (l.logger = x_logger_name)
    WHEN MATCHED THEN
      UPDATE
         SET l.log_level = log_level
    WHEN NOT MATCHED THEN
      INSERT
        (logger, log_level)
      VALUES
        (x_logger_name, x_log_level);

    l_hlogger := hash_logger_name(x_logger_name);
    dbms_session.set_context(c_logger_names_ctx(c_global_flag), l_hlogger, x_logger_name);
    dbms_session.set_context(c_logger_levels_ctx(c_global_flag), l_hlogger, x_log_level);

    $IF logging.ver_ge_11_2 $THEN
      set_context_on_rac(c_logger_names_ctx(c_global_flag), l_hlogger, x_logger_name);
      set_context_on_rac(c_logger_levels_ctx(c_global_flag), l_hlogger, x_log_level);
    $END

    COMMIT;
  END set_global_level;

  /**
  * Procedure sets session log level for given logger.
  * @param x_logger Logger name.
  * @param x_log_level Log level.
  */
  PROCEDURE set_session_level(x_logger_name IN t_logger.logger%TYPE,
                              x_log_level   IN t_logger.log_level%TYPE) IS
    l_hlogger hash_type;
  BEGIN
    l_hlogger := hash_logger_name(x_logger_name);
    dbms_session.set_context(c_logger_names_ctx(c_session_flag), l_hlogger, x_logger_name);
    dbms_session.set_context(c_logger_levels_ctx(c_session_flag), l_hlogger, x_log_level);
  END set_session_level;

  /**
  * Function returns severity for given log level.
  * @param x_level Log level.
  * @return Severity of given log level.
  */
  FUNCTION get_level_severity(x_level IN t_log_level.log_level%TYPE) RETURN t_log_level.severity%TYPE IS
  BEGIN
    RETURN CAST(sys_context(c_global_levels_ctx, x_level) AS NUMBER);
  END get_level_severity;

  /**
  * Funtion formats given message with given layout.
  * @param x_message Message.
  * @param x_layout Layout.
  * @param x_logger_name Logger name.
  * @param x_level Log level.
  * @return Formated message.
  */
  FUNCTION format_message(x_message     IN VARCHAR2,
                          x_layout      IN t_app_appender.parameter_value%TYPE,
                          x_logger_name IN t_logger.logger%TYPE,
                          x_level       IN t_logger.log_level%TYPE) RETURN VARCHAR2 IS
    l_message t_log.message%TYPE;
  BEGIN
    l_message := REPLACE(REPLACE(REPLACE(REPLACE(x_layout, '%m', x_message), '%l', x_logger_name),
                                 '%L',
                                 x_level),
                         '%t',
                         systimestamp);
    IF instr(l_message, '%b') > 0 THEN
      l_message := REPLACE(l_message,
                           '%b',
                           dbms_utility.format_error_stack || dbms_utility.format_error_backtrace);
    END IF;
    IF instr(l_message, '%c') > 0 THEN
      l_message := REPLACE(l_message, '%c', format_call_stack());
    END IF;
    RETURN l_message;
  END format_message;

  /**
  * Procedure initializes cyclic buffer.
  * @param x_app Application
  */
  PROCEDURE init_email_cyclic_buffer(x_app IN VARCHAR2) IS
  BEGIN
      g_mail_buffer.head      := 1;
      g_mail_buffer.tail      := 1;
      g_mail_buffer.buffer    := mail_table_type();
      g_mail_buffer.buff_size := 1000;
      g_mail_buffer.buff_size := nvl(get_global_appender_param(x_app, 'SMTP', 'MAIL_BUFFER_LINES'), 1000);
      g_mail_buffer.buffer.EXTEND(g_mail_buffer.buff_size);
  END init_email_cyclic_buffer;

  /** Procedure enqueues a message to cyclic buffer (queue).
  * If the buffer is full the first message in queue is discarded.
  * @param x_app Application.
  * @param x_message Message.
  */
  PROCEDURE enqueue_into_cyclic_buffer(x_app IN VARCHAR2, x_message IN VARCHAR2) IS
  BEGIN

    -- if the size is NULL, buffer was not initialized
    IF g_mail_buffer.buff_size IS NULL THEN
      init_email_cyclic_buffer(x_app);
    END IF;

    -- enqueque the message
    g_mail_buffer.buffer(g_mail_buffer.tail) := x_message;

    -- cyclic incrementation of the tail
    IF g_mail_buffer.tail = g_mail_buffer.buff_size THEN
      g_mail_buffer.tail := 1;
    ELSE
      g_mail_buffer.tail := g_mail_buffer.tail + 1;
    END IF;

    -- if the tail position and the head position is the same
    -- then the buffer is full
    IF g_mail_buffer.head = g_mail_buffer.tail THEN

      -- discard the first message by cyclic incrementation of the head position
      IF g_mail_buffer.head = g_mail_buffer.buff_size THEN
        g_mail_buffer.head := 1;
      ELSE
        g_mail_buffer.head := g_mail_buffer.head + 1;
      END IF;
    END IF;
  END enqueue_into_cyclic_buffer;

  /** Function dequeues a message from cyclic buffer (queue).
  * @return Dequeued message.
  */
  FUNCTION dequeue_from_cyclic_buffer RETURN VARCHAR2 IS
    l_last_head PLS_INTEGER;
  BEGIN
    l_last_head := g_mail_buffer.head;

    IF g_mail_buffer.head = g_mail_buffer.buff_size THEN
      g_mail_buffer.head := 1;
    ELSE
      g_mail_buffer.head := g_mail_buffer.head + 1;
    END IF;
    RETURN g_mail_buffer.buffer(l_last_head);
  END dequeue_from_cyclic_buffer;

  /**
  * Function checks whether is cyclic buffer empty or not.
  * @return Flags inidicating whether is buffer empty or not.
  * {*} TRUE if the buffer is empty
  * {*} FALSE if the buffer is not empty
  */
  FUNCTION is_cyclic_buffer_empty RETURN BOOLEAN IS
  BEGIN
    RETURN nvl(g_mail_buffer.head = g_mail_buffer.tail, TRUE);
  END is_cyclic_buffer_empty;

  /**
  * Procedure parses call_stack and extracts logger and application from the stack.
  * @param o_logger Logger extracted from the stack.
  * @param o_app Application extracted from the stack.
  * @param x_method Method name.
  * @param x_call_stack Call stack.
  */
  PROCEDURE parse_stack(o_logger     OUT VARCHAR2,
                        o_app        OUT VARCHAR2,
                        x_method     IN VARCHAR2 DEFAULT NULL,
                        x_call_stack IN VARCHAR2 DEFAULT dbms_utility.format_call_stack()) IS
    l_offset       PLS_INTEGER;
    l_next_nl      PLS_INTEGER;
    l_stack_object VARCHAR2(92 CHAR);
    l_stack_line   VARCHAR2(255 CHAR);
    l_obj_schema   user_users.username%TYPE;
  BEGIN
    -- end of last line for this package
    l_offset := instr(x_call_stack, c_nl, instr(x_call_stack, c_package_name, pos => -1)) + c_nl_length;

    -- next end of line
    l_next_nl := instr(x_call_stack, c_nl, l_offset);
    IF l_next_nl = 0 THEN
      o_logger := c_root_logger_name;
      o_app := c_user;
    ELSE
      -- first line after last line of this package
      l_stack_line   := substr(x_call_stack, l_offset, l_next_nl - l_offset);
      -- object on that line
      l_stack_object := substr(l_stack_line, instr(l_stack_line, c_space, -1) + 1);
      -- schema of the object
      l_obj_schema   := substr(l_stack_object, 1, instr(l_stack_object, '.') - 1);
      IF x_method IS NOT NULL THEN
        l_stack_object := l_stack_object || '.' || upper(x_method);
      END IF;

      -- if schema was not determined, logging was called from anonymous block.
      IF l_obj_schema IS NULL THEN
        l_obj_schema := c_user;
      END IF;

      o_app  := get_app(l_obj_schema);

      o_logger := o_app || '.' || l_stack_object;
    END IF;

  END parse_stack;

  /**
  * Returns a logger configuration for given method.
  * @param x_method Name of method.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  FUNCTION get_logger(x_method          IN VARCHAR2 DEFAULT NULL,
                      x_always_from_ctx IN ctx_boolean DEFAULT c_false) RETURN logger_type IS
    l_logger       logger_type;
  BEGIN
    parse_stack(l_logger.logger, l_logger.app, x_method);
    l_logger.always_from_ctx := x_always_from_ctx;

    l_logger.log_level_severity := get_level_severity(get_current_used_level(l_logger.logger));
    l_logger.enabled_appenders  := get_current_used_appenders(l_logger.logger);

    RETURN l_logger;
  END get_logger;

  /**
  * Returns the root logger configuration.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  FUNCTION get_root_logger(x_always_from_ctx IN ctx_boolean DEFAULT c_false) RETURN logger_type IS
    l_logger logger_type;
  BEGIN
    parse_stack(l_logger.logger, l_logger.app);
    l_logger.logger          := c_root_logger_name;
    l_logger.always_from_ctx := x_always_from_ctx;

    l_logger.log_level_severity := get_level_severity(get_current_used_level(l_logger.logger));
    l_logger.enabled_appenders  := get_current_used_appenders(l_logger.logger);
    RETURN l_logger;
  END get_root_logger;

  /**
  * Returns the application logger configuration.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  FUNCTION get_app_logger(x_always_from_ctx IN ctx_boolean DEFAULT c_false) RETURN logger_type IS
    l_logger logger_type;
  BEGIN
    parse_stack(l_logger.logger, l_logger.app);
    l_logger.logger          := l_logger.app;
    l_logger.always_from_ctx := x_always_from_ctx;
    l_logger.log_level_severity := get_level_severity(get_current_used_level(l_logger.logger));
    l_logger.enabled_appenders  := get_current_used_appenders(l_logger.logger);
    RETURN l_logger;
  END get_app_logger;

  /**
  * Returns the schema logger configuration.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  FUNCTION get_schema_logger(x_always_from_ctx IN ctx_boolean DEFAULT c_false) RETURN logger_type IS
    l_logger logger_type;
  BEGIN
    parse_stack(l_logger.logger, l_logger.app);
    l_logger.logger          := l_logger.app || '.' || c_user;
    l_logger.always_from_ctx := x_always_from_ctx;
    l_logger.log_level_severity := get_level_severity(get_current_used_level(l_logger.logger));
    l_logger.enabled_appenders  := get_current_used_appenders(l_logger.logger);
    RETURN l_logger;
  END get_schema_logger;

  /** Procedure sends cyclic buffer. Parameters for sending are obtained
  * for given application.
  * @param x_app Application name.
  */
  PROCEDURE send_buffer(x_app IN t_app_appender.app%TYPE) IS
    l_host      t_app_appender.parameter_value%TYPE;
    l_from      t_app_appender.parameter_value%TYPE;
    l_to        t_app_appender.parameter_value%TYPE;
    l_cc        t_app_appender.parameter_value%TYPE;
    l_bcc       t_app_appender.parameter_value%TYPE;
    l_reply     utl_smtp.reply;
    l_conn      utl_smtp.connection;
    l_offset    PLS_INTEGER;
    l_comma_pos PLS_INTEGER;
    l_csv_list  VARCHAR2(32767);
  BEGIN
    l_host := get_current_appender_param(x_app, 'SMTP', 'SEND_MAIL_HOST');
    l_from := get_current_appender_param(x_app, 'SMTP', 'SEND_MAIL_FROM');

    l_conn := utl_smtp.open_connection(host       => l_host,
                                       port       => get_current_appender_param(x_app, 'SMTP', 'SEND_MAIL_PORT'),
                                       tx_timeout => get_current_appender_param(x_app,
                                                                                'SMTP',
                                                                                'SEND_MAIL_TIMEOUT'));

    l_reply := utl_smtp.helo(l_conn, l_host);

    IF l_reply.code <> 200 THEN
      NULL;
    END IF;

    l_reply := utl_smtp.mail(l_conn, l_from);

    IF l_reply.code <> 200 THEN
      NULL;
    END IF;

    l_to  := get_current_appender_param(x_app, 'SMTP', 'SEND_MAIL_TO');
    l_cc  := get_current_appender_param(x_app, 'SMTP', 'SEND_MAIL_CC');
    l_bcc := get_current_appender_param(x_app, 'SMTP', 'SEND_MAIL_BCC');

    IF l_to IS NOT NULL THEN
      l_csv_list := l_to || ',';
    END IF;

    IF l_cc IS NOT NULL THEN
      l_csv_list := l_csv_list || l_cc || ',';
    END IF;

    IF l_bcc IS NOT NULL THEN
      l_csv_list := l_csv_list || l_bcc || ',';
    END IF;

    l_offset := 1;
    LOOP
      l_comma_pos := instr(l_csv_list, ',', l_offset);
      EXIT WHEN l_comma_pos = 0;

      l_reply := utl_smtp.rcpt(l_conn, substr(l_csv_list, l_offset, l_comma_pos - l_offset));

      l_offset := l_comma_pos + 1;
    END LOOP;

    utl_smtp.open_data(l_conn);

    utl_smtp.write_data(l_conn, 'Content-Type: text/plain' || utl_tcp.crlf);
    utl_smtp.write_data(l_conn, 'From: ' || l_from || utl_tcp.crlf);
    utl_smtp.write_data(l_conn, 'To: ' || l_to || utl_tcp.crlf);
    utl_smtp.write_data(l_conn, 'Cc: ' || l_cc || utl_tcp.crlf);
    utl_smtp.write_data(l_conn,
                        'Subject: ' || get_current_appender_param(x_app, 'SMTP', 'SEND_MAIL_SUBJECT') ||
                        utl_tcp.crlf);

    WHILE NOT is_cyclic_buffer_empty() LOOP
      utl_smtp.write_data(l_conn,
                          --                          convert(
                          dequeue_from_cyclic_buffer() --,
                          --                                  get_current_appender_param(x_app,
                          --                                                             'SMTP',
                          --                                                             'SEND_MAIL_ORA_CHARSET'))
                          /*utl_encode.text_encode(dequeue_from_cyclic_buffer(),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           f_get_current_appender_param(x_app, 'SMTP', 'send_mail_ora_charset'),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           utl_encode.base64)*/);
    END LOOP;

    utl_smtp.close_data(l_conn);

    l_reply := utl_smtp.quit(l_conn);

    IF l_reply.code <> 200 THEN
      NULL;
    END IF;

  END send_buffer;

  /**
  * Procedure logs given message using SMTP protocol.
  * @param x_app Application name
  * @param x_logger_name Logger name.
  * @param x_log_level Log level.
  * @param x_message Message.
  * @param x_call_stack Flag, whether to log call stack.
  * @param x_log_backtrace Flag, whether to log backtrace.
  */
  PROCEDURE log_smtp(x_app           IN t_app_appender.app%TYPE,
                     x_logger_name   IN t_logger.logger%TYPE,
                     x_level         IN t_log_level.log_level%TYPE,
                     x_message       IN VARCHAR2,
                     x_call_stack    IN BOOLEAN,
                     x_log_backtrace IN BOOLEAN) IS
    l_layout        t_app_appender.parameter_value%TYPE;
    l_trigger_level t_log_level.log_level%TYPE;
  BEGIN
    l_layout := get_current_layout(x_app, c_smtp_appender);

    enqueue_into_cyclic_buffer(x_app, format_message(x_message, l_layout, x_logger_name, x_level) || c_nl);

    IF x_log_backtrace THEN
      enqueue_into_cyclic_buffer(x_app, dbms_utility.format_error_stack() || dbms_utility.format_error_backtrace() || c_nl);
    END IF;

    IF x_call_stack THEN
      enqueue_into_cyclic_buffer(x_app, format_call_stack() || c_nl);
    END IF;

    l_trigger_level := get_current_appender_param(x_app, 'SMTP', 'TRIGGER_LEVEL');

    IF get_level_severity(x_level) >= get_level_severity(l_trigger_level) THEN
      send_buffer(x_app);
    END IF;
  END log_smtp;

  /**
  * Procedure logs given message using dbms_output package.
  * @param x_app Application name
  * @param x_logger_name Logger name.
  * @param x_log_level Log level.
  * @param x_message Message.
  * @param x_call_stack Flag, whether to log call stack.
  * @param x_log_backtrace Flag, whether to log backtrace.
  */
  PROCEDURE log_stdout(x_app           IN t_app_appender.app%TYPE,
                       x_logger_name   IN t_logger.logger%TYPE,
                       x_level         IN t_log_level.log_level%TYPE,
                       x_message       IN VARCHAR2,
                       x_call_stack    IN BOOLEAN,
                       x_log_backtrace IN BOOLEAN) IS
    l_layout t_app_appender.parameter_value%TYPE;
  BEGIN
    l_layout := get_current_layout(x_app, c_dbms_output_appender);

    dbms_output.put_line(format_message(x_message, l_layout, x_logger_name, x_level));

    IF x_log_backtrace THEN
      dbms_output.put_line(dbms_utility.format_error_stack || dbms_utility.format_error_backtrace);
    END IF;

    IF x_call_stack THEN
      dbms_output.put_line(format_call_stack());
    END IF;
  END log_stdout;

  /**
  * Procedure logs given message to the log table.
  * @param x_app Application name
  * @param x_logger_name Logger name.
  * @param x_log_level Log level.
  * @param x_message Message.
  * @param x_call_stack Flag, whether to log call stack.
  * @param x_log_backtrace Flag, whether to log backtrace.
  */
  PROCEDURE log_table(x_app           IN t_app_appender.app%TYPE,
                      x_logger_name   IN t_logger.logger%TYPE,
                      x_level         IN t_log_level.log_level%TYPE,
                      x_message       IN VARCHAR2,
                      x_call_stack    IN BOOLEAN,
                      x_log_backtrace IN BOOLEAN) IS
    PRAGMA AUTONOMOUS_TRANSACTION;
    l_backtrace        t_log.backtrace%TYPE := NULL;
    l_call_stack       t_log.call_stack%TYPE := NULL;
    l_timestamp        t_log.log_date%TYPE := systimestamp;
    l_layout           t_app_appender.parameter_value%TYPE;
    l_formated_message t_log.message%TYPE;
  BEGIN
    l_layout := get_current_layout(x_app, c_table_appender);

    IF x_log_backtrace THEN
      l_backtrace := dbms_utility.format_error_stack || dbms_utility.format_error_backtrace;
    END IF;

    IF x_call_stack THEN
      l_call_stack := format_call_stack();
    END IF;

    l_formated_message := format_message(x_message, l_layout, x_logger_name, x_level);

    INSERT INTO t_log
      (id, logger, message, log_date, call_stack, backtrace, log_level)
    VALUES
      (seq_log_id.NEXTVAL,
       x_logger_name,
       l_formated_message,
       l_timestamp,
       l_call_stack,
       l_backtrace,
       x_level);
    COMMIT;
  END log_table;

  /**
  * Procedure logs given message.
  * @param x_app Application name
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_call_stack Flag, whether to log call stack.
  * @param x_log_backtrace Flag, whether to log backtrace.
  */
  PROCEDURE log(x_level          IN t_log_level.log_level%TYPE,
                x_logger         IN OUT NOCOPY logger_type,
                x_message        IN VARCHAR2,
                x_log_backtrace  IN BOOLEAN,
                x_log_call_stack IN BOOLEAN) IS
  BEGIN
    IF x_logger.always_from_ctx = c_true THEN
      x_logger.log_level_severity := get_level_severity(get_current_used_level(x_logger.logger));
    END IF;

    IF x_logger.log_level_severity > get_level_severity(x_level) THEN
      RETURN;
    END IF;

    IF x_logger.always_from_ctx = c_true THEN
      x_logger.enabled_appenders := get_current_used_appenders(x_logger.logger);
    END IF;

    --    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_appender_code', 'YES');
    --    $END
    IF bitand(x_logger.enabled_appenders, get_appender_code(c_table_appender)) > 0 THEN
      log_table(x_logger.app, x_logger.logger, x_level, x_message, x_log_call_stack, x_log_backtrace);
    END IF;

    --$IF  dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_appender_code', 'YES');
    --$END
    IF bitand(x_logger.enabled_appenders, get_appender_code(c_dbms_output_appender)) > 0 THEN
      log_stdout(x_logger.app, x_logger.logger, x_level, x_message, x_log_call_stack, x_log_backtrace);
    END IF;

    --$IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_appender_code', 'YES'); $END
    IF bitand(x_logger.enabled_appenders, get_appender_code(c_smtp_appender)) > 0 THEN
      log_smtp(x_logger.app, x_logger.logger, x_level, x_message, x_log_call_stack, x_log_backtrace);
    END IF;

  END log;

  /**
  * Procedure logs message with TRACE log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  PROCEDURE trace(x_logger         IN OUT NOCOPY logger_type,
                  x_message        IN VARCHAR2 DEFAULT SQLERRM,
                  x_log_backtrace  IN BOOLEAN DEFAULT FALSE,
                  x_log_call_stack IN BOOLEAN DEFAULT TRUE) IS
  BEGIN
    --$IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('log', 'YES');
    --$END
    log(c_trace_level, x_logger, x_message, x_log_backtrace, x_log_call_stack);
  END trace;

  /**
  * Procedure logs message with INFO log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  PROCEDURE info(x_logger         IN OUT NOCOPY logger_type,
                 x_message        IN VARCHAR2 DEFAULT SQLERRM,
                 x_log_backtrace  IN BOOLEAN DEFAULT FALSE,
                 x_log_call_stack IN BOOLEAN DEFAULT TRUE) IS
  BEGIN
    --$IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('log', 'YES');
    -- $END
    log(c_info_level, x_logger, x_message, x_log_backtrace, x_log_call_stack);
  END info;

  /**
  * Procedure logs message with DEBUG log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  PROCEDURE debug(x_logger         IN OUT NOCOPY logger_type,
                  x_message        IN VARCHAR2 DEFAULT SQLERRM,
                  x_log_backtrace  IN BOOLEAN DEFAULT FALSE,
                  x_log_call_stack IN BOOLEAN DEFAULT TRUE) IS
  BEGIN
    --$IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('log', 'YES');
    -- $END
    log(c_debug_level, x_logger, x_message, x_log_backtrace, x_log_call_stack);
  END debug;

  /**
  * Procedure logs message with WARN log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  PROCEDURE warn(x_logger         IN OUT NOCOPY logger_type,
                 x_message        IN VARCHAR2 DEFAULT SQLERRM,
                 x_log_backtrace  IN BOOLEAN DEFAULT TRUE,
                 x_log_call_stack IN BOOLEAN DEFAULT TRUE) IS
  BEGIN
    -- $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('log', 'YES');
    --$END
    log(c_warn_level, x_logger, x_message, x_log_backtrace, x_log_call_stack);
  END warn;

  /**
  * Procedure logs message with ERROR log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  PROCEDURE error(x_logger         IN OUT NOCOPY logger_type,
                  x_message        IN VARCHAR2 DEFAULT SQLERRM,
                  x_log_backtrace  IN BOOLEAN DEFAULT TRUE,
                  x_log_call_stack IN BOOLEAN DEFAULT TRUE) IS
  BEGIN
    --  $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('log', 'YES');
    -- $END
    log(c_error_level, x_logger, x_message, x_log_backtrace, x_log_call_stack);
  END error;

  /**
  * Procedure logs message with FATAL log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  PROCEDURE fatal(x_logger         IN OUT NOCOPY logger_type,
                  x_message        IN VARCHAR2 DEFAULT SQLERRM,
                  x_log_backtrace  IN BOOLEAN DEFAULT TRUE,
                  x_log_call_stack IN BOOLEAN DEFAULT TRUE) IS
  BEGIN
    --$IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('log', 'YES');
    --$END
    log(c_fatal_level, x_logger, x_message, x_log_backtrace, x_log_call_stack);
  END fatal;

  /**
  * Function checks, whether TRACE log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is TRACE or lower.
  * {*} FALSE if log level for given logger is higher than TRACE.
  */
  FUNCTION is_trace_enabled(x_logger IN OUT NOCOPY logger_type) RETURN BOOLEAN IS
  BEGIN
    --$IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity', 'YES');
    -- $END
    RETURN x_logger.log_level_severity <= get_level_severity(c_trace_level);
  END is_trace_enabled;

  /**
  * Function checks whether DEBUG log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is DEBUG or lower.
  * {*} FALSE if log level for given logger is higher than DEBUG.
  */
  FUNCTION is_debug_enabled(x_logger IN OUT NOCOPY logger_type) RETURN BOOLEAN IS
  BEGIN
    --$IF dbms_db_version.version >= 11 $THEN
    --  PRAGMA INLINE('get_level_severity', 'YES');
    --$END
    RETURN x_logger.log_level_severity <= get_level_severity(c_debug_level);
  END is_debug_enabled;

  /**
  * Function checks whether INFO log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is INFO or lower.
  * {*} FALSE if log level for given logger is higher than INFO.
  */
  FUNCTION is_info_enabled(x_logger IN OUT NOCOPY logger_type) RETURN BOOLEAN IS
  BEGIN
    --$IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity', 'YES');
    --$END
    RETURN x_logger.log_level_severity <= get_level_severity(c_info_level);
  END is_info_enabled;

  /**
  * Function checks whether WARN log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is WARN or lower.
  * {*} FALSE if log level for given logger is higher than WARN.
  */
  FUNCTION is_warn_enabled(x_logger IN OUT NOCOPY logger_type) RETURN BOOLEAN IS
  BEGIN
    --$IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity', 'YES');
    -- $END
    RETURN x_logger.log_level_severity <= get_level_severity(c_warn_level);
  END is_warn_enabled;

  /**
  * Function checks whether ERROR log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is ERROR or lower.
  * {*} FALSE if log level for given logger is higher than ERROR.
  */
  FUNCTION is_error_enabled(x_logger IN OUT NOCOPY logger_type) RETURN BOOLEAN IS
  BEGIN
    -- $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity', 'YES');
    --$END
    RETURN x_logger.log_level_severity <= get_level_severity(c_error_level);
  END is_error_enabled;

  /**
  * Function checks, whether FATAL log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is FATAL or lower.
  * {*} FALSE if log level for given logger is higher than FATAL.
  */
  FUNCTION is_fatal_enabled(x_logger IN OUT NOCOPY logger_type) RETURN BOOLEAN IS
  BEGIN
    -- $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity', 'YES');
    -- $END
    -- (get_level_severity(get_current_used_level(x_logger))
    RETURN x_logger.log_level_severity <= get_level_severity(c_fatal_level);
  END is_fatal_enabled;

  /** Procedure purges all global contexts used by logging */
  PROCEDURE purge_global_contexts IS
  BEGIN
    FOR l_row IN (SELECT a.base_context_name
                    FROM t_appender a) LOOP
      dbms_session.clear_all_context(l_row.base_context_name || '_G');
    END LOOP;
    dbms_session.clear_all_context(c_additivity_ctx(c_global_flag));
    dbms_session.clear_all_context(c_global_appenders_ctx);
    dbms_session.clear_all_context(c_logger_levels_ctx(c_global_flag));
    dbms_session.clear_all_context(c_global_levels_ctx);
    dbms_session.clear_all_context(c_logger_names_ctx(c_global_flag));
    dbms_session.clear_all_context(c_parameters_ctx(c_global_flag));
    dbms_session.clear_all_context(c_global_user_app_ctx);
    dbms_session.clear_all_context(c_logger_appenders_ctx(c_global_flag));
  END purge_global_contexts;

  /** Procedure purges all session contexts used by logging */
  PROCEDURE purge_session_contexts IS
  BEGIN
    FOR l_row IN (SELECT a.base_context_name
                    FROM t_appender a) LOOP
      dbms_session.clear_all_context(l_row.base_context_name || '_L');
    END LOOP;
    dbms_session.clear_all_context(c_additivity_ctx(c_session_flag));
    dbms_session.clear_all_context(c_logger_appenders_ctx(c_session_flag));
    dbms_session.clear_all_context(c_logger_levels_ctx(c_session_flag));
    dbms_session.clear_all_context(c_logger_names_ctx(c_session_flag));
    dbms_session.clear_all_context(c_parameters_ctx(c_global_flag));
  END purge_session_contexts;

  /** Procedure copies global setting to session settings */
  PROCEDURE copy_global_to_session IS
  BEGIN
    purge_session_contexts();
    init_params(c_session_flag);
    init_appenders(c_session_flag);
    init_loggers(c_session_flag);
  END copy_global_to_session;

BEGIN
  -- $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('hash_logger_name', 'YES');
  --$END
  g_root_logger_hash := hash_logger_name(c_root_logger_name);
  init_user_app;
  init_params(c_global_flag);
  init_levels;
  init_appenders(c_global_flag);
  init_loggers(c_global_flag);
END logging;
/

