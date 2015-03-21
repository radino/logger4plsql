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
  * (*) project page        https://github.com/radino/logger4plsql/
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

  -- these elements are defined only if internal debugging is set to TRUE
  $IF $$debug $THEN
  /** Type for severity collection for the internal debugging. */
  TYPE log_level_severity_type IS TABLE OF PLS_INTEGER INDEX BY t_log_level.log_level%TYPE;
  /** Collection for severities for the internal debugging. */
  g_log_level_severities log_level_severity_type;
  $END

  -- these elements are public when unit testing precompiler option is set to TRUE
  -- therefore they cannot be defined in the body
  $IF NOT $$unit_test OR $$unit_test IS NULL $THEN
  /** Type for visibility: session or global */
  SUBTYPE visibility_type IS PLS_INTEGER RANGE 1 .. 2;

  /** Flag for visibility: global */
  c_global_flag CONSTANT visibility_type := 1;

  /** Flag for visibility: session */
  c_session_flag CONSTANT visibility_type := 2;

  /** Type used for serialization of loggers parameters */
  TYPE logger_settings_type IS RECORD (
     enabled_appenders  t_logger.appenders%TYPE,
     log_level          t_logger.log_level%TYPE,
     additivity         t_logger.additivity%TYPE       
  );
  
  /** Collection type of loggers used for serialization of loggers parameters */
  TYPE logger_settings_col_type IS TABLE OF logger_settings_type INDEX BY t_logger.logger%TYPE;
  
  /** Type for application settings: application parameters and appender parameters */  
  TYPE app_settings_type IS RECORD (
     app_params       appender_params_type,
     appenders_params appenders_params_type
  );
  
  /** Collection of application settings */
  TYPE app_settings_col_type IS TABLE OF app_settings_type INDEX BY t_app.app%TYPE;
  
  /** Type for serialized logging settings. */
  TYPE deserialized_settings_type IS RECORD (
     loggers          logger_settings_col_type,
     app_settings     app_settings_col_type
  );

  $END

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

  /** Name of global context containing settings for a session. */
  c_modify_session_ctx CONSTANT ctx_namespace_type := 'CTX_LOGGER_MODIFY_SESSION_G';

  /** Names of global and session contexts containing logger names. */
  c_logger_names_ctx CONSTANT context_list_type := context_list_type('CTX_LOGGER_NAME_G', 'CTX_LOGGER_NAME_L');

  /** Name of global context containing schema-application mapping. */
  c_global_user_app_ctx CONSTANT ctx_namespace_type := 'CTX_LOGGER_USER_APP_G';

  /** Names of global and session context containing parameters for logging system. */
  c_parameters_ctx CONSTANT context_list_type := context_list_type('CTX_LOGGER_PARAMS_G',
                                                                   'CTX_LOGGER_PARAMS_L');

  /** Name of global context containing appenders' codes. */
  c_global_appenders_ctx CONSTANT ctx_namespace_type := 'CTX_LOGGER_APPENDERS_G';

  /** Suffixes of appenders context names regarding to visibility. */
  c_append_vis_suffix context_list_type := context_list_type('_G','_L');

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

  /** Identifier for the session which consists of two componets:
  * {*} instance number  Retrieved from sys_context('userenv', 'instance')
  * {*} session_id       Retrieved from sys_context('userenv', 'sessionid')
  * session_id has been chosen, bacause sid is recycled and serial# is not held in userenv context
  * The implication is: Feature for setting context for a given session cannot be used for SYSDBA accounts.
  */
  g_session_identifier global_context.attribute%TYPE;

  /** SMTP appender: Cyclic buffer global variable. */
  g_mail_buffer mail_cyclic_buffer_type;
  
  /** Hash for the root logger */
  g_root_logger_hash hash_type;

  /** A variable for builtding serialized settings */
  g_serialized_settings deserialized_settings_type;

   -- these elements are defined only if internal debugging is set to TRUE
  $IF $$debug $THEN
  /**
  * Procedure initializes log level severities for internal debugging
  */
  PROCEDURE init_log_level_severities IS    
  BEGIN
     FOR l_row IN (select ll.log_level, ll.severity FROM t_log_level ll) LOOP
       g_log_level_severities(l_row.log_level) := l_row.severity;
     END LOOP;  
  END init_log_level_severities;
  $END
  
  -- these elements are defined only if internal debugging is set to TRUE
  $IF $$debug $THEN
  /**
  * Procedure logs given message as an internal log.
  * @param x_level Log levelof the message.
  * @param x_logger Logger name.
  * @param x_message Message.
  * @param x_appender Binary encoded appenders (1 - table, 2 - dbms_output)
  */
  PROCEDURE internal_log(x_level    IN t_log_level.log_level%TYPE,
                         x_logger   IN t_logger.logger%TYPE,
                         x_message  IN message_type,
                         x_appender IN PLS_INTEGER DEFAULT g_internal_appenders) IS
    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN 
    IF g_log_level_severities(g_internal_log_level) > g_log_level_severities(x_level) THEN
       RETURN;
    END IF;
    
    IF bitand(x_appender, 1) = 1 THEN
      INSERT INTO t_log
        (id, logger, message, log_date, call_stack, backtrace, log_level)
      VALUES
        (seq_log_id.NEXTVAL,
         x_logger,
         x_message,
         systimestamp,
         NULL,
         NULL,
         x_level);
    END IF;
    
    IF bitand(x_appender, 2) = 2 THEN
      dbms_output.put_line(to_char(systimestamp, 'DD.MM.YYYY HH24:MI:SS.FF3') || ' ' || x_logger ||
                           '['||x_level||']: ' || x_message);
    END IF;
    
    COMMIT;
  END internal_log;
  $END

  /** Procedure raises uniplemented feature exception.
  */
  PROCEDURE unimplemented IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'unimplemented'; $END
  BEGIN
    $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'Unimplemented feature'); $END 
    raise_application_error('-20999', 'Unimplemented feature');
  END unimplemented;

  /**
  * Procedure sets given attribute of given context to given value.
  * @param x_namespace Name of context
  * @param x_attribute Attribute
  * @param x_value Value of attribute
  * @raises e_internal_use_only Can not be called from another schema.
  */
  PROCEDURE set_context(x_namespace IN ctx_namespace_type,
                        x_attribute IN ctx_attribute_type,
                        x_value     IN ctx_value_type) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_context'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_namespace: ' || x_namespace);
      internal_log(logging.c_debug_level, l_intlogger, 'x_attribute: ' || x_attribute);
      internal_log(logging.c_debug_level, l_intlogger, 'x_value: ' || x_value);
    $END 
  
    IF x_value IS NULL THEN
      $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'Clearing context'); $END             
      dbms_session.clear_context(x_namespace, x_attribute);
    ELSE
     $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'Setting context'); $END
     dbms_session.set_context(x_namespace, x_attribute, x_value);
    END IF;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END set_context;

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
                            x_value     IN ctx_value_type) IS                           
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_context_job'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_namespace: ' || x_namespace);
      internal_log(logging.c_debug_level, l_intlogger, 'x_attribute: ' || x_attribute);
      internal_log(logging.c_debug_level, l_intlogger, 'x_value: ' || x_value);
      internal_log(logging.c_debug_level, l_intlogger, 'c_user: ' || c_user);
      internal_log(logging.c_debug_level, l_intlogger, 'c_schema_name: ' || c_schema_name);
    $END 

    IF c_user <> c_schema_name THEN      
      $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'Access violation'); $END       
      raise_application_error(-20003, 'For internal use only');
    END IF;    
    
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('set_context',  'YES'); $END
    set_context(x_namespace, x_attribute, x_value);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END set_context_job;
  
  /**
  * Procedure clears all contexts. 
  * For internal use only. Do not use.
  * @param x_namespace Name of context
  * @raises e_internal_use_only Can not be called from another schema.
  */
  PROCEDURE clear_all_context(x_namespace IN ctx_namespace_type) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'clear_all_context'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');    
      internal_log(logging.c_debug_level, l_intlogger, 'x_namespace: ' || x_namespace);
      internal_log(logging.c_debug_level, l_intlogger, 'c_user: ' || c_user);
      internal_log(logging.c_debug_level, l_intlogger, 'c_schema_name: ' || c_schema_name);
    $END 

    IF c_user <> c_schema_name THEN
      $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'Access violation'); $END       
      raise_application_error(-20003, 'For internal use only');
    END IF;    
  
    $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'Clearing context'); $END
    dbms_session.clear_all_context(x_namespace);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END clear_all_context;
  
  /**
  * Procedure clears all contexts. 
  * @param x_namespace Name of context
  * @param x_visibility Flag, whether global or session contexts should cleared
  * {*} c_global_flag Set flag for a global context - rac aware
  * {*} c_session_flag Set flag for a session context - current instance
  */
  PROCEDURE clear_all_context_rac_aware(x_namespace IN ctx_namespace_type,
                                        x_visibility IN visibility_type) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'clear_all_context_rac_aware'; $END
    l_instance_count NUMBER;
    l_instance_table dbms_utility.instance_table;
    l_what           user_jobs.what%TYPE;
    e_invalid_instance EXCEPTION;
    PRAGMA EXCEPTION_INIT(e_invalid_instance, -23428);
  BEGIN
    $IF $$debug $THEN 
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_namespace: ' || x_namespace);
    $END 

    -- until 11.2 the context changes are not replicated across the instances
    -- a workaround is to use an instance affinity for jobs to set the context on all active instances
    $IF logging.ver_lt_11_2 $THEN      
    $IF $$debug $THEN internal_log(logging.c_warning_level, l_intlogger, 'RAC-aware context not supported.'); $END 
    dbms_utility.active_instances(instance_table => l_instance_table, instance_count => l_instance_count);
    
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'Number of RAC instances: ' || l_instance_count); $END 

    IF l_instance_count = 0 OR x_visibility = c_session_flag THEN 
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'No RAC - clearing the context' ); $END 
      dbms_session.clear_all_context(x_namespace => x_namespace);
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
      RETURN;
    END IF;    

    l_what := 'logging.clear_all_context(''' || x_namespace || ''');';
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_what: ' || l_what); $END 
    
    DECLARE 
      PRAGMA AUTONOMOUS_TRANSACTION;
    BEGIN
      FOR i IN 1 .. l_instance_count LOOP
        BEGIN          
          $IF $$debug $THEN 
            internal_log(logging.c_trace_level, l_intlogger, 'l_what: ' || l_what); 
            internal_log(logging.c_trace_level, l_intlogger, 'inst_number: ' || l_instance_table(i).inst_number); 
          $END 
          dbms_job.submit(job       => l_job_number,
                          what      => l_what,
                          next_date => SYSDATE,
                          INTERVAL  => NULL,
                          instance  => l_instance_table(i).inst_number);
          -- if there is no such instance ignore error (or it is not running)
          $IF $$debug $THEN 
            internal_log(logging.c_trace_level, l_intlogger, 'l_job_number: ' || l_job_number); 
            internal_log(logging.c_info_level, l_intlogger, 'job created: '); 
          $END 
        EXCEPTION          
          WHEN e_invalid_instance THEN
            $IF $$debug $THEN 
              internal_log(logging.c_warning_level, l_intlogger, 'Invalid instance: ' || l_instance_table(i).inst_number); 
            $END 

            NULL;
        END;
      END LOOP;
      COMMIT;
    END;
    $ELSE
      -- if the database version is >=11.2, applicaton context is replicated across the instances
      $IF $$debug $THEN 
        internal_log(logging.c_info_level, l_intlogger, 'RAC >=11.2, context supported'); 
        internal_log(logging.c_trace_level, l_intlogger, 'Clearing context: ' || x_namespace); 
      $END 
      dbms_session.clear_all_context(namespace => x_namespace);
    $END
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END clear_all_context_rac_aware;

  /** Procedure sets given attribute of given context to the given value. 
  * Procedure is RAC-aware. If the database is in RAC the context is set 
  * @param x_namespace Name of context
  * @param x_attribute Attribute
  * @param x_value Value of attribute
  * @param x_visibility Flag, whether global or session contexts should be initialized.
  * {*} c_global_flag Set flag for a global context - rac aware
  * {*} c_session_flag Set flag for a session context - current instance
  */
  PROCEDURE set_context_rac_aware(x_namespace      IN ctx_namespace_type,
                                  x_attribute      IN ctx_attribute_type,
                                  x_value          IN ctx_value_type,
                                  x_visibility     IN visibility_type) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_context_rac_aware'; $END
    l_job_number     BINARY_INTEGER;
    l_what           user_jobs.what%TYPE;
    l_instance_count NUMBER;
    l_instance_table dbms_utility.instance_table;
    e_invalid_instance EXCEPTION;
    PRAGMA EXCEPTION_INIT(e_invalid_instance, -23428);
    
  BEGIN    
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_namespace: ' || x_namespace);
      internal_log(logging.c_debug_level, l_intlogger, 'x_attribute: ' || x_attribute);
      internal_log(logging.c_debug_level, l_intlogger, 'x_value: ' || x_value);
    $END 


    -- until 11.2 the context changes are not replicated across the instances
    -- a workaround is to use an instance affinity for jobs to set the context on all active instances
    $IF logging.ver_lt_11_2 $THEN 
      $IF $$debug $THEN internal_log(logging.c_warning_level, l_intlogger, 'version < 11.2 - RAC aware context not supported'); $END 
      dbms_utility.active_instances(instance_table => l_instance_table, instance_count => l_instance_count);
      $IF $$debug $THEN 
        internal_log(logging.c_debug_level, l_intlogger, 'instance table: ');
        FOR i IN 1 .. l_instance_count LOOP
          internal_log(logging.c_trace_level, 'Instance number: ' || l_instance_table(i).instance_number);          
          internal_log(logging.c_trace_level, 'Instance number: ' || l_instance_table(i).instance_name);
        END LOOP;
      $END 
   

      IF l_instance_count = 0 or x_visibility = c_session_flag  THEN 
        $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'No RAC - setting the context locally '); $END
        $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('set_context',  'YES'); $END
        set_context(x_namespace => x_namespace, x_attribute => x_attribute, x_value => x_value); 
        RETURN;
      END IF;    

      l_what := 'logging.set_context_job(
                      ''' || x_namespace || ''',
                      ''' || x_attribute || ''',
                      ''' || x_value || '''
                  );';

      $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'job: ' || l_what); $END
        
      DECLARE 
        PRAGMA AUTONOMOUS_TRANSACTION;
      BEGIN
        FOR i IN 1 .. l_instance_count LOOP
          BEGIN
            dbms_job.submit(job       => l_job_number,
                            what      => l_what,
                            next_date => SYSDATE,
                            INTERVAL  => NULL,
                            instance  => l_instance_table(i).inst_number);
            -- if there is no such instance ignore error (or it is not running)
            $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'job: ' || l_job_number || ' for instance ' || l_instance_table(i).inst_number); $END
          EXCEPTION
            WHEN e_invalid_instance THEN
              $IF $$debug $THEN internal_log(logging.c_warning_level, l_intlogger, 'Invalid instance: ' || l_instance_table(i).inst_number); $END
              NULL;
          END;

        END LOOP;
        COMMIT;
      END;
    $ELSE
      -- if the database version is >=11.2, applicaton context is replicated across the instances
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'version >=11.2, rac aware context supported'); $END
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('set_context',  'YES'); $END
      set_context(x_namespace => x_namespace, x_attribute => x_attribute, x_value => x_value);
    $END
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END set_context_rac_aware;

  /**
  * Function returns bitwise OR of given numbers.
  * @param x_n1 First operand.
  * @param x_n2 Second operand.
  * @return Bitwise OR of given numbers.
  */
  FUNCTION bit_or(x_n1 IN NUMBER,
                  x_n2 IN NUMBER) RETURN NUMBER IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'bit_or'; $END
    l_result NUMBER;
  BEGIN
    $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'x_n1: ' || x_n1 || ', x_n2 ' || x_n2); $END
    l_result := x_n1 + x_n2 - bitand(x_n1, x_n2);
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
    RETURN l_result; 
  END bit_or;

  /**
  * Function returns bitwise exclusive OR of given numbers.
  * @param x_n1 First operand.
  * @param x_n2 Second operand.
  * @return Bitwise exclusive OR of given numbers.
  */
  FUNCTION bit_xor(x_n1 IN NUMBER,
                   x_n2 IN NUMBER) RETURN NUMBER IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'bit_xor'; $END
    l_result NUMBER;
  BEGIN
    $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'x_n1: ' || x_n1 || ', x_n2 ' || x_n2); $END
    -- would be nice to have bitwise shifts in PL/SQL: *2 => << 1
    l_result := x_n1 + x_n2 - 2 * bitand(x_n1, x_n2);
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
    RETURN l_result;
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'serialize_to_xml'; $END
    l_result VARCHAR2(4000);
  BEGIN
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'start'); $END
    IF x_names.FIRST IS NULL THEN
      $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'Names collection is empty'); $END
      RETURN NULL;
    END IF;
    l_result := '<params>';
    FOR i IN x_names.FIRST .. x_names.LAST LOOP
      $IF $$debug $THEN 
        internal_log(logging.c_trace_level, l_intlogger, 'x_names('||i||'): ' || x_names(i)); 
        internal_log(logging.c_trace_level, l_intlogger, 'x_values('||i||'): ' || x_values(i));
      $END
      IF x_values(i) IS NULL THEN
        l_result := l_result || '<' || x_names(i) || '/>';
      ELSE
        l_result := l_result || '<' || x_names(i) || '>' || x_values(i) || '</' || x_names(i) || '>';
      END IF;
    END LOOP;
    l_result := l_result || '</params>';
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'serialize_to_json'; $END
    l_result VARCHAR2(4000);
    c_nl CONSTANT VARCHAR2(1) := chr(10);
  BEGIN
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'start'); $END
    IF x_names.FIRST IS NULL THEN
      $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'Names collection is empty'); $END
      RETURN NULL;
    END IF;

    FOR i IN x_names.FIRST .. x_names.LAST LOOP
      $IF $$debug $THEN 
        internal_log(logging.c_trace_level, l_intlogger, 'x_names('||i||'): ' || x_names(i)); 
        internal_log(logging.c_trace_level, l_intlogger, 'x_values('||i||'): ' || x_values(i));
      $END
      IF x_values(i) IS NULL THEN
        l_result := l_result || x_values(i) || ': ' || x_names(i) || c_nl;
      END IF;
    END LOOP;
    
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_app'; $END
  BEGIN
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'start and end'); $END
    RETURN sys_context(c_global_user_app_ctx, x_schema);
  END get_app;

  /**
  * Function returns call stack (without functions of this package).
  * @return Call stack.
  */
  FUNCTION format_call_stack RETURN VARCHAR2 IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'format_call_stack'; $END
    l_header_end  PLS_INTEGER;
    l_logging_end PLS_INTEGER;
    l_call_stack  VARCHAR2(2000 CHAR);
    l_result VARCHAR2(2000 CHAR);
    c_stack_body_offset CONSTANT PLS_INTEGER := 3;
  BEGIN    
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'start'); $END
    l_call_stack := dbms_utility.format_call_stack();
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_call_stack: ' || l_call_stack); $END
    -- skip header
    l_header_end := instr(l_call_stack, c_nl, nth => c_stack_body_offset) + c_nl_length;
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_header_end: ' || l_header_end); $END

    l_logging_end := instr(l_call_stack, c_nl, instr(l_call_stack, c_package_name, -1, 1));
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_logging_end: ' || l_logging_end); $END    

    l_result := substr(l_call_stack, 1, l_header_end) || substr(l_call_stack, l_logging_end + c_nl_length);
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END    
    RETURN l_result;
  END format_call_stack;
  
  /**
  * Procedure adds given schema to given application.
  * @param x_app Application name.
  * @param x_schema Schema name.
  */
  PROCEDURE add_schema_to_app(x_app    IN t_schema_app.app%TYPE,
                              x_schema IN t_schema_app.schema%TYPE DEFAULT USER) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'add_schema_to_app'; $END
    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN
    $IF $$debug $THEN 
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_schema: ' || x_schema); 
    $END    
    INSERT INTO t_schema_app
      (SCHEMA, app)
    VALUES
      (upper(x_schema), upper(x_app));
    
    set_context_rac_aware(c_global_user_app_ctx, upper(x_schema), upper(x_app), c_global_flag);

    COMMIT;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  EXCEPTION
    WHEN dup_val_on_index THEN
      $IF $$debug $THEN 
        internal_log(logging.c_warn_level, l_intlogger, 'Schema is already assigned to the app');
        internal_log(logging.c_info_level, l_intlogger, 'end');
      $END 
      NULL;
  END add_schema_to_app;

  /**
  * Procedure removes given schema from given application.
  * @param x_app Application name.
  * @param x_schema Schema name.
  */
  PROCEDURE remove_schema_from_app(x_app    IN t_schema_app.app%TYPE,
                                   x_schema IN t_schema_app.schema%TYPE DEFAULT USER) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'remove_schema_from_app'; $END
    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN
    $IF $$debug $THEN 
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_schema: ' || x_schema); 
    $END    

    DELETE FROM t_schema_app ua
     WHERE ua.SCHEMA = upper(x_schema)
       AND ua.app = upper(x_app);

    $IF $$debug $THEN 
      IF SQL%NOTFOUND THEN
        internal_log(logging.c_warn_level, l_intlogger, 'Schema is not assigned to the app'); 
      END IF;
    $END 
    set_context_rac_aware(c_global_user_app_ctx, upper(x_schema), NULL, c_global_flag); -- it's case insensitive, but for clarity..
    COMMIT;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END remove_schema_from_app;

  /**
  * Procedure sets initialization flag or clears a given context.
  * @param x_ctx Context name.
  * @param x_initialized Intialization flag.
  * {*} TRUE Set context flag as initialized.
  * {*} FALSE Clear the context. Flag will be NULL.
  * @param x_visibility Flag, whether global or session contexts should be initialized.
  * {*} c_global_flag Set flag for a global context - rac aware
  * {*} c_session_flag Set flag for a session context - current instance
  */
  PROCEDURE set_initialization(x_ctx         IN ctx_namespace_type,
                               x_initialized IN BOOLEAN,
                               x_visibility  IN visibility_type) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_initialization'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_ctx: ' || x_ctx); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_initialized: ' || bool_to_int(x_initialized)); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_visibility: ' || x_visibility); 
    $END

    IF x_initialized THEN
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'Setting the context'); $END      
      set_context_rac_aware(x_ctx, c_init_param, bool_to_int(x_initialized), x_visibility);
    ELSE
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'Clearing the context'); $END
      set_context_rac_aware(x_ctx, c_init_param, NULL, x_visibility);
    END IF;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END set_initialization;

  /**
  * Function returns whether is context initialized or not.
  * @param x_ctx Context name.
  * @return Flag indication whether is context initialized or not.
  */
  FUNCTION is_initialized(x_ctx IN ctx_namespace_type) RETURN BOOLEAN IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'is_initialized'; $END
  BEGIN
    $IF $$debug $THEN 
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_ctx: ' || x_ctx);
    $END

    IF sys_context(x_ctx, c_init_param) IS NOT NULL THEN
      $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'Result (end): TRUE'); $END
      RETURN TRUE;
    END IF;

    $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'Result (end): FALSE'); $END
    RETURN FALSE;
  END is_initialized;

  /**
  * Procedure initializes a context for log levels. Lazy initialization is used.
  */
  PROCEDURE init_levels IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'init_levels'; $END
  BEGIN
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'start'); $END  
  
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('is_initialized',  'YES'); $END
    IF is_initialized(c_global_levels_ctx) THEN
      $IF $$debug $THEN 
        internal_log(logging.c_info_level, l_intlogger, 'Context already initialized');
        internal_log(logging.c_debug_level, l_intlogger, 'end'); 
      $END
      RETURN;
    END IF;

    FOR l_row IN (SELECT ll.log_level, ll.severity
                    FROM t_log_level ll) LOOP
      $IF $$debug $THEN 
        internal_log(logging.c_trace_level, l_intlogger, 'log_level' || l_row.log_level);
        internal_log(logging.c_trace_level, l_intlogger, 'severity' || l_row.severity);
      $END
      set_context_rac_aware(c_global_levels_ctx, l_row.log_level, l_row.severity, c_global_flag);
    END LOOP;

    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'Context has been initialized'); $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('set_initialization',  'YES'); $END
    set_initialization(c_global_levels_ctx, TRUE, c_global_flag);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END init_levels;

  /**
  * Procedure initializes a global or session contexts for appenders.
  * Lazy initialization is used.
  * @param x_global Flag, whether global or session contexts should be initialized.
  * {*} c_global_flag Initialize global contexts
  * {*} c_session_flag Initialize session contexts
  */
  PROCEDURE init_appenders(x_visibility IN visibility_type DEFAULT c_global_flag) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'init_appenders'; $END
    l_current_appender_ctx ctx_namespace_type;
    l_ctx_suffix           VARCHAR2(2);
  BEGIN
    $IF $$debug $THEN 
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_visibility: ' || x_visibility);
    $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('is_initialized',  'YES'); $END
    IF x_visibility = c_global_flag AND is_initialized(c_global_appenders_ctx) THEN
      $IF $$debug $THEN 
        internal_log(logging.c_info_level, l_intlogger, 'Context already initialized');
        internal_log(logging.c_debug_level, l_intlogger, 'end'); 
      $END
      RETURN;
    END IF;

    l_ctx_suffix := CASE x_visibility WHEN c_global_flag THEN '_G' ELSE '_L' END;
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_ctx_suffix: ' || l_ctx_suffix); $END

    FOR l_row IN (SELECT a.appender, a.code, a.base_context_name
                    FROM t_appender a) LOOP

      l_current_appender_ctx := l_row.base_context_name || l_ctx_suffix;
      $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_current_appender_ctx: ' || l_current_appender_ctx); $END

      -- there is no session context for appenders
      IF x_visibility = c_global_flag THEN
        set_context_rac_aware(c_global_appenders_ctx, l_row.appender, l_row.base_context_name, c_global_flag);
      END IF;      
      
      set_context_rac_aware(l_current_appender_ctx, 'DEFAULT#CODE', l_row.code, x_visibility);

      FOR l_row2 IN (SELECT aa.app, aa.parameter_name, aa.parameter_value
                       FROM t_app_appender aa
                      WHERE aa.appender = l_row.appender) LOOP
        $IF $$debug $THEN 
          internal_log(logging.c_trace_level, l_intlogger, 'l_current_appender_ctx: ' || l_current_appender_ctx); 
          internal_log(logging.c_trace_level, l_intlogger, 'app#parameter_name: ' || l_row2.app || '#' || l_row2.parameter_name); 
          internal_log(logging.c_trace_level, l_intlogger, 'parameter_value: ' || l_row2.parameter_value); 
        $END
        set_context_rac_aware(l_current_appender_ctx,
                              l_row2.app || '#' || l_row2.parameter_name,
                              l_row2.parameter_value,
                              x_visibility);
      END LOOP;

      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('set_initialization',  'YES'); $END
      set_initialization(l_current_appender_ctx, TRUE, x_visibility);
    END LOOP;

    -- there is no session context for appenders
    IF x_visibility = c_global_flag THEN
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('set_initialization',  'YES'); $END
      set_initialization(c_global_appenders_ctx, TRUE, c_global_flag);
    END IF;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END init_appenders;

  /**
  * Procedure initializes a global context for parameters of logging system.
  * Lazy initialization is used.
  * @param x_visibility Flag, whether global or session contexts should be initialized.
  * {*} c_global_flag  Initialize global contexts
  * {*} c_session_flag Initialize session contexts
  * @param x_app Application. Useful for the session context initialization. If set,
  *              only context for given application is initialized.
  */
  PROCEDURE init_params(x_visibility IN visibility_type DEFAULT c_global_flag,
                        x_app        IN t_param.app%TYPE DEFAULT NULL) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'init_params'; $END
  BEGIN
    $IF $$debug $THEN 
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_visibility: ' || x_visibility); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
    $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('is_initialized',  'YES'); $END
    IF is_initialized(c_parameters_ctx(x_visibility)) THEN
      $IF $$debug $THEN 
        internal_log(logging.c_info_level, l_intlogger, 'Context already initialized');
        internal_log(logging.c_debug_level, l_intlogger, 'end'); 
      $END
      RETURN;
    END IF;

    FOR l_row IN (SELECT p.app, p.param_name, p.param_value
                    FROM t_param p
                   WHERE p.app = x_app OR x_app IS NULL) LOOP

      $IF $$debug $THEN 
        internal_log(logging.c_trace_level, l_intlogger, 'c_parameters_ctx(x_visibility): ' || c_parameters_ctx(x_visibility)); 
        internal_log(logging.c_trace_level, l_intlogger, 'app#param_name: ' || l_row.app || '#' || l_row.param_name); 
        internal_log(logging.c_trace_level, l_intlogger, 'param_value: ' || l_row.param_value); 
      $END
      
      set_context_rac_aware(c_parameters_ctx(x_visibility),
                            l_row.app || '#' || l_row.param_name,
                            l_row.param_value,
                            x_visibility);
    END LOOP;
    
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('set_initialization',  'YES'); $END
    set_initialization(c_parameters_ctx(x_visibility), TRUE, x_visibility);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
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
  * @param x_app Application. Useful for the session context initialization. If set,
  *              only context for given application is initialized.
  */
  PROCEDURE init_loggers(x_visibility IN visibility_type DEFAULT c_global_flag,
                         x_app        IN t_app.app%TYPE DEFAULT NULL) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'init_loggers'; $END
    l_hlogger hash_type;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_visibility: ' || x_visibility); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
    $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('is_initialized',  'YES'); $END
    IF is_initialized(c_logger_names_ctx(x_visibility)) THEN
      $IF $$debug $THEN 
        internal_log(logging.c_info_level, l_intlogger, 'Context already initialized');
        internal_log(logging.c_debug_level, l_intlogger, 'end'); 
      $END
      RETURN;
    END IF;

    FOR l_row IN (SELECT l.logger, l.log_level, l.appenders, l.additivity
                    FROM t_logger l
                   WHERE l.logger LIKE x_app || '.%' OR x_app IS NULL) LOOP

      $IF $$debug $THEN 
        internal_log(logging.c_trace_level, l_intlogger, 'c_logger_names_ctx(x_visibility): ' || c_logger_names_ctx(x_visibility)); 
        internal_log(logging.c_trace_level, l_intlogger, 'c_logger_levels_ctx(x_visibility): ' || c_logger_levels_ctx(x_visibility)); 
        internal_log(logging.c_trace_level, l_intlogger, 'c_logger_appenders_ctx(x_visibility): ' || c_logger_appenders_ctx(x_visibility)); 
        internal_log(logging.c_trace_level, l_intlogger, 'c_additivity_ctx(x_visibility): ' || c_additivity_ctx(x_visibility)); 
        internal_log(logging.c_trace_level, l_intlogger, 'l_row.logger: ' || l_row.logger);
        internal_log(logging.c_trace_level, l_intlogger, 'l_row.log_level: ' || l_row.log_level);
        internal_log(logging.c_trace_level, l_intlogger, 'l_row.appenders: ' || l_row.appenders); 
        internal_log(logging.c_trace_level, l_intlogger, 'l_row.additivity: ' || l_row.additivity); 
      $END

      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('hash_logger_name',  'YES'); $END
      l_hlogger := hash_logger_name(l_row.logger);
      $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger: ' || l_hlogger); $END 

      set_context_rac_aware(c_logger_names_ctx(x_visibility), l_hlogger, l_row.logger, x_visibility);
      set_context_rac_aware(c_logger_levels_ctx(x_visibility), l_hlogger, l_row.log_level, x_visibility);
      set_context_rac_aware(c_logger_appenders_ctx(x_visibility), l_hlogger, l_row.appenders, x_visibility);
      set_context_rac_aware(c_additivity_ctx(x_visibility), l_hlogger, l_row.additivity, x_visibility);
    END LOOP;
     
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('set_initialization',  'YES'); $END
    set_initialization(c_logger_names_ctx(x_visibility), TRUE, x_visibility);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END init_loggers;

  /**
  * Procedure initializes g_session_identified.
  */  
  PROCEDURE init_session_identifier IS 
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'init_session_identifier'; $END
  BEGIN
    $IF $$debug $THEN 
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'end'); 
    $END
    g_session_identifier := sys_context('userenv', 'instance') || '#' || sys_context('userenv', 'sessionid');
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'g_session_identifier: ' || g_session_identifier);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
  END init_session_identifier; 

  /**
  * Procedure initializes a global context schema-application mapping.
  * Lazy initialization is used.
  */
  PROCEDURE init_user_app IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'init_user_app'; $END
  BEGIN
    $IF $$debug $THEN
        internal_log(logging.c_info_level, l_intlogger, 'start');
        internal_log(logging.c_debug_level, l_intlogger, 'end'); 
    $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('is_initialized',  'YES'); $END
    IF is_initialized(c_global_user_app_ctx) THEN
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'Context already initialized'); $END
      RETURN;
    END IF;

    FOR l_row IN (SELECT ua.schema, ua.app
                    FROM t_schema_app ua) LOOP

      $IF $$debug $THEN 
        internal_log(logging.c_trace_level, l_intlogger, 'schema: ' || l_row.schema); 
        internal_log(logging.c_trace_level, l_intlogger, 'app: ' || l_row.app); 
      $END
      set_context_rac_aware(c_global_user_app_ctx, l_row.schema, l_row.app, c_global_flag);
    END LOOP;
    
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('set_initialization',  'YES'); $END
    set_initialization(c_global_user_app_ctx, TRUE, c_global_flag);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END init_user_app;

  /**
  * Procedure sets flag, which indicates that session setting (session context) will be used for logging.
  * @param x_usage Flag, which indicates that session setting (session context) will be used for logging.
  * {*} TRUE Session settings will be used (local context)
  * {*} FALSE Global settings will be used (global context)
  */
  PROCEDURE set_session_ctx_usage(x_usage IN BOOLEAN) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_session_ctx_usage'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_usage: ' || bool_to_int(x_usage)); 
      internal_log(logging.c_trace_level, l_intlogger, 'c_parameters_ctx(c_session_flag): ' || c_parameters_ctx(c_session_flag)); 
    $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bool_to_int',  'YES'); $END
    set_context_rac_aware(c_parameters_ctx(c_session_flag),
                          c_session_usage_param,
                          bool_to_int(x_usage),
                          c_session_flag);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_global_layout'; $END
    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN
    $IF $$debug $THEN 
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender: ' || x_appender); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_layout: ' || x_layout); 
    $END
    
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('set_global_appender_param',  'YES'); $END
    set_global_appender_param(x_app             => x_app,
                              x_appender        => x_appender,
                              x_parameter_name  => c_layout_param,
                              x_parameter_value => x_layout);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_session_layout'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender: ' || x_appender); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_layout: ' || x_layout); 
    $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('set_session_appender_param','YES'); $END
    set_session_appender_param(x_app             => x_app,
                               x_appender        => x_appender,
                               x_parameter_name  => c_layout_param,
                               x_parameter_value => x_layout);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_global_appender_param'; $END
    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender: ' || x_appender); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_parameter_name: ' || x_parameter_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_parameter_value: ' || x_parameter_value); 
    $END


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
    
    set_context_rac_aware(sys_context(c_global_appenders_ctx, x_appender) || '_G',
                          x_app || '#' || x_parameter_name,
                          x_parameter_value,
                          c_global_flag);

    COMMIT;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_session_appender_param'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender: ' || x_appender); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_parameter_name: ' || x_parameter_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_parameter_value: ' || x_parameter_value); 
    $END

    set_context_rac_aware(sys_context(c_global_appenders_ctx, x_appender) || '_L',
                          x_app || '#' || x_parameter_name,
                          x_parameter_value,
                          c_session_flag);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END set_session_appender_param;

  /**
  * Function returns a flag whether session settings are used for logging.
  * @return Flag whether session settings are used for logging.
  */
  FUNCTION get_session_ctx_usage RETURN BOOLEAN IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_session_ctx_usage'; $END
    l_result BOOLEAN;
  BEGIN
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'start'); $END
    l_result := CASE sys_context(c_parameters_ctx(c_session_flag), c_session_usage_param) WHEN '1' THEN TRUE ELSE FALSE END;
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || bool_to_int(l_result)); 
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
    RETURN l_result;
  END get_session_ctx_usage;

  /**
  * Function returns appender code for given appender name.
  * @param x_appender Appender name.
  * @return Appender code.
  */
  FUNCTION get_appender_code(x_appender IN t_appender.appender%TYPE) RETURN t_appender.code%TYPE IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_appender_code'; $END
    l_result t_appender.code%TYPE;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender: ' || x_appender); 
    $END
    l_result := sys_context(sys_context(c_global_appenders_ctx, x_appender) || '_G', 'DEFAULT#CODE');
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
    RETURN l_result;
  END get_appender_code;

  /**
  * Function returns global or session layout for given application, appender and visibility.
  * If layout is not set, default layout is returned.
  * @param x_app Application name.
  * @param x_appender Appender name.
  * @param x_visibility Global or session visibility  
  * @return 
  *   {*} Global layout for given application and appender, if visibility is c_global_flag
  *   {*} Session layout for given application and appender, if visibility is c_session_flag
  */
  FUNCTION get_layout(x_app        IN t_app_appender.app%TYPE,
                      x_appender   IN t_app_appender.appender%TYPE,
                      x_visibility IN visibility_type DEFAULT c_global_flag)
    RETURN t_app_appender.parameter_value%TYPE IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_layout'; $END
    l_result t_app_appender.parameter_value%TYPE;
  BEGIN
    $IF $$debug $THEN 
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender: ' || x_appender); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_visibility: ' || x_visibility); 
    $END
    l_result := coalesce(sys_context(sys_context(c_global_appenders_ctx, x_appender) || c_append_vis_suffix(x_visibility), x_app || '#LAYOUT'),
                         sys_context(c_parameters_ctx(c_global_flag), c_default_layout_param));
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result); 
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
    RETURN l_result;
  END get_layout;

  /**
  * Function returns global or session parameter value for given application, appender, parameter and visibility.
  * @param x_app Application name.
  * @param x_appender Appender name.
  * @param x_parameter_name Parameter name.
  * @param x_visibility Global or session visibility  
  * @return Global or session parameter value for given application, appender and parameter.
  *   {*} Global parameter value for given application, appender and parameter, if visibility is c_global_flag
  *   {*} Session parameter value for given application, appender and parameter, if visibility is c_session_flag
  */
  FUNCTION get_appender_param(x_app            IN t_app_appender.app%TYPE,
                              x_appender       IN t_app_appender.appender%TYPE,
                              x_parameter_name IN t_app_appender.parameter_name%TYPE,
                              x_visibility     IN visibility_type DEFAULT c_global_flag)
    RETURN t_app_appender.parameter_value%TYPE IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_appender_param'; $END
    l_result t_app_appender.parameter_value%TYPE;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender: ' || x_appender); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_parameter_name: ' || x_parameter_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_visibility: ' || x_visibility);
    $END
    l_result := sys_context(sys_context(c_global_appenders_ctx, x_appender) || c_append_vis_suffix(x_visibility),
                            x_app || '#' || x_parameter_name);
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result); 
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
    RETURN l_result;
  END get_appender_param;

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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_current_appender_param'; $END
    l_result t_app_appender.parameter_value%TYPE; 
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender: ' || x_appender); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_parameter_name: ' || x_parameter_name); 
    $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_session_ctx_usage',  'YES'); $END
    IF get_session_ctx_usage() THEN
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'session context'); $END
      l_result := get_appender_param(x_app, x_appender, x_parameter_name, c_session_flag);
    ELSE
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'global context'); $END
      l_result := get_appender_param(x_app, x_appender, x_parameter_name, c_global_flag);
    END IF;
    
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result); 
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
    RETURN l_result;
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_current_layout'; $END
    l_result t_app_appender.parameter_value%TYPE;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender: ' || x_appender); 
    $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_session_ctx_usage',  'YES'); $END
    IF get_session_ctx_usage() THEN
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'session context'); $END
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_layout',  'YES'); $END
      l_result := get_layout(x_app, x_appender, c_session_flag);
    ELSE 
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'global context'); $END
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_layout',  'YES'); $END
      l_result := get_layout(x_app, x_appender, c_global_flag);
    END IF;
    
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result); 
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END    
    RETURN l_result;
  END get_current_layout;

  /**
  * Function returns a nth descendant from given logger.
  * @param x_logger_name Loger name (Named hierarchy).
  * @param x_nth Level of descendant we want to obtain.
  * @return Name of descendant.
  */
  FUNCTION get_nth_logger_name(x_logger_name IN t_logger.logger%TYPE,
                               x_nth         IN PLS_INTEGER) RETURN t_logger.logger%TYPE IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_nth_logger_name'; $END
    l_logger_name t_logger.logger%TYPE;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_nth: ' || x_nth); 
    $END
 
    l_logger_name := substr(x_logger_name, 1, instr(x_logger_name || c_separator, c_separator, -1, x_nth) - 1);
 
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_logger_name: ' || l_logger_name); 
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END    
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_level'; $END
    i             PLS_INTEGER;
    l_logger_name t_logger.logger%TYPE;
    l_level       t_logger.log_level%TYPE;
    l_hlogger     hash_type;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_ctx_name: ' || x_ctx_name); 
    $END

    i := 1;
    LOOP
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_nth_logger_name',  'YES'); $END
      l_logger_name := get_nth_logger_name(x_logger_name, i);
      $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_logger_name: ' || l_logger_name); $END

      EXIT WHEN l_logger_name IS NULL;
      
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('hash_logger_name',  'YES'); $END
      l_hlogger := hash_logger_name(l_logger_name);
      $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger: ' || l_hlogger); $END

      l_level := sys_context(x_ctx_name, l_hlogger);
      $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_level: ' || l_level); $END

      IF l_level IS NOT NULL THEN
        $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'Exiting the cycle'); $END
        EXIT;
      END IF;

      i := i + 1;
    END LOOP;

    l_level := coalesce(l_level, sys_context(x_ctx_name, g_root_logger_hash));
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, l_level); 
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
    RETURN l_level;
  END get_level;

  /**
  * Function returns binary encoded list of appenders for given logger.
  * @param x_logger_name Logger name.
  * @param x_ctx_name Name of a context containing appenders.
  * @param x_ctx_name Name of a context containing additivity flag.
  * @return Binary encoded list of appenders for given logger.
  */
  FUNCTION get_appenders(x_logger_name  IN t_logger.logger%TYPE,
                         x_app_ctx_name IN ctx_namespace_type,
                         x_add_ctx_name IN ctx_namespace_type) RETURN t_logger.appenders%TYPE IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_appenders'; $END
    i             PLS_INTEGER;
    l_logger_name t_logger.logger%TYPE;
    l_hlogger     hash_type;
    l_appenders   t_logger.appenders%TYPE;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_app_ctx_name: ' || x_app_ctx_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_add_ctx_name: ' || x_add_ctx_name); 
    $END

    i           := 1;
    l_appenders := 0;
    LOOP
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_nth_logger_name',  'YES'); $END
      l_logger_name := get_nth_logger_name(x_logger_name, i);
      EXIT WHEN l_logger_name IS NULL;

      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('hash_logger_name',  'YES'); $END
      l_hlogger   := hash_logger_name(l_logger_name);
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bit_or',  'YES'); $END
      l_appenders := bit_or(l_appenders, nvl(sys_context(x_app_ctx_name, l_hlogger), 0));

      IF sys_context(x_add_ctx_name, l_hlogger) = 0 THEN
        $IF $$debug $THEN 
          internal_log(logging.c_info_level, l_intlogger, 'Stopping because of the additivity'); 
          internal_log(logging.c_debug_level, l_intlogger, 'l_appenders: ' || l_appenders); 
        $END
        RETURN l_appenders;
      END IF;

      i := i + 1;
    END LOOP;

    l_hlogger := g_root_logger_hash;
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bit_or',  'YES'); $END
    l_appenders := bit_or(l_appenders, nvl(sys_context(x_app_ctx_name, l_hlogger), 0));
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_appenders: ' || l_appenders); 
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
    RETURN l_appenders;
  END get_appenders;

  /**
  * Function returns binary encoded list of currently used appenders (based on get_session_ctx_usage) for given logger.
  * @param x_logger_name Logger name.
  * @return Binary encoded list of currently used appenders for given logger.
  */
  FUNCTION get_current_used_appenders(x_logger_name IN t_logger.logger%TYPE) RETURN t_logger.appenders%TYPE IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_current_used_appenders'; $END
    l_result t_logger.appenders%TYPE;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
    $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_session_ctx_usage',  'YES'); $END
    IF get_session_ctx_usage() THEN
      l_result := get_appenders(x_logger_name, 
                                c_logger_appenders_ctx(c_session_flag),
                                c_additivity_ctx(c_session_flag));
    ELSE 
      l_result := get_appenders(x_logger_name, 
                               c_logger_appenders_ctx(c_global_flag),
                               c_additivity_ctx(c_global_flag));
    END IF;

    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result); 
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
    RETURN l_result;
  END get_current_used_appenders;

  /**
  * Function returns currently used (based on get_session_ctx_usage) log level for given logger.
  * @param x_logger_name Logger name.
  * @return Currently used log level for given logger.
  */
  FUNCTION get_current_used_level(x_logger_name IN t_logger.logger%TYPE) RETURN t_logger.log_level%TYPE IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_current_used_level'; $END
    l_result t_logger.log_level%TYPE;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
    $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_session_ctx_usage',  'YES'); $END
    IF get_session_ctx_usage() THEN
      l_result := get_level(x_logger_name, c_logger_levels_ctx(c_session_flag));
    ELSE
      l_result := get_level(x_logger_name, c_logger_levels_ctx(c_global_flag));
    END IF;

    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result); 
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
    RETURN l_result;
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'add_global_appender'; $END
    PRAGMA AUTONOMOUS_TRANSACTION;
    l_app        t_schema_app.app%TYPE;
    l_code       t_appender.code%TYPE;
    l_appenders  t_logger.appenders%TYPE;
    l_hlogger    hash_type;
    l_additivity t_logger.additivity%TYPE;
  BEGIN
    $IF $$debug $THEN 
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender: ' || x_appender); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_additivity: ' || bool_to_int(x_additivity)); 
    $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_app',  'YES'); $END
    l_app := get_app(c_user);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_app: ' || l_app); $END

    IF l_app IS NULL AND c_user NOT IN ('SYS', c_schema_name) THEN
      $IF $$debug $THEN internal_log(logging.c_error_level, l_intlogger, 'no privileges'); $END
      raise_application_error(-20002, 'You have no privilege to set the appender.');
    END IF;

    BEGIN
      SELECT a.code
        INTO l_code
        FROM t_appender a
       WHERE a.appender = x_appender;
      $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_code: ' || l_code); $END
    EXCEPTION
      WHEN no_data_found THEN
        $IF $$debug $THEN internal_log(logging.c_error_level, l_intlogger, 'No such appender'); $END
        raise_application_error(-20001, 'No such appender');
    END;

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bool_to_int',  'YES'); $END
    l_additivity := bool_to_int(x_additivity);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_additivity: ' || l_additivity); $END
                
    UPDATE t_logger l
       SET l.appenders = bit_or(l.appenders, l_code), l.additivity = l_additivity
     WHERE l.logger = x_logger_name
    RETURNING appenders INTO l_appenders;

    IF SQL%NOTFOUND THEN
      l_appenders := l_code;
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'Inserting a new row'); $END
      INSERT INTO t_logger
        (logger, appenders, additivity)
      VALUES
        (x_logger_name, l_appenders, l_additivity);
    END IF;

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('hash_logger_name',  'YES'); $END
    l_hlogger := hash_logger_name(x_logger_name);
    
    $IF $$debug $THEN 
      internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger: ' || l_hlogger);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'l_appenders: ' || l_appenders);
      internal_log(logging.c_trace_level, l_intlogger, 'l_additivity: ' || l_additivity);
    $END
    set_context_rac_aware(c_logger_names_ctx(c_global_flag), l_hlogger, x_logger_name, c_global_flag);
    set_context_rac_aware(c_logger_appenders_ctx(c_global_flag), l_hlogger, l_appenders, c_global_flag);
    set_context_rac_aware(c_additivity_ctx(c_global_flag), l_hlogger, l_additivity, c_global_flag);
 
    COMMIT;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END add_global_appender;

  /**
  * Procedure sets global additivity flag for given logger.
  * @param x_logger_name Loger name.
  * @param x_additivity Additivity flag.
  */
  PROCEDURE set_global_additivity(x_logger_name IN t_logger.logger%TYPE,
                                  x_additivity  IN BOOLEAN) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_global_additivity'; $END
    PRAGMA AUTONOMOUS_TRANSACTION;
    l_app        t_schema_app.app%TYPE;
    l_appenders  t_logger.appenders%TYPE;
    l_dummy      VARCHAR2(1);
    l_hlogger    hash_type;
    l_additivity t_logger.additivity%TYPE;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_additivity: ' || bool_to_int(x_additivity)); 
    $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_app',  'YES'); $END
    l_app := get_app(c_user);

    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_app: ' || l_app); $END

    BEGIN
      SELECT NULL
        INTO l_dummy
        FROM t_schema_app ua
       WHERE ua.schema = c_user
         AND ua.app = l_app;
    EXCEPTION
      WHEN no_data_found THEN
        $IF $$debug $THEN internal_log(logging.c_error_level, l_intlogger, 'No such application'); $END
        raise_application_error(-20001, 'No such application');
    END;


    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bool_to_int',  'YES'); $END
    l_additivity := bool_to_int(x_additivity);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_additivity: ' || l_additivity); $END

    UPDATE t_logger l
       SET l.additivity = l_additivity
     WHERE l.logger = x_logger_name
    RETURNING appenders INTO l_appenders;
    
    IF SQL%NOTFOUND THEN
      l_appenders := 0;
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'Creating a new row'); $END
      INSERT INTO t_logger
        (logger, appenders, additivity)
      VALUES
        (x_logger_name, l_appenders, l_additivity);
    END IF;
    
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_appenders: ' || l_appenders); $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('hash_logger_name',  'YES'); $END
    l_hlogger := hash_logger_name(x_logger_name);
    
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_hlogger: ' || l_hlogger); 
      internal_log(logging.c_debug_level, l_intlogger, 'l_additivity: ' || l_appenders); 
    $END    
    
    set_context_rac_aware(c_logger_names_ctx(c_global_flag), l_hlogger, x_logger_name, c_global_flag);
    set_context_rac_aware(c_logger_appenders_ctx(c_global_flag), l_hlogger, l_appenders, c_global_flag);
    set_context_rac_aware(c_additivity_ctx(c_global_flag), l_hlogger, l_additivity, c_global_flag);

    COMMIT;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END set_global_additivity;

  /**
  * Procedure removes givne global appenders from given logger.
  * @param x_logger_name Logger name.
  * @param x_appender Binary coded Appender list.
  */
  PROCEDURE remove_global_appender(x_logger_name IN t_logger.logger%TYPE,
                                   x_appender    IN t_appender.appender%TYPE) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'remove_global_appender'; $END
    PRAGMA AUTONOMOUS_TRANSACTION;
    l_app       t_schema_app.app%TYPE;
    l_code      t_appender.code%TYPE;
    l_appenders t_logger.appenders%TYPE;
    l_hlogger   hash_type;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender: ' || x_appender); 
    $END    

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_app',  'YES'); $END
    l_app := get_app(c_user);

    IF l_app IS NULL AND c_user NOT IN ('SYS', c_schema_name) THEN
      $IF $$debug $THEN internal_log(logging.c_error_level, l_intlogger, 'Insufficient privileges for removing appender'); $END
      raise_application_error(-20002, 'You have no privilege to set the appender.');
    END IF;

    BEGIN
      SELECT a.code
        INTO l_code
        FROM t_appender a
       WHERE a.appender = x_appender;
    EXCEPTION
      WHEN no_data_found THEN
        $IF $$debug $THEN internal_log(logging.c_error_level, l_intlogger, 'Appender not found'); $END
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
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('hash_logger_name',  'YES'); $END
      l_hlogger := hash_logger_name(x_logger_name);
      
      $IF $$debug $THEN 
        internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger:' || l_hlogger); 
        internal_log(logging.c_trace_level, l_intlogger, 'l_appenders:' || l_appenders); 
      $END 
      set_context_rac_aware(c_logger_names_ctx(c_global_flag), l_hlogger, x_logger_name, c_global_flag);
      set_context_rac_aware(c_logger_appenders_ctx(c_global_flag), l_hlogger, l_appenders, c_global_flag);
    END IF;
 
    COMMIT;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'add_session_appender'; $END
    l_appenders t_logger.appenders%TYPE;
    l_code      t_appender.code%TYPE;
    l_hlogger   hash_type;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender: ' || x_appender); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_additivity: ' || bool_to_int(x_additivity));
    $END    
  
    BEGIN
      SELECT a.code
        INTO l_code
        FROM t_appender a
       WHERE a.appender = x_appender;
    EXCEPTION
      WHEN no_data_found THEN
        $IF $$debug $THEN internal_log(logging.c_error_level, l_intlogger, 'No such appender'); $END    
        raise_application_error(-20001, 'No such appender');
    END;
    
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('hash_logger_name',  'YES'); $END
    l_hlogger := hash_logger_name(x_logger_name);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger: ' || l_hlogger); $END    

    l_appenders := nvl(sys_context(c_logger_appenders_ctx(c_session_flag), l_hlogger), 0);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_appenders: ' || l_appenders); $END    
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bit_or',  'YES'); $END
    l_appenders := bit_or(l_appenders, l_code);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_appenders: ' || l_appenders); $END    
    set_context_rac_aware(c_logger_names_ctx(c_session_flag), l_hlogger, x_logger_name, c_session_flag);
    set_context_rac_aware(c_logger_appenders_ctx(c_session_flag), l_hlogger, l_appenders, c_session_flag);
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bool_to_int',  'YES'); $END
    set_context_rac_aware(c_additivity_ctx(c_session_flag),
                          l_hlogger,
                          bool_to_int(x_additivity),
                          c_session_flag);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END add_session_appender;

  /**
  * Procedure sets session additivity flag for given logger.
  * @param x_logger_name Loger name.
  * @param x_additivity Additivity flag.
  */
  PROCEDURE set_session_additivity(x_logger_name IN t_logger.logger%TYPE,
                                   x_additivity  IN BOOLEAN) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_session_additivity'; $END
    l_appenders t_logger.appenders%TYPE;
    l_hlogger   hash_type;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_additivity: ' || bool_to_int(x_additivity));
    $END    

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('hash_logger_name',  'YES'); $END
    l_hlogger   := hash_logger_name(x_logger_name);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger: ' || l_hlogger); $END
    l_appenders := nvl(sys_context(c_logger_appenders_ctx(c_session_flag), l_hlogger), 0);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_appenders: ' || l_appenders); $END
    set_context_rac_aware(c_logger_names_ctx(c_session_flag), l_hlogger, x_logger_name, c_session_flag);
    set_context_rac_aware(c_logger_appenders_ctx(c_session_flag), l_hlogger, l_appenders, c_session_flag);
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bool_to_int',  'YES'); $END
    set_context_rac_aware(c_additivity_ctx(c_session_flag),
                          l_hlogger,
                          bool_to_int(x_additivity),
                          c_session_flag);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_global_parameter'; $END
    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_param_name: ' || x_param_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_param_value: ' || x_param_value);
    $END    

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

    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'Merge OK, merged: ' || SQL%ROWCOUNT || ' rows'); $END
    set_context_rac_aware(c_parameters_ctx(c_global_flag),
                          x_app || '#' || x_param_name,
                          x_param_value,
                          c_global_flag);

    COMMIT;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_session_parameter'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_param_name: ' || x_param_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_param_value: ' || x_param_value);
    $END    
    set_context_rac_aware(c_parameters_ctx(c_session_flag), x_app || '#' || x_param_name, x_param_value, c_session_flag);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END set_session_parameter;

  /**
  * Procedure clears all serialized settings.
  */  
  PROCEDURE clear_serialized_settings IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'clear_serialized_settings'; $END
  BEGIN
    internal_log(logging.c_info_level, l_intlogger, 'start');
    g_serialized_settings := NULL;
    internal_log(logging.c_info_level, l_intlogger, 'end');
  END clear_serialized_settings;
  
  /**
  * Procedure adds given appender to given logger and sets additivity flag for the logger in serialized settings.
  * @param x_logger_name Logger name.
  * @param x_appender Binary coded appender.
  * @param x_additivity Additivity flag.
  */
  PROCEDURE add_serialized_appender(x_logger_name IN t_logger.logger%TYPE,
                                    x_appender    IN t_appender.appender%TYPE,
                                    x_additivity  IN BOOLEAN DEFAULT TRUE) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'add_serialized_appender'; $END
    l_appenders t_logger.appenders%TYPE;
    l_code      t_appender.code%TYPE;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender: ' || x_appender); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_additivity: ' || bool_to_int(x_additivity));
    $END    
  
    BEGIN
      SELECT a.code
        INTO l_code
        FROM t_appender a
       WHERE a.appender = x_appender;
    EXCEPTION
      WHEN no_data_found THEN
        $IF $$debug $THEN internal_log(logging.c_error_level, l_intlogger, 'No such appender'); $END    
        raise_application_error(-20001, 'No such appender');
    END;
    
    -- add appender    
    l_appenders := coalesce(g_serialized_settings.loggers(x_logger_name).enabled_appenders, 0);
    l_appenders := bit_or(l_appenders, l_code);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_appenders: ' || l_appenders); $END
    g_serialized_settings.loggers(x_logger_name).enabled_appenders := l_appenders;
    g_serialized_settings.loggers(x_logger_name).additivity := bool_to_int(x_additivity);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END add_serialized_appender;  

  /**
  * Procedure removes given appender from given logger in serialized settings.
  * @param x_logger_name Logger name.
  * @param x_appender Binary coded appender.
  */
  PROCEDURE remove_serialized_appender(x_logger_name IN t_logger.logger%TYPE,
                                       x_appender    IN t_appender.appender%TYPE) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'remove_serialized_appender'; $END
    l_appenders t_logger.appenders%TYPE;
    l_code      t_appender.code%TYPE;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender: ' || x_appender); 
    $END    

    BEGIN
      SELECT a.code
        INTO l_code
        FROM t_appender a
       WHERE a.appender = x_appender;
    EXCEPTION
      WHEN no_data_found THEN
        $IF $$debug $THEN internal_log(logging.c_error_level, l_intlogger, 'No such appender'); $END 
        raise_application_error(-20001, 'No such appender');
    END;

    l_appenders := coalesce(g_serialized_settings.loggers(x_logger_name).enabled_appenders, 0);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_appenders' || l_appenders); $END     
    
    IF bitand(l_appenders, l_code) > 0 THEN
      l_appenders := bit_xor(l_appenders, l_code);
      $IF $$debug $THEN 
        internal_log(logging.c_info_level, l_intlogger, 'Removing appender.'); 
        internal_log(logging.c_debug_level, l_intlogger, 'l_appenders' || l_appenders); 
      $END     
      g_serialized_settings.loggers(x_logger_name).enabled_appenders := l_appenders;
    END IF;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END remove_serialized_appender;


  /**
  * Procedure sets additivity flag for given logger in serialized settings.
  * @param x_logger_name Loger name.
  * @param x_additivity Additivity flag.
  */
  PROCEDURE set_serialized_additivity(x_logger_name IN t_logger.logger%TYPE,
                                      x_additivity  IN BOOLEAN) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_serialized_additivity'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_additivity: ' || bool_to_int(x_additivity));
    $END    
    g_serialized_settings.loggers(x_logger_name).additivity := bool_to_int(x_additivity);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END set_serialized_additivity;

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
                                          x_parameter_value IN t_app_appender.parameter_value%TYPE) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_serialized_appender_param'; $END
    l_code t_appender.code%type;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender: ' || x_appender); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_parameter_name: ' || x_parameter_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_parameter_value: ' || x_parameter_value); 
    $END
    
    l_code := get_appender_code(x_appender);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_code: ' || l_code); $END

    g_serialized_settings.app_settings(x_app).appenders_params(l_code)(x_parameter_name) := x_parameter_value;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END set_serialized_appender_param;


  /**
  * Procedure sets session layout for given appender and given application.
  * @param x_app Application name.
  * @param x_appender Appender name.
  * @param x_layout Layout.
  */
  PROCEDURE set_serialized_layout(x_app      IN t_app_appender.app%TYPE,
                                  x_appender IN t_app_appender.appender%TYPE,
                                  x_layout   IN t_app_appender.parameter_value%TYPE) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_serialized_layout'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender: ' || x_appender); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_layout: ' || x_layout); 
    $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('set_serialized_appender_param','YES'); $END
    set_serialized_appender_param(x_app             => x_app,
                                  x_appender        => x_appender,
                                  x_parameter_name  => c_layout_param,
                                  x_parameter_value => x_layout);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END set_serialized_layout;

  /**
  * Procedure sets session log level for given logger.
  * @param x_logger Logger name.
  * @param x_log_level Log level.
  */
  PROCEDURE set_serialized_level(x_logger_name IN t_logger.logger%TYPE,
                                 x_log_level   IN t_logger.log_level%TYPE) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_serialized_level'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_log_level: ' || x_log_level); 
    $END    
    g_serialized_settings.loggers(x_logger_name).log_level := x_log_level;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END set_serialized_level;

  /**
  * Procedure sets session parameter value for given app and parameter name.
  * @param x_app Application.
  * @param x_param_name Parameter name.
  * @param x_param_value Parameter value.
  */
  PROCEDURE set_serialized_parameter(x_app         IN t_param.app%TYPE,
                                     x_param_name  IN t_param.param_name%TYPE,
                                     x_param_value IN t_param.param_value%TYPE) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_serialized_parameter'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_param_name: ' || x_param_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_param_value: ' || x_param_value);
    $END    
    g_serialized_settings.app_settings(x_app).app_params(x_param_name) := x_param_value;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END set_serialized_parameter;

  /**
  * Function serializes given settings variable to a string.
  * @return Serialized settings in a string
  */  
  FUNCTION serialize_settings RETURN ctx_value_type IS
    l_result ctx_value_type := NULL;
    l_logger_name t_logger.logger%TYPE;
    l_param_name t_app_appender.parameter_name%TYPE;
    l_append_param_name t_app_appender.parameter_name%TYPE;
    l_appender PLS_INTEGER;
    l_app_name t_app.app%TYPE;
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'serialize_settings'; $END
  BEGIN
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'start'); $END

    l_logger_name := g_serialized_settings.loggers.first;
    WHILE l_logger_name IS NOT NULL LOOP
       l_result := l_result || c_set_logger_op || c_ser_delim
                   || l_logger_name || c_ser_delim
                   || CAST(g_serialized_settings.loggers(l_logger_name).enabled_appenders AS VARCHAR2) || c_ser_delim
                   || g_serialized_settings.loggers(l_logger_name).log_level || c_ser_delim
                   || CAST(g_serialized_settings.loggers(l_logger_name).additivity AS VARCHAR2) || c_ser_delim;
       l_logger_name := g_serialized_settings.loggers.next(l_logger_name);
    END LOOP; 
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'Loggers added: ' || l_result); $END

    l_app_name := g_serialized_settings.app_settings.first;
    WHILE l_app_name IS NOT NULL LOOP    
      l_param_name := g_serialized_settings.app_settings(l_app_name).app_params.first;
      WHILE l_param_name IS NOT NULL LOOP
         l_result := l_result || c_set_app_param_op || c_ser_delim
                     || l_app_name || c_ser_delim 
                     || l_param_name || c_ser_delim
                     || g_serialized_settings.app_settings(l_app_name).app_params(l_param_name)  || c_ser_delim;
         l_param_name := g_serialized_settings.app_settings(l_app_name).app_params.next(l_param_name);
      END LOOP;    
      $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'App params added: ' || l_result); $END

      l_appender := g_serialized_settings.app_settings(l_app_name).appenders_params.first;
      WHILE l_appender IS NOT NULL LOOP
         l_append_param_name := g_serialized_settings.app_settings(l_app_name).appenders_params(l_appender).first;       
         WHILE l_append_param_name IS NOT NULL LOOP
           l_result := l_result || c_set_app_appender_param_op || c_ser_delim || l_app_name || c_ser_delim
                       || l_appender || c_ser_delim
                       || l_append_param_name || c_ser_delim
                       || g_serialized_settings.app_settings(l_app_name).appenders_params(l_appender)(l_append_param_name)  || c_ser_delim;

           l_append_param_name := g_serialized_settings.app_settings(l_app_name).appenders_params(l_appender).next(l_append_param_name);
         END LOOP;
         l_appender := g_serialized_settings.app_settings(l_app_name).appenders_params.next(l_appender);
      END LOOP;    
      
      l_app_name := g_serialized_settings.app_settings.next(l_app_name);
    END LOOP;
    $IF $$debug $THEN
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end'); 
    $END
    RETURN l_result;    
  END serialize_settings;
  
  /** 
  * Function deserializes given settings to a record  
  * @return a record containing deserialized settings.
  */
  FUNCTION get_deserialized_settings(x_settings IN ctx_value_type) RETURN deserialized_settings_type IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_deserialized_settings'; $END
    l_serialized_settings deserialized_settings_type; 
    l_operation serialization_ops_type;
    l_logger t_logger.logger%TYPE;
    l_log_level t_logger.log_level%TYPE;
    l_additivity PLS_INTEGER;
    l_appenders PLS_INTEGER;
    l_app t_app.app%TYPE;
    l_appender t_appender.code%TYPE;
    l_param_name ctx_attribute_type;
    l_param_value ctx_value_type;
        
    l_pos PLS_INTEGER;
    FUNCTION get_str_val(x_str IN ctx_value_type, x_start IN PLS_INTEGER, x_pos OUT PLS_INTEGER) RETURN ctx_value_type IS
      l_end PLS_INTEGER;
      l_result ctx_value_type;
    BEGIN
      l_end := instr(x_str, c_ser_delim, x_start);
      x_pos := l_end + CASE l_end WHEN 0 THEN 0 ELSE length(c_ser_delim) END;
      l_result := substr(x_str, x_start, l_end - x_start);
      $IF $$debug $THEN 
        internal_log(logging.c_trace_level, l_intlogger, 'x_str: ' || x_str);
        internal_log(logging.c_trace_level, l_intlogger, 'x_start: '|| x_start || ', x_end: ' || l_end || ', l_result: ' || l_result || ', x_pos: ' || x_pos);
      $END
      RETURN l_result;
    END;
      
    FUNCTION get_int_val(x_str IN ctx_value_type, x_start IN PLS_INTEGER, x_pos OUT PLS_INTEGER) RETURN NUMBER IS
      l_end PLS_INTEGER;
      l_result ctx_value_type;
    BEGIN
      l_end := instr(x_str, c_ser_delim, x_start);
      x_pos := l_end + CASE l_end WHEN 0 THEN 0 ELSE length(c_ser_delim) END;
      l_result := substr(x_str, x_start, l_end - x_start);
      $IF $$debug $THEN 
        internal_log(logging.c_trace_level, l_intlogger, 'x_str: ' || x_str);
        internal_log(logging.c_trace_level, l_intlogger, 'x_start: '|| x_start || ', x_end: ' || l_end || ', l_result: ' || l_result || ', x_pos: ' || x_pos); 
      $END       
      RETURN CAST(l_result AS NUMBER);
    END;
    
  BEGIN
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'start'); $END

    l_pos := 1;
    LOOP    
      l_operation := get_str_val(x_settings, l_pos, l_pos);
      EXIT WHEN l_operation IS NULL;
      
      CASE l_operation
      WHEN c_set_logger_op THEN
        l_logger := get_str_val(x_settings, l_pos, l_pos);
        l_appenders :=  get_int_val(x_settings, l_pos, l_pos);
        l_log_level := get_str_val(x_settings, l_pos, l_pos);
        l_additivity := get_int_val(x_settings, l_pos, l_pos);
        IF l_log_level IS NOT NULL THEN
          set_serialized_level(x_logger_name => l_logger, x_log_level => l_log_level);
        END IF;
        IF l_additivity IS NOT NULL THEN
          set_serialized_additivity(x_logger_name => l_logger, x_additivity => int_to_bool(l_additivity));
        END IF;
        -- TODO: remove select (add_appender should accept a code)
        FOR l_row IN (SELECT a.appender FROM t_appender a WHERE bitand(a.code, l_appenders) = a.code) LOOP
          add_serialized_appender(x_logger_name => l_logger, x_appender => l_row.appender);
        END LOOP;
      WHEN c_set_app_param_op THEN
        l_app := get_str_val(x_settings, l_pos, l_pos);
        l_param_name := get_str_val(x_settings, l_pos, l_pos);
        l_param_value := get_str_val(x_settings, l_pos, l_pos);
        set_serialized_parameter(x_app => l_app, x_param_name => l_param_name, x_param_value => l_param_value);       
      WHEN c_set_app_appender_param_op THEN
        l_app := get_str_val(x_settings, l_pos, l_pos);
        l_appender := get_int_val(x_settings, l_pos, l_pos);
        l_param_name := get_str_val(x_settings, l_pos, l_pos);
        l_param_value := get_str_val(x_settings, l_pos, l_pos);
        set_serialized_parameter(x_app => l_app, x_param_name => l_param_name, x_param_value => l_param_value);       
      END CASE;
    END LOOP;

    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
    RETURN g_serialized_settings;
  END get_deserialized_settings;
  
  /** Procedure shows serialized settings. */
  PROCEDURE show_serialized_settings IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'show_serialized_settings'; $END
  BEGIN
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'start'); $END
    dbms_output.put_line(serialize_settings());
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END show_serialized_settings;   

  /** Procedure set sets given settings in the current session by calling set_session* methods. 
  * @param x_settings Deserialized settings.
  */
  PROCEDURE set_session_settings(x_settings IN deserialized_settings_type) IS 
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_session_settings'; $END
    l_logger     t_logger.logger%TYPE;
    l_app        t_app.app%TYPE;
    l_appender   t_appender.code%TYPE;
    l_param_name ctx_attribute_type;
  BEGIN
    
    -- set loggers
    l_logger := x_settings.loggers.first;
    WHILE l_logger IS NOT NULL LOOP
      IF x_settings.loggers(l_logger).log_level IS NOT NULL THEN 
        set_session_level(x_logger_name => l_logger, x_log_level => x_settings.loggers(l_logger).log_level);
      END IF;
      IF x_settings.loggers(l_logger).log_level IS NOT NULL THEN 
        set_session_additivity(x_logger_name => l_logger, 
                               x_additivity => int_to_bool(x_settings.loggers(l_logger).additivity));
      END IF;

      --TODO: remove select - use code
      FOR l_row IN (SELECT a.appender FROM t_appender a WHERE bitand(a.code, x_settings.loggers(l_logger).enabled_appenders) = a.code) LOOP
        add_session_appender(x_logger_name => l_logger, x_appender => l_row.appender);
      END LOOP;
      l_logger :=  x_settings.loggers.next(l_logger);
    END LOOP;
     
    -- set app params
    l_app := x_settings.app_settings.first;
    WHILE l_app IS NOT NULL LOOP
      l_param_name := x_settings.app_settings(l_app).app_params.first;
      WHILE l_param_name IS NOT NULL LOOP
          set_session_parameter(x_app => l_app,
                                x_param_name => l_param_name, 
                                x_param_value => x_settings.app_settings(l_app).app_params(l_param_name));
        l_param_name := x_settings.app_settings(l_app).app_params.next(l_param_name);
      END LOOP;
      

      l_appender := g_serialized_settings.app_settings(l_app).appenders_params.first;
      WHILE l_appender IS NOT NULL LOOP
        l_param_name := g_serialized_settings.app_settings(l_app).appenders_params(l_appender).first;       
        WHILE l_param_name IS NOT NULL LOOP
          set_session_appender_param(x_app => l_app,
                                     x_appender =>  l_appender,
                                     x_parameter_name => l_param_name,
                                     x_parameter_value => g_serialized_settings.app_settings(l_app).appenders_params(l_appender)(l_param_name));
          l_param_name := g_serialized_settings.app_settings(l_app).appenders_params(l_appender).next(l_param_name);
        END LOOP;
          
        l_appender := g_serialized_settings.app_settings(l_app).appenders_params.next(l_appender);
      END LOOP;

      l_app :=  x_settings.app_settings.next(l_app);
    END LOOP;
  END set_session_settings;
  
  /**
  * Procedure applies requested session settings for current session and given application.
  * @param x_app Application.
  */
  PROCEDURE use_requested_session_settings(x_app IN t_app.app%TYPE) IS 
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'use_requested_session_settings'; $END
    l_settings ctx_value_type;
    l_deserialized_settings deserialized_settings_type; 
  BEGIN
    $IF $$debug $THEN 
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
    $END
    l_settings := sys_context(c_modify_session_ctx,  g_session_identifier);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_settings: ' || l_settings); $END
    IF l_settings IS NULL THEN
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'No session settings requested'); $END
      RETURN;
    END IF;
    
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'Session settings requested, applying...'); $END
    -- if there have been some settings requested, apply them to current session
    IF l_settings IS NOT NULL THEN
      copy_global_to_session(x_app => x_app);
      l_deserialized_settings := get_deserialized_settings(l_settings);
      set_session_settings(l_deserialized_settings);
      set_session_ctx_usage(x_usage => true);
      
      -- clear the settings from context to not be applied multiple times
      set_context_rac_aware(c_modify_session_ctx, g_session_identifier, NULL, c_global_flag);      
    END IF;      
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END use_requested_session_settings;

  /**
  * Function obtains current parameter value for given app and parameter name.
  * @param x_app Application.
  * @param x_param_name Parameter name.
  * @return Current parameter value for given application and parameter name.
  */
  FUNCTION get_current_parameter(x_app        IN t_param.app%TYPE,
                                 x_param_name IN t_param.param_name%TYPE) RETURN t_param.param_value%TYPE IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_current_parameter'; $END
    l_result t_param.param_value%TYPE;
  BEGIN
    $IF $$debug $THEN 
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_param_name: ' || x_param_name); 
    $END    

    IF get_session_ctx_usage() THEN
      l_result := sys_context(c_parameters_ctx(c_session_flag), x_app || '#' || x_param_name);
    ELSE
      l_result := sys_context(c_parameters_ctx(c_global_flag), x_app || '#' || x_param_name);
    END IF;

    $IF $$debug $THEN
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result); 
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END    
    RETURN l_result;
  END get_current_parameter;

  /**
  * Procedure removes given session appenders from given logger.
  * @param x_logger_name Logger name.
  * @param x_appender Binary coded Appender list.
  */
  PROCEDURE remove_session_appender(x_logger_name IN t_logger.logger%TYPE,
                                    x_appender    IN t_appender.appender%TYPE) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'remove_session_appender'; $END
    l_appenders t_logger.appenders%TYPE;
    l_code      t_appender.code%TYPE;
    l_hlogger   hash_type;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender: ' || x_appender); 
    $END    

    BEGIN
      SELECT a.code
        INTO l_code
        FROM t_appender a
       WHERE a.appender = x_appender;
    EXCEPTION
      WHEN no_data_found THEN
        $IF $$debug $THEN internal_log(logging.c_error_level, l_intlogger, 'No such appender'); $END 
        raise_application_error(-20001, 'No such appender');
    END;
    
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('hash_logger_name',  'YES'); $END
    l_hlogger   := hash_logger_name(x_logger_name);
    l_appenders := nvl(sys_context(c_logger_appenders_ctx(c_session_flag), l_hlogger), 0);

    $IF $$debug $THEN 
      internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger' || l_hlogger); 
      internal_log(logging.c_trace_level, l_intlogger, 'l_appenders' || l_appenders); 
    $END     
    
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bitand',  'YES'); $END
    IF bitand(l_appenders, l_code) > 0 THEN
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bit_xor',  'YES'); $END
      l_appenders := bit_xor(l_appenders, l_code);
      $IF $$debug $THEN 
        internal_log(logging.c_info_level, l_intlogger, 'Removing appender from context' || l_hlogger); 
        internal_log(logging.c_debug_level, l_intlogger, 'l_appenders' || l_appenders); 
      $END     
      set_context_rac_aware(c_logger_appenders_ctx(c_session_flag), l_hlogger, l_appenders, c_session_flag);
    END IF;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END remove_session_appender;

  /**
  * Procedure sets global log level for given logger.
  * @param x_logger Logger name.
  * @param x_log_level Log level.
  */
  PROCEDURE set_global_level(x_logger_name IN t_logger.logger%TYPE,
                             x_log_level   IN t_logger.log_level%TYPE) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_global_level'; $END
    PRAGMA AUTONOMOUS_TRANSACTION;
    l_app       t_schema_app.app%TYPE;
    l_dummy     VARCHAR2(1);
    l_hlogger   hash_type;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_log_level: ' || x_log_level); 
    $END    

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_app',  'YES'); $END
    l_app := get_app(c_user);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_app: ' || l_app); $END    

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

    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'Merged: ' || SQL%ROWCOUNT || ' rows'); $END    
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('hash_logger_name',  'YES'); $END
    l_hlogger := hash_logger_name(x_logger_name);

    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger: ' || l_hlogger); $END    
    set_context_rac_aware(c_logger_names_ctx(c_global_flag), l_hlogger, x_logger_name, c_global_flag);
    set_context_rac_aware(c_logger_levels_ctx(c_global_flag), l_hlogger, x_log_level, c_global_flag);

    COMMIT;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END set_global_level;

  /**
  * Procedure sets session log level for given logger.
  * @param x_logger Logger name.
  * @param x_log_level Log level.
  */
  PROCEDURE set_session_level(x_logger_name IN t_logger.logger%TYPE,
                              x_log_level   IN t_logger.log_level%TYPE) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'set_session_level'; $END
    l_hlogger hash_type;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_log_level: ' || x_log_level); 
    $END    

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('hash_logger_name',  'YES'); $END
    l_hlogger := hash_logger_name(x_logger_name);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger: ' || l_hlogger); $END
    set_context_rac_aware(c_logger_names_ctx(c_session_flag), l_hlogger, x_logger_name, c_session_flag);
    set_context_rac_aware(c_logger_levels_ctx(c_session_flag), l_hlogger, x_log_level, c_session_flag);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END set_session_level;

  /**
  * Function returns severity for given log level.
  * @param x_level Log level.
  * @return Severity of given log level.
  */
  FUNCTION get_level_severity(x_level IN t_log_level.log_level%TYPE) RETURN t_log_level.severity%TYPE IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_level_severity'; $END    
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_level: ' || x_level); 
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END    
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
  FUNCTION format_message(x_message     IN message_type,
                          x_layout      IN t_app_appender.parameter_value%TYPE,
                          x_logger_name IN t_logger.logger%TYPE,
                          x_level       IN t_logger.log_level%TYPE) RETURN VARCHAR2 IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'format_message'; $END
    l_message t_log.message%TYPE;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_message: ' || x_message);
      internal_log(logging.c_debug_level, l_intlogger, 'x_layout: ' || x_layout);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_level: ' || x_level);
    $END      
  
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

    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_message: ' || l_message);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
    RETURN l_message;
  END format_message;

  /**
  * Procedure initializes cyclic buffer.
  * @param x_app Application
  */
  PROCEDURE init_email_cyclic_buffer(x_app IN VARCHAR2) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'init_email_cyclic_buffer'; $END
  BEGIN
      $IF $$debug $THEN
        internal_log(logging.c_info_level, l_intlogger, 'start');
        internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      $END
      g_mail_buffer.head      := 1;
      g_mail_buffer.tail      := 1;
      g_mail_buffer.buffer    := mail_table_type();
      g_mail_buffer.buff_size := 1000;
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_appender_param',  'YES'); $END
      g_mail_buffer.buff_size := coalesce(get_appender_param(x_app, 'SMTP', 'MAIL_BUFFER_LINES', c_global_flag), 1000);
      g_mail_buffer.buffer.EXTEND(g_mail_buffer.buff_size);
      $IF $$debug $THEN 
        internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.buff_size: ' || g_mail_buffer.buff_size);
        internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.buffer.size: ' || g_mail_buffer.buffer.count);
        internal_log(logging.c_info_level, l_intlogger, 'end');
      $END
  END init_email_cyclic_buffer;

  /** Procedure enqueues a message to cyclic buffer (queue).
  * If the buffer is full the first message in queue is discarded.
  * @param x_app Application.
  * @param x_message Message.
  */
  PROCEDURE enqueue_into_cyclic_buffer(x_app IN VARCHAR2, x_message IN message_type) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'enqueue_into_cyclic_buffer'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_message: ' || x_message); 
    $END

    -- if the size is NULL, buffer was not initialized
    IF g_mail_buffer.buff_size IS NULL THEN
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'initializing buffer'); $END
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('init_email_cyclic_buffer',  'YES'); $END
      init_email_cyclic_buffer(x_app);
    END IF;

    -- enqueque the message    
    g_mail_buffer.buffer(g_mail_buffer.tail) := x_message;
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.tail' || g_mail_buffer.tail); $END
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.buffer(g_mail_buffer.tail)' || g_mail_buffer.buffer(g_mail_buffer.tail)); $END

    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.buff_size' || g_mail_buffer.buff_size); $END
    -- cyclic incrementation of the tail    
    IF g_mail_buffer.tail = g_mail_buffer.buff_size THEN
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'End of buffer, reseting to 1'); $END
      g_mail_buffer.tail := 1;
    ELSE
      g_mail_buffer.tail := g_mail_buffer.tail + 1;
    END IF;
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.tail' || g_mail_buffer.tail); $END


    -- if the tail position and the head position is the same
    -- then the buffer is full
    IF g_mail_buffer.head = g_mail_buffer.tail THEN
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'Buffer is full'); $END
      -- discard the first message by cyclic incrementation of the head position
      IF g_mail_buffer.head = g_mail_buffer.buff_size THEN
        g_mail_buffer.head := 1;
      ELSE
        g_mail_buffer.head := g_mail_buffer.head + 1;
      END IF;
      $IF $$debug $THEN
        internal_log(logging.c_info_level, l_intlogger, 'Discarding first message in queue');
        internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.head' || g_mail_buffer.head);
      $END
    END IF;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END enqueue_into_cyclic_buffer;

  /** Function dequeues a message from cyclic buffer (queue).
  * @return Dequeued message.
  */
  FUNCTION dequeue_from_cyclic_buffer RETURN VARCHAR2 IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'dequeue_from_cyclic_buffer'; $END
    l_last_head PLS_INTEGER;
  BEGIN
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'start'); $END
    l_last_head := g_mail_buffer.head;
    $IF $$debug $THEN 
      internal_log(logging.c_trace_level, l_intlogger, 'l_last_head' || l_last_head); 
      internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.buff_size' || g_mail_buffer.buff_size);
    $END
    
    IF g_mail_buffer.head = g_mail_buffer.buff_size THEN
      g_mail_buffer.head := 1;
    ELSE
      g_mail_buffer.head := g_mail_buffer.head + 1;
    END IF;
    $IF $$debug $THEN 
      internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.head' || g_mail_buffer.head);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
    RETURN g_mail_buffer.buffer(l_last_head);
  END dequeue_from_cyclic_buffer;

  /**
  * Function checks whether is cyclic buffer empty or not.
  * @return Flags inidicating whether is buffer empty or not.
  * {*} TRUE if the buffer is empty
  * {*} FALSE if the buffer is not empty
  */
  FUNCTION is_cyclic_buffer_empty RETURN BOOLEAN IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'is_cyclic_buffer_empty'; $END    
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.head' || g_mail_buffer.head); 
      internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.tail' || g_mail_buffer.tail);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END

    RETURN coalesce(g_mail_buffer.head = g_mail_buffer.tail, TRUE);
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'parse_stack'; $END
    l_offset       PLS_INTEGER;
    l_next_nl      PLS_INTEGER;
    l_stack_object VARCHAR2(92 CHAR);
    l_stack_line   VARCHAR2(255 CHAR);
    l_obj_schema   user_users.username%TYPE;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_method: ' || x_method); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_call_stack: ' || x_call_stack); 
    $END
  
    -- end of last line for this package
    l_offset := instr(x_call_stack, c_nl, instr(x_call_stack, c_package_name, pos => -1)) + c_nl_length;
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_offset: ' || l_offset); $END

    -- next end of line
    l_next_nl := instr(x_call_stack, c_nl, l_offset);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_next_nl: ' || l_next_nl); $END
    IF l_next_nl = 0 THEN
      o_logger := c_root_logger_name;
      o_app := c_user;
    ELSE
      -- first line after last line of this package
      l_stack_line   := substr(x_call_stack, l_offset, l_next_nl - l_offset);
      $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_stack_line: ' || l_stack_line); $END
      -- object on that line
      l_stack_object := substr(l_stack_line, instr(l_stack_line, c_space, -1) + 1);
      $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_stack_object: ' || l_stack_object); $END
      -- schema of the object
      l_obj_schema   := substr(l_stack_object, 1, instr(l_stack_object, '.') - 1);
      $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_obj_schema: ' || l_obj_schema); $END
      IF x_method IS NOT NULL THEN
        l_stack_object := l_stack_object || '.' || upper(x_method);
      END IF;
      $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_stack_object: ' || l_stack_object); $END

      -- if schema was not determined, logging was called from anonymous block.
      IF l_obj_schema IS NULL THEN
        l_obj_schema := c_user;
      END IF;
      $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_obj_schema: ' || l_obj_schema); $END
     
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_app',  'YES'); $END
      o_app  := get_app(l_obj_schema);
      o_logger := o_app || '.' || l_stack_object;
    END IF;
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'o_logger: ' || o_logger);
      internal_log(logging.c_debug_level, l_intlogger, 'o_app: ' || o_app);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_logger'; $END
    l_logger       logger_type;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_method: ' || x_method); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_always_from_ctx: ' || x_always_from_ctx); 
    $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('parse_stack',  'YES'); $END
    parse_stack(l_logger.logger, l_logger.app, x_method);
    l_logger.always_from_ctx := x_always_from_ctx;
    
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity',  'YES'); $END
    l_logger.log_level_severity := get_level_severity(get_current_used_level(l_logger.logger));
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_used_appenders',  'YES'); $END
    l_logger.enabled_appenders  := get_current_used_appenders(l_logger.logger);

    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_logger.always_from_ctx: ' || l_logger.always_from_ctx); 
      internal_log(logging.c_debug_level, l_intlogger, 'l_logger.log_level_severity: ' || l_logger.log_level_severity); 
      internal_log(logging.c_debug_level, l_intlogger, 'l_logger.enabled_appenders: ' || l_logger.enabled_appenders); 
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
    RETURN l_logger;
  END get_logger;

  /**
  * Returns the root logger configuration.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  FUNCTION get_root_logger(x_always_from_ctx IN ctx_boolean DEFAULT c_false) RETURN logger_type IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_root_logger'; $END
    l_logger logger_type;
  BEGIN
    $IF $$debug $THEN 
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_always_from_ctx: ' || x_always_from_ctx);
    $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('parse_stack',  'YES'); $END
    parse_stack(l_logger.logger, l_logger.app);
    l_logger.logger          := c_root_logger_name;
    l_logger.always_from_ctx := x_always_from_ctx;
   
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_used_level',  'YES'); $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity',  'YES'); $END
    l_logger.log_level_severity := get_level_severity(get_current_used_level(l_logger.logger));
    $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'l_logger.log_level_severity: ' || l_logger.log_level_severity); $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_used_appenders',  'YES'); $END
    l_logger.enabled_appenders  := get_current_used_appenders(l_logger.logger);
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_logger.enabled_appenders: ' || l_logger.enabled_appenders);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
    RETURN l_logger;
  END get_root_logger;

  /**
  * Returns the application logger configuration.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  FUNCTION get_app_logger(x_always_from_ctx IN ctx_boolean DEFAULT c_false) RETURN logger_type IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_app_logger'; $END
    l_logger logger_type;
  BEGIN
    $IF $$debug $THEN 
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_always_from_ctx: ' || x_always_from_ctx);
    $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('parse_stack',  'YES'); $END
    parse_stack(l_logger.logger, l_logger.app);
    l_logger.logger          := l_logger.app;
    l_logger.always_from_ctx := x_always_from_ctx;
    $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'l_logger.logger: ' || l_logger.logger); $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_used_level',  'YES'); $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity',  'YES'); $END
    l_logger.log_level_severity := get_level_severity(get_current_used_level(l_logger.logger));
    $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'l_logger.log_level_severity: ' || l_logger.log_level_severity); $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_used_appenders',  'YES'); $END
    l_logger.enabled_appenders  := get_current_used_appenders(l_logger.logger);
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_logger.enabled_appenders: ' || l_logger.enabled_appenders);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
    RETURN l_logger;
  END get_app_logger;

  /**
  * Returns the schema logger configuration.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  FUNCTION get_schema_logger(x_always_from_ctx IN ctx_boolean DEFAULT c_false) RETURN logger_type IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'get_schema_logger'; $END
    l_logger logger_type;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_always_from_ctx: ' || x_always_from_ctx);
    $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('parse_stack',  'YES'); $END
    parse_stack(l_logger.logger, l_logger.app);
    l_logger.logger          := l_logger.app || '.' || c_user;
    l_logger.always_from_ctx := x_always_from_ctx;
    $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'l_logger.logger: ' || l_logger.logger); $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_used_level',  'YES'); $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity',  'YES'); $END
    l_logger.log_level_severity := get_level_severity(get_current_used_level(l_logger.logger));
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_used_appenders',  'YES'); $END
    l_logger.enabled_appenders  := get_current_used_appenders(l_logger.logger);
    $IF $$debug $THEN 
      internal_log(logging.c_debug_level, l_intlogger, 'l_logger.enabled_appenders: ' || l_logger.enabled_appenders);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $END
    RETURN l_logger;
  END get_schema_logger;

  /** Procedure sends cyclic buffer. Parameters for sending are obtained
  * for given application.
  * @param x_app Application name.
  */
  PROCEDURE send_buffer(x_app IN t_app_appender.app%TYPE) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'send_buffer'; $END
    l_host      t_app_appender.parameter_value%TYPE;
    l_from      t_app_appender.parameter_value%TYPE;
    l_to        t_app_appender.parameter_value%TYPE;
    l_cc        t_app_appender.parameter_value%TYPE;
    l_bcc       t_app_appender.parameter_value%TYPE;
    l_port      t_app_appender.parameter_value%TYPE;
    l_timeout   t_app_appender.parameter_value%TYPE;
    l_reply     utl_smtp.reply;
    l_conn      utl_smtp.connection;
    l_offset    PLS_INTEGER;
    l_comma_pos PLS_INTEGER;
    l_csv_list  VARCHAR2(32767);
  BEGIN
    $IF $$debug $THEN 
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app); 
    $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_appender_param',  'YES'); $END
    l_host := get_current_appender_param(x_app, 'SMTP', 'SEND_MAIL_HOST');
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_host: ' || l_host); $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_appender_param',  'YES'); $END
    l_from := get_current_appender_param(x_app, 'SMTP', 'SEND_MAIL_FROM');
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_from: ' || l_from); $END
    
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_appender_param',  'YES'); $END
    l_port := get_current_appender_param(x_app, 'SMTP', 'SEND_MAIL_PORT');
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_port: ' || l_port); $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_appender_param',  'YES'); $END
    l_timeout := get_current_appender_param(x_app, 'SMTP', 'SEND_MAIL_TIMEOUT');
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_timeout: ' || l_timeout); $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_appender_param',  'YES'); $END
    l_conn := utl_smtp.open_connection(host => l_host, port => l_port, tx_timeout => l_timeout);

    l_reply := utl_smtp.helo(l_conn, l_host);

    IF l_reply.code <> 200 THEN
      $IF $$debug $THEN internal_log(logging.c_error_level, l_intlogger, 'HELO failed'); $END
      raise_application_error(-20002, 'HELO SMTP command failed');
    END IF;

    l_reply := utl_smtp.mail(l_conn, l_from);

    IF l_reply.code <> 200 THEN
      $IF $$debug $THEN internal_log(logging.c_error_level, l_intlogger, 'MAIL failed'); $END
      raise_application_error(-20002, 'MAIL SMTP command failed');
    END IF;
    
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_appender_param',  'YES'); $END
    l_to  := get_current_appender_param(x_app, 'SMTP', 'SEND_MAIL_TO');
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_to: ' || l_to); $END    
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_appender_param',  'YES'); $END    
    l_cc  := get_current_appender_param(x_app, 'SMTP', 'SEND_MAIL_CC');
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_cc: ' || l_cc); $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_appender_param',  'YES'); $END
    l_bcc := get_current_appender_param(x_app, 'SMTP', 'SEND_MAIL_BCC');
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_bcc: ' || l_bcc); $END

    IF l_to IS NOT NULL THEN
      l_csv_list := l_to || ',';
    END IF;

    IF l_cc IS NOT NULL THEN
      l_csv_list := l_csv_list || l_cc || ',';
    END IF;

    IF l_bcc IS NOT NULL THEN
      l_csv_list := l_csv_list || l_bcc || ',';
    END IF;
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_csv_list: ' || l_csv_list); $END
    
    l_offset := 1;
    LOOP
      l_comma_pos := instr(l_csv_list, ',', l_offset);
      $IF $$debug $THEN 
        internal_log(logging.c_trace_level, l_intlogger, 'l_comma_pos: ' || l_comma_pos); 
        internal_log(logging.c_trace_level, l_intlogger, 'l_offset: ' || l_offset); 
      $END
      EXIT WHEN l_comma_pos = 0;

      l_reply := utl_smtp.rcpt(l_conn, substr(l_csv_list, l_offset, l_comma_pos - l_offset));
      $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_reply.code: ' || l_reply.code); $END

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

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('is_cyclic_buffer_empty',  'YES'); $END
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
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_reply.code: ' || l_reply.code); $END

    IF l_reply.code <> 200 THEN
      $IF $$debug $THEN internal_log(logging.c_error_level, l_intlogger, 'QUIT command failed'); $END
      raise_application_error(-20002, 'QUIT SMTP command failed');
    END IF;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
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
                     x_message       IN message_type,
                     x_call_stack    IN BOOLEAN,
                     x_log_backtrace IN BOOLEAN) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'log_smtp'; $END
    l_layout        t_app_appender.parameter_value%TYPE;
    l_trigger_level t_log_level.log_level%TYPE;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_level: ' || x_level);
      internal_log(logging.c_debug_level, l_intlogger, 'x_message: ' || x_message); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_call_stack: ' || bool_to_int(x_call_stack)); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_log_backtrace: ' || bool_to_int(x_log_backtrace)); 
    $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_layout',  'YES'); $END
    l_layout := get_current_layout(x_app, c_smtp_appender);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_layout: ' || l_layout);  $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('enqueue_into_cyclic_buffer',  'YES'); $END
    enqueue_into_cyclic_buffer(x_app, format_message(x_message, l_layout, x_logger_name, x_level) || c_nl);

    IF x_log_backtrace THEN
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('enqueue_into_cyclic_buffer',  'YES'); $END
      enqueue_into_cyclic_buffer(x_app, dbms_utility.format_error_stack() || dbms_utility.format_error_backtrace() || c_nl);
    END IF;

    IF x_call_stack THEN
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('enqueue_into_cyclic_buffer',  'YES'); $END
      enqueue_into_cyclic_buffer(x_app, format_call_stack() || c_nl);
    END IF;

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_appender_param',  'YES'); $END
    l_trigger_level := get_current_appender_param(x_app, 'SMTP', 'TRIGGER_LEVEL');
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_trigger_level: ' || l_trigger_level);  $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity',  'YES'); $END
    IF get_level_severity(x_level) >= get_level_severity(l_trigger_level) THEN
      send_buffer(x_app);
    END IF;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
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
                       x_message       IN message_type,
                       x_call_stack    IN BOOLEAN,
                       x_log_backtrace IN BOOLEAN) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'log_stdout'; $END
    l_layout t_app_appender.parameter_value%TYPE;
  BEGIN
    $IF $$debug $THEN 
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_level: ' || x_level);
      internal_log(logging.c_debug_level, l_intlogger, 'x_message: ' || x_message); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_call_stack: ' || bool_to_int(x_call_stack)); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_log_backtrace: ' || bool_to_int(x_log_backtrace)); 
    $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_layout',  'YES'); $END
    l_layout := get_current_layout(x_app, c_dbms_output_appender);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_layout: ' || l_layout);  $END

    dbms_output.put_line(format_message(x_message, l_layout, x_logger_name, x_level));

    IF x_log_backtrace THEN
      dbms_output.put_line(dbms_utility.format_error_stack || dbms_utility.format_error_backtrace);
    END IF;

    IF x_call_stack THEN
      dbms_output.put_line(format_call_stack());
    END IF;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
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
                      x_message       IN message_type,
                      x_call_stack    IN BOOLEAN,
                      x_log_backtrace IN BOOLEAN) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'log_table'; $END
    PRAGMA AUTONOMOUS_TRANSACTION;
    l_backtrace        t_log.backtrace%TYPE := NULL;
    l_call_stack       t_log.call_stack%TYPE := NULL;
    l_timestamp        t_log.log_date%TYPE := systimestamp;
    l_layout           t_app_appender.parameter_value%TYPE;
    l_formated_message t_log.message%TYPE;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_level: ' || x_level);
      internal_log(logging.c_debug_level, l_intlogger, 'x_message: ' || x_message); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_call_stack: ' || bool_to_int(x_call_stack)); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_log_backtrace: ' || bool_to_int(x_log_backtrace)); 
    $END

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_layout',  'YES'); $END
    l_layout := get_current_layout(x_app, c_table_appender);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_layout: ' || l_layout);  $END

    IF x_log_backtrace THEN
      l_backtrace := dbms_utility.format_error_stack || dbms_utility.format_error_backtrace;
    END IF;

    IF x_call_stack THEN
      l_call_stack := format_call_stack();
    END IF;

    l_formated_message := format_message(x_message, l_layout, x_logger_name, x_level);
    $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'l_formated_message: ' || l_formated_message);  $END

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
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
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
                x_message        IN message_type,
                x_log_backtrace  IN BOOLEAN,
                x_log_call_stack IN BOOLEAN) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'log'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.logger: ' || x_logger.logger);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.always_from_ctx: ' || x_logger.always_from_ctx);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level_severity);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.enabled_appenders: ' || x_logger.enabled_appenders);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.app: ' || x_logger.app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_level: ' || x_level);
      internal_log(logging.c_debug_level, l_intlogger, 'x_message: ' || x_message); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_call_stack: ' || bool_to_int(x_log_call_stack)); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_log_backtrace: ' || bool_to_int(x_log_backtrace)); 
    $END

    IF x_logger.always_from_ctx = c_true THEN
      -- check whether custom session settings has been requested, if yes, use them
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('use_requested_session_settings',  'YES'); $END
      use_requested_session_settings(x_logger.app);      
  
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity',  'YES'); $END
      x_logger.log_level_severity := get_level_severity(get_current_used_level(x_logger.logger));
      $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level_severity); $END
    END IF;
    
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity',  'YES'); $END
    IF x_logger.log_level_severity > get_level_severity(x_level) THEN
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'Insignificant message.');  $END
      RETURN;
    END IF;

    IF x_logger.always_from_ctx = c_true THEN
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_used_appenders',  'YES'); $END
      x_logger.enabled_appenders := get_current_used_appenders(x_logger.logger);
      $IF $$debug $THEN internal_log(logging.c_trace_level, l_intlogger, 'x_logger.enabled_appenders: ' || x_logger.enabled_appenders); $END
    END IF;

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bitand', 'YES'); $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_appender_code', 'YES'); $END
    IF bitand(x_logger.enabled_appenders, get_appender_code(c_table_appender)) > 0 THEN
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'Table appender enabled.');  $END
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('log_table',  'YES'); $END
      log_table(x_logger.app, x_logger.logger, x_level, x_message, x_log_call_stack, x_log_backtrace);
    END IF;

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bitand', 'YES'); $END
    $IF  dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_appender_code', 'YES'); $END
    IF bitand(x_logger.enabled_appenders, get_appender_code(c_dbms_output_appender)) > 0 THEN
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'dbms_output appender enabled.');  $END
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('log_stdout',  'YES'); $END
      log_stdout(x_logger.app, x_logger.logger, x_level, x_message, x_log_call_stack, x_log_backtrace);
    END IF;

    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('bitand', 'YES'); $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_appender_code', 'YES'); $END
    IF bitand(x_logger.enabled_appenders, get_appender_code(c_smtp_appender)) > 0 THEN
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'SMTP appender enabled.');  $END
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('log_smtp',  'YES'); $END
      log_smtp(x_logger.app, x_logger.logger, x_level, x_message, x_log_call_stack, x_log_backtrace);
    END IF;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END log;

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
                  x_log_call_stack IN BOOLEAN DEFAULT TRUE) IS
  BEGIN
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('log', 'YES'); $END
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
                 x_message        IN message_type DEFAULT SQLERRM,
                 x_log_backtrace  IN BOOLEAN DEFAULT FALSE,
                 x_log_call_stack IN BOOLEAN DEFAULT TRUE) IS
  BEGIN
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('log', 'YES'); $END
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
                  x_message        IN message_type DEFAULT SQLERRM,
                  x_log_backtrace  IN BOOLEAN DEFAULT FALSE,
                  x_log_call_stack IN BOOLEAN DEFAULT TRUE) IS
  BEGIN
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('log', 'YES'); $END
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
                 x_message        IN message_type DEFAULT SQLERRM,
                 x_log_backtrace  IN BOOLEAN DEFAULT TRUE,
                 x_log_call_stack IN BOOLEAN DEFAULT TRUE) IS
  BEGIN
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('log', 'YES'); $END
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
                  x_message        IN message_type DEFAULT SQLERRM,
                  x_log_backtrace  IN BOOLEAN DEFAULT TRUE,
                  x_log_call_stack IN BOOLEAN DEFAULT TRUE) IS
  BEGIN
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('log', 'YES'); $END
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
                  x_message        IN message_type DEFAULT SQLERRM,
                  x_log_backtrace  IN BOOLEAN DEFAULT TRUE,
                  x_log_call_stack IN BOOLEAN DEFAULT TRUE) IS
  BEGIN
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('log', 'YES'); $END
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'is_trace_enabled'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.logger: ' || x_logger.logger);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.always_from_ctx: ' || x_logger.always_from_ctx);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level_severity);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.enabled_appenders: ' || x_logger.enabled_appenders);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.app: ' || x_logger.app);
    $END

    IF x_logger.always_from_ctx = c_true THEN
      -- check whether custom session settings has been requested, if yes, use them
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('use_requested_session_settings',  'YES'); $END
      use_requested_session_settings(x_logger.app);      
      
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity', 'YES'); $END
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_used_level', 'YES'); $END
      x_logger.log_level_severity := get_level_severity(get_current_used_level(x_logger.logger));
    END IF;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity', 'YES'); $END
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'is_trace_enabled'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.logger: ' || x_logger.logger);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.always_from_ctx: ' || x_logger.always_from_ctx);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level_severity);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.enabled_appenders: ' || x_logger.enabled_appenders);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.app: ' || x_logger.app);
    $END

    IF x_logger.always_from_ctx = c_true THEN
      -- check whether custom session settings has been requested, if yes, use them
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('use_requested_session_settings',  'YES'); $END
      use_requested_session_settings(x_logger.app);      

      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity', 'YES'); $END
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_used_level', 'YES'); $END
      x_logger.log_level_severity := get_level_severity(get_current_used_level(x_logger.logger));
      $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level_severity); $END
    END IF;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity', 'YES'); $END
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'is_info_enabled'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.logger: ' || x_logger.logger);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.always_from_ctx: ' || x_logger.always_from_ctx);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level_severity);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.enabled_appenders: ' || x_logger.enabled_appenders);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.app: ' || x_logger.app);
    $END

    IF x_logger.always_from_ctx = c_true THEN
      -- check whether custom session settings has been requested, if yes, use them
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('use_requested_session_settings',  'YES'); $END
      use_requested_session_settings(x_logger.app);      
      
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity', 'YES'); $END
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_used_level', 'YES'); $END
      x_logger.log_level_severity := get_level_severity(get_current_used_level(x_logger.logger));
      $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level_severity); $END
    END IF;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity', 'YES'); $END
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'is_warn_enabled'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.logger: ' || x_logger.logger);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.always_from_ctx: ' || x_logger.always_from_ctx);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level_severity);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.enabled_appenders: ' || x_logger.enabled_appenders);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.app: ' || x_logger.app);
    $END

    IF x_logger.always_from_ctx = c_true THEN
      -- check whether custom session settings has been requested, if yes, use them
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('use_requested_session_settings',  'YES'); $END
      use_requested_session_settings(x_logger.app);      

      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity', 'YES'); $END
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_used_level', 'YES'); $END
      x_logger.log_level_severity := get_level_severity(get_current_used_level(x_logger.logger));
      $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level_severity); $END
    END IF;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity', 'YES'); $END
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'is_error_enabled'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.logger: ' || x_logger.logger);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.always_from_ctx: ' || x_logger.always_from_ctx);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level_severity);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.enabled_appenders: ' || x_logger.enabled_appenders);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.app: ' || x_logger.app);
    $END

    IF x_logger.always_from_ctx = c_true THEN
      -- check whether custom session settings has been requested, if yes, use them
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('use_requested_session_settings',  'YES'); $END
      use_requested_session_settings(x_logger.app);      

      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity', 'YES'); $END
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_used_level', 'YES'); $END
      x_logger.log_level_severity := get_level_severity(get_current_used_level(x_logger.logger));
      $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level_severity); $END
    END IF;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity', 'YES'); $END
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
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'is_fatal_enabled'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.logger: ' || x_logger.logger);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.always_from_ctx: ' || x_logger.always_from_ctx);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level_severity);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.enabled_appenders: ' || x_logger.enabled_appenders);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.app: ' || x_logger.app);
    $END

    IF x_logger.always_from_ctx = c_true THEN
      -- check whether custom session settings has been requested, if yes, use them
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('use_requested_session_settings',  'YES'); $END
      use_requested_session_settings(x_logger.app);      
      
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity', 'YES'); $END
      $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_current_used_level', 'YES'); $END
      x_logger.log_level_severity := get_level_severity(get_current_used_level(x_logger.logger));
      $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level_severity); $END
    END IF;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
    $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('get_level_severity', 'YES'); $END
    RETURN x_logger.log_level_severity <= get_level_severity(c_fatal_level);
  END is_fatal_enabled;

  /** Procedure purges all global contexts used by logging */
  PROCEDURE purge_global_contexts IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'purge_global_contexts'; $END
  BEGIN
    $IF $$debug $THEN 
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_info_level, l_intlogger, 'purging global contexts');
    $END
    FOR l_row IN (SELECT a.base_context_name
                    FROM t_appender a) LOOP
      clear_all_context_rac_aware(l_row.base_context_name || '_G', c_global_flag);
    END LOOP;
    clear_all_context_rac_aware(c_additivity_ctx(c_global_flag), c_global_flag);
    clear_all_context_rac_aware(c_global_appenders_ctx, c_global_flag);
    clear_all_context_rac_aware(c_logger_levels_ctx(c_global_flag), c_global_flag);
    clear_all_context_rac_aware(c_global_levels_ctx, c_global_flag);
    clear_all_context_rac_aware(c_logger_names_ctx(c_global_flag), c_global_flag);
    clear_all_context_rac_aware(c_parameters_ctx(c_global_flag), c_global_flag);
    clear_all_context_rac_aware(c_global_user_app_ctx, c_global_flag);
    clear_all_context_rac_aware(c_logger_appenders_ctx(c_global_flag), c_global_flag);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END purge_global_contexts;

  /** Procedure purges all session contexts used by logging */
  PROCEDURE purge_session_contexts IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'purge_session_contexts'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_info_level, l_intlogger, 'purging session contexts');
    $END
    FOR l_row IN (SELECT a.base_context_name
                    FROM t_appender a) LOOP
      clear_all_context_rac_aware(l_row.base_context_name || '_L', c_session_flag);
    END LOOP;
    clear_all_context_rac_aware(c_additivity_ctx(c_session_flag), c_session_flag);
    clear_all_context_rac_aware(c_logger_appenders_ctx(c_session_flag), c_session_flag);
    clear_all_context_rac_aware(c_logger_levels_ctx(c_session_flag), c_session_flag);
    clear_all_context_rac_aware(c_logger_names_ctx(c_session_flag), c_session_flag);
    clear_all_context_rac_aware(c_parameters_ctx(c_session_flag), c_session_flag);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END purge_session_contexts;

  /** Procedure copies global setting to session settings.
  * @param x_ap p Application name. If set, copying will be done only for the given application.
  */
  PROCEDURE copy_global_to_session(x_app IN t_app.app%TYPE) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'copy_global_to_session'; $END
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start'); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_app' || x_app); 
      internal_log(logging.c_info_level, l_intlogger, 'copying global context to session context'); 
    $END
    purge_session_contexts();
    init_params(c_session_flag, x_app);
    init_appenders(c_session_flag);
    init_loggers(c_session_flag, x_app);
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END copy_global_to_session;
  
  /**
  * Procedure enables custom settings for given session.
  * @param x_instance Id of instance for the session (e.g. from gv$session.inst_id).
  * @param x_sessionid Audit session identifier (from gv$session.audsid)
  */
  PROCEDURE apply_settings_for_session(x_instance  IN PLS_INTEGER,
                                       x_sessionid IN NUMBER) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'apply_settings_for_session'; $END
    l_settings ctx_value_type;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_instance: ' || x_instance);
      internal_log(logging.c_debug_level, l_intlogger, 'x_sessionid: ' || x_sessionid);
    $END
    
    l_settings := serialize_settings();
    $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 'l_settings: ' || l_settings); $END
        
    set_context_rac_aware(c_modify_session_ctx, x_instance || '#' || x_sessionid, l_settings, c_global_flag);
     $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end'); $END
  END apply_settings_for_session;

  /**
  * Procedure adds an application to the configuration.
  * @param x_app Application name.
  * @param x_app_descr Application description.
  */
  PROCEDURE add_app(x_app IN t_app.app%TYPE,
                    x_app_descr IN t_app.app_desc%TYPE) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'add_app'; $END
    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_app_descr: ' || x_app_descr);
    $END
    
    INSERT INTO t_app(app, app_desc)
    VALUES (x_app, x_app_descr);
        
    COMMIT;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end - application was added'); $END
  END add_app;

  /**
  * Procedure removes the given application and all related configuration.
  * @param x_app Application name.
  */
  PROCEDURE remove_app(x_app IN t_app.app%TYPE) IS
    $IF $$debug $THEN l_intlogger t_logger.logger%TYPE := 'remove_app'; $END
    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN
    $IF $$debug $THEN
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
    $END

    DELETE FROM t_app_appender aa
    WHERE aa.app = x_app;
    $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 't_app_appender, deleted: ' || SQL%ROWCOUNT); $END
    
    DELETE FROM t_schema_app sa
    WHERE sa.app = x_app;
    $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 't_schema_app, deleted: ' || SQL%ROWCOUNT); $END
    
    DELETE FROM t_param sa
    WHERE sa.app = x_app;
    $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 't_param, deleted: ' || SQL%ROWCOUNT); $END

    DELETE FROM t_logger a
    WHERE a.logger LIKE x_app || '.%';
    $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 't_logger, deleted: ' || SQL%ROWCOUNT); $END
  
    DELETE FROM t_app a
    WHERE a.app = x_app;
    $IF $$debug $THEN internal_log(logging.c_debug_level, l_intlogger, 't_app, deleted: ' || SQL%ROWCOUNT); $END


    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'Clearing contexts'); $END
    FOR l_row IN (
        select gc.namespace, gc.attribute
          from global_context gc
         where gc.attribute in
               (select gc2.attribute
                  from global_context gc2
                 where gc2.value like x_app || '.%'
                   and gc2.namespace = c_logger_names_ctx(c_global_flag))
            or (gc.value = x_app 
            and gc.namespace = c_global_user_app_ctx)
            or (gc.attribute like x_app || '#')
    ) LOOP    
       set_context_rac_aware(l_row.namespace, l_row.attribute, NULL, c_global_flag);
    END LOOP;
        
    COMMIT;
    $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'end - application was removed'); $END
  END remove_app;

BEGIN
 -- these elements are defined only if internal debugging is set to TRUE
  $IF $$debug $THEN  
    init_log_level_severities();
    internal_log(logging.c_info_level, 'initialization', 'start');
  $END
  
  $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('init_session_identifier', 'YES'); $END
  init_session_identifier();  
  $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('hash_logger_name', 'YES'); $END
  g_root_logger_hash := hash_logger_name(c_root_logger_name);
  $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('init_user_app', 'YES'); $END
  init_user_app;
  $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('init_params', 'YES'); $END
  init_params(c_global_flag);
  $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('init_levels', 'YES'); $END
  init_levels;
  $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('init_appenders', 'YES'); $END
  init_appenders(c_global_flag);
  $IF dbms_db_version.version >= 11 $THEN PRAGMA INLINE('init_loggers', 'YES'); $END
  init_loggers(c_global_flag);

  $IF $$debug $THEN  
    internal_log(logging.c_info_level, 'initialization', 'end');
  $END
END logging;
/

