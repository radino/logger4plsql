create or replace package body logging is
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
  
  /** Type for exception messages. */ 
  subtype exception_msg_type is varchar2(255);
  
  /** exeption message for unimplemented feature. */ 
  c_unimplemented_feature_msg constant exception_msg_type := 'Unimplemented feature: %{1}.';

  /** exeption message for unimplemented feature. */ 
  c_internal_use_msg constant exception_msg_type := 'For internal use only.';

  /** exeption message for insufficient privileges feature. */ 
  c_insufficient_privs_msg constant exception_msg_type := 'Insufficient privileges to %{1}.';
 
  /** exeption message for non-existing appender. */ 
  c_no_such_appender_msg constant exception_msg_type := 'Appender with code %{1} does not exist.';

  /** exeption message for non-existing application. */ 
  c_no_such_app_msg constant exception_msg_type := 'Application %{1} does not exist.';

  /** exeption message for non-existing handle. */ 
  c_no_such_handle_msg constant exception_msg_type := 'Handle %{1} does not exist.';

  /** exeption message for failure in SMTP protokol. */ 
  c_smtp_failure_msg constant exception_msg_type := 'SMTP command %{1} failed with %{2} %{3}.';
  
  /** User using a logger. */
  c_user constant varchar2(32) := upper(user);

  /** SMTP appender: Type for a cyclic buffer item. */
  subtype mail_item_type is varchar2(4000);

  /** SMTP appender: Type for cyclic buffer. */
  type mail_table_type is table of mail_item_type;

  /** SMTP appender: Type for cyclic buffer structure (queue). */
  type mail_cyclic_buffer_type is record(
    buff_size pls_integer, -- buffer size
    head      pls_integer, -- position of head in the queue
    tail      pls_integer, -- position of tail in the queue
    buffer    mail_table_type -- buffer containg logs
    );

  -- these elements are public when unit testing precompiler option is set to TRUE
  -- therefore they cannot be defined in the body
  $if not $$unit_test or $$unit_test is null $then

  /** Type for exception parameters. */
  type exception_params_type is table of message_type;

  /** Type for visibility: session or global */
  subtype visibility_type is pls_integer range 1 .. 2;

  /** Flag for visibility: global */
  c_global_flag constant visibility_type := 1;

  /** Flag for visibility: session */
  c_session_flag constant visibility_type := 2;

  /** Type used for serialization of loggers parameters */
  type logger_settings_type is record (
     enabled_appenders  t_logger.appenders%type,
     log_level          t_logger.log_level%type,
     additivity         t_logger.additivity%type,
     backtrace          t_logger.backtrace%type,
     callstack          t_logger.callstack%type
  );

  /** Collection type of loggers used for serialization of loggers parameters */
  type logger_settings_col_type is table of logger_settings_type index by t_logger.logger%type;

  /** Type for application settings: application parameters and appender parameters */
  type app_settings_type is record (
     app_params       appender_params_type,
     appenders_params appenders_params_type
  );

  /** Collection of application settings */
  type app_settings_col_type is table of app_settings_type index by t_app.app%type;

  /** Type for serialized logging settings. */
  type deserialized_settings_type is record (
     loggers          logger_settings_col_type,
     app_settings     app_settings_col_type
  );

  /** Type for collection of serialized logging settings. */
  type deserialized_settings_col_type is table of deserialized_settings_type;

  $end

  /** Type for context list */
  type context_list_type is varray(2) of ctx_namespace_type;

  /** Names of global and session contexts containing flags for loggers. */
  c_flags_ctx constant context_list_type := context_list_type('CTX_LOGGER_FLAGS_G', 'CTX_LOGGER_FLAGS_L');

  /** Names of global and session contexts containing appenders set in loggers. */
  c_logger_appenders_ctx constant context_list_type := context_list_type('CTX_LOGGER_APP_G', 'CTX_LOGGER_APP_L');

  /** Names of global and session contexts containing set log levels for loggers. */
  c_logger_levels_ctx constant context_list_type := context_list_type('CTX_LOGGER_LEV_G', 'CTX_LOGGER_LEV_L');

  /** Name of global context containing settings for a session. */
  c_modify_session_ctx constant ctx_namespace_type := 'CTX_LOGGER_MODIFY_SESSION_G';

  /** Names of global and session contexts containing logger names. */
  c_logger_names_ctx constant context_list_type := context_list_type('CTX_LOGGER_NAME_G', 'CTX_LOGGER_NAME_L');

  /** Name of global context containing schema-application mapping. */
  c_global_user_app_ctx constant ctx_namespace_type := 'CTX_LOGGER_USER_APP_G';

  /** Names of global and session context containing parameters for logging system. */
  c_parameters_ctx constant context_list_type := context_list_type('CTX_LOGGER_PARAMS_G', 'CTX_LOGGER_PARAMS_L');

  /** Name of global context containing appenders' base context name. */
  c_global_appenders_ctx constant ctx_namespace_type := 'CTX_LOGGER_APPENDERS_G';

  /** Suffixes of appenders context names regarding to visibility. */
  c_append_vis_suffix context_list_type := context_list_type('_G','_L');

  /** Root logger name. */
  c_root_logger_name constant ctx_namespace_type := '/';

  /** Key for flag indicating whether session context is in use. */
  c_session_usage_param constant ctx_attribute_type := 'USAGE';

  /** Key for layout parameter in context. */
  c_layout_param constant ctx_attribute_type := 'LAYOUT';

  /** Key for flag inticating whether a context containing this key was initialized or not. */
  c_init_param constant ctx_attribute_type := 'INITIALIZED';

  /** Key for default appenders' layout. */
  c_default_layout_param constant ctx_attribute_type := 'DEFAULT#DEFAULT_LAYOUT';

  /** Separator char in named hierarchy. */
  c_separator constant varchar2(1) := '.';

  /** End of line character(s). */
  c_nl constant varchar2(2) := chr(10);

  /** Space character. */
  c_space constant varchar2(1) := chr(32);

  /** Lenght of end of line character(s). */
  c_nl_length constant pls_integer := length(c_nl);

  /** Schema where the logger is installed. */
  c_schema_name constant ctx_namespace_type := sys_context('USERENV', 'CURRENT_SCHEMA');

  /** Qualified name of this package. */
  c_package_name constant ctx_namespace_type := c_schema_name || '.' || $$PLSQL_UNIT;
  
  /** Binary flag for additivity. */
  c_flag_add constant pls_integer := 1;

  /** Binary flag for backtrace. */
  c_flag_backtrace constant pls_integer := 2;

  /** Binary flag for callstack. */
  c_flag_callstack constant pls_integer := 4;
  

  /** Identifier for the session which consists of two componets:
  * {*} instance number  Retrieved from sys_context('userenv', 'instance')
  * {*} session_id       Retrieved from sys_context('userenv', 'sessionid')
  * session_id has been chosen, bacause sid is recycled and serial# is not held in userenv context
  * The implication is: Feature for setting context for a given session cannot be used for SYSDBA accounts.
  */
  g_session_identifier global_context.attribute%type;

  /** SMTP appender: Cyclic buffer global variable. */
  g_mail_buffer mail_cyclic_buffer_type;

  /** Hash for the root logger */
  g_root_logger_hash hash_type;

  /** A variable for builtding serialized settings */
  g_serialized_settings deserialized_settings_col_type;

  -- these elements are defined only if internal debugging is set to TRUE
  $if $$debug $then
  /**
  * Procedure logs given message as an internal log.
  * @param x_level Log levelof the message.
  * @param x_logger Logger name.
  * @param x_message Message.
  * @param x_appender Binary encoded appenders (1 - table, 2 - dbms_output)
  */
  procedure internal_log(x_level    in t_logger.log_level%type,
                         x_logger   in t_logger.logger%type,
                         x_message  in message_type,
                         x_appender in pls_integer default g_internal_appenders) is
    pragma autonomous_transaction;
  begin
    if g_internal_log_level > x_level then
       return;
    end if;

    if bitand(x_appender, 1) = 1 then
      insert into t_log
        (id, logger, message, log_date, call_stack, backtrace, log_level)
      values
        (seq_log_id.nextval,
         x_logger,
         x_message,
         systimestamp,
         null,
         null,
         x_level);
    end if;

    if bitand(x_appender, 2) = 2 then
      dbms_output.put_line(to_char(systimestamp, 'dd.mm.yyyy hh24:mi:ss.ff3') || ' ' || x_logger ||
                           '['||x_level||']: ' || x_message);
    end if;

    commit;
  end internal_log;
  $end

  /**
  * Function binds parameter values to given error message.
  * @param x_message Name of context
  * @param x_params Attribute
  * @return Message with instantiated parameters
  */
  function bind_params(x_message in message_type,
                       x_params  in exception_params_type) return message_type is
    l_message message_type;
  begin
    l_message := x_message;
    for i in 1..x_params.count loop
      l_message := replace(l_message, '%{'||i||'}', x_params(i));
    end loop;
    return l_message;
  end bind_params;
  
  /** Procedure raises uniplemented feature exception.
  */
  procedure unimplemented(x_feature in varchar2) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'unimplemented'; $end
  begin
    $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'unimplemented feature'); $end
    raise_application_error(c_unimplemented_feature_code, 
                            bind_params(c_unimplemented_feature_msg, exception_params_type(x_feature)));
  end unimplemented;

  /**
  * Procedure sets given attribute of given context to given value.
  * @param x_namespace Name of context
  * @param x_attribute Attribute
  * @param x_value Value of attribute
  * @raises e_internal_use_only Can not be called from another schema.
  */
  procedure set_context(x_namespace in ctx_namespace_type,
                        x_attribute in ctx_attribute_type,
                        x_value     in ctx_value_type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_context'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_namespace: ' || x_namespace);
      internal_log(logging.c_debug_level, l_intlogger, 'x_attribute: ' || x_attribute);
      internal_log(logging.c_debug_level, l_intlogger, 'x_value: ' || x_value);
    $end

    if x_value is null then
      $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'clearing context'); $end
      dbms_session.clear_context(x_namespace, x_attribute);
    else
     $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'setting context'); $end
     dbms_session.set_context(x_namespace, x_attribute, x_value);
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_context;

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
                            x_value     in ctx_value_type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_context_job'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_namespace: ' || x_namespace);
      internal_log(logging.c_debug_level, l_intlogger, 'x_attribute: ' || x_attribute);
      internal_log(logging.c_debug_level, l_intlogger, 'x_value: ' || x_value);
      internal_log(logging.c_debug_level, l_intlogger, 'c_user: ' || c_user);
      internal_log(logging.c_debug_level, l_intlogger, 'c_schema_name: ' || c_schema_name);
    $end

    if c_user <> c_schema_name then
      $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'access violation'); $end
      raise_application_error(c_internal_use_code, c_internal_use_msg);
    end if;

    $if dbms_db_version.version >= 11 $then pragma inline('set_context',  'YES'); $end
    set_context(x_namespace, x_attribute, x_value);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_context_job;

  /**
  * Procedure clears all contexts.
  * For internal use only. Do not use.
  * @param x_namespace Name of context
  * @raises e_internal_use_only Can not be called from another schema.
  */
  procedure clear_all_context(x_namespace in ctx_namespace_type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'clear_all_context'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_namespace: ' || x_namespace);
      internal_log(logging.c_debug_level, l_intlogger, 'c_user: ' || c_user);
      internal_log(logging.c_debug_level, l_intlogger, 'c_schema_name: ' || c_schema_name);
    $end

    if c_user <> c_schema_name then
      $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'access violation'); $end
      raise_application_error(c_internal_use_code, c_internal_use_msg);
    end if;

    $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'clearing context'); $end
    dbms_session.clear_all_context(x_namespace);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end clear_all_context;

  /**
  * Procedure clears all contexts.
  * @param x_namespace Name of context
  * @param x_visibility Flag, whether global or session contexts should cleared
  * {*} c_global_flag Set flag for a global context - rac aware
  * {*} c_session_flag Set flag for a session context - current instance
  */
  procedure clear_all_context_rac_aware(x_namespace in ctx_namespace_type,
                                        x_visibility in visibility_type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'clear_all_context_rac_aware'; $end
    l_instance_count number;
    l_instance_table dbms_utility.instance_table;
    l_what           user_jobs.what%type;
    e_invalid_instance exception;
    pragma exception_init(e_invalid_instance, -23428);
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_namespace: ' || x_namespace);
    $end

    -- until 11.2 the context changes are not replicated across the instances
    -- a workaround is to use an instance affinity for jobs to set the context on all active instances
    $if logging.ver_lt_11_2 $then
    $if $$debug $then internal_log(logging.c_warning_level, l_intlogger, 'RAC-aware context not supported.'); $end
    dbms_utility.active_instances(instance_table => l_instance_table, instance_count => l_instance_count);

    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'Number of RAC instances: ' || l_instance_count); $end

    if l_instance_count = 0 or x_visibility = c_session_flag then
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'No RAC - clearing the context' ); $end
      dbms_session.clear_all_context(x_namespace => x_namespace);
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
      return;
    end if;

    l_what := 'logging.clear_all_context(''' || x_namespace || ''');';
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_what: ' || l_what); $end

    declare
      pragma autonomous_transaction;
    begin
      for i in 1 .. l_instance_count loop
        begin
          $if $$debug $then
            internal_log(logging.c_trace_level, l_intlogger, 'l_what: ' || l_what);
            internal_log(logging.c_trace_level, l_intlogger, 'inst_number: ' || l_instance_table(i).inst_number);
          $end
          dbms_job.submit(job       => l_job_number,
                          what      => l_what,
                          next_date => sysdate,
                          interval  => null,
                          instance  => l_instance_table(i).inst_number);
          -- if there is no such instance ignore error (or it is not running)
          $if $$debug $then
            internal_log(logging.c_trace_level, l_intlogger, 'l_job_number: ' || l_job_number);
            internal_log(logging.c_info_level, l_intlogger, 'job created: ');
          $end
        exception
          when e_invalid_instance then
            $if $$debug $then
              internal_log(logging.c_warning_level, l_intlogger, 'Invalid instance: ' || l_instance_table(i).inst_number);
            $end

            null;
        end;
      end loop;
      commit;
    end;
    $else
      -- if the database version is >=11.2, applicaton context is replicated across the instances
      $if $$debug $then
        internal_log(logging.c_info_level, l_intlogger, 'RAC >=11.2, context supported');
        internal_log(logging.c_trace_level, l_intlogger, 'Clearing context: ' || x_namespace);
      $end
      dbms_session.clear_all_context(namespace => x_namespace);
    $end
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end clear_all_context_rac_aware;

  /** Procedure sets given attribute of given context to the given value.
  * Procedure is RAC-aware. If the database is in RAC the context is set
  * @param x_namespace Name of context
  * @param x_attribute Attribute
  * @param x_value Value of attribute
  * @param x_visibility Flag, whether global or session contexts should be initialized.
  * {*} c_global_flag Set flag for a global context - rac aware
  * {*} c_session_flag Set flag for a session context - current instance
  */
  procedure set_context_rac_aware(x_namespace      in ctx_namespace_type,
                                  x_attribute      in ctx_attribute_type,
                                  x_value          in ctx_value_type,
                                  x_visibility     in visibility_type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_context_rac_aware'; $end
    l_job_number     binary_integer;
    l_what           user_jobs.what%type;
    l_instance_count number;
    l_instance_table dbms_utility.instance_table;
    e_invalid_instance exception;
    pragma exception_init(e_invalid_instance, -23428);

  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_namespace: ' || x_namespace);
      internal_log(logging.c_debug_level, l_intlogger, 'x_attribute: ' || x_attribute);
      internal_log(logging.c_debug_level, l_intlogger, 'x_value: ' || x_value);
    $end


    -- until 11.2 the context changes are not replicated across the instances
    -- a workaround is to use an instance affinity for jobs to set the context on all active instances
    $if logging.ver_lt_11_2 $then
      $if $$debug $then internal_log(logging.c_warning_level, l_intlogger, 'version < 11.2 - RAC aware context not supported'); $end
      dbms_utility.active_instances(instance_table => l_instance_table, instance_count => l_instance_count);
      $if $$debug $then
        internal_log(logging.c_debug_level, l_intlogger, 'instance table: ');
        for i in 1 .. l_instance_count loop
          internal_log(logging.c_trace_level, 'instance number: ' || l_instance_table(i).instance_number);
          internal_log(logging.c_trace_level, 'instance number: ' || l_instance_table(i).instance_name);
        end loop;
      $end


      if l_instance_count = 0 or x_visibility = c_session_flag  then
        $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'No RAC - setting the context locally '); $end
        $if dbms_db_version.version >= 11 $then pragma inline('set_context',  'YES'); $end
        set_context(x_namespace => x_namespace, x_attribute => x_attribute, x_value => x_value);
        return;
      end if;

      l_what := 'logging.set_context_job(
                      ''' || x_namespace || ''',
                      ''' || x_attribute || ''',
                      ''' || x_value || '''
                  );';

      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'job: ' || l_what); $end

      declare
        pragma autonomous_transaction;
      begin
        for i in 1 .. l_instance_count loop
          begin
            dbms_job.submit(job       => l_job_number,
                            what      => l_what,
                            next_date => sysdate,
                            interval  => null,
                            instance  => l_instance_table(i).inst_number);
            -- if there is no such instance ignore error (or it is not running)
            $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'job: ' || l_job_number || ' for instance ' || l_instance_table(i).inst_number); $end
          exception
            when e_invalid_instance then
              $if $$debug $then internal_log(logging.c_warning_level, l_intlogger, 'Invalid instance: ' || l_instance_table(i).inst_number); $end
              null;
          end;

        end loop;
        commit;
      end;
    $else
      -- if the database version is >=11.2, applicaton context is replicated across the instances
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'version >=11.2, rac aware context supported'); $end
      $if dbms_db_version.version >= 11 $then pragma inline('set_context',  'YES'); $end
      set_context(x_namespace => x_namespace, x_attribute => x_attribute, x_value => x_value);
    $end
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_context_rac_aware;

  /**
  * Function returns bitwise OR of given numbers.
  * @param x_n1 First operand.
  * @param x_n2 Second operand.
  * @return Bitwise OR of given numbers.
  */
  function bit_or(x_n1 in number,
                  x_n2 in number) return number is
    $if $$debug $then l_intlogger t_logger.logger%type := 'bit_or'; $end
    l_result number;
  begin
    $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'x_n1: ' || x_n1 || ', x_n2 ' || x_n2); $end
    l_result := x_n1 + x_n2 - bitand(x_n1, x_n2);
    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_result;
  end bit_or;

  /**
  * Function returns bitwise exclusive OR of given numbers.
  * @param x_n1 First operand.
  * @param x_n2 Second operand.
  * @return Bitwise exclusive OR of given numbers.
  */
  function bit_xor(x_n1 in number,
                   x_n2 in number) return number is
    $if $$debug $then l_intlogger t_logger.logger%type := 'bit_xor'; $end
    l_result number;
  begin
    $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'x_n1: ' || x_n1 || ', x_n2 ' || x_n2); $end
    -- would be nice to have bitwise shifts in PL/SQL: *2 => << 1
    l_result := x_n1 + x_n2 - 2 * bitand(x_n1, x_n2);
    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_result;
  end bit_xor;

  /* Function translates 3-valued BOOLEAN to NUMBER
  * @param x_boolean A boolean value
  * @return The translated boolean to integer
  *         {*} 0 when the given value is FALSE
  *         {*} 1 when the given value is TRUE
  *         {*} NULL, otherwise
  */
  function bool_to_int(x_boolean in boolean) return number is
  begin
    return case x_boolean when true then 1 when false then 0 end;
  end bool_to_int;

  /* Function translates an NUMBER to BOOLEAN
  * @param x_number A number value.
  * @return The translated number to boolean
  *         {*} FALSE when the given value is 0
  *         {*} TRUE when the given value is 1
  *         {*} NULL, otherwise
  */
  function int_to_bool(x_number in number) return boolean is
  begin
    return case x_number when 1 then true when 0 then false end;
  end int_to_bool;

  /**
  * Funtion creates a XML string containing given parameters.
  * @param x_names Parameter names (has to be dense collection).
  * @param x_values Parameter values (has to be dense collection).
  * @return XML string containing given parameters.
  */
  function serialize_to_xml(x_names  in param_names_type,
                            x_values in param_values_type) return varchar2 is
    $if $$debug $then l_intlogger t_logger.logger%type := 'serialize_to_xml'; $end
    l_result varchar2(4000);
  begin
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'start'); $end
    if x_names.first is null then
      $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'Names collection is empty'); $end
      return null;
    end if;
    l_result := '<params>';
    for i in x_names.first .. x_names.last loop
      $if $$debug $then
        internal_log(logging.c_trace_level, l_intlogger, 'x_names('||i||'): ' || x_names(i));
        internal_log(logging.c_trace_level, l_intlogger, 'x_values('||i||'): ' || x_values(i));
      $end
      if x_values(i) is null then
        l_result := l_result || '<' || x_names(i) || '/>';
      else
        l_result := l_result || '<' || x_names(i) || '>' || x_values(i) || '</' || x_names(i) || '>';
      end if;
    end loop;
    l_result := l_result || '</params>';
    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_result;
  end serialize_to_xml;

  /**
  * Funtion creates a JSON string containing given parameters.
  * @param x_names Parameter names (has to be dense collection).
  * @param x_values Parameter values (has to be dense collection).
  * @return a JSON string containing given parameters.
  */
  function serialize_to_json(x_names  in param_names_type,
                             x_values in param_values_type) return varchar2 is
    $if $$debug $then l_intlogger t_logger.logger%type := 'serialize_to_json'; $end
    l_result varchar2(4000);
    c_nl constant varchar2(1) := chr(10);
  begin
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'start'); $end
    if x_names.first is null then
      $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'Names collection is empty'); $end
      return null;
    end if;

    for i in x_names.first .. x_names.last loop
      $if $$debug $then
        internal_log(logging.c_trace_level, l_intlogger, 'x_names('||i||'): ' || x_names(i));
        internal_log(logging.c_trace_level, l_intlogger, 'x_values('||i||'): ' || x_values(i));
      $end
      if x_values(i) is null then
        l_result := l_result || x_values(i) || ': ' || x_names(i) || c_nl;
      end if;
    end loop;

    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_result;
  end serialize_to_json;

  /**
  * Function hashes given logger name.
  * @param x_logger Logger name.
  * @return Hash of given logger name.
  */
  function hash_logger_name(x_logger_name in t_logger.logger%type) return hash_type is
  begin
    return dbms_utility.get_hash_value(x_logger_name, 1, 1073741824);
  end hash_logger_name;

  /**
  * Function encodes given flags to a binary number
  * @param x_additivity Additivity flag.
  * @param x_backtrace Backtrace flag.
  * @param x_callstack Callstack flag.
  * @return Binary encoded flags.
  */
  function encode_flags(x_additivity in t_logger.additivity%type,
                        x_backtrace  in t_logger.backtrace%type,
                        x_callstack  in t_logger.callstack%type) return pls_integer is
    $if $$debug $then l_intlogger t_logger.logger%type := 'encode_flags'; $end
    l_result pls_integer;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_additivity: ' || x_additivity);
      internal_log(logging.c_debug_level, l_intlogger, 'x_backtrace: ' || x_backtrace);
      internal_log(logging.c_debug_level, l_intlogger, 'x_callstack: ' || x_callstack);
    $end
    l_result := nvl(x_additivity, 0) * c_flag_add + 
                nvl(x_backtrace, 0)  * c_flag_backtrace +
                nvl(x_callstack, 0)  * c_flag_callstack;
    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_result;
  end encode_flags;
  
  /**
  * Function returns application name for given schema.
  * @param x_schema Schema name.
  * @return Application name for given schema.
  */
  function get_app(x_schema in t_schema_app.schema%type) return t_schema_app.app%type is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_app'; $end
  begin
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'start and end'); $end
    return sys_context(c_global_user_app_ctx, x_schema);
  end get_app;

  /**
  * Function returns level name for given level.
  * @param x_log_level
  * @return Log level name
  */
  function get_level_name(x_log_level in t_logger.log_level%type) return varchar2 is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_level_name'; $end
    l_result varchar2(10);
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_log_level: ' || x_log_level);
    $end

    l_result := CASE x_log_level
                WHEN c_all_level THEN 'ALL'
                WHEN c_trace_level THEN 'TRACE'
                WHEN c_debug_level THEN 'DEBUG'
                WHEN c_info_level THEN 'INFO'
                WHEN c_warn_level THEN 'WARN'
                WHEN c_error_level THEN 'ERROR'
                WHEN c_fatal_level THEN 'FATAL'
                WHEN c_off_level THEN 'OFF'
                ELSE 'CUST('||x_log_level||')' END;

    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_result;
  end get_level_name;

  /**
  * Function returns call stack (without functions of this package).
  * @return Call stack.
  */
  function format_call_stack return varchar2 is
    $if $$debug $then l_intlogger t_logger.logger%type := 'format_call_stack'; $end
    l_header_end  pls_integer;
    l_logging_end pls_integer;
    l_call_stack  varchar2(2000 char);
    l_result varchar2(2000 char);
    c_stack_body_offset constant pls_integer := 3;
  begin
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'start'); $end
    l_call_stack := dbms_utility.format_call_stack();
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_call_stack: ' || l_call_stack); $end
    -- skip header
    l_header_end := instr(l_call_stack, c_nl, nth => c_stack_body_offset) + c_nl_length;
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_header_end: ' || l_header_end); $end

    l_logging_end := instr(l_call_stack, c_nl, instr(l_call_stack, c_package_name, -1, 1));
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_logging_end: ' || l_logging_end); $end

    l_result := substr(l_call_stack, 1, l_header_end) || substr(l_call_stack, l_logging_end + c_nl_length);
    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_result;
  end format_call_stack;

  /**
  * Procedure adds given schema to given application.
  * @param x_app Application name.
  * @param x_schema Schema name.
  */
  procedure add_schema_to_app(x_app    in t_schema_app.app%type,
                              x_schema in t_schema_app.schema%type default user) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'add_schema_to_app'; $end
    pragma autonomous_transaction;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_schema: ' || x_schema);
    $end
    insert into t_schema_app
      (schema, app)
    values
      (upper(x_schema), upper(x_app));

    set_context_rac_aware(c_global_user_app_ctx, upper(x_schema), upper(x_app), c_global_flag);

    commit;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  exception
    when dup_val_on_index then
      $if $$debug $then
        internal_log(logging.c_warn_level, l_intlogger, 'Schema is already assigned to the app');
        internal_log(logging.c_info_level, l_intlogger, 'end');
      $end
      null;
  end add_schema_to_app;

  /**
  * Procedure removes given schema from given application.
  * @param x_app Application name.
  * @param x_schema Schema name.
  */
  procedure remove_schema_from_app(x_app    in t_schema_app.app%type,
                                   x_schema in t_schema_app.schema%type default user) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'remove_schema_from_app'; $end
    pragma autonomous_transaction;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_schema: ' || x_schema);
    $end

    delete from t_schema_app ua
     where ua.schema = upper(x_schema)
       and ua.app = upper(x_app);

    $if $$debug $then
      if sql%notfound then
        internal_log(logging.c_warn_level, l_intlogger, 'Schema is not assigned to the app');
      end if;
    $end
    set_context_rac_aware(c_global_user_app_ctx, upper(x_schema), null, c_global_flag); -- it's case insensitive, but for clarity..
    commit;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end remove_schema_from_app;

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
  procedure set_initialization(x_ctx         in ctx_namespace_type,
                               x_initialized in boolean,
                               x_visibility  in visibility_type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_initialization'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_ctx: ' || x_ctx);
      internal_log(logging.c_debug_level, l_intlogger, 'x_initialized: ' || bool_to_int(x_initialized));
      internal_log(logging.c_debug_level, l_intlogger, 'x_visibility: ' || x_visibility);
    $end

    if x_initialized then
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'setting the context'); $end
      set_context_rac_aware(x_ctx, c_init_param, bool_to_int(x_initialized), x_visibility);
    else
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'clearing the context'); $end
      set_context_rac_aware(x_ctx, c_init_param, null, x_visibility);
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_initialization;

  /**
  * Function returns whether is context initialized or not.
  * @param x_ctx Context name.
  * @return Flag indication whether is context initialized or not.
  */
  function is_initialized(x_ctx in ctx_namespace_type) return boolean is
    $if $$debug $then l_intlogger t_logger.logger%type := 'is_initialized'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_ctx: ' || x_ctx);
    $end

    if sys_context(x_ctx, c_init_param) is not null then
      $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'result (end): true'); $end
      return true;
    end if;

    $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'result (end): false'); $end
    return false;
  end is_initialized;

  /**
  * Procedure initializes a global or session contexts for appenders.
  * Lazy initialization is used.
  * @param x_global Flag, whether global or session contexts should be initialized.
  * {*} c_global_flag Initialize global contexts
  * {*} c_session_flag Initialize session contexts
  */
  procedure init_appenders(x_visibility in visibility_type default c_global_flag) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'init_appenders'; $end
    l_current_appender_ctx ctx_namespace_type;
    l_ctx_suffix           varchar2(2);
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_visibility: ' || x_visibility);
    $end
    $if dbms_db_version.version >= 11 $then pragma inline('is_initialized',  'YES'); $end
    if x_visibility = c_global_flag and is_initialized(c_global_appenders_ctx) then
      $if $$debug $then
        internal_log(logging.c_info_level, l_intlogger, 'Context already initialized');
        internal_log(logging.c_debug_level, l_intlogger, 'end');
      $end
      return;
    end if;

    l_ctx_suffix := case x_visibility when c_global_flag then '_g' else '_l' end;
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_ctx_suffix: ' || l_ctx_suffix); $end

    for l_row in (select a.code, a.base_context_name
                    from t_appender a) loop

      l_current_appender_ctx := l_row.base_context_name || l_ctx_suffix;
      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_current_appender_ctx: ' || l_current_appender_ctx); $end

      -- there is no session context for appenders
      if x_visibility = c_global_flag then
        set_context_rac_aware(c_global_appenders_ctx, l_row.code, l_row.base_context_name, c_global_flag);
      end if;

      for l_row2 in (select aa.app, aa.parameter_name, aa.parameter_value
                       from t_app_appender aa
                      where aa.appender_code = l_row.code) loop
        $if $$debug $then
          internal_log(logging.c_trace_level, l_intlogger, 'l_current_appender_ctx: ' || l_current_appender_ctx);
          internal_log(logging.c_trace_level, l_intlogger, 'app#parameter_name: ' || l_row2.app || '#' || l_row2.parameter_name);
          internal_log(logging.c_trace_level, l_intlogger, 'parameter_value: ' || l_row2.parameter_value);
        $end
        set_context_rac_aware(l_current_appender_ctx,
                              l_row2.app || '#' || l_row2.parameter_name,
                              l_row2.parameter_value,
                              x_visibility);
      end loop;

      $if dbms_db_version.version >= 11 $then pragma inline('set_initialization',  'YES'); $end
      set_initialization(l_current_appender_ctx, true, x_visibility);
    end loop;

    -- there is no session context for appenders
    if x_visibility = c_global_flag then
      $if dbms_db_version.version >= 11 $then pragma inline('set_initialization',  'YES'); $end
      set_initialization(c_global_appenders_ctx, true, c_global_flag);
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end init_appenders;

  /**
  * Procedure initializes a global context for parameters of logging system.
  * Lazy initialization is used.
  * @param x_visibility Flag, whether global or session contexts should be initialized.
  * {*} c_global_flag  Initialize global contexts
  * {*} c_session_flag Initialize session contexts
  * @param x_app Application. Useful for the session context initialization. If set,
  *              only context for given application is initialized.
  */
  procedure init_params(x_visibility in visibility_type default c_global_flag,
                        x_app        in t_param.app%type default null) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'init_params'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_visibility: ' || x_visibility);
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
    $end
    $if dbms_db_version.version >= 11 $then pragma inline('is_initialized',  'YES'); $end
    if is_initialized(c_parameters_ctx(x_visibility)) then
      $if $$debug $then
        internal_log(logging.c_info_level, l_intlogger, 'Context already initialized');
        internal_log(logging.c_debug_level, l_intlogger, 'end');
      $end
      return;
    end if;

    for l_row in (select p.app, p.param_name, p.param_value
                    from t_param p
                   where p.app = x_app or x_app is null) loop

      $if $$debug $then
        internal_log(logging.c_trace_level, l_intlogger, 'c_parameters_ctx(x_visibility): ' || c_parameters_ctx(x_visibility));
        internal_log(logging.c_trace_level, l_intlogger, 'app#param_name: ' || l_row.app || '#' || l_row.param_name);
        internal_log(logging.c_trace_level, l_intlogger, 'param_value: ' || l_row.param_value);
      $end

      set_context_rac_aware(c_parameters_ctx(x_visibility),
                            l_row.app || '#' || l_row.param_name,
                            l_row.param_value,
                            x_visibility);
    end loop;

    $if dbms_db_version.version >= 11 $then pragma inline('set_initialization',  'YES'); $end
    set_initialization(c_parameters_ctx(x_visibility), true, x_visibility);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end init_params;

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
  procedure init_loggers(x_visibility in visibility_type default c_global_flag,
                         x_app        in t_app.app%type default null) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'init_loggers'; $end
    l_hlogger hash_type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_visibility: ' || x_visibility);
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('is_initialized',  'YES'); $end
    if is_initialized(c_logger_names_ctx(x_visibility)) then
      $if $$debug $then
        internal_log(logging.c_info_level, l_intlogger, 'context already initialized');
        internal_log(logging.c_debug_level, l_intlogger, 'end');
      $end
      return;
    end if;

    for l_row in (select l.logger, l.log_level, l.appenders, l.additivity, l.backtrace, l.callstack
                    from t_logger l
                   where l.logger like x_app || '.%' or x_app is null) loop

      $if $$debug $then
        internal_log(logging.c_trace_level, l_intlogger, 'c_logger_names_ctx(x_visibility): ' || c_logger_names_ctx(x_visibility));
        internal_log(logging.c_trace_level, l_intlogger, 'c_logger_levels_ctx(x_visibility): ' || c_logger_levels_ctx(x_visibility));
        internal_log(logging.c_trace_level, l_intlogger, 'c_logger_appenders_ctx(x_visibility): ' || c_logger_appenders_ctx(x_visibility));
        internal_log(logging.c_trace_level, l_intlogger, 'c_additivity_ctx(x_visibility): ' || c_additivity_ctx(x_visibility));
        internal_log(logging.c_trace_level, l_intlogger, 'l_row.logger: ' || l_row.logger);
        internal_log(logging.c_trace_level, l_intlogger, 'l_row.log_level: ' || l_row.log_level);
        internal_log(logging.c_trace_level, l_intlogger, 'l_row.appenders: ' || l_row.appenders);
        internal_log(logging.c_trace_level, l_intlogger, 'l_row.additivity: ' || l_row.additivity);
        internal_log(logging.c_trace_level, l_intlogger, 'l_row.backtrace: ' || l_row.backtrace);
        internal_log(logging.c_trace_level, l_intlogger, 'l_row.callstack: ' || l_row.callstack);
      $end

      $if dbms_db_version.version >= 11 $then pragma inline('hash_logger_name',  'YES'); $end
      l_hlogger := hash_logger_name(l_row.logger);
      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger: ' || l_hlogger); $end

      set_context_rac_aware(c_logger_names_ctx(x_visibility), l_hlogger, l_row.logger, x_visibility);
      set_context_rac_aware(c_logger_levels_ctx(x_visibility), l_hlogger, l_row.log_level, x_visibility);
      set_context_rac_aware(c_logger_appenders_ctx(x_visibility), l_hlogger, l_row.appenders, x_visibility);
      set_context_rac_aware(c_flags_ctx(x_visibility), l_hlogger, encode_flags(l_row.additivity, l_row.backtrace, l_row.callstack), x_visibility);
    end loop;

    $if dbms_db_version.version >= 11 $then pragma inline('set_initialization',  'YES'); $end
    set_initialization(c_logger_names_ctx(x_visibility), true, x_visibility);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end init_loggers;

  /**
  * Procedure initializes g_session_identified.
  */
  procedure init_session_identifier is
    $if $$debug $then l_intlogger t_logger.logger%type := 'init_session_identifier'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'end');
    $end
    g_session_identifier := sys_context('userenv', 'instance') || '#' || sys_context('userenv', 'sessionid');
    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'g_session_identifier: ' || g_session_identifier);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
  end init_session_identifier;

  /**
  * Procedure initializes a global context schema-application mapping.
  * Lazy initialization is used.
  */
  procedure init_user_app is
    $if $$debug $then l_intlogger t_logger.logger%type := 'init_user_app'; $end
  begin
    $if $$debug $then
        internal_log(logging.c_info_level, l_intlogger, 'start');
        internal_log(logging.c_debug_level, l_intlogger, 'end');
    $end
    $if dbms_db_version.version >= 11 $then pragma inline('is_initialized',  'YES'); $end
    if is_initialized(c_global_user_app_ctx) then
      $IF $$debug $THEN internal_log(logging.c_info_level, l_intlogger, 'Context already initialized'); $END
      return;
    end if;

    for l_row in (select ua.schema, ua.app
                    from t_schema_app ua) loop

      $if $$debug $then
        internal_log(logging.c_trace_level, l_intlogger, 'schema: ' || l_row.schema);
        internal_log(logging.c_trace_level, l_intlogger, 'app: ' || l_row.app);
      $end
      set_context_rac_aware(c_global_user_app_ctx, l_row.schema, l_row.app, c_global_flag);
    end loop;

    $if dbms_db_version.version >= 11 $then pragma inline('set_initialization',  'YES'); $end
    set_initialization(c_global_user_app_ctx, true, c_global_flag);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end init_user_app;

  /**
  * Procedure sets flag, which indicates that session setting (session context) will be used for logging.
  * @param x_usage Flag, which indicates that session setting (session context) will be used for logging.
  * {*} TRUE Session settings will be used (local context)
  * {*} FALSE Global settings will be used (global context)
  */
  procedure set_session_ctx_usage(x_usage in boolean) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_session_ctx_usage'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_usage: ' || bool_to_int(x_usage));
      internal_log(logging.c_trace_level, l_intlogger, 'c_parameters_ctx(c_session_flag): ' || c_parameters_ctx(c_session_flag));
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('bool_to_int',  'YES'); $end
    set_context_rac_aware(c_parameters_ctx(c_session_flag),
                          c_session_usage_param,
                          bool_to_int(x_usage),
                          c_session_flag);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_session_ctx_usage;

  /**
  * Procedure sets global layout for given appender and given application.
  * @param x_app Application name.
  * @param x_appender_code Appender code.
  * @param x_layout Layout.
  */
  procedure set_global_layout(x_app      in t_app_appender.app%type,
                              x_appender_code in t_app_appender.appender_code%type,
                              x_layout   in t_app_appender.parameter_value%type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_global_layout'; $end
    pragma autonomous_transaction;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender_code: ' || x_appender_code);
      internal_log(logging.c_debug_level, l_intlogger, 'x_layout: ' || x_layout);
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('set_global_appender_param',  'YES'); $end
    set_global_appender_param(x_app             => x_app,
                              x_appender_code   => x_appender_code,
                              x_parameter_name  => c_layout_param,
                              x_parameter_value => x_layout);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_global_layout;

  /**
  * Procedure sets session layout for given appender and given application.
  * @param x_app Application name.
  * @param x_appender_code Appender code.
  * @param x_layout Layout.
  */
  procedure set_session_layout(x_app           in t_app_appender.app%type,
                               x_appender_code in t_app_appender.appender_code%type,
                               x_layout        in t_app_appender.parameter_value%type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_session_layout'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender_code: ' || x_appender_code);
      internal_log(logging.c_debug_level, l_intlogger, 'x_layout: ' || x_layout);
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('set_session_appender_param','YES'); $end
    set_session_appender_param(x_app             => x_app,
                               x_appender_code   => x_appender_code,
                               x_parameter_name  => c_layout_param,
                               x_parameter_value => x_layout);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_session_layout;

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
                                      x_parameter_value in t_app_appender.parameter_value%type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_global_appender_param'; $end
    pragma autonomous_transaction;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender_code: ' || x_appender_code);
      internal_log(logging.c_debug_level, l_intlogger, 'x_parameter_name: ' || x_parameter_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_parameter_value: ' || x_parameter_value);
    $end


    merge into t_app_appender aa
    using (select null
             from dual) dummy
    on (aa.app = x_app and aa.appender_code = x_appender_code and aa.parameter_name = x_parameter_name)
    when matched then
      update
         set aa.parameter_value = x_parameter_value
    when not matched then
      insert
        (app, appender_code, parameter_name, parameter_value)
      values
        (x_app, x_appender_code, x_parameter_name, x_parameter_value);

    set_context_rac_aware(sys_context(c_global_appenders_ctx, x_appender_code) || '_g',
                          x_app || '#' || x_parameter_name,
                          x_parameter_value,
                          c_global_flag);

    commit;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_global_appender_param;

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
                                       x_parameter_value in t_app_appender.parameter_value%type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_session_appender_param'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender_code: ' || x_appender_code);
      internal_log(logging.c_debug_level, l_intlogger, 'x_parameter_name: ' || x_parameter_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_parameter_value: ' || x_parameter_value);
    $end

    set_context_rac_aware(sys_context(c_global_appenders_ctx, x_appender_code) || '_l',
                          x_app || '#' || x_parameter_name,
                          x_parameter_value,
                          c_session_flag);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_session_appender_param;

  /**
  * Function returns a flag whether session settings are used for logging.
  * @return Flag whether session settings are used for logging.
  */
  function get_session_ctx_usage return boolean is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_session_ctx_usage'; $end
    l_result boolean;
  begin
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'start'); $end
    l_result := case sys_context(c_parameters_ctx(c_session_flag), c_session_usage_param) when '1' then true else false end;
    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || bool_to_int(l_result));
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_result;
  end get_session_ctx_usage;

  /**
  * Function returns global or session layout for given application, appender and visibility.
  * If layout is not set, default layout is returned.
  * @param x_app Application name.
  * @param x_appender_code Appender code.
  * @param x_visibility Global or session visibility
  * @return
  *   {*} Global layout for given application and appender, if visibility is c_global_flag
  *   {*} Session layout for given application and appender, if visibility is c_session_flag
  */
  function get_layout(x_app           in t_app_appender.app%type,
                      x_appender_code in t_app_appender.appender_code%type,
                      x_visibility    in visibility_type default c_global_flag)
    return t_app_appender.parameter_value%type is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_layout'; $end
    l_result t_app_appender.parameter_value%type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender_code: ' || x_appender_code);
      internal_log(logging.c_debug_level, l_intlogger, 'x_visibility: ' || x_visibility);
    $end
    l_result := coalesce(sys_context(sys_context(c_global_appenders_ctx, x_appender_code) || c_append_vis_suffix(x_visibility), x_app || '#layout'),
                         sys_context(c_parameters_ctx(c_global_flag), c_default_layout_param));
    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_result;
  end get_layout;

  /**
  * Function returns global or session parameter value for given application, appender, parameter and visibility.
  * @param x_app Application name.
  * @param x_appender_code Appender code.
  * @param x_parameter_name Parameter name.
  * @param x_visibility Global or session visibility
  * @return Global or session parameter value for given application, appender and parameter.
  *   {*} Global parameter value for given application, appender and parameter, if visibility is c_global_flag
  *   {*} Session parameter value for given application, appender and parameter, if visibility is c_session_flag
  */
  function get_appender_param(x_app            in t_app_appender.app%type,
                              x_appender_code  in t_app_appender.appender_code%type,
                              x_parameter_name in t_app_appender.parameter_name%type,
                              x_visibility     in visibility_type default c_global_flag)
    return t_app_appender.parameter_value%type is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_appender_param'; $end
    l_result t_app_appender.parameter_value%type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender_code: ' || x_appender_code);
      internal_log(logging.c_debug_level, l_intlogger, 'x_parameter_name: ' || x_parameter_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_visibility: ' || x_visibility);
    $end
    l_result := sys_context(sys_context(c_global_appenders_ctx, x_appender_code) || c_append_vis_suffix(x_visibility),
                            x_app || '#' || x_parameter_name);
    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_result;
  end get_appender_param;

  /**
  * Function returns current (global or session based on get_session_ctx_usage) parameter
  * value for given application, appender and parameter.
  * @param x_app Application name.
  * @param x_appender_code Appender code.
  * @param x_parameter_name Parameter name.
  * @return Current parameter value for given application, appender and parameter.
  */
  function get_current_appender_param(x_app            in t_app_appender.app%type,
                                      x_appender_code  in t_app_appender.appender_code%type,
                                      x_parameter_name in t_app_appender.parameter_name%type)
    return t_app_appender.parameter_value%type is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_current_appender_param'; $end
    l_result t_app_appender.parameter_value%type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender_code: ' || x_appender_code);
      internal_log(logging.c_debug_level, l_intlogger, 'x_parameter_name: ' || x_parameter_name);
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_session_ctx_usage',  'YES'); $end
    if get_session_ctx_usage() then
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'session context'); $end
      l_result := get_appender_param(x_app, x_appender_code, x_parameter_name, c_session_flag);
    else
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'global context'); $end
      l_result := get_appender_param(x_app, x_appender_code, x_parameter_name, c_global_flag);
    end if;

    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_result;
  end get_current_appender_param;

  /**
  * Function returns current (global or session based on get_session_ctx_usage) layout
  * for given application and appender.
  * @param x_app Application name.
  * @param x_appender_code Appender code.
  * @return Current layout for given application and appender.
  */
  function get_current_layout(x_app           in t_app_appender.app%type,
                              x_appender_code in t_app_appender.appender_code%type)
    return t_app_appender.parameter_value%type is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_current_layout'; $end
    l_result t_app_appender.parameter_value%type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender_code: ' || x_appender_code);
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_session_ctx_usage',  'YES'); $end
    if get_session_ctx_usage() then
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'session context'); $end
      $if dbms_db_version.version >= 11 $then pragma inline('get_layout',  'YES'); $end
      l_result := get_layout(x_app, x_appender_code, c_session_flag);
    else
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'global context'); $end
      $if dbms_db_version.version >= 11 $then pragma inline('get_layout',  'YES'); $end
      l_result := get_layout(x_app, x_appender_code, c_global_flag);
    end if;

    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_result;
  end get_current_layout;

  /**
  * Function returns a nth descendant from given logger.
  * @param x_logger_name Loger name (Named hierarchy).
  * @param x_nth Level of descendant we want to obtain.
  * @return Name of descendant.
  */
  function get_nth_logger_name(x_logger_name in t_logger.logger%type,
                               x_nth         in pls_integer) return t_logger.logger%type is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_nth_logger_name'; $end
    l_logger_name t_logger.logger%type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_nth: ' || x_nth);
    $end

    l_logger_name := substr(x_logger_name, 1, instr(x_logger_name || c_separator, c_separator, -1, x_nth) - 1);

    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_logger_name: ' || l_logger_name);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_logger_name;
  end get_nth_logger_name;

  /**
  * Function returns log level for given logger name and context.
  * @param x_logger_name Logger name.
  * @param x_ctx_name Context name.
  * @return Log level assigned for given logger on given context.
  */
  function get_level(x_logger_name in t_logger.logger%type,
                     x_ctx_name    in varchar2) return t_logger.log_level%type is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_level'; $end
    i             pls_integer;
    l_logger_name t_logger.logger%type;
    l_level       t_logger.log_level%type;
    l_hlogger     hash_type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_ctx_name: ' || x_ctx_name);
    $end

    i := 1;
    loop
      $if dbms_db_version.version >= 11 $then pragma inline('get_nth_logger_name',  'YES'); $end
      l_logger_name := get_nth_logger_name(x_logger_name, i);
      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_logger_name: ' || l_logger_name); $end

      exit when l_logger_name is null;

      $if dbms_db_version.version >= 11 $then pragma inline('hash_logger_name',  'YES'); $end
      l_hlogger := hash_logger_name(l_logger_name);
      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger: ' || l_hlogger); $end

      l_level := sys_context(x_ctx_name, l_hlogger);
      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_level: ' || l_level); $end

      if l_level is not null then
        $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'exiting the cycle'); $end
        exit;
      end if;

      i := i + 1;
    end loop;

    l_level := coalesce(l_level, sys_context(x_ctx_name, g_root_logger_hash));
    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, l_level);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_level;
  end get_level;

  /**
  * Function returns binary encoded list of appenders for given logger.
  * @param x_logger_name Logger name.
  * @param x_app_ctx_name Name of a context containing appenders.
  * @param x_flags_name Name of a context containing flags.
  * @return Binary encoded list of appenders for given logger.
  */
  function get_appenders(x_logger_name  in t_logger.logger%type,
                         x_app_ctx_name in ctx_namespace_type,
                         x_flags_ctx_name in ctx_namespace_type) return t_logger.appenders%type is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_appenders'; $end
    i             pls_integer;
    l_logger_name t_logger.logger%type;
    l_hlogger     hash_type;
    l_appenders   t_logger.appenders%type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_app_ctx_name: ' || x_app_ctx_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_flags_ctx_name: ' || x_flags_ctx_name);
    $end

    i           := 1;
    l_appenders := 0;
    loop
      $if dbms_db_version.version >= 11 $then pragma inline('get_nth_logger_name',  'YES'); $end
      l_logger_name := get_nth_logger_name(x_logger_name, i);
      exit when l_logger_name is null;

      $if dbms_db_version.version >= 11 $then pragma inline('hash_logger_name',  'YES'); $end
      l_hlogger   := hash_logger_name(l_logger_name);
      $if dbms_db_version.version >= 11 $then pragma inline('bit_or',  'YES'); $end
      l_appenders := bit_or(l_appenders, nvl(sys_context(x_app_ctx_name, l_hlogger), 0));

      if bitand(sys_context(x_flags_ctx_name, l_hlogger), c_flag_add) = 0 then
        $if $$debug $then
          internal_log(logging.c_info_level, l_intlogger, 'stopping because of the additivity');
          internal_log(logging.c_debug_level, l_intlogger, 'l_appenders: ' || l_appenders);
        $end
        return l_appenders;
      end if;

      i := i + 1;
    end loop;

    l_hlogger := g_root_logger_hash;
    $if dbms_db_version.version >= 11 $then pragma inline('bit_or',  'YES'); $end
    l_appenders := bit_or(l_appenders, nvl(sys_context(x_app_ctx_name, l_hlogger), 0));
    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_appenders: ' || l_appenders);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_appenders;
  end get_appenders;

  /**
  * Function returns binary encoded list of currently used appenders (based on get_session_ctx_usage) for given logger.
  * @param x_logger_name Logger name.
  * @return Binary encoded list of currently used appenders for given logger.
  */
  function get_current_used_appenders(x_logger_name in t_logger.logger%type) return t_logger.appenders%type is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_current_used_appenders'; $end
    l_result t_logger.appenders%type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_session_ctx_usage',  'YES'); $end
    if get_session_ctx_usage() then
      l_result := get_appenders(x_logger_name,
                                c_logger_appenders_ctx(c_session_flag),
                                c_flags_ctx(c_session_flag));
    else
      l_result := get_appenders(x_logger_name,
                               c_logger_appenders_ctx(c_global_flag),
                               c_flags_ctx(c_global_flag));
    end if;

    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_result;
  end get_current_used_appenders;

  /**
  * Function returns binary encoded flags for given logger.
  * @param x_logger_name Logger name.
  * @return Binary encoded flags.
  */
  function get_current_used_flags(x_logger_name in t_logger.logger%type) return pls_integer is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_current_used_flags'; $end
    l_result pls_integer;
    l_hlogger hash_type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('hash_logger_name',  'YES'); $end
    l_hlogger := hash_logger_name(x_logger_name);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger: ' || l_hlogger); $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_session_ctx_usage',  'YES'); $end
    if get_session_ctx_usage() then
      l_result := sys_context(c_flags_ctx(c_session_flag), l_hlogger);
    else
      l_result := sys_context(c_flags_ctx(c_global_flag), l_hlogger);
    end if;

    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_result;
  end get_current_used_flags;

  /**
  * Function returns currently used (based on get_session_ctx_usage) log level for given logger.
  * @param x_logger_name Logger name.
  * @return Currently used log level for given logger.
  */
  function get_current_used_level(x_logger_name in t_logger.logger%type) return t_logger.log_level%type is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_current_used_level'; $end
    l_result t_logger.log_level%type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
    $end
    $if dbms_db_version.version >= 11 $then pragma inline('get_session_ctx_usage',  'YES'); $end
    if get_session_ctx_usage() then
      l_result := get_level(x_logger_name, c_logger_levels_ctx(c_session_flag));
    else
      l_result := get_level(x_logger_name, c_logger_levels_ctx(c_global_flag));
    end if;

    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_result;
  end get_current_used_level;

  /**
  * Procedure adds given global appenders to given logger and sets global additivity flag for the logger.
  * @param x_logger Logger name.
  * @param x_appender_code Appender code.
  * @param x_additivity Additivity flag.
  */
  procedure add_global_appender(x_logger_name   in t_logger.logger%type,
                                x_appender_code in t_appender.code%type,
                                x_additivity    in boolean default true) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'add_global_appender'; $end
    pragma autonomous_transaction;
    l_app        t_schema_app.app%type;
    l_dummy      varchar2(1);
    l_appenders  t_logger.appenders%type;
    l_hlogger    hash_type;
    l_additivity t_logger.additivity%type;
    l_callstack  t_logger.callstack%type;
    l_backtrace  t_logger.backtrace%type;
    l_flags      pls_integer;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender_code: ' || x_appender_code);
      internal_log(logging.c_debug_level, l_intlogger, 'x_additivity: ' || bool_to_int(x_additivity));
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_app',  'YES'); $end
    l_app := get_app(c_user);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_app: ' || l_app); $end

    if l_app is null and c_user not in ('SYS', c_schema_name) then
      $if $$debug $then internal_log(logging.c_error_level, l_intlogger, 'no privileges'); $end
      raise_application_error(c_insufficient_privs_code, bind_params(c_insufficient_privs_msg, exception_params_type('set appender for ' || x_logger_name)));
    end if;

    begin
      select null
        into l_dummy
        from t_appender a
       where a.code = x_appender_code;
    exception
      when no_data_found then
        $if $$debug $then internal_log(logging.c_error_level, l_intlogger, 'no such appender'); $end
        raise_application_error(c_no_such_appender_code, bind_params(c_no_such_appender_msg, exception_params_type(x_appender_code)));
    end;

    $if dbms_db_version.version >= 11 $then pragma inline('bool_to_int',  'YES'); $end
    l_additivity := bool_to_int(x_additivity);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_additivity: ' || l_additivity); $end

    update t_logger l
       set l.appenders = bit_or(l.appenders, x_appender_code), l.additivity = l_additivity
     where l.logger = x_logger_name
    returning appenders, backtrace, callstack into l_appenders, l_backtrace, l_callstack;

    if sql%notfound then
      l_appenders := x_appender_code;
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'inserting a new row'); $end
      insert into t_logger
        (logger, appenders, additivity, backtrace, callstack)
      values
        (x_logger_name, l_appenders, l_additivity, default, default)
      returning backtrace, callstack into l_backtrace, l_callstack;
    end if;

    $if dbms_db_version.version >= 11 $then pragma inline('hash_logger_name',  'YES'); $end
    l_hlogger := hash_logger_name(x_logger_name);

    $if $$debug $then
      internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger: ' || l_hlogger);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'l_appenders: ' || l_appenders);
      internal_log(logging.c_trace_level, l_intlogger, 'l_additivity: ' || l_additivity);
      internal_log(logging.c_trace_level, l_intlogger, 'l_backtrace: ' || l_backtrace);
      internal_log(logging.c_trace_level, l_intlogger, 'l_callstack: ' || l_callstack);
    $end
    
    l_flags := encode_flags(l_additivity, l_backtrace, l_callstack);
    
    set_context_rac_aware(c_logger_names_ctx(c_global_flag), l_hlogger, x_logger_name, c_global_flag);
    set_context_rac_aware(c_logger_appenders_ctx(c_global_flag), l_hlogger, l_appenders, c_global_flag);
    set_context_rac_aware(c_flags_ctx(c_global_flag), l_hlogger, l_flags, c_global_flag);

    commit;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end add_global_appender;

  /**
  * Procedure sets global additivity flag for given logger.
  * @param x_logger_name Loger name.
  * @param x_additivity Additivity flag.
  */
  procedure set_global_additivity(x_logger_name in t_logger.logger%type,
                                  x_additivity  in boolean) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_global_additivity'; $end
    pragma autonomous_transaction;
    l_app        t_schema_app.app%type;
    l_appenders  t_logger.appenders%type;
    l_dummy      varchar2(1);
    l_hlogger    hash_type;
    l_additivity t_logger.additivity%type;
    l_callstack  t_logger.callstack%type;
    l_backtrace  t_logger.backtrace%type;
    l_flags      pls_integer;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_additivity: ' || bool_to_int(x_additivity));
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_app',  'YES'); $end
    l_app := get_app(c_user);

    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_app: ' || l_app); $end

    begin
      select null
        into l_dummy
        from t_schema_app ua
       where ua.schema = c_user
         and ua.app = l_app;
    exception
      when no_data_found then
        $if $$debug $then internal_log(logging.c_error_level, l_intlogger, 'no such application'); $end
        raise_application_error(c_no_such_app_code, bind_params(c_no_such_app_msg, exception_params_type(l_app)));
    end;


    $if dbms_db_version.version >= 11 $then pragma inline('bool_to_int',  'YES'); $end
    l_additivity := bool_to_int(x_additivity);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_additivity: ' || l_additivity); $end

    update t_logger l
       set l.additivity = l_additivity
     where l.logger = x_logger_name
    returning appenders, backtrace, callstack into l_appenders, l_backtrace, l_callstack;

    if sql%notfound then
      l_appenders := 0;
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'creating a new row'); $end
      insert into t_logger
        (logger, appenders, additivity, backtrace, callstack)
      values
        (x_logger_name, l_appenders, l_additivity, default, default)
      returning backtrace, callstack into l_backtrace, l_callstack;
    end if;

    $if $$debug $then
      internal_log(logging.c_trace_level, l_intlogger, 'l_appenders: ' || l_appenders);
      internal_log(logging.c_trace_level, l_intlogger, 'l_backtrace: ' || l_backtrace);
      internal_log(logging.c_trace_level, l_intlogger, 'l_callstack: ' || l_callstack);
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('hash_logger_name',  'YES'); $end
    l_hlogger := hash_logger_name(x_logger_name);

    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_hlogger: ' || l_hlogger);
      internal_log(logging.c_debug_level, l_intlogger, 'l_additivity: ' || l_appenders);
    $end
    
    l_flags := encode_flags(l_additivity, l_backtrace, l_callstack);

    set_context_rac_aware(c_logger_names_ctx(c_global_flag), l_hlogger, x_logger_name, c_global_flag);
    set_context_rac_aware(c_logger_appenders_ctx(c_global_flag), l_hlogger, l_appenders, c_global_flag);
    set_context_rac_aware(c_flags_ctx(c_global_flag), l_hlogger, l_flags, c_global_flag);

    commit;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_global_additivity;

  /**
  * Procedure removes givne global appenders from given logger.
  * @param x_logger_name Logger name.
  * @param x_appender_code Appender code.
  */
  procedure remove_global_appender(x_logger_name   in t_logger.logger%type,
                                   x_appender_code in t_appender.code%type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'remove_global_appender'; $end
    pragma autonomous_transaction;
    l_app       t_schema_app.app%type;
    l_dummy     varchar2(1);
    l_appenders t_logger.appenders%type;
    l_hlogger   hash_type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender_code: ' || x_appender_code);
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_app',  'YES'); $end
    l_app := get_app(c_user);

    if l_app is null and c_user not in ('SYS', c_schema_name) then
      $if $$debug $then internal_log(logging.c_error_level, l_intlogger, 'insufficient privileges for removing appender'); $end
      raise_application_error(c_insufficient_privs_code, bind_params(c_insufficient_privs_msg, exception_params_type('remove appender for ' || x_logger_name)));
    end if;

    begin
      select null
        into l_dummy
        from t_appender a
       where a.code = x_appender_code;
    exception
      when no_data_found then
        $if $$debug $then internal_log(logging.c_error_level, l_intlogger, 'appender not found'); $end
        raise_application_error(c_no_such_appender_code, bind_params(c_no_such_appender_msg, exception_params_type(x_appender_code)));
    end;

    -- unset appenders
    update t_logger l
       set l.appenders = bit_xor(l.appenders, x_appender_code)
     where l.logger = x_logger_name
       and bitand(l.appenders, x_appender_code) > 0
    returning appenders into l_appenders;

    -- an appender with given code was set for given logger
    if sql%found then
      $if dbms_db_version.version >= 11 $then pragma inline('hash_logger_name',  'YES'); $end
      l_hlogger := hash_logger_name(x_logger_name);

      $if $$debug $then
        internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger:' || l_hlogger);
        internal_log(logging.c_trace_level, l_intlogger, 'l_appenders:' || l_appenders);
      $end
      set_context_rac_aware(c_logger_names_ctx(c_global_flag), l_hlogger, x_logger_name, c_global_flag);
      set_context_rac_aware(c_logger_appenders_ctx(c_global_flag), l_hlogger, l_appenders, c_global_flag);
    end if;

    commit;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end remove_global_appender;

  /**
  * Procedure adds given session (session) appenders to given logger and sets session additivity flag for the logger.
  * @param x_logger_name Logger name.
  * @param x_appender_code Appender code.
  * @param x_additivity Additivity flag.
  */
  procedure add_session_appender(x_logger_name   in t_logger.logger%type,
                                 x_appender_code in t_appender.code%type,
                                 x_additivity    in boolean default true) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'add_session_appender'; $end
    l_appenders t_logger.appenders%type;
    l_flags     pls_integer;
    l_dummy     varchar2(1);
    l_hlogger   hash_type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender_code: ' || x_appender_code);
      internal_log(logging.c_debug_level, l_intlogger, 'x_additivity: ' || bool_to_int(x_additivity));
    $end

    begin
      select null
        into l_dummy
        from t_appender a
       where a.code = x_appender_code;
    exception
      when no_data_found then
        $if $$debug $then internal_log(logging.c_error_level, l_intlogger, 'no such appender'); $end
        raise_application_error(c_no_such_appender_code, bind_params(c_no_such_appender_msg, exception_params_type(x_appender_code)));
    end;

    $if dbms_db_version.version >= 11 $then pragma inline('hash_logger_name',  'YES'); $end
    l_hlogger := hash_logger_name(x_logger_name);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger: ' || l_hlogger); $end

    l_appenders := nvl(sys_context(c_logger_appenders_ctx(c_session_flag), l_hlogger), 0);
    l_flags := nvl(sys_context(c_flags_ctx(c_session_flag), l_hlogger), 0);
    $if $$debug $then 
      internal_log(logging.c_trace_level, l_intlogger, 'l_appenders: ' || l_appenders);
      internal_log(logging.c_trace_level, l_intlogger, 'l_flags: ' || l_flags);
    $end
    $if dbms_db_version.version >= 11 $then pragma inline('bit_or',  'YES'); $end
    l_appenders := bit_or(l_appenders, x_appender_code);
    l_flags := bit_or(l_flags, bool_to_int(x_additivity) * c_flag_add);
    $if $$debug $then 
      internal_log(logging.c_trace_level, l_intlogger, 'l_appenders: ' || l_appenders); 
      internal_log(logging.c_trace_level, l_intlogger, 'l_flags: ' || l_flags);
    $end
    set_context_rac_aware(c_logger_names_ctx(c_session_flag), l_hlogger, x_logger_name, c_session_flag);
    set_context_rac_aware(c_logger_appenders_ctx(c_session_flag), l_hlogger, l_appenders, c_session_flag);
    $if dbms_db_version.version >= 11 $then pragma inline('bool_to_int',  'YES'); $end
    set_context_rac_aware(c_flags_ctx(c_session_flag), l_hlogger, l_flags, c_session_flag);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end add_session_appender;

  /**
  * Procedure sets session additivity flag for given logger.
  * @param x_logger_name Loger name.
  * @param x_additivity Additivity flag.
  */
  procedure set_session_additivity(x_logger_name in t_logger.logger%type,
                                   x_additivity  in boolean) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_session_additivity'; $end
    l_appenders t_logger.appenders%type;
    l_flags     pls_integer;
    l_hlogger   hash_type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_additivity: ' || bool_to_int(x_additivity));
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('hash_logger_name',  'YES'); $end
    l_hlogger   := hash_logger_name(x_logger_name);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger: ' || l_hlogger); $end
    l_appenders := nvl(sys_context(c_logger_appenders_ctx(c_session_flag), l_hlogger), 0);
    l_flags := nvl(sys_context(c_flags_ctx(c_session_flag), l_hlogger), 0);
    $if $$debug $then 
      internal_log(logging.c_trace_level, l_intlogger, 'l_appenders: ' || l_appenders);
      internal_log(logging.c_trace_level, l_intlogger, 'l_flags: ' || l_flags);
    $end
    l_flags := bit_or(l_flags, bool_to_int(x_additivity) * c_flag_add);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_flags: ' || l_flags); $end
    
    set_context_rac_aware(c_logger_names_ctx(c_session_flag), l_hlogger, x_logger_name, c_session_flag);
    set_context_rac_aware(c_logger_appenders_ctx(c_session_flag), l_hlogger, l_appenders, c_session_flag);
    $if dbms_db_version.version >= 11 $then pragma inline('bool_to_int',  'YES'); $end
    set_context_rac_aware(c_flags_ctx(c_session_flag), l_hlogger, l_flags, c_session_flag);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_session_additivity;

  /**
  * Procedure sets global parameter value for given app and parameter name.
  * @param x_app Application.
  * @param x_param_name Parameter name.
  * @param x_param_value Parameter value.
  */
  procedure set_global_parameter(x_app         in t_param.app%type,
                                 x_param_name  in t_param.param_name%type,
                                 x_param_value in t_param.param_value%type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_global_parameter'; $end
    pragma autonomous_transaction;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_param_name: ' || x_param_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_param_value: ' || x_param_value);
    $end

    merge into t_param p
    using (select null
             from dual) dummy
    on (p.app = x_app and p.param_name = x_param_name)
    when matched then
      update
         set p.param_value = x_param_value
    when not matched then
      insert
        (app, param_name, param_value)
      values
        (x_app, x_param_name, x_param_value);

    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'merge ok, merged: ' || sql%rowcount || ' rows'); $end
    set_context_rac_aware(c_parameters_ctx(c_global_flag),
                          x_app || '#' || x_param_name,
                          x_param_value,
                          c_global_flag);

    commit;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_global_parameter;

  /**
  * Procedure sets session parameter value for given app and parameter name.
  * @param x_app Application.
  * @param x_param_name Parameter name.
  * @param x_param_value Parameter value.
  */
  procedure set_session_parameter(x_app         in t_param.app%type,
                                  x_param_name  in t_param.param_name%type,
                                  x_param_value in t_param.param_value%type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_session_parameter'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_param_name: ' || x_param_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_param_value: ' || x_param_value);
    $end
    set_context_rac_aware(c_parameters_ctx(c_session_flag), x_app || '#' || x_param_name, x_param_value, c_session_flag);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_session_parameter;

  /**
  * Funtion creates a handle all serialized settings.
  */
  function get_serialized_setting_handle return pls_integer is 
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_serialized_setting_handle'; $end
  begin
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'start'); $end

    g_serialized_settings.extend;
    
    $if $$debug $then 
      internal_log(logging.c_debug_level, l_intlogger, 'handle: ' || g_serialized_settings.last);
      internal_log(logging.c_info_level, l_intlogger, 'end'); 
    $end
    return g_serialized_settings.last;
  end get_serialized_setting_handle;

  /**
  * Procedure clears all serialized settings.
  * @param x_setting_handle A handle for settings.
  */
  procedure clear_serialized_settings(x_setting_handle in pls_integer default 1) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'clear_serialized_settings'; $end
  begin
    $if $$debug $then 
      internal_log(logging.c_info_level, l_intlogger, 'start'); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_setting_handle: ' || x_setting_handle);
    $end

    if not g_serialized_settings.exists(x_setting_handle) then 
      raise_application_error(c_no_such_handle_code, bind_params(c_no_such_handle_msg, exception_params_type(x_setting_handle)));
    end if;
    
    g_serialized_settings.delete(x_setting_handle);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end clear_serialized_settings;

  /**
  * Procedure adds given appender to given logger and sets additivity flag for the logger in serialized settings.
  * @param x_logger_name Logger name.
  * @param x_appender_code Appender code.
  * @param x_additivity Additivity flag.
  * @param x_setting_handle A handle for settings. A handle represents a set of parameters.
  */
  procedure add_serialized_appender(x_logger_name    in t_logger.logger%type,
                                    x_appender_code  in t_appender.code%type,
                                    x_additivity     in boolean default true,
                                    x_setting_handle in pls_integer default 1) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'add_serialized_appender'; $end
    l_appenders t_logger.appenders%type;
    l_dummy     varchar2(1);
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender_code: ' || x_appender_code);
      internal_log(logging.c_debug_level, l_intlogger, 'x_additivity: ' || bool_to_int(x_additivity));
      internal_log(logging.c_debug_level, l_intlogger, 'x_setting_handle: ' || x_setting_handle);
    $end

    if not g_serialized_settings.exists(x_setting_handle) then 
      raise_application_error(c_no_such_handle_code, bind_params(c_no_such_handle_msg, exception_params_type(x_setting_handle)));
    end if;

    begin
      select null
        into l_dummy
        from t_appender a
       where a.code = x_appender_code;
    exception
      when no_data_found then
        $if $$debug $then internal_log(logging.c_error_level, l_intlogger, 'no such appender'); $end
        raise_application_error(c_no_such_appender_code, bind_params(c_no_such_appender_msg, exception_params_type(x_appender_code)));
    end;

    -- add appender
    l_appenders := coalesce(g_serialized_settings(x_setting_handle).loggers(x_logger_name).enabled_appenders, 0);
    l_appenders := bit_or(l_appenders, x_appender_code);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_appenders: ' || l_appenders); $end
    g_serialized_settings(x_setting_handle).loggers(x_logger_name).enabled_appenders := l_appenders;
    g_serialized_settings(x_setting_handle).loggers(x_logger_name).additivity := bool_to_int(x_additivity);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end add_serialized_appender;

  /**
  * Procedure removes given appender from given logger in serialized settings.
  * @param x_logger_name Logger name.
  * @param x_appender_code Appender code.
  * @param x_setting_handle A handle for settings. A handle represents a set of parameters.
  */
  procedure remove_serialized_appender(x_logger_name   in t_logger.logger%type,
                                       x_appender_code in t_appender.code%type,
                                       x_setting_handle in pls_integer default 1) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'remove_serialized_appender'; $end
    l_appenders t_logger.appenders%type;
    l_dummy     varchar2(1);
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender_code: ' || x_appender_code);
      internal_log(logging.c_debug_level, l_intlogger, 'x_setting_handle: ' || x_setting_handle);
    $end

    if not g_serialized_settings.exists(x_setting_handle) then 
      raise_application_error(c_no_such_handle_code, bind_params(c_no_such_handle_msg, exception_params_type(x_setting_handle)));
    end if;

    begin
      select null
        into l_dummy
        from t_appender a
       where a.code = x_appender_code;
    exception
      when no_data_found then
        $if $$debug $then internal_log(logging.c_error_level, l_intlogger, 'no such appender'); $end
        raise_application_error(c_no_such_appender_code, bind_params(c_no_such_appender_msg, exception_params_type(x_appender_code)));
    end;

    l_appenders := coalesce(g_serialized_settings(x_setting_handle).loggers(x_logger_name).enabled_appenders, 0);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_appenders' || l_appenders); $end

    if bitand(l_appenders, x_appender_code) > 0 then
      l_appenders := bit_xor(l_appenders, x_appender_code);
      $if $$debug $then
        internal_log(logging.c_info_level, l_intlogger, 'removing appender.');
        internal_log(logging.c_debug_level, l_intlogger, 'l_appenders' || l_appenders);
      $end
      g_serialized_settings(x_setting_handle).loggers(x_logger_name).enabled_appenders := l_appenders;
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end remove_serialized_appender;


  /**
  * Procedure sets additivity flag for given logger in serialized settings.
  * @param x_logger_name Loger name.
  * @param x_additivity Additivity flag.
  * @param x_setting_handle A handle for settings. A handle represents a set of parameters.
  */
  procedure set_serialized_additivity(x_logger_name in t_logger.logger%type,
                                      x_additivity  in boolean,
                                      x_setting_handle in pls_integer default 1) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_serialized_additivity'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_additivity: ' || bool_to_int(x_additivity));
      internal_log(logging.c_debug_level, l_intlogger, 'x_setting_handle: ' || x_setting_handle);
    $end

    if not g_serialized_settings.exists(x_setting_handle) then 
      raise_application_error(c_no_such_handle_code, bind_params(c_no_such_handle_msg, exception_params_type(x_setting_handle)));
    end if;

    g_serialized_settings(x_setting_handle).loggers(x_logger_name).additivity := bool_to_int(x_additivity);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_serialized_additivity;
  
  /**
  * Procedure sets flags for given logger in serialized settings.
  * @param x_logger Logger name.
  * @param x_additivity Additivity flag.
  * @param x_backtrace Flag whether backtrace will be logged.
  * @param x_callstack Flag whether callstack will be logged.    
  * @param x_setting_handle A handle for settings. A handle represents a set of parameters.
  */
  procedure set_serialized_flags(x_logger_name    in t_logger.logger%type,
                                 x_additivity     in boolean default null,
                                 x_backtrace      in boolean default null,
                                 x_callstack      in boolean default null,
                                 x_setting_handle in pls_integer default 1) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_serialized_flags'; $end
    pragma autonomous_transaction;
    l_app        t_schema_app.app%type;
    l_dummy      varchar2(1);
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_additivity: ' || bool_to_int(x_additivity));
      internal_log(logging.c_debug_level, l_intlogger, 'x_backtrace: ' || bool_to_int(x_backtrace));
      internal_log(logging.c_debug_level, l_intlogger, 'x_callstack: ' || bool_to_int(x_callstack));
      internal_log(logging.c_debug_level, l_intlogger, 'x_setting_handle: ' || x_setting_handle);
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_app',  'YES'); $end
    l_app := get_app(c_user);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_app: ' || l_app); $end

    select null
      into l_dummy
      from t_schema_app ua
     where ua.schema = c_user
       and ua.app = l_app;
    
    $if dbms_db_version.version >= 11 $then pragma inline('hash_logger_name',  'YES'); $end
    g_serialized_settings(x_setting_handle).loggers(x_logger_name).additivity := bool_to_int(x_additivity);
    g_serialized_settings(x_setting_handle).loggers(x_logger_name).backtrace := bool_to_int(x_backtrace);
    g_serialized_settings(x_setting_handle).loggers(x_logger_name).callstack := bool_to_int(x_callstack);

    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_serialized_flags;  

  /**
  * Procedure sets session value for given parameter name, appender and application.
  * @param x_app Application name.
  * @param x_appender_code_code Appender code.
  * @param x_parameter_name Parameter name.
  * @param x_parameter_value Parameter value.
  * @param x_setting_handle A handle for settings. A handle represents a set of parameters.
  */
  procedure set_serialized_appender_param(x_app             in t_app_appender.app%type,
                                          x_appender_code   in t_app_appender.appender_code%type,
                                          x_parameter_name  in t_app_appender.parameter_name%type,
                                          x_parameter_value in t_app_appender.parameter_value%type,
                                          x_setting_handle  in pls_integer default 1) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_serialized_appender_param'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender_code: ' || x_appender_code);
      internal_log(logging.c_debug_level, l_intlogger, 'x_parameter_name: ' || x_parameter_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_parameter_value: ' || x_parameter_value);
      internal_log(logging.c_debug_level, l_intlogger, 'x_setting_handle: ' || x_setting_handle);
    $end

    if not g_serialized_settings.exists(x_setting_handle) then 
      raise_application_error(c_no_such_handle_code, bind_params(c_no_such_handle_msg, exception_params_type(x_setting_handle)));
    end if;

    g_serialized_settings(x_setting_handle).app_settings(x_app).appenders_params(x_appender_code)(x_parameter_name) := x_parameter_value;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_serialized_appender_param;


  /**
  * Procedure sets session layout for given appender and given application.
  * @param x_app Application name.
  * @param x_appender_code Appender code.
  * @param x_layout Layout.
  * @param x_setting_handle A handle for settings. A handle represents a set of parameters.
  */
  procedure set_serialized_layout(x_app            in t_app_appender.app%type,
                                  x_appender_code  in t_app_appender.appender_code%type,
                                  x_layout         in t_app_appender.parameter_value%type,
                                  x_setting_handle in pls_integer default 1) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_serialized_layout'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender_code: ' || x_appender_code);
      internal_log(logging.c_debug_level, l_intlogger, 'x_layout: ' || x_layout);
      internal_log(logging.c_debug_level, l_intlogger, 'x_setting_handle: ' || x_setting_handle);
    $end

    if not g_serialized_settings.exists(x_setting_handle) then 
      raise_application_error(c_no_such_handle_code, bind_params(c_no_such_handle_msg, exception_params_type(x_setting_handle)));
    end if;

    $if dbms_db_version.version >= 11 $then pragma inline('set_serialized_appender_param','YES'); $end
    set_serialized_appender_param(x_app             => x_app,
                                  x_appender_code   => x_appender_code,
                                  x_parameter_name  => c_layout_param,
                                  x_parameter_value => x_layout,
                                  x_setting_handle  => x_setting_handle);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_serialized_layout;

  /**
  * Procedure sets session log level for given logger.
  * @param x_logger Logger name.
  * @param x_log_level Log level.
  * @param x_setting_handle A handle for settings. A handle represents a set of parameters.
  */
  procedure set_serialized_level(x_logger_name in t_logger.logger%type,
                                 x_log_level   in t_logger.log_level%type,
                                 x_setting_handle in pls_integer default 1) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_serialized_level'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_log_level: ' || x_log_level);
      internal_log(logging.c_debug_level, l_intlogger, 'x_setting_handle: ' || x_setting_handle);
    $end

    if not g_serialized_settings.exists(x_setting_handle) then 
      raise_application_error(c_no_such_handle_code, bind_params(c_no_such_handle_msg, exception_params_type(x_setting_handle)));
    end if;

    g_serialized_settings(x_setting_handle).loggers(x_logger_name).log_level := x_log_level;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_serialized_level;

  /**
  * Procedure sets session parameter value for given app and parameter name.
  * @param x_app Application.
  * @param x_param_name Parameter name.
  * @param x_param_value Parameter value.
  * @param x_setting_handle A handle for settings. A handle represents a set of parameters.
  */
  procedure set_serialized_parameter(x_app         in t_param.app%type,
                                     x_param_name  in t_param.param_name%type,
                                     x_param_value in t_param.param_value%type,
                                     x_setting_handle in pls_integer default 1) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_serialized_parameter'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_param_name: ' || x_param_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_param_value: ' || x_param_value);
      internal_log(logging.c_debug_level, l_intlogger, 'x_setting_handle: ' || x_setting_handle);
    $end

    if not g_serialized_settings.exists(x_setting_handle) then 
      raise_application_error(c_no_such_handle_code, bind_params(c_no_such_handle_msg, exception_params_type(x_setting_handle)));
    end if;

    g_serialized_settings(x_setting_handle).app_settings(x_app).app_params(x_param_name) := x_param_value;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_serialized_parameter;

  /**
  * Function serializes given settings to a string.
  * @param x_setting_handle A handle for settings. A handle represents a set of parameters.
  * @return Serialized settings in a string
  */
  function serialize_settings(x_setting_handle in pls_integer default 1) return ctx_value_type is
    l_result ctx_value_type := null;
    l_logger_name t_logger.logger%type;
    l_param_name t_app_appender.parameter_name%type;
    l_append_param_name t_app_appender.parameter_name%type;
    l_appender pls_integer;
    l_app_name t_app.app%type;
    $if $$debug $then l_intlogger t_logger.logger%type := 'serialize_settings'; $end
  begin
    $if $$debug $then 
      internal_log(logging.c_info_level, l_intlogger, 'start'); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_setting_handle: ' || x_setting_handle);
    $end

    if not g_serialized_settings.exists(x_setting_handle) then 
      raise_application_error(c_no_such_handle_code, bind_params(c_no_such_handle_msg, exception_params_type(x_setting_handle)));
    end if;

    l_logger_name := g_serialized_settings(x_setting_handle).loggers.first;
    while l_logger_name is not null loop
       l_result := l_result || c_set_logger_op || c_ser_delim
                   || l_logger_name || c_ser_delim
                   || cast(g_serialized_settings(x_setting_handle).loggers(l_logger_name).enabled_appenders as varchar2) || c_ser_delim
                   || g_serialized_settings(x_setting_handle).loggers(l_logger_name).log_level || c_ser_delim
                   || cast(
                        encode_flags(
                          g_serialized_settings(x_setting_handle).loggers(l_logger_name).additivity,
                          g_serialized_settings(x_setting_handle).loggers(l_logger_name).backtrace,
                          g_serialized_settings(x_setting_handle).loggers(l_logger_name).callstack)
                      as varchar2) || c_ser_delim;
       l_logger_name := g_serialized_settings(x_setting_handle).loggers.next(l_logger_name);
    end loop;
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'loggers added: ' || l_result); $end

    l_app_name := g_serialized_settings(x_setting_handle).app_settings.first;
    while l_app_name is not null loop
      l_param_name := g_serialized_settings(x_setting_handle).app_settings(l_app_name).app_params.first;
      while l_param_name is not null loop
         l_result := l_result || c_set_app_param_op || c_ser_delim
                     || l_app_name || c_ser_delim
                     || l_param_name || c_ser_delim
                     || g_serialized_settings(x_setting_handle).app_settings(l_app_name).app_params(l_param_name)  || c_ser_delim;
         l_param_name := g_serialized_settings(x_setting_handle).app_settings(l_app_name).app_params.next(l_param_name);
      end loop;
      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'app params added: ' || l_result); $end

      l_appender := g_serialized_settings(x_setting_handle).app_settings(l_app_name).appenders_params.first;
      while l_appender is not null loop
         l_append_param_name := g_serialized_settings(x_setting_handle).app_settings(l_app_name).appenders_params(l_appender).first;
         while l_append_param_name is not null loop
           l_result := l_result || c_set_app_appender_param_op || c_ser_delim || l_app_name || c_ser_delim
                       || l_appender || c_ser_delim
                       || l_append_param_name || c_ser_delim
                       || g_serialized_settings(x_setting_handle).app_settings(l_app_name).appenders_params(l_appender)(l_append_param_name)  || c_ser_delim;

           l_append_param_name := g_serialized_settings(x_setting_handle).app_settings(l_app_name).appenders_params(l_appender).next(l_append_param_name);
         end loop;
         l_appender := g_serialized_settings(x_setting_handle).app_settings(l_app_name).appenders_params.next(l_appender);
      end loop;

      l_app_name := g_serialized_settings(x_setting_handle).app_settings.next(l_app_name);
    end loop;
    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_result;
  end serialize_settings;

  /**
  * Function deserializes given settings to a record
  * @param x_settings Serialized settings.
  * @return A handle for settings. A handle represents a set of parameters.
  */
  function get_deserialized_settings(x_settings in ctx_value_type) return pls_integer is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_deserialized_settings'; $end
    l_handle pls_integer;
    l_operation serialization_ops_type;
    l_logger t_logger.logger%type;
    l_log_level t_logger.log_level%type;
    l_flags pls_integer;
    l_appenders pls_integer;
    l_app t_app.app%type;
    l_appender t_appender.code%type;
    l_param_name ctx_attribute_type;
    l_param_value ctx_value_type;

    l_pos pls_integer;
    function get_str_val(x_str in ctx_value_type, x_start in pls_integer, x_pos out pls_integer) return ctx_value_type is
      l_end pls_integer;
      l_result ctx_value_type;
    begin
      l_end := instr(x_str, c_ser_delim, x_start);
      x_pos := l_end + case l_end when 0 then 0 else length(c_ser_delim) end;
      l_result := substr(x_str, x_start, l_end - x_start);
      $if $$debug $then
        internal_log(logging.c_trace_level, l_intlogger, 'x_str: ' || x_str);
        internal_log(logging.c_trace_level, l_intlogger, 'x_start: '|| x_start || ', x_end: ' || l_end || ', l_result: ' || l_result || ', x_pos: ' || x_pos);
      $end
      return l_result;
    end;

    function get_int_val(x_str in ctx_value_type, x_start in pls_integer, x_pos out pls_integer) return number is
      l_end pls_integer;
      l_result ctx_value_type;
    begin
      l_end := instr(x_str, c_ser_delim, x_start);
      x_pos := l_end + case l_end when 0 then 0 else length(c_ser_delim) end;
      l_result := substr(x_str, x_start, l_end - x_start);
      $if $$debug $then
        internal_log(logging.c_trace_level, l_intlogger, 'x_str: ' || x_str);
        internal_log(logging.c_trace_level, l_intlogger, 'x_start: '|| x_start || ', x_end: ' || l_end || ', l_result: ' || l_result || ', x_pos: ' || x_pos);
      $end
      return cast(l_result as number);
    end;

  begin
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'start'); $end

    l_handle := get_serialized_setting_handle();

    l_pos := 1;
    loop
      l_operation := get_str_val(x_settings, l_pos, l_pos);
      exit when l_operation is null;

      case l_operation
      when c_set_logger_op then
        l_logger := get_str_val(x_settings, l_pos, l_pos);
        l_appenders :=  get_int_val(x_settings, l_pos, l_pos);
        l_log_level := get_str_val(x_settings, l_pos, l_pos);
        l_flags := get_int_val(x_settings, l_pos, l_pos);
        if l_log_level is not null then
          set_serialized_level(x_logger_name => l_logger, x_log_level => l_log_level, x_setting_handle => l_handle);
        end if;
        if l_flags is not null then
          set_serialized_flags(x_logger_name => l_logger,
                               x_additivity => bitand(l_flags, c_flag_add) > 0,
                               x_backtrace => bitand(l_flags, c_flag_backtrace) > 0,
                               x_callstack => bitand(l_flags, c_flag_callstack) > 0,
                               x_setting_handle => l_handle);
        end if;

        l_appender := 1;
        while l_appenders > l_appender loop
          if bitand(l_appenders, l_appender) = l_appender then
            add_serialized_appender(x_logger_name => l_logger, x_appender_code => l_appender, x_setting_handle => l_handle);
          end if;
          l_appender := l_appender*2;
        end loop;
      when c_set_app_param_op then
        l_app := get_str_val(x_settings, l_pos, l_pos);
        l_param_name := get_str_val(x_settings, l_pos, l_pos);
        l_param_value := get_str_val(x_settings, l_pos, l_pos);
        set_serialized_parameter(x_app => l_app, x_param_name => l_param_name, x_param_value => l_param_value, x_setting_handle => l_handle);
      when c_set_app_appender_param_op then
        l_app := get_str_val(x_settings, l_pos, l_pos);
        l_appender := get_int_val(x_settings, l_pos, l_pos);
        l_param_name := get_str_val(x_settings, l_pos, l_pos);
        l_param_value := get_str_val(x_settings, l_pos, l_pos);
        set_serialized_parameter(x_app => l_app, x_param_name => l_param_name, x_param_value => l_param_value, x_setting_handle => l_handle);
      end case;
    end loop;

    $if $$debug $then 
      internal_log(logging.c_debug_level, l_intlogger, 'l_handle: ' || l_handle);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_handle;
  end get_deserialized_settings;

  /** Procedure shows serialized settings. 
  * @param x_setting_handle A handle for settings. A handle represents a set of parameters.
  */
  procedure show_serialized_settings(x_setting_handle in pls_integer default 1) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'show_serialized_settings'; $end
  begin
    $if $$debug $then 
      internal_log(logging.c_info_level, l_intlogger, 'start'); 
      internal_log(logging.c_debug_level, l_intlogger, 'x_setting_handle: ' || x_setting_handle);
    $end

    if not g_serialized_settings.exists(x_setting_handle) then 
      raise_application_error(c_no_such_handle_code, bind_params(c_no_such_handle_msg, exception_params_type(x_setting_handle)));
    end if;

    dbms_output.put_line(serialize_settings(x_setting_handle));
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end show_serialized_settings;

  /** Procedure set sets given settings in the current session by calling set_session* methods.
  * @param x_setting_handle A handle for settings. A handle represents a set of parameters.
  */
  procedure set_session_settings(x_setting_handle in pls_integer default 1) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_session_settings'; $end
    l_logger     t_logger.logger%type;
    l_app        t_app.app%type;
    l_appender   t_appender.code%type;
    l_param_name ctx_attribute_type;
  begin
    $if $$debug $then 
      internal_log(logging.c_info_level, l_intlogger, 'begin');
      internal_log(logging.c_debug_level, l_intlogger, 'x_setting_handle: ' || x_setting_handle);
    $end

    if not g_serialized_settings.exists(x_setting_handle) then 
      raise_application_error(c_no_such_handle_code, bind_params(c_no_such_handle_msg, exception_params_type(x_setting_handle)));
    end if;

    -- set loggers
    l_logger := g_serialized_settings(x_setting_handle).loggers.first;
    while l_logger is not null loop
      if g_serialized_settings(x_setting_handle).loggers(l_logger).log_level is not null then
        $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'setting level: ' || g_serialized_settings(x_setting_handle).loggers(l_logger).log_level || ' for logger: ' || l_logger); $end
        set_session_level(x_logger_name => l_logger, x_log_level => g_serialized_settings(x_setting_handle).loggers(l_logger).log_level);
      end if;
      if g_serialized_settings(x_setting_handle).loggers(l_logger).additivity is not null then
        $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'setting additivity: ' || g_serialized_settings(x_setting_handle).loggers(l_logger).additivity || ' for logger: ' || l_logger); $end
        set_session_additivity(x_logger_name => l_logger,
                               x_additivity => int_to_bool(g_serialized_settings(x_setting_handle).loggers(l_logger).additivity));
      end if;

      l_appender := 1;
      while g_serialized_settings(x_setting_handle).loggers(l_logger).enabled_appenders > l_appender loop
        if bitand(g_serialized_settings(x_setting_handle).loggers(l_logger).enabled_appenders, l_appender) = l_appender then
          $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'setting appender: ' || l_appender || ' for logger: ' || l_logger); $end
          add_session_appender(x_logger_name => l_logger, x_appender_code => l_appender);
        end if;
        l_appender := l_appender*2;
      end loop;

      l_logger :=  g_serialized_settings(x_setting_handle).loggers.next(l_logger);
    end loop;

    -- set app params
    l_app := g_serialized_settings(x_setting_handle).app_settings.first;
    while l_app is not null loop
      l_param_name := g_serialized_settings(x_setting_handle).app_settings(l_app).app_params.first;
      while l_param_name is not null loop
          $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'setting parameter: ' || l_param_name || ' to value: ' ||  g_serialized_settings(x_setting_handle).app_settings(l_app).app_params(l_param_name) || ' for app: ' || l_app); $end
          set_session_parameter(x_app => l_app,
                                x_param_name => l_param_name,
                                x_param_value => g_serialized_settings(x_setting_handle).app_settings(l_app).app_params(l_param_name));
        l_param_name := g_serialized_settings(x_setting_handle).app_settings(l_app).app_params.next(l_param_name);
      end loop;


      l_appender := g_serialized_settings(x_setting_handle).app_settings(l_app).appenders_params.first;
      while l_appender is not null loop
        l_param_name := g_serialized_settings(x_setting_handle).app_settings(l_app).appenders_params(l_appender).first;
        while l_param_name is not null loop
          $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'setting parameter: ' || l_param_name || ' to value: ' ||   g_serialized_settings(x_setting_handle).app_settings(l_app).appenders_params(l_appender)(l_param_name) || ' for appender: ' || l_appender || ' and for app: ' || l_app); $end
          set_session_appender_param(x_app => l_app,
                                     x_appender_code =>  l_appender,
                                     x_parameter_name => l_param_name,
                                     x_parameter_value => g_serialized_settings(x_setting_handle).app_settings(l_app).appenders_params(l_appender)(l_param_name));
          l_param_name := g_serialized_settings(x_setting_handle).app_settings(l_app).appenders_params(l_appender).next(l_param_name);
        end loop;

        l_appender := g_serialized_settings(x_setting_handle).app_settings(l_app).appenders_params.next(l_appender);
      end loop;

      l_app :=  g_serialized_settings(x_setting_handle).app_settings.next(l_app);
    end loop;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_session_settings;

  /**
  * Procedure applies requested session settings for current session and given application.
  * @param x_app Application.
  */
  procedure use_requested_session_settings(x_app in t_app.app%type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'use_requested_session_settings'; $end
    l_settings ctx_value_type;
    l_setting_handle pls_integer;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
    $end
    l_settings := sys_context(c_modify_session_ctx,  g_session_identifier);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_settings: ' || l_settings); $end
    if l_settings is null then
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'No session settings requested'); $end
      return;
    end if;

    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'session settings requested, applying...'); $end
    -- if there have been some settings requested, apply them to current session
    if l_settings	is not null then
      copy_global_to_session(x_app => x_app);
      l_setting_handle := get_deserialized_settings(l_settings);
      set_session_settings(l_setting_handle);
      clear_serialized_settings(l_setting_handle);
      set_session_ctx_usage(x_usage => true);
      
      -- clear the settings from context to not be applied multiple times
      set_context_rac_aware(c_modify_session_ctx, g_session_identifier, null, c_global_flag);
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end use_requested_session_settings;

  /**
  * Function obtains current parameter value for given app and parameter name.
  * @param x_app Application.
  * @param x_param_name Parameter name.
  * @return Current parameter value for given application and parameter name.
  */
  function get_current_parameter(x_app        in t_param.app%type,
                                 x_param_name in t_param.param_name%type) return t_param.param_value%type is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_current_parameter'; $end
    l_result t_param.param_value%type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_param_name: ' || x_param_name);
    $end

    if get_session_ctx_usage() then
      l_result := sys_context(c_parameters_ctx(c_session_flag), x_app || '#' || x_param_name);
    else
      l_result := sys_context(c_parameters_ctx(c_global_flag), x_app || '#' || x_param_name);
    end if;

    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_result: ' || l_result);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_result;
  end get_current_parameter;

  /**
  * Procedure removes given session appenders from given logger.
  * @param x_logger_name Logger name.
  * @param x_appender_code Appender code.
  */
  procedure remove_session_appender(x_logger_name   in t_logger.logger%type,
                                    x_appender_code in t_appender.code%type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'remove_session_appender'; $end
    l_appenders t_logger.appenders%type;
    l_dummy     varchar2(1);
    l_hlogger   hash_type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_appender_code: ' || x_appender_code);
    $end

    begin
      select null
        into l_dummy
        from t_appender a
       where a.code = x_appender_code;
    exception
      when no_data_found then
        $if $$debug $then internal_log(logging.c_error_level, l_intlogger, 'No such appender'); $end
        raise_application_error(c_no_such_appender_code, bind_params(c_no_such_appender_msg, exception_params_type(x_appender_code)));
    end;

    $if dbms_db_version.version >= 11 $then pragma inline('hash_logger_name',  'YES'); $end
    l_hlogger   := hash_logger_name(x_logger_name);
    l_appenders := nvl(sys_context(c_logger_appenders_ctx(c_session_flag), l_hlogger), 0);

    $if $$debug $then
      internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger' || l_hlogger);
      internal_log(logging.c_trace_level, l_intlogger, 'l_appenders' || l_appenders);
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('bitand',  'YES'); $end
    if bitand(l_appenders, x_appender_code) > 0 then
      $if dbms_db_version.version >= 11 $then pragma inline('bit_xor',  'YES'); $end
      l_appenders := bit_xor(l_appenders, x_appender_code);
      $if $$debug $then
        internal_log(logging.c_info_level, l_intlogger, 'Removing appender from context' || l_hlogger);
        internal_log(logging.c_debug_level, l_intlogger, 'l_appenders' || l_appenders);
      $end
      set_context_rac_aware(c_logger_appenders_ctx(c_session_flag), l_hlogger, l_appenders, c_session_flag);
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end remove_session_appender;

  /**
  * Procedure sets global flags for given logger.
  * @param x_logger Logger name.
  * @param x_additivity Additivity flag.
  * @param x_backtrace Flag whether backtrace will be logged.
  * @param x_callstack Flag whether callstack will be logged.    
  */
  procedure set_global_flags(x_logger_name in t_logger.logger%type,
                             x_additivity  in boolean default null,
                             x_backtrace   in boolean default null,
                             x_callstack   in boolean default null) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_global_flags'; $end
    pragma autonomous_transaction;
    l_app        t_schema_app.app%type;
    l_flags      pls_integer;
    l_additivity pls_integer;
    l_backtrace  pls_integer;
    l_callstack  pls_integer;
    l_dummy      varchar2(1);
    l_hlogger    hash_type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_additivity: ' || bool_to_int(x_additivity));
      internal_log(logging.c_debug_level, l_intlogger, 'x_backtrace: ' || bool_to_int(x_backtrace));
      internal_log(logging.c_debug_level, l_intlogger, 'x_callstack: ' || bool_to_int(x_callstack));      
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_app',  'YES'); $end
    l_app := get_app(c_user);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_app: ' || l_app); $end

    select null
      into l_dummy
      from t_schema_app ua
     where ua.schema = c_user
       and ua.app = l_app;

    l_additivity := bool_to_int(x_additivity);
    l_backtrace := bool_to_int(x_backtrace);
    l_callstack := bool_to_int(x_callstack);
    
    update t_logger l
       set l.additivity = coalesce(l_additivity, l.additivity),
           l.backtrace = coalesce(l_backtrace, l.backtrace),
           l.callstack = coalesce(l_callstack, l.callstack)
     where l.logger = x_logger_name;

    if sql%notfound then 
      insert into t_logger(logger, log_level, appenders, additivity, backtrace, callstack)
        values (x_logger_name, null, null, l_additivity, l_backtrace, l_callstack);
      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'new row inserted'); $end
    else
      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'row updated'); $end
      null;
    end if;

    $if dbms_db_version.version >= 11 $then pragma inline('hash_logger_name',  'YES'); $end
    l_flags := encode_flags(l_additivity, l_backtrace, l_callstack);
    l_hlogger := hash_logger_name(x_logger_name);
    $if $$debug $then 
      internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger: ' || l_hlogger);
      internal_log(logging.c_trace_level, l_intlogger, 'l_flags: ' || l_flags);
    $end
    set_context_rac_aware(c_logger_names_ctx(c_global_flag), l_hlogger, x_logger_name, c_global_flag);
    set_context_rac_aware(c_flags_ctx(c_global_flag), l_hlogger, l_flags, c_global_flag);

    commit;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_global_flags;

  /**
  * Procedure sets session flags for given logger.
  * @param x_logger Logger name.
  * @param x_additivity Additivity flag.
  * @param x_backtrace Flag whether backtrace will be logged.
  * @param x_callstack Flag whether callstack will be logged.    
  */
  procedure set_session_flags(x_logger_name in t_logger.logger%type,
                              x_additivity  in boolean default null,
                              x_backtrace   in boolean default null,
                              x_callstack   in boolean default null) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_session_flags'; $end
    pragma autonomous_transaction;
    l_app        t_schema_app.app%type;
    l_flags      pls_integer;
    l_additivity pls_integer;
    l_backtrace  pls_integer;
    l_callstack  pls_integer;
    l_dummy      varchar2(1);
    l_hlogger    hash_type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_additivity: ' || bool_to_int(x_additivity));
      internal_log(logging.c_debug_level, l_intlogger, 'x_backtrace: ' || bool_to_int(x_backtrace));
      internal_log(logging.c_debug_level, l_intlogger, 'x_callstack: ' || bool_to_int(x_callstack));      
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_app',  'YES'); $end
    l_app := get_app(c_user);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_app: ' || l_app); $end

    select null
      into l_dummy
      from t_schema_app ua
     where ua.schema = c_user
       and ua.app = l_app;

    l_additivity := bool_to_int(x_additivity);
    l_backtrace := bool_to_int(x_backtrace);
    l_callstack := bool_to_int(x_callstack);
    
    $if dbms_db_version.version >= 11 $then pragma inline('hash_logger_name',  'YES'); $end
    l_flags := encode_flags(l_additivity, l_backtrace, l_callstack);
    l_hlogger := hash_logger_name(x_logger_name);
    $if $$debug $then 
      internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger: ' || l_hlogger);
      internal_log(logging.c_trace_level, l_intlogger, 'l_flags: ' || l_flags);
    $end
    set_context_rac_aware(c_logger_names_ctx(c_session_flag), l_hlogger, x_logger_name, c_global_flag);
    set_context_rac_aware(c_flags_ctx(c_session_flag), l_hlogger, l_flags, c_global_flag);

    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_session_flags;

  /**
  * Procedure sets global log level for given logger.
  * @param x_logger Logger name.
  * @param x_log_level Log level.
  */
  procedure set_global_level(x_logger_name in t_logger.logger%type,
                             x_log_level   in t_logger.log_level%type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_global_level'; $end
    pragma autonomous_transaction;
    l_app       t_schema_app.app%type;
    l_dummy     varchar2(1);
    l_hlogger   hash_type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_log_level: ' || x_log_level);
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_app',  'YES'); $end
    l_app := get_app(c_user);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_app: ' || l_app); $end

    select null
      into l_dummy
      from t_schema_app ua
     where ua.schema = c_user
       and ua.app = l_app;

    merge into t_logger l
    using (select null
             from dual) dummy
    on (l.logger = x_logger_name)
    when matched then
      update
         set l.log_level = log_level
    when not matched then
      insert
        (logger, log_level)
      values
        (x_logger_name, x_log_level);

    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'merged: ' || sql%rowcount || ' rows'); $end
    $if dbms_db_version.version >= 11 $then pragma inline('hash_logger_name',  'YES'); $end
    l_hlogger := hash_logger_name(x_logger_name);

    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger: ' || l_hlogger); $end
    set_context_rac_aware(c_logger_names_ctx(c_global_flag), l_hlogger, x_logger_name, c_global_flag);
    set_context_rac_aware(c_logger_levels_ctx(c_global_flag), l_hlogger, x_log_level, c_global_flag);

    commit;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_global_level;

  /**
  * Procedure sets session log level for given logger.
  * @param x_logger Logger name.
  * @param x_log_level Log level.
  */
  procedure set_session_level(x_logger_name in t_logger.logger%type,
                              x_log_level   in t_logger.log_level%type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'set_session_level'; $end
    l_hlogger hash_type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_log_level: ' || x_log_level);
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('hash_logger_name',  'YES'); $end
    l_hlogger := hash_logger_name(x_logger_name);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_hlogger: ' || l_hlogger); $end
    set_context_rac_aware(c_logger_names_ctx(c_session_flag), l_hlogger, x_logger_name, c_session_flag);
    set_context_rac_aware(c_logger_levels_ctx(c_session_flag), l_hlogger, x_log_level, c_session_flag);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end set_session_level;

  /**
  * Funtion formats given message with given layout.
  * @param x_message Message.
  * @param x_layout Layout.
  * @param x_logger_name Logger name.
  * @param x_level Log level name.
  * @return Formated message.
  */
  function format_message(x_message     in message_type,
                          x_layout      in t_app_appender.parameter_value%type,
                          x_logger_name in t_logger.logger%type,
                          x_level       in t_log.log_level%type) return varchar2 is
    $if $$debug $then l_intlogger t_logger.logger%type := 'format_message'; $end
    l_message t_log.message%type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_message: ' || x_message);
      internal_log(logging.c_debug_level, l_intlogger, 'x_layout: ' || x_layout);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_level: ' || x_level);
    $end

    l_message := replace(replace(replace(replace(x_layout, '%m', x_message), '%l', x_logger_name),
                                 '%L',
                                 x_level),
                         '%t',
                         systimestamp);
    if instr(l_message, '%b') > 0 then
      l_message := replace(l_message,
                           '%b',
                           dbms_utility.format_error_stack || dbms_utility.format_error_backtrace);
    end if;
    if instr(l_message, '%c') > 0 then
      l_message := replace(l_message, '%c', format_call_stack());
    end if;

    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_message: ' || l_message);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_message;
  end format_message;

  /**
  * Procedure initializes cyclic buffer.
  * @param x_app Application
  */
  procedure init_email_cyclic_buffer(x_app in varchar2) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'init_email_cyclic_buffer'; $end
  begin
      $if $$debug $then
        internal_log(logging.c_info_level, l_intlogger, 'start');
        internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      $end
      g_mail_buffer.head      := 1;
      g_mail_buffer.tail      := 1;
      g_mail_buffer.buffer    := mail_table_type();
      g_mail_buffer.buff_size := 1000;
      $if dbms_db_version.version >= 11 $then pragma inline('get_appender_param',  'YES'); $end
      g_mail_buffer.buff_size := coalesce(get_appender_param(x_app, c_smtp_appender, 'MAIL_BUFFER_LINES', c_global_flag), 1000);
      g_mail_buffer.buffer.extend(g_mail_buffer.buff_size);
      $if $$debug $then
        internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.buff_size: ' || g_mail_buffer.buff_size);
        internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.buffer.size: ' || g_mail_buffer.buffer.count);
        internal_log(logging.c_info_level, l_intlogger, 'end');
      $end
  end init_email_cyclic_buffer;

  /** Procedure enqueues a message to cyclic buffer (queue).
  * If the buffer is full the first message in queue is discarded.
  * @param x_app Application.
  * @param x_message Message.
  */
  procedure enqueue_into_cyclic_buffer(x_app in varchar2, x_message in message_type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'enqueue_into_cyclic_buffer'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_message: ' || x_message);
    $end

    -- if the size is null, buffer was not initialized
    if g_mail_buffer.buff_size is null then
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'initializing buffer'); $end
      $if dbms_db_version.version >= 11 $then pragma inline('init_email_cyclic_buffer',  'YES'); $end
      init_email_cyclic_buffer(x_app);
    end if;

    -- enqueque the message
    g_mail_buffer.buffer(g_mail_buffer.tail) := x_message;
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.tail' || g_mail_buffer.tail); $end
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.buffer(g_mail_buffer.tail)' || g_mail_buffer.buffer(g_mail_buffer.tail)); $end

    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.buff_size' || g_mail_buffer.buff_size); $end
    -- cyclic incrementation of the tail
    if g_mail_buffer.tail = g_mail_buffer.buff_size then
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end of buffer, reseting to 1'); $end
      g_mail_buffer.tail := 1;
    else
      g_mail_buffer.tail := g_mail_buffer.tail + 1;
    end if;
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.tail' || g_mail_buffer.tail); $end


    -- if the tail position and the head position is the same
    -- then the buffer is full
    if g_mail_buffer.head = g_mail_buffer.tail then
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'buffer is full'); $end
      -- discard the first message by cyclic incrementation of the head position
      if g_mail_buffer.head = g_mail_buffer.buff_size then
        g_mail_buffer.head := 1;
      else
        g_mail_buffer.head := g_mail_buffer.head + 1;
      end if;
      $if $$debug $then
        internal_log(logging.c_info_level, l_intlogger, 'discarding first message in queue');
        internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.head' || g_mail_buffer.head);
      $end
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end enqueue_into_cyclic_buffer;

  /** Function dequeues a message from cyclic buffer (queue).
  * @return Dequeued message.
  */
  FUNCTION dequeue_from_cyclic_buffer RETURN VARCHAR2 IS
    $if $$debug $then l_intlogger t_logger.logger%type := 'dequeue_from_cyclic_buffer'; $end
    l_last_head pls_integer;
  begin
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'start'); $end
    l_last_head := g_mail_buffer.head;
    $if $$debug $then
      internal_log(logging.c_trace_level, l_intlogger, 'l_last_head' || l_last_head);
      internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.buff_size' || g_mail_buffer.buff_size);
    $end

    if g_mail_buffer.head = g_mail_buffer.buff_size then
      g_mail_buffer.head := 1;
    else
      g_mail_buffer.head := g_mail_buffer.head + 1;
    end if;
    $if $$debug $then
      internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.head' || g_mail_buffer.head);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return g_mail_buffer.buffer(l_last_head);
  end dequeue_from_cyclic_buffer;

  /**
  * Function checks whether is cyclic buffer empty or not.
  * @return Flags inidicating whether is buffer empty or not.
  * {*} TRUE if the buffer is empty
  * {*} FALSE if the buffer is not empty
  */
  function is_cyclic_buffer_empty return boolean is
    $if $$debug $then l_intlogger t_logger.logger%type := 'is_cyclic_buffer_empty'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.head' || g_mail_buffer.head);
      internal_log(logging.c_trace_level, l_intlogger, 'g_mail_buffer.tail' || g_mail_buffer.tail);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end

    return coalesce(g_mail_buffer.head = g_mail_buffer.tail, true);
  end is_cyclic_buffer_empty;

  /**
  * Procedure parses call_stack and extracts logger and application from the stack.
  * @param o_logger Logger extracted from the stack.
  * @param o_app Application extracted from the stack.
  * @param x_method Method name.
  * @param x_call_stack Call stack.
  */
  procedure parse_stack(o_logger     out varchar2,
                        o_app        out varchar2,
                        x_method     in varchar2 default null,
                        x_call_stack in varchar2 default dbms_utility.format_call_stack()) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'parse_stack'; $end
    l_offset       pls_integer;
    l_next_nl      pls_integer;
    l_stack_object varchar2(92 char);
    l_stack_line   varchar2(255 char);
    l_obj_schema   user_users.username%type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_method: ' || x_method);
      internal_log(logging.c_debug_level, l_intlogger, 'x_call_stack: ' || x_call_stack);
    $end

    -- end of last line for this package
    l_offset := instr(x_call_stack, c_nl, instr(x_call_stack, c_package_name, pos => -1)) + c_nl_length;
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_offset: ' || l_offset); $end

    -- next end of line
    l_next_nl := instr(x_call_stack, c_nl, l_offset);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_next_nl: ' || l_next_nl); $end
    if l_next_nl = 0 then
      o_logger := c_root_logger_name;
      o_app := c_user;
    else
      -- first line after last line of this package
      l_stack_line   := substr(x_call_stack, l_offset, l_next_nl - l_offset);
      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_stack_line: ' || l_stack_line); $end
      -- object on that line
      l_stack_object := substr(l_stack_line, instr(l_stack_line, c_space, -1) + 1);
      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_stack_object: ' || l_stack_object); $end
      -- schema of the object
      l_obj_schema   := substr(l_stack_object, 1, instr(l_stack_object, '.') - 1);
      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_obj_schema: ' || l_obj_schema); $end
      if x_method is not null then
        l_stack_object := l_stack_object || '.' || upper(x_method);
      end if;
      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_stack_object: ' || l_stack_object); $end

      -- if schema was not determined, logging was called from anonymous block.
      if l_obj_schema is null then
        l_obj_schema := c_user;
      end if;
      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_obj_schema: ' || l_obj_schema); $end

      $if dbms_db_version.version >= 11 $then pragma inline('get_app',  'YES'); $end
      o_app  := get_app(l_obj_schema);
      o_logger := o_app || '.' || l_stack_object;
    end if;
    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'o_logger: ' || o_logger);
      internal_log(logging.c_debug_level, l_intlogger, 'o_app: ' || o_app);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
  end parse_stack;

  /**
  * Returns a logger configuration for given method.
  * @param x_method Name of method.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  function get_logger(x_method          in varchar2 default null,
                      x_always_from_ctx in ctx_boolean default c_false) return logger_type is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_logger'; $end
    l_logger       logger_type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_method: ' || x_method);
      internal_log(logging.c_debug_level, l_intlogger, 'x_always_from_ctx: ' || x_always_from_ctx);
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('parse_stack',  'YES'); $end
    parse_stack(l_logger.logger, l_logger.app, x_method);
    l_logger.always_from_ctx := x_always_from_ctx;

    l_logger.log_level := get_current_used_level(l_logger.logger);
    $if dbms_db_version.version >= 11 $then pragma inline('get_current_used_appenders',  'YES'); $end
    l_logger.enabled_appenders  := get_current_used_appenders(l_logger.logger);

    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_logger.always_from_ctx: ' || l_logger.always_from_ctx);
      internal_log(logging.c_debug_level, l_intlogger, 'l_logger.log_level_severity: ' || l_logger.log_level);
      internal_log(logging.c_debug_level, l_intlogger, 'l_logger.enabled_appenders: ' || l_logger.enabled_appenders);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_logger;
  end get_logger;

  /**
  * Returns the root logger configuration.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  function get_root_logger(x_always_from_ctx in ctx_boolean default c_false) return logger_type is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_root_logger'; $end
    l_logger logger_type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_always_from_ctx: ' || x_always_from_ctx);
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('parse_stack',  'YES'); $end
    parse_stack(l_logger.logger, l_logger.app);
    l_logger.logger          := c_root_logger_name;
    l_logger.always_from_ctx := x_always_from_ctx;

    $if dbms_db_version.version >= 11 $then pragma inline('get_current_used_level',  'YES'); $end
    l_logger.log_level := get_current_used_level(l_logger.logger);
    $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'l_logger.log_level_severity: ' || l_logger.log_level); $end
    $if dbms_db_version.version >= 11 $then pragma inline('get_current_used_appenders',  'YES'); $end
    l_logger.enabled_appenders  := get_current_used_appenders(l_logger.logger);
    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_logger.enabled_appenders: ' || l_logger.enabled_appenders);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_logger;
  end get_root_logger;

  /**
  * Returns the application logger configuration.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  function get_app_logger(x_always_from_ctx in ctx_boolean default c_false) return logger_type is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_app_logger'; $end
    l_logger logger_type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_always_from_ctx: ' || x_always_from_ctx);
    $end
    $if dbms_db_version.version >= 11 $then pragma inline('parse_stack',  'YES'); $end
    parse_stack(l_logger.logger, l_logger.app);
    l_logger.logger          := l_logger.app;
    l_logger.always_from_ctx := x_always_from_ctx;
    $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'l_logger.logger: ' || l_logger.logger); $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_current_used_level',  'YES'); $end
    l_logger.log_level := get_current_used_level(l_logger.logger);
    $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'l_logger.log_level_severity: ' || l_logger.log_level); $end
    $if dbms_db_version.version >= 11 $then pragma inline('get_current_used_appenders',  'YES'); $end
    l_logger.enabled_appenders  := get_current_used_appenders(l_logger.logger);
    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_logger.enabled_appenders: ' || l_logger.enabled_appenders);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_logger;
  end get_app_logger;

  /**
  * Returns the schema logger configuration.
  * @param x_always_from_ctx Flag whether the configuration is always obtained
  *                          from application context.
  * @return Logger configuration.
  */
  function get_schema_logger(x_always_from_ctx in ctx_boolean default c_false) return logger_type is
    $if $$debug $then l_intlogger t_logger.logger%type := 'get_schema_logger'; $end
    l_logger logger_type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_always_from_ctx: ' || x_always_from_ctx);
    $end
    $if dbms_db_version.version >= 11 $then pragma inline('parse_stack',  'YES'); $end
    parse_stack(l_logger.logger, l_logger.app);
    l_logger.logger          := l_logger.app || '.' || c_user;
    l_logger.always_from_ctx := x_always_from_ctx;
    $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'l_logger.logger: ' || l_logger.logger); $end
    $if dbms_db_version.version >= 11 $then pragma inline('get_current_used_level',  'YES'); $end
    l_logger.log_level := get_current_used_level(l_logger.logger);
    $if dbms_db_version.version >= 11 $then pragma inline('get_current_used_appenders',  'YES'); $end
    l_logger.enabled_appenders  := get_current_used_appenders(l_logger.logger);
    $if $$debug $then
      internal_log(logging.c_debug_level, l_intlogger, 'l_logger.enabled_appenders: ' || l_logger.enabled_appenders);
      internal_log(logging.c_info_level, l_intlogger, 'end');
    $end
    return l_logger;
  end get_schema_logger;

  /** Procedure sends cyclic buffer. Parameters for sending are obtained
  * for given application.
  * @param x_app Application name.
  */
  procedure send_buffer(x_app in t_app_appender.app%type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'send_buffer'; $end
    l_host      t_app_appender.parameter_value%type;
    l_from      t_app_appender.parameter_value%type;
    l_to        t_app_appender.parameter_value%type;
    l_cc        t_app_appender.parameter_value%type;
    l_bcc       t_app_appender.parameter_value%type;
    l_port      t_app_appender.parameter_value%type;
    l_timeout   t_app_appender.parameter_value%type;
    l_reply     utl_smtp.reply;
    l_conn      utl_smtp.connection;
    l_offset    pls_integer;
    l_comma_pos pls_integer;
    l_csv_list  varchar2(32767);
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_current_appender_param',  'YES'); $end
    l_host := get_current_appender_param(x_app, c_smtp_appender, 'SEND_MAIL_HOST');
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_host: ' || l_host); $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_current_appender_param',  'YES'); $end
    l_from := get_current_appender_param(x_app, c_smtp_appender, 'SEND_MAIL_FROM');
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_from: ' || l_from); $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_current_appender_param',  'YES'); $end
    l_port := get_current_appender_param(x_app, c_smtp_appender, 'SEND_MAIL_PORT');
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_port: ' || l_port); $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_current_appender_param',  'YES'); $end
    l_timeout := get_current_appender_param(x_app, c_smtp_appender, 'SEND_MAIL_TIMEOUT');
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_timeout: ' || l_timeout); $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_current_appender_param',  'YES'); $end
    l_conn := utl_smtp.open_connection(host => l_host, port => l_port, tx_timeout => l_timeout);

    l_reply := utl_smtp.helo(l_conn, l_host);

    if l_reply.code <> 200 then
      $if $$debug $THEN internal_log(logging.c_error_level, l_intlogger, 'HELO failed'); $end
      raise_application_error(c_smtp_failure_code, bind_params(c_smtp_failure_msg, exception_params_type('HELO', l_reply.code, l_reply.text)));
    end if;

    l_reply := utl_smtp.mail(l_conn, l_from);

    if l_reply.code <> 200 then
      $if $$debug $then internal_log(logging.c_error_level, l_intlogger, 'mail failed'); $end
      raise_application_error(c_smtp_failure_code, bind_params(c_smtp_failure_msg, exception_params_type('MAIL', l_reply.code, l_reply.text)));
    end if;

    $if dbms_db_version.version >= 11 $then pragma inline('get_current_appender_param',  'YES'); $end
    l_to  := get_current_appender_param(x_app, c_smtp_appender, 'SEND_MAIL_TO');
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_to: ' || l_to); $end
    $if dbms_db_version.version >= 11 $then pragma inline('get_current_appender_param',  'YES'); $end
    l_cc  := get_current_appender_param(x_app, c_smtp_appender, 'SEND_MAIL_CC');
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_cc: ' || l_cc); $end
    $if dbms_db_version.version >= 11 $then pragma inline('get_current_appender_param',  'YES'); $end
    l_bcc := get_current_appender_param(x_app, c_smtp_appender, 'SEND_MAIL_BCC');
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_bcc: ' || l_bcc); $end

    if l_to is not null then
      l_csv_list := l_to || ',';
    end if;

    if l_cc is not null then
      l_csv_list := l_csv_list || l_cc || ',';
    end if;

    if l_bcc is not null then
      l_csv_list := l_csv_list || l_bcc || ',';
    end if;
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_csv_list: ' || l_csv_list); $end

    l_offset := 1;
    loop
      l_comma_pos := instr(l_csv_list, ',', l_offset);
      $if $$debug $then
        internal_log(logging.c_trace_level, l_intlogger, 'l_comma_pos: ' || l_comma_pos);
        internal_log(logging.c_trace_level, l_intlogger, 'l_offset: ' || l_offset);
      $end
      exit when l_comma_pos = 0;

      l_reply := utl_smtp.rcpt(l_conn, substr(l_csv_list, l_offset, l_comma_pos - l_offset));      
      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_reply.code: ' || l_reply.code); $end
      if l_reply.code <> 200 then
        raise_application_error(c_smtp_failure_code, bind_params(c_smtp_failure_msg, exception_params_type('RCPT', l_reply.code, l_reply.text)));
      end if;

      l_offset := l_comma_pos + 1;
    end loop;

    utl_smtp.open_data(l_conn);

    utl_smtp.write_data(l_conn, 'content-type: text/plain' || utl_tcp.crlf);
    utl_smtp.write_data(l_conn, 'from: ' || l_from || utl_tcp.crlf);
    utl_smtp.write_data(l_conn, 'to: ' || l_to || utl_tcp.crlf);
    utl_smtp.write_data(l_conn, 'cc: ' || l_cc || utl_tcp.crlf);
    utl_smtp.write_data(l_conn,
                        'Subject: ' || get_current_appender_param(x_app, c_smtp_appender, 'SEND_MAIL_SUBJECT') ||
                        utl_tcp.crlf);

    $if dbms_db_version.version >= 11 $then pragma inline('is_cyclic_buffer_empty',  'YES'); $end
    while not is_cyclic_buffer_empty() loop
      utl_smtp.write_data(l_conn,
                          --                          convert(
                          dequeue_from_cyclic_buffer() --,
                          --                                  get_current_appender_param(x_app,
                          --                                                             c_smtp_appender,
                          --                                                             'SEND_MAIL_ORA_CHARSET'))
                          /*utl_encode.text_encode(dequeue_from_cyclic_buffer(),
                         f_get_current_appender_param(x_app, c_smtp_appender, 'send_mail_ora_charset'),
                         utl_encode.base64)*/);
    end loop;

    utl_smtp.close_data(l_conn);

    l_reply := utl_smtp.quit(l_conn);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_reply.code: ' || l_reply.code); $end

    if l_reply.code <> 200 then
      $if $$debug $then internal_log(logging.c_error_level, l_intlogger, 'QUIT command failed'); $end
      raise_application_error(c_smtp_failure_code, bind_params(c_smtp_failure_msg, exception_params_type('QUIT', l_reply.code, l_reply.text)));
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end send_buffer;

  /**
  * Procedure logs given message using SMTP protocol.
  * @param x_app Application name
  * @param x_logger_name Logger name.
  * @param x_log_level Log level.
  * @param x_message Message.
  * @param x_call_stack Flag, whether to log call stack.
  * @param x_log_backtrace Flag, whether to log backtrace.
  */
  procedure log_smtp(x_app           in t_app_appender.app%type,
                     x_logger_name   in t_logger.logger%type,
                     x_level         in t_logger.log_level%type,
                     x_message       in message_type,
                     x_call_stack    in boolean,
                     x_log_backtrace in boolean) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'log_smtp'; $end
    l_layout        t_app_appender.parameter_value%type;
    l_trigger_level t_logger.log_level%type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_level: ' || x_level);
      internal_log(logging.c_debug_level, l_intlogger, 'x_message: ' || x_message);
      internal_log(logging.c_debug_level, l_intlogger, 'x_call_stack: ' || bool_to_int(x_call_stack));
      internal_log(logging.c_debug_level, l_intlogger, 'x_log_backtrace: ' || bool_to_int(x_log_backtrace));
    $end
    $if dbms_db_version.version >= 11 $then pragma inline('get_current_layout',  'YES'); $end
    l_layout := get_current_layout(x_app, c_smtp_appender);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_layout: ' || l_layout);  $end

    $if dbms_db_version.version >= 11 $then pragma inline('enqueue_into_cyclic_buffer',  'YES'); $end
    enqueue_into_cyclic_buffer(x_app, format_message(x_message, l_layout, x_logger_name, get_level_name(x_level)) || c_nl);

    if x_log_backtrace then
      $if dbms_db_version.version >= 11 $then pragma inline('enqueue_into_cyclic_buffer',  'YES'); $end
      enqueue_into_cyclic_buffer(x_app, dbms_utility.format_error_stack() || dbms_utility.format_error_backtrace() || c_nl);
    end if;

    if x_call_stack then
      $if dbms_db_version.version >= 11 $then pragma inline('enqueue_into_cyclic_buffer',  'YES'); $end
      enqueue_into_cyclic_buffer(x_app, format_call_stack() || c_nl);
    end if;

    $if dbms_db_version.version >= 11 $then pragma inline('get_current_appender_param',  'YES'); $end
    l_trigger_level := get_current_appender_param(x_app, c_smtp_appender, 'TRIGGER_LEVEL');
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_trigger_level: ' || l_trigger_level);  $end

    if x_level >= l_trigger_level then
      send_buffer(x_app);
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end log_smtp;

  /**
  * Procedure logs given message using dbms_output package.
  * @param x_app Application name
  * @param x_logger_name Logger name.
  * @param x_log_level Log level.
  * @param x_message Message.
  * @param x_call_stack Flag, whether to log call stack.
  * @param x_log_backtrace Flag, whether to log backtrace.
  */
  procedure log_stdout(x_app           in t_app_appender.app%type,
                       x_logger_name   in t_logger.logger%type,
                       x_level         in t_logger.log_level%type,
                       x_message       in message_type,
                       x_call_stack    in boolean,
                       x_log_backtrace in boolean) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'log_stdout'; $end
    l_layout t_app_appender.parameter_value%type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_level: ' || x_level);
      internal_log(logging.c_debug_level, l_intlogger, 'x_message: ' || x_message);
      internal_log(logging.c_debug_level, l_intlogger, 'x_call_stack: ' || bool_to_int(x_call_stack));
      internal_log(logging.c_debug_level, l_intlogger, 'x_log_backtrace: ' || bool_to_int(x_log_backtrace));
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_current_layout',  'YES'); $end
    l_layout := get_current_layout(x_app, c_dbms_output_appender);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_layout: ' || l_layout);  $end

    dbms_output.put_line(format_message(x_message, l_layout, x_logger_name, get_level_name(x_level)));

    if x_log_backtrace then
      dbms_output.put_line(dbms_utility.format_error_stack || dbms_utility.format_error_backtrace);
    end if;

    if x_call_stack then
      dbms_output.put_line(format_call_stack());
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end log_stdout;

  /**
  * Procedure logs given message to the log table.
  * @param x_app Application name
  * @param x_logger_name Logger name.
  * @param x_log_level Log level.
  * @param x_message Message.
  * @param x_call_stack Flag, whether to log call stack.
  * @param x_log_backtrace Flag, whether to log backtrace.
  */
  procedure log_table(x_app           in t_app_appender.app%type,
                      x_logger_name   in t_logger.logger%type,
                      x_level         in t_logger.log_level%type,
                      x_message       in message_type,
                      x_call_stack    in boolean,
                      x_log_backtrace in boolean) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'log_table'; $end
    pragma autonomous_transaction;
    l_backtrace        t_log.backtrace%type := null;
    l_call_stack       t_log.call_stack%type := null;
    l_timestamp        t_log.log_date%type := systimestamp;
    l_layout           t_app_appender.parameter_value%type;
    l_formated_message t_log.message%type;
    l_level_name       t_log.log_level%TYPE;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger_name: ' || x_logger_name);
      internal_log(logging.c_debug_level, l_intlogger, 'x_level: ' || x_level);
      internal_log(logging.c_debug_level, l_intlogger, 'x_message: ' || x_message);
      internal_log(logging.c_debug_level, l_intlogger, 'x_call_stack: ' || bool_to_int(x_call_stack));
      internal_log(logging.c_debug_level, l_intlogger, 'x_log_backtrace: ' || bool_to_int(x_log_backtrace));
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('get_current_layout',  'YES'); $end
    l_layout := get_current_layout(x_app, c_table_appender);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_layout: ' || l_layout);  $end

    if x_log_backtrace then
      l_backtrace := dbms_utility.format_error_stack || dbms_utility.format_error_backtrace;
    end if;

    if x_call_stack then
      l_call_stack := format_call_stack();
    end if;

    l_level_name := get_level_name(x_level);

    l_formated_message := format_message(x_message, l_layout, x_logger_name, l_level_name);
    $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_formated_message: ' || l_formated_message);  $end

    insert into t_log
      (id, logger, message, log_date, call_stack, backtrace, log_level)
    values
      (seq_log_id.nextval,
       x_logger_name,
       l_formated_message,
       l_timestamp,
       l_call_stack,
       l_backtrace,
       l_level_name);
    commit;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end log_table;

  /**
  * Procedure logs given message.
  * @param x_logger Logger settings.
  * @param x_level Log level.
  * @param x_message Message.
  * @param x_call_stack Flag, whether to log call stack.
  * @param x_log_backtrace Flag, whether to log backtrace.
  */
  procedure log(x_logger         in out nocopy logger_type,
                x_log_level      in t_logger.log_level%type,
                x_message        in message_type,
                x_log_backtrace  in boolean default null,
                x_log_call_stack in boolean default null) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'log'; $end
    l_flags pls_integer;
    l_backtrace boolean;
    l_callstack boolean;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.logger: ' || x_logger.logger);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.always_from_ctx: ' || x_logger.always_from_ctx);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.enabled_appenders: ' || x_logger.enabled_appenders);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.app: ' || x_logger.app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_level: ' || x_log_level);
      internal_log(logging.c_debug_level, l_intlogger, 'x_message: ' || x_message);
      internal_log(logging.c_debug_level, l_intlogger, 'x_call_stack: ' || bool_to_int(x_log_call_stack));
      internal_log(logging.c_debug_level, l_intlogger, 'x_log_backtrace: ' || bool_to_int(x_log_backtrace));
    $end

    if x_logger.always_from_ctx = c_true then
      -- check whether custom session settings has been requested, if yes, use them
      $if dbms_db_version.version >= 11 $then pragma inline('use_requested_session_settings',  'YES'); $end
      use_requested_session_settings(x_logger.app);

      x_logger.log_level := get_current_used_level(x_logger.logger);
      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level); $end
    end if;

    if x_logger.log_level > x_log_level then
      $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'insignificant message.');  $end
      return;
    end if;

    if x_logger.always_from_ctx = c_true then
      $if dbms_db_version.version >= 11 $then pragma inline('get_current_used_appenders',  'YES'); $end
      x_logger.enabled_appenders := get_current_used_appenders(x_logger.logger);
      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'x_logger.enabled_appenders: ' || x_logger.enabled_appenders); $end
      
      $if dbms_db_version.version >= 11 $then pragma inline('get_current_used_flags',  'YES'); $end
      l_flags := get_current_used_flags(x_logger.logger);
      x_logger.callstack := bitand(l_flags, c_flag_callstack) > 0;
      x_logger.backtrace := bitand(l_flags, c_flag_backtrace) > 0;
      $if $$debug $then internal_log(logging.c_trace_level, l_intlogger, 'l_flags: ' || l_flags); $end      
    end if;

    l_backtrace := coalesce(x_log_backtrace, x_logger.backtrace);
    l_callstack := coalesce(x_log_call_stack, x_logger.callstack);    
    $if $$debug $then
      internal_log(logging.c_trace_level, l_intlogger, 'l_backtrace: ' || l_backtrace);
      internal_log(logging.c_trace_level, l_intlogger, 'l_callstack: ' || l_callstack);
    $end

    $if dbms_db_version.version >= 11 $then pragma inline('bitand', 'YES'); $end
    $if dbms_db_version.version >= 11 $then pragma inline('get_appender_code', 'YES'); $end
    if bitand(x_logger.enabled_appenders, c_table_appender) > 0 then
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'table appender enabled.');  $end
      $if dbms_db_version.version >= 11 $then pragma inline('log_table',  'YES'); $end
      log_table(x_logger.app, x_logger.logger, x_log_level, x_message, l_callstack, l_backtrace);
    end if;

    $if dbms_db_version.version >= 11 $then pragma inline('bitand', 'YES'); $end
    $if  dbms_db_version.version >= 11 $then pragma inline('get_appender_code', 'YES'); $end
    if bitand(x_logger.enabled_appenders, c_dbms_output_appender) > 0 then
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'dbms_output appender enabled.');  $end
      $if dbms_db_version.version >= 11 $then pragma inline('log_stdout',  'YES'); $end
      log_stdout(x_logger.app, x_logger.logger, x_log_level, x_message, l_callstack, l_backtrace);
    end if;

    $if dbms_db_version.version >= 11 $then pragma inline('bitand', 'YES'); $end
    $if dbms_db_version.version >= 11 $then pragma inline('get_appender_code', 'YES'); $end
    if bitand(x_logger.enabled_appenders, c_smtp_appender) > 0 then
      $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'smtp appender enabled.');  $end
      $if dbms_db_version.version >= 11 $then pragma inline('log_smtp',  'YES'); $end
      log_smtp(x_logger.app, x_logger.logger, x_log_level, x_message, l_callstack, l_backtrace);
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end log;

  /**
  * Procedure logs message with TRACE log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  procedure trace(x_logger         in out nocopy logger_type,
                  x_message        in message_type default sqlerrm,
                  x_log_backtrace  in boolean default null,
                  x_log_call_stack in boolean default null) is
  begin
    $if dbms_db_version.version >= 11 $then pragma inline('log', 'YES'); $end
    log(x_logger, c_trace_level, x_message, x_log_backtrace, x_log_call_stack);
  end trace;

  /**
  * Procedure logs message with INFO log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  procedure info(x_logger         in out nocopy logger_type,
                 x_message        in message_type default sqlerrm,
                 x_log_backtrace  in boolean default null,
                 x_log_call_stack in boolean default null) is
  begin
    $if dbms_db_version.version >= 11 $then pragma inline('log', 'YES'); $end
    log(x_logger, c_info_level, x_message, x_log_backtrace, x_log_call_stack);
  end info;

  /**
  * Procedure logs message with DEBUG log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  procedure debug(x_logger         in out nocopy logger_type,
                  x_message        in message_type default sqlerrm,
                  x_log_backtrace  in boolean default null,
                  x_log_call_stack in boolean default null) is
  begin
    $if dbms_db_version.version >= 11 $then pragma inline('log', 'YES'); $end
    log(x_logger, c_debug_level, x_message, x_log_backtrace, x_log_call_stack);
  end debug;

  /**
  * Procedure logs message with WARN log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  procedure warn(x_logger         in out nocopy logger_type,
                 x_message        in message_type default sqlerrm,
                 x_log_backtrace  in boolean default null,
                 x_log_call_stack in boolean default null) is
  begin
    $if dbms_db_version.version >= 11 $then pragma inline('log', 'YES'); $end
    log(x_logger, c_warn_level, x_message, x_log_backtrace, x_log_call_stack);
  end warn;

  /**
  * Procedure logs message with ERROR log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  procedure error(x_logger         in out nocopy logger_type,
                  x_message        in message_type default sqlerrm,
                  x_log_backtrace  in boolean default null,
                  x_log_call_stack in boolean default null) is
  begin
    $if dbms_db_version.version >= 11 $then pragma inline('log', 'YES'); $end
    log(x_logger, c_error_level, x_message, x_log_backtrace, x_log_call_stack);
  end error;

  /**
  * Procedure logs message with FATAL log level (according to the selection rule) for given logger.
  * @param x_logger Logger settings.
  * @param x_message Message.
  * @param x_log_backtrace Flag, whether log backtrace.
  * @param x_log_call_stack Flag, whether log callstack.
  */
  procedure fatal(x_logger         in out nocopy logger_type,
                  x_message        in message_type default sqlerrm,
                  x_log_backtrace  in boolean default null,
                  x_log_call_stack in boolean default null) is
  begin
    $if dbms_db_version.version >= 11 $then pragma inline('log', 'YES'); $end
    log(x_logger, c_fatal_level, x_message, x_log_backtrace, x_log_call_stack);
  end fatal;

  /**
  * Function checks, whether the given level is enabled for for given logger.
  * @param x_logger Logger settings.
  * @param x_log_level Log level.
  * @return
  * {*} TRUE if log level for given logger is TRACE or lower.
  * {*} FALSE if log level for given logger is higher than TRACE.
  */
  function is_level_enabled(x_logger    in out nocopy logger_type,
                            x_log_level in t_logger.log_level%type) return boolean is
    $if $$debug $then l_intlogger t_logger.logger%type := 'is_trace_enabled'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.logger: ' || x_logger.logger);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.always_from_ctx: ' || x_logger.always_from_ctx);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.enabled_appenders: ' || x_logger.enabled_appenders);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.app: ' || x_logger.app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_log_level: ' || x_log_level);
    $end

    if x_logger.always_from_ctx = c_true then
      -- check whether custom session settings has been requested, if yes, use them
      $if dbms_db_version.version >= 11 $then pragma inline('use_requested_session_settings',  'YES'); $end
      use_requested_session_settings(x_logger.app);

      $if dbms_db_version.version >= 11 $then pragma inline('get_current_used_level', 'YES'); $end
      x_logger.log_level := get_current_used_level(x_logger.logger);
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
    return x_logger.log_level <= x_log_level;
  end is_level_enabled;


  /**
  * Function checks, whether TRACE log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is TRACE or lower.
  * {*} FALSE if log level for given logger is higher than TRACE.
  */
  function is_trace_enabled(x_logger in out nocopy logger_type) return boolean is
    $if $$debug $then l_intlogger t_logger.logger%type := 'is_trace_enabled'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.logger: ' || x_logger.logger);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.always_from_ctx: ' || x_logger.always_from_ctx);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.enabled_appenders: ' || x_logger.enabled_appenders);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.app: ' || x_logger.app);
    $end

    if x_logger.always_from_ctx = c_true then
      -- check whether custom session settings has been requested, if yes, use them
      $if dbms_db_version.version >= 11 $then pragma inline('use_requested_session_settings',  'YES'); $end
      use_requested_session_settings(x_logger.app);

      $if dbms_db_version.version >= 11 $then pragma inline('get_current_used_level', 'YES'); $end
      x_logger.log_level := get_current_used_level(x_logger.logger);
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
    return x_logger.log_level <= c_trace_level;
  end is_trace_enabled;

  /**
  * Function checks whether DEBUG log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is DEBUG or lower.
  * {*} FALSE if log level for given logger is higher than DEBUG.
  */
  function is_debug_enabled(x_logger in out nocopy logger_type) return boolean is
    $if $$debug $then l_intlogger t_logger.logger%type := 'is_trace_enabled'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.logger: ' || x_logger.logger);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.always_from_ctx: ' || x_logger.always_from_ctx);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.enabled_appenders: ' || x_logger.enabled_appenders);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.app: ' || x_logger.app);
    $end

    if x_logger.always_from_ctx = c_true then
      -- check whether custom session settings has been requested, if yes, use them
      $if dbms_db_version.version >= 11 $then pragma inline('use_requested_session_settings',  'YES'); $end
      use_requested_session_settings(x_logger.app);

      $if dbms_db_version.version >= 11 $then pragma inline('get_current_used_level', 'YES'); $end
      x_logger.log_level := get_current_used_level(x_logger.logger);
      $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level); $end
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
    return x_logger.log_level <= c_debug_level;
  end is_debug_enabled;

  /**
  * Function checks whether INFO log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is INFO or lower.
  * {*} FALSE if log level for given logger is higher than INFO.
  */
  function is_info_enabled(x_logger in out nocopy logger_type) return boolean is
    $if $$debug $then l_intlogger t_logger.logger%type := 'is_info_enabled'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.logger: ' || x_logger.logger);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.always_from_ctx: ' || x_logger.always_from_ctx);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.enabled_appenders: ' || x_logger.enabled_appenders);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.app: ' || x_logger.app);
    $end

    if x_logger.always_from_ctx = c_true then
      -- check whether custom session settings has been requested, if yes, use them
      $if dbms_db_version.version >= 11 $then pragma inline('use_requested_session_settings',  'YES'); $end
      use_requested_session_settings(x_logger.app);

      $if dbms_db_version.version >= 11 $then pragma inline('get_current_used_level', 'YES'); $end
      x_logger.log_level := get_current_used_level(x_logger.logger);
      $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level); $end
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
    return x_logger.log_level <= c_info_level;
  end is_info_enabled;

  /**
  * Function checks whether WARN log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is WARN or lower.
  * {*} FALSE if log level for given logger is higher than WARN.
  */
  function is_warn_enabled(x_logger in out nocopy logger_type) return boolean is
    $if $$debug $then l_intlogger t_logger.logger%type := 'is_warn_enabled'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.logger: ' || x_logger.logger);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.always_from_ctx: ' || x_logger.always_from_ctx);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.enabled_appenders: ' || x_logger.enabled_appenders);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.app: ' || x_logger.app);
    $end

    if x_logger.always_from_ctx = c_true then
      -- check whether custom session settings has been requested, if yes, use them
      $if dbms_db_version.version >= 11 $then pragma inline('use_requested_session_settings',  'YES'); $end
      use_requested_session_settings(x_logger.app);

      $if dbms_db_version.version >= 11 $then pragma inline('get_current_used_level', 'YES'); $end
      x_logger.log_level := get_current_used_level(x_logger.logger);
      $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level); $end
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
    return x_logger.log_level <= c_warn_level;
  end is_warn_enabled;

  /**
  * Function checks whether ERROR log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is ERROR or lower.
  * {*} FALSE if log level for given logger is higher than ERROR.
  */
  function is_error_enabled(x_logger in out nocopy logger_type) return boolean is
    $if $$debug $then l_intlogger t_logger.logger%type := 'is_error_enabled'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.logger: ' || x_logger.logger);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.always_from_ctx: ' || x_logger.always_from_ctx);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.enabled_appenders: ' || x_logger.enabled_appenders);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.app: ' || x_logger.app);
    $end

    if x_logger.always_from_ctx = c_true then
      -- check whether custom session settings has been requested, if yes, use them
      $if dbms_db_version.version >= 11 $then pragma inline('use_requested_session_settings',  'YES'); $end
      use_requested_session_settings(x_logger.app);

      $if dbms_db_version.version >= 11 $then pragma inline('get_current_used_level', 'YES'); $end
      x_logger.log_level := get_current_used_level(x_logger.logger);
      $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level: ' || x_logger.log_level); $end
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
    return x_logger.log_level <= c_error_level;
  end is_error_enabled;

  /**
  * Function checks, whether FATAL log message will be logged for given logger.
  * @param x_logger Logger settings.
  * @return
  * {*} TRUE if log level for given logger is FATAL or lower.
  * {*} FALSE if log level for given logger is higher than FATAL.
  */
  function is_fatal_enabled(x_logger in out nocopy logger_type) return boolean is
    $if $$debug $then l_intlogger t_logger.logger%type := 'is_fatal_enabled'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.logger: ' || x_logger.logger);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.always_from_ctx: ' || x_logger.always_from_ctx);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.enabled_appenders: ' || x_logger.enabled_appenders);
      internal_log(logging.c_debug_level, l_intlogger, 'x_logger.app: ' || x_logger.app);
    $end

    if x_logger.always_from_ctx = c_true then
      -- check whether custom session settings has been requested, if yes, use them
      $if dbms_db_version.version >= 11 $then pragma inline('use_requested_session_settings',  'YES'); $end
      use_requested_session_settings(x_logger.app);

      $if dbms_db_version.version >= 11 $then pragma inline('get_current_used_level', 'YES'); $end
      x_logger.log_level := get_current_used_level(x_logger.logger);
      $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'x_logger.log_level_severity: ' || x_logger.log_level); $end
    end if;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
    return x_logger.log_level <= c_fatal_level;
  end is_fatal_enabled;

  /** Procedure purges all global contexts used by logging */
  procedure purge_global_contexts is
    $if $$debug $then l_intlogger t_logger.logger%type := 'purge_global_contexts'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_info_level, l_intlogger, 'purging global contexts');
    $end
    for l_row in (select a.base_context_name
                    from t_appender a) loop
      clear_all_context_rac_aware(l_row.base_context_name || '_g', c_global_flag);
    end loop;
    clear_all_context_rac_aware(c_flags_ctx(c_global_flag), c_global_flag);
    clear_all_context_rac_aware(c_global_appenders_ctx, c_global_flag);
    clear_all_context_rac_aware(c_logger_levels_ctx(c_global_flag), c_global_flag);
    clear_all_context_rac_aware(c_logger_names_ctx(c_global_flag), c_global_flag);
    clear_all_context_rac_aware(c_parameters_ctx(c_global_flag), c_global_flag);
    clear_all_context_rac_aware(c_global_user_app_ctx, c_global_flag);
    clear_all_context_rac_aware(c_logger_appenders_ctx(c_global_flag), c_global_flag);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end purge_global_contexts;

  /** Procedure purges all session contexts used by logging */
  procedure purge_session_contexts is
    $if $$debug $then l_intlogger t_logger.logger%type := 'purge_session_contexts'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_info_level, l_intlogger, 'purging session contexts');
    $end
    for l_row in (select a.base_context_name
                    from t_appender a) loop
      clear_all_context_rac_aware(l_row.base_context_name || '_l', c_session_flag);
    end loop;
    clear_all_context_rac_aware(c_flags_ctx(c_session_flag), c_session_flag);
    clear_all_context_rac_aware(c_logger_appenders_ctx(c_session_flag), c_session_flag);
    clear_all_context_rac_aware(c_logger_levels_ctx(c_session_flag), c_session_flag);
    clear_all_context_rac_aware(c_logger_names_ctx(c_session_flag), c_session_flag);
    clear_all_context_rac_aware(c_parameters_ctx(c_session_flag), c_session_flag);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end purge_session_contexts;

  /** Procedure copies global setting to session settings.
  * @param x_ap p Application name. If set, copying will be done only for the given application.
  */
  procedure copy_global_to_session(x_app in t_app.app%type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'copy_global_to_session'; $end
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app' || x_app);
      internal_log(logging.c_info_level, l_intlogger, 'copying global context to session context');
    $end
    purge_session_contexts();
    init_params(c_session_flag, x_app);
    init_appenders(c_session_flag);
    init_loggers(c_session_flag, x_app);
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end copy_global_to_session;

  /**
  * Procedure enables custom settings for given session.
  * @param x_instance Id of instance for the session (e.g. from gv$session.inst_id).
  * @param x_sessionid Audit session identifier (from gv$session.audsid)
  */
  procedure apply_settings_for_session(x_instance in pls_integer,
                                       x_sessionid in number,
                                       x_setting_handle in pls_integer default 1) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'apply_settings_for_session'; $end
    l_settings ctx_value_type;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_instance: ' || x_instance);
      internal_log(logging.c_debug_level, l_intlogger, 'x_sessionid: ' || x_sessionid);
    $end

    l_settings := serialize_settings(x_setting_handle);
    $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 'l_settings: ' || l_settings); $end

    set_context_rac_aware(c_modify_session_ctx, x_instance || '#' || x_sessionid, l_settings, c_global_flag);
     $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end'); $end
  end apply_settings_for_session;

  /**
  * Procedure adds an application to the configuration.
  * @param x_app Application name.
  * @param x_app_descr Application description.
  */
  procedure add_app(x_app in t_app.app%type,
                    x_app_descr in t_app.app_desc%type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'add_app'; $end
    pragma autonomous_transaction;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
      internal_log(logging.c_debug_level, l_intlogger, 'x_app_descr: ' || x_app_descr);
    $end

    insert into t_app(app, app_desc)
    values (x_app, x_app_descr);

    commit;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end - application was added'); $end
  end add_app;

  /**
  * Procedure removes the given application and all related configuration.
  * @param x_app Application name.
  */
  procedure remove_app(x_app in t_app.app%type) is
    $if $$debug $then l_intlogger t_logger.logger%type := 'remove_app'; $end
    pragma autonomous_transaction;
  begin
    $if $$debug $then
      internal_log(logging.c_info_level, l_intlogger, 'start');
      internal_log(logging.c_debug_level, l_intlogger, 'x_app: ' || x_app);
    $end

    delete from t_app_appender aa
    where aa.app = x_app;
    $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 't_app_appender, deleted: ' || sql%rowcount); $end

    delete from t_schema_app sa
    where sa.app = x_app;
    $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 't_schema_app, deleted: ' || sql%rowcount); $end

    delete from t_param sa
    where sa.app = x_app;
    $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 't_param, deleted: ' || sql%rowcount); $end

    delete from t_logger a
    where a.logger like x_app || '.%';
    $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 't_logger, deleted: ' || sql%rowcount); $end

    delete from t_app a
    where a.app = x_app;
    $if $$debug $then internal_log(logging.c_debug_level, l_intlogger, 't_app, deleted: ' || sql%rowcount); $end


    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'clearing contexts'); $end
    for l_row in (
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
    ) loop
       set_context_rac_aware(l_row.namespace, l_row.attribute, null, c_global_flag);
    end loop;

    commit;
    $if $$debug $then internal_log(logging.c_info_level, l_intlogger, 'end - application was removed'); $end
  end remove_app;

begin
 -- these elements are defined only if internal debugging is set to true
  $if $$debug $then
    internal_log(logging.c_info_level, 'initialization', 'start');
  $end

  -- default handle (= 1)
  g_serialized_settings.extend;

  $if dbms_db_version.version >= 11 $then pragma inline('init_session_identifier', 'YES'); $end
  init_session_identifier();
  $if dbms_db_version.version >= 11 $then pragma inline('hash_logger_name', 'YES'); $end
  g_root_logger_hash := hash_logger_name(c_root_logger_name);
  $if dbms_db_version.version >= 11 $then pragma inline('init_user_app', 'YES'); $end
  init_user_app;
  $if dbms_db_version.version >= 11 $then pragma inline('init_params', 'YES'); $end
  init_params(c_global_flag);
  $if dbms_db_version.version >= 11 $then pragma inline('init_appenders', 'YES'); $end
  init_appenders(c_global_flag);
  $if dbms_db_version.version >= 11 $then pragma inline('init_loggers', 'YES'); $end
  init_loggers(c_global_flag);

  $if $$debug $then
    internal_log(logging.c_info_level, 'initialization', 'end');
  $end
end logging;
/

