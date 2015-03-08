create or replace package logging_settings is
  /**
  * Settings for logger package.
  * Implementation of log4j for PL/SQL.
  * Licence: MIT License (@see license.txt)
  * {*} web                 http://radino.eu
  * {*} email, gtalk(XMPP)  radoslav.golian@gmail.com, rgolian@gmail.com
  * {*} facebook            http://www.facebook.com/radoslav.golian
  * {*} twitter             http://twitter.com/radoslavgolian
  * (*) project page        https://github.com/radino/logger4plsql/
  * @author Radoslav Golian
  */

  /** 
  *  Precompiler setting for debugging.
  *  If set then the package is compiled with internal debugging.
  */
  c_precompiler_debug CONSTANT BOOLEAN := TRUE;

  /** 
  *  Precompiler setting for unit testing.
  *  If set then the package is compiled with all methods set to public to support 
  *  unit testing.
  */
  c_precompiler_unit_test CONSTANT BOOLEAN := TRUE;

end logging_settings;
/

