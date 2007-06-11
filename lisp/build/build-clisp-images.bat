@SETLOCAL
@REM  This file rebuilds the memory image of CLisp.  The climage.lsp
@REM  file is run in the base image that comes with the CLisp
@REM  distribution.  It configures the compiler, package system, and
@REM  base readtable, patches any Lisp bugs, and loads base
@REM  third-party code like the series package and ASDF.
@REM
@REM  User values of CLISP_DIRECTORY, CLISP_BASE_IMAGE,
@REM    DERIVED_IMAGE_PATH, DERIVED_IMAGE_NAME, and BUILD_SCRIPT may
@REM    be used to override the built-in names.
@REM
@REM  Author: jrm
@REM
@GOTO :mainentry

:default
@IF NOT DEFINED %1 SET %1=%2
@GOTO :EOF

:getparent
@SET %1=%~dp2
@GOTO :EOF

:mainentry
@IF NOT DEFINED CLISP_DIRECTORY SET CLISP_DIRECTORY=C:\Program Files\clisp-2.41
@CALL :default CLISP_BASE_IMAGE clisp.exe
@IF NOT DEFINED DERIVED_IMAGE_PATH SET DERIVED_IMAGE_PATH=%CLISP_DIRECTORY%
@CALL :default DERIVED_IMAGE_NAME clisp-devel
@CALL :default BUILD_SCRIPT %~dp0climage.lsp

@CALL :getparent BUILD_DIR %BUILD_SCRIPT%

@ECHO Rebuilding lisp environment from %BUILD_SCRIPT%.
@ECHO CLisp is %CLISP_DIRECTORY%\%CLISP_BASE_IMAGE%
@ECHO Build dir is %BUILD_DIR%
@ECHO Build script is %BUILD_SCRIPT%

@PUSHD "%CLISP_DIRECTORY%"
@ECHO Rebuilding image %DERIVED_IMAGE_NAME%
@START "Rebuild Lisp" /WAIT "%CLISP_BASE_IMAGE%" -M base\lispinit.mem -m 128MB -norc "%BUILD_SCRIPT%" "%CLISP_DIRECTORY%\base\%DERIVED_IMAGE_NAME%"
@POPD
@ECHO Done.

@ENDLOCAL
