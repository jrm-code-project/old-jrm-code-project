@SETLOCAL
@REM  This file rebuilds the memory image of Lispworks.  The lwimage.lsp
@REM  file is run in the base image that comes with the Lispworks
@REM  distribution.  It configures the compiler, package system, and
@REM  base readtable, patches any Lisp bugs, and loads base
@REM  third-party code like the series package and ASDF.
@REM
@REM  User values of LISPWORKS_DIRECTORY,  LISPWORKS_BASE_IMAGE,
@REM    DERIVED_IMAGE_PATH, DERIVED_IMAGE_NAME,
@REM    and BUILD_SCRIPT may be used to override the built-in names.
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
@IF NOT DEFINED LISPWORKS_DIRECTORY SET LISPWORKS_DIRECTORY=C:\Program Files\Xanalys\LispWorks
@CALL :default LISPWORKS_BASE_IMAGE lispworks-4200.exe
@IF NOT DEFINED DERIVED_IMAGE_PATH SET DERIVED_IMAGE_PATH=%LISPWORKS_DIRECTORY%
@CALL :default DERIVED_IMAGE_NAME lisp-devel
@CALL :default DERIVED_CONSOLE_IMAGE_NAME %DERIVED_IMAGE_NAME%-console
@CALL :default DERIVED_GUI_IMAGE_NAME %DERIVED_IMAGE_NAME%-gui
@CALL :default BUILD_SCRIPT %~dp0lwimage.lsp

@CALL :getparent BUILD_DIR %BUILD_SCRIPT%

@ECHO Rebuilding lisp environment from %BUILD_SCRIPT%.
@ECHO Lispworks is %LISPWORKS_DIRECTORY%\%LISPWORKS_BASE_IMAGE%
@ECHO Build dir is %BUILD_DIR%
@ECHO Build script is %BUILD_SCRIPT%

@PUSHD "%LISPWORKS_DIRECTORY%"
@ECHO Rebuilding GUI image %DERIVED_GUI_IMAGE_NAME%
@START "Rebuild Lisp" /WAIT "%LISPWORKS_BASE_IMAGE%" -siteinit - -init "%BUILD_SCRIPT%" -image "%DERIVED_IMAGE_PATH%\%DERIVED_GUI_IMAGE_NAME%.exe"
@ECHO Rebuilding console image %DERIVED_CONSOLE_IMAGE_NAME%
@START "Rebuild Lisp" /WAIT "%LISPWORKS_BASE_IMAGE%" -siteinit - -init "%BUILD_SCRIPT%" -console -image "%DERIVED_IMAGE_PATH%\%DERIVED_CONSOLE_IMAGE_NAME%.exe"
@POPD
@ECHO Done.

@ENDLOCAL
