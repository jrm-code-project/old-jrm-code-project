@SETLOCAL
@REM
@REM  Build a ChangeSafe server for delivery.
@REM
@REM  User values of LISPWORKS_DIRECTORY, LISPWORKS_BASE_IMAGE,
@REM    DELIVERED_IMAGE_PATH, DELIVERED_IMAGE_NAME,
@REM    and DELIVERY_SCRIPT may be used to override these.
@REM
@GOTO :mainentry

:default
@IF NOT DEFINED %1 SET %1=%2
@GOTO :EOF

:getparent
@SET %1=%~dp2
@GOTO :EOF

:mainentry

@IF NOT DEFINED LISPWORKS_DIRECTORY SET LISPWORKS_DIRECTORY=D:\Program Files\Xanalys\LispWorks
@CALL :default LISPWORKS_BASE_IMAGE lispworks-4200.exe
@CALL :default DELIVERED_IMAGE_NAME Server\ChangeSafe
@CALL :default DELIVERY_SCRIPT %~dp0lwdeliver.lsp

@CALL :getparent BUILD_DIR %DELIVERY_SCRIPT%
@IF NOT DEFINED DELIVERED_IMAGE_PATH SET DELIVERED_IMAGE_PATH=%BUILD_DIR%..\Delivery

@ECHO Rebuilding ChangeSafe from %DELIVERY_SCRIPT%.
@ECHO Lispworks is %LISPWORKS_DIRECTORY%\%LISPWORKS_BASE_IMAGE%
@ECHO Build dir is %BUILD_DIR%
@ECHO Delivered image will be in %DELIVERED_IMAGE_PATH%\%DELIVERED_IMAGE_NAME%.exe

@PUSHD "%LISPWORKS_DIRECTORY%"
@START "Rebuild ChangeSafe" /WAIT "%LISPWORKS_BASE_IMAGE%" -console -siteinit - -init "%DELIVERY_SCRIPT%" -image "%DELIVERED_IMAGE_PATH%\%DELIVERED_IMAGE_NAME%.exe"
@POPD
@ECHO Done.
@ENDLOCAL
