@REM Clear lib and run MIT scheme from Staging lib to resyntax.
@SETLOCAL

@SET AUXILIARY=C:\jrm-code-project\MIT-Scheme\Auxiliary\
@SET FINAL_LIB=C:\jrm-code-project\MIT-Scheme\lib\
@SET SCHEME_BAND=all.com
@SET SCHEME_DIRECTORY="C:\jrm-code-project\MIT-Scheme\Listener\bin\Release"
@SET SCHEME_EXECUTABLE="C:\jrm-code-project\MIT-Scheme\Listener\bin\Release\Listener.exe"
@SET SCHEME_SRC="C:\GitRepository\mit-scheme\src\"

@REM Save the site.scm file
@COPY /Y %FINAL_LIB%Runtime\site.scm %TEMP%

@REM Copy the runtime
@DEL %FINAL_LIB%Runtime\* /Q
@COPY /Y %SCHEME_SRC%runtime\*.scm %FINAL_LIB%Runtime
@COPY /Y %SCHEME_SRC%runtime\runtime.* %FINAL_LIB%Runtime

@REM Restore the site.scm file
@MOVE /Y %TEMP%\site.scm %FINAL_LIB%Runtime\site.scm

@REM Copy SF
@DEL %FINAL_LIB%Sf\* /Q
@COPY /Y %SCHEME_SRC%sf\*.scm %FINAL_LIB%Sf
@COPY /Y %SCHEME_SRC%sf\sf.* %FINAL_LIB%Sf

@REM Copy Cref
@DEL %FINAL_LIB%Cref\* /Q
@COPY /Y %SCHEME_SRC%cref\*.scm %FINAL_LIB%Cref
@COPY /Y %SCHEME_SRC%cref\cref.* %FINAL_LIB%Cref
@COPY /Y %SCHEME_SRC%cref\triv.pkg %FINAL_LIB%Cref

@REM Copy star-parser
@DEL %FINAL_LIB%star-parser\* /Q
@COPY /Y %SCHEME_SRC%star-parser\*.scm %FINAL_LIB%Star-parser
@COPY /Y %SCHEME_SRC%star-parser\parser.pkg %FINAL_LIB%Star-parser

@REM Syntax
@START "Scheme" /D %SCHEME_DIRECTORY% %SCHEME_EXECUTABLE%

@ENDLOCAL
