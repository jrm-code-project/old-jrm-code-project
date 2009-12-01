@REM Resyntax MIT scheme and copy over to BootstrapLib
@SETLOCAL

@SET AUXILIARY=C:\jrm-code-project\MIT-Scheme\Auxiliary\
@SET BOOTSTRAP_LIB=C:\jrm-code-project\MIT-Scheme\BootstrapLib\
@SET FINAL_LIB=C:\jrm-code-project\MIT-Scheme\lib\
@SET SCHEME_BAND=all.com
@SET SCHEME_DIRECTORY="C:\Program Files\MIT-GNU Scheme"
@SET SCHEME_EXECUTABLE="C:\Program Files\MIT-GNU Scheme\bin\scheme.exe"
@SET SCHEME_HEAP=5000
@SET SCHEME_LIBRARY="C:\Program Files\MIT-GNU Scheme\lib"
@SET SCHEME_SRC=C:\GitRepository\mit-scheme\src\
@SET STAGING_LIB=C:\jrm-code-project\MIT-Scheme\StagingLib\

@REM Save the site.scm file
@COPY /Y %BOOTSTRAP_LIB%Runtime\site.scm %TEMP%

@REM Copy the runtime
@DEL %BOOTSTRAP_LIB%Runtime\* /Q
@COPY /Y %SCHEME_SRC%runtime\*.scm %BOOTSTRAP_LIB%Runtime
@COPY /Y %SCHEME_SRC%runtime\runtime.* %BOOTSTRAP_LIB%Runtime

@REM Restore the site.scm file
@MOVE /Y %TEMP%\site.scm %BOOTSTRAP_LIB%Runtime\site.scm

@REM Copy SF
@DEL %BOOTSTRAP_LIB%Sf\* /Q
@COPY /Y %SCHEME_SRC%sf\*.scm %BOOTSTRAP_LIB%Sf
@COPY /Y %SCHEME_SRC%sf\sf.* %BOOTSTRAP_LIB%Sf

@REM Copy Cref
@DEL %BOOTSTRAP_LIB%Cref\* /Q
@COPY /Y %SCHEME_SRC%cref\*.scm %BOOTSTRAP_LIB%Cref
@COPY /Y %SCHEME_SRC%cref\cref.* %BOOTSTRAP_LIB%Cref
@COPY /Y %SCHEME_SRC%cref\triv.pkg %BOOTSTRAP_LIB%Cref

@REM Copy star-parser
@DEL %BOOTSTRAP_LIB%star-parser\* /Q
@COPY /Y %SCHEME_SRC%star-parser\*.scm %BOOTSTRAP_LIB%Star-parser
@COPY /Y %SCHEME_SRC%star-parser\parser.pkg %BOOTSTRAP_LIB%Star-parser

@REM Save the site.scm file
@COPY /Y %STAGING_LIB%Runtime\site.scm %TEMP%

@REM Copy the runtime
@DEL %STAGING_LIB%Runtime\* /Q
@COPY /Y %SCHEME_SRC%runtime\*.scm %STAGING_LIB%Runtime
@COPY /Y %SCHEME_SRC%runtime\runtime.* %STAGING_LIB%Runtime

@REM Restore the site.scm file
@MOVE /Y %TEMP%\site.scm %STAGING_LIB%Runtime\site.scm

@REM Copy SF
@DEL %STAGING_LIB%Sf\* /Q
@COPY /Y %SCHEME_SRC%sf\*.scm %STAGING_LIB%Sf
@COPY /Y %SCHEME_SRC%sf\sf.* %STAGING_LIB%Sf

@REM Copy Cref
@DEL %STAGING_LIB%Cref\* /Q
@COPY /Y %SCHEME_SRC%cref\*.scm %STAGING_LIB%Cref
@COPY /Y %SCHEME_SRC%cref\cref.* %STAGING_LIB%Cref
@COPY /Y %SCHEME_SRC%cref\triv.pkg %STAGING_LIB%Cref

@REM Copy star-parser
@DEL %STAGING_LIB%star-parser\* /Q
@COPY /Y %SCHEME_SRC%star-parser\*.scm %STAGING_LIB%Star-parser
@COPY /Y %SCHEME_SRC%star-parser\parser.pkg %STAGING_LIB%Star-parser

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
@START "Scheme" /D %SCHEME_DIRECTORY% /WAIT %SCHEME_EXECUTABLE% --library %SCHEME_LIBRARY% --band %SCHEME_BAND% --heap %SCHEME_HEAP% --no-init-file --load %AUXILIARY%bootstrap.scm

@ENDLOCAL
