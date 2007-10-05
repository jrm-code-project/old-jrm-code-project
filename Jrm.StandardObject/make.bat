"c:\Program Files\Microsoft Visual Studio 8\SDK\v2.0\Bin\ildasm.exe" /all "c:\jrm-code-project\Jrm.Lisp\bin\Debug\Jrm.Lisp.dll" /out=Jrm.Lisp.il
"c:\Program Files\Microsoft Visual Studio 8\SDK\v2.0\Bin\ildasm.exe" /all "c:\jrm-code-project\Jrm.StandardObject\bin\Debug\Jrm.StandardObject.dll" /out=Jrm.StandardObject.il
gacutil /u Jrm.StandardObject
"C:\WINDOWS\Microsoft.NET\Framework\v2.0.50727\ilasm.exe" /dll StandardObject.il /keyfile="LispKeyFile.snk" /debug /output="Jrm.StandardObject.dll"
gacutil /i Jrm.StandardObject.dll



