"c:\Program Files\Microsoft Visual Studio 8\SDK\v2.0\Bin\ildasm.exe" /all "c:\jrm-code-project\Jrm.Lisp\bin\Debug\Jrm.Lisp.dll" /out=Jrm.Lisp.il
gacutil /u Jrm.StandardObject
"C:\WINDOWS\Microsoft.NET\Framework\v2.0.50727\ilasm.exe" /dll StandardObject.il /keyfile="C:\jrm-code-project\Jrm.Lisp\LispKeyFile.snk" /output="Jrm.StandardObject.dll"
gacutil /i Jrm.StandardObject.dll

