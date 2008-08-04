using Microcode;
using System;
using System.Diagnostics;

namespace Listener
{
    class Listener
    {
        static void ColdLoad ()
        {
            string originalDirectory = System.Environment.CurrentDirectory;
            try {
                // System.Environment.CurrentDirectory = System.Environment.CurrentDirectory + "\\..\\..\\..\\Runtime7\\";
                Channel.Initialize (Console.In, Console.Out);
                System.Environment.CurrentDirectory = "C:\\Program Files\\MIT\\src\\runtime\\";
                SCode bootstrap = Fasl.Fasload ("make.bin") as SCode;
                Interpreter interpreter = new Interpreter ();
                Termination term = interpreter.Start (bootstrap);
                Console.WriteLine (term.Message);
            }
            finally
            {
                System.Environment.CurrentDirectory = originalDirectory;
            }
        }

        static bool CheckOverflowChecking ()
        {
            int i = 2;
            while (i > 0) {
                try {
                    i += i;
                }
                catch (OverflowException) {
                    return true;
                }
            }
            return false;
        }

        static void Main (string [] args)
        {
            string appName = AppDomain.CurrentDomain.FriendlyName;
            Console.WriteLine ("{0}", appName);
            Debug.Listeners.Add (new TextWriterTraceListener (Console.Out));
            Debug.WriteLine ("DEBUG build");

            Debug.WriteLine (CheckOverflowChecking ()
                ? "Overflow checking is enabled."
                : "Overflow checking is disabled.");

            Primitive.Initialize ();
            FixedObjectsVector.Initialize ();
            ColdLoad ();

            Console.WriteLine ();
            Console.WriteLine ("{0} exits", appName);
        }
    }
}
