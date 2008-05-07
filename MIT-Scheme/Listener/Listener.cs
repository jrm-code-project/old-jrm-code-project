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
                System.Environment.CurrentDirectory = "C:\\Documents and Settings\\jmarshall\\test\\mit-scheme\\v7\\src\\runtime\\";
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

        static void Main (string [] args)
        {
            string appName = AppDomain.CurrentDomain.FriendlyName;
            Console.WriteLine ("{0}", appName);
            Debug.Listeners.Add (new TextWriterTraceListener (Console.Out));
            Debug.WriteLine ("DEBUG build");

            Primitive.Initialize ();
            FixedObjectsVector.Initialize ();
            ColdLoad ();

            Console.WriteLine ();
            Console.WriteLine ("{0} exits", appName);
        }
    }
}
