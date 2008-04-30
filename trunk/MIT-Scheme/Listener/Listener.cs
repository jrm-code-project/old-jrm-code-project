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
                System.Environment.CurrentDirectory = System.Environment.CurrentDirectory + "\\..\\..\\..\\Runtime\\";
                SCode bootstrap = Fasl.Fasload ("make.bin") as SCode;
                Debug.WriteLine ("Initial form " + bootstrap.ToString ());
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

            ColdLoad ();

            Console.WriteLine ();
            Console.WriteLine ("{0} exits", appName);
        }
    }
}
