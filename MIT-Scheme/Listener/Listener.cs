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
                Object answer;
                if (true) {
                    // Load a band
                    System.Environment.CurrentDirectory = "C:\\jrm-code-project\\MIT-Scheme\\Runtime\\";
                    try {
                        System.IO.FileStream input = System.IO.File.OpenRead ("C:\\jrm-code-project\\MIT-Scheme\\foo.band");
                        System.Runtime.Serialization.Formatters.Binary.BinaryFormatter bfmt = new System.Runtime.Serialization.Formatters.Binary.BinaryFormatter ();
                        WorldState ws = (WorldState) bfmt.Deserialize (input);
                        answer = Continuation.Initial (ws);
                    }

                    catch (System.IO.FileNotFoundException) {
                        // Cold load the Scheme runtime
                        SCode bootstrap = Fasl.Fasload ("make.bin") as SCode;
                        Microcode.Environment initial = Microcode.Environment.Global;
                       answer = Continuation.Initial (bootstrap.Bind(new RootBindingEnvironment (initial)), initial);
                    }
                    Console.WriteLine ("Evaluation exited with {0}", answer);
                }
                else {
                    // Tak
                    System.Environment.CurrentDirectory = "C:\\jrm-code-project\\TakTest\\";
                    SCode tak = Fasl.Fasload ("tak.bin") as SCode;
                    Microcode.Environment initial = Microcode.Environment.Global;
                    SCode tak1 = tak.Bind (new RootBindingEnvironment (initial));

                    //for (int i = 0; i < 20; i++) {
                    //    ControlState ctl = new ControlState (tak, initial);
                    //    ctl.expression = tak;
                    //    ctl.environment = initial;
                    //    Stopwatch takWatch = Stopwatch.StartNew ();
                    //    object result = null;
                    //    while (ctl.expression.Eval (ref result, ref ctl)) { };
                    //    if (result == Interpreter.CaptureContinuation) throw new NotImplementedException ();
                    //    long ticks = takWatch.ElapsedTicks;
                    //    Console.WriteLine ("Ticks {0}", ticks);

                    //}
                    //Console.WriteLine ("e");

                    for (int i = 0; i < 20; i++) {
                        Control expr = tak1;
                        Microcode.Environment env = initial;
                        answer = null;
                        Stopwatch takWatch = Stopwatch.StartNew ();
                        while (expr.EvalStep (out answer, ref expr, ref env)) { };
                        if (answer == Interpreter.UnwindStack) throw new NotImplementedException ();
                        //TakLoop (tak, initial);

                        long ticks = takWatch.ElapsedTicks;
                        Console.WriteLine ("Ticks {0}", ticks);
                    }
                    //Console.WriteLine ("e");
                    //for (int i = 0; i < 20; i++) {
                    //    Stopwatch takWatch1 = Stopwatch.StartNew ();
                    //    PartialResult result = tak.Eval (Microcode.Environment.Global);
                    //    while (result.Continue != null) { result = result.Continue.Step (); }
                    //    if (result.CaptureContinuation != null) throw new NotImplementedException ();
                    //    long ticks = takWatch1.ElapsedTicks;
                    //    Console.WriteLine ("Ticks {0}", ticks);
                    //}
                }

                // Interpreter interpreter = new Interpreter ();
                //PartialResult result = bootstrap.Eval (Microcode.Environment.Global);
                //while (result.Continue != null) { result = result.Continue.Step(); }
                //if (result.CaptureContinuation != null) throw new NotImplementedException ();
                //Console.WriteLine ("Returned with {0}", result.Value);
                //                Termination term = interpreter.Start (bootstrap);
                //                Console.WriteLine (term.Message);
            }
            finally {
                System.Environment.CurrentDirectory = originalDirectory;
            }
        }

        private static void TakLoop (Control tak, Microcode.Environment initial)
        {
            object answer = null;
            while (tak.EvalStep (out answer, ref tak, ref initial)) { };
            if ((int) answer != 7)
                throw new NotImplementedException ();
            return;
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
