using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Management;
using Microcode;

namespace TakTest
{
    struct Tak2Answer
    {
        bool ok;
        public int value;
        public Tak2Answer (bool ok, int value)
        {
            this.ok = ok;
            this.value = value;
        }
    }

    struct TakValAnswer
    {
        public int value;
        public TakValAnswer (int value)
        {
            this.value = value;
        }
    }

    struct TakVal2Answer
    {
        public readonly int value;
        public readonly bool ok;
        public TakVal2Answer (bool ok, int value)
        {
            this.ok = ok;
            this.value = value;
        }
    }

    delegate Answer2 Step2 ();
    delegate Answer3 Step3 ();

    struct Answer2
    {
        public readonly Step2 Next;
        public readonly object FinalAnswer;

        public Answer2 (Step2 next, object finalAnswer)
        {
            this.Next = next;
            this.FinalAnswer = finalAnswer;
        }
    }

    struct Answer3
    {
        public readonly Step3 Next;
        public readonly object Unload;
        public readonly object FinalAnswer;

        public Answer3 (Step3 next, object finalAnswer)
        {
            this.Next = next;
            this.Unload = null;
            this.FinalAnswer = finalAnswer;
        }
    }


    delegate bool TakOut3d (out TakOut3d again, out int answer);

    class Program
    {
        static int Tak (int x, int y, int z)
        {
            return (!(y < x))
                ? z
                : Tak (Tak (x - 1, y, z),
                       Tak (y - 1, z, x),
                       Tak (z - 1, x, y));
        }

        static int Tak1a (int dummy, int x, int y, int z)
        {
            return (!(y < x))
            ? z
            : Tak1a (dummy, Tak1a (dummy, x - 1, y, z),
                            Tak1a (dummy, y - 1, z, x),
                            Tak1a (dummy, z - 1, x, y));
        }

        static int Tak1b (int dummy, int x, int y, int z)
        {
            return (!(y < x))
            ? z
            : Tak1b (z, Tak1b (y, x - 1, y, z),
                        Tak1b (x, y - 1, z, x),
                        Tak1b (dummy, z - 1, x, y));
        }


        static int Tak2 (int dummy0, int dummy1, int x, int y, int z)
        {
            return (!(y < x))
            ? z
            : Tak2 (z, y, Tak2 (x, dummy1, x - 1, y, z),
                          Tak2 (dummy0, z, y - 1, z, x),
                          Tak2 (y, x, z - 1, x, y));
        }

        static int Tak3 (int dummy0, int dummy1, int dummy2, int x, int y, int z)
        {
            return (!(y < x))
            ? z
            : Tak3 (z, dummy0, y, Tak3 (dummy1, x, dummy2, x - 1, y, z),
                                  Tak3 (z, y, x, y - 1, z, x),
                                  Tak3 (dummy2, dummy1, dummy0, z - 1, x, y));
        }

        static int Tak4 (int count, int arg, int arg1, int arg2, int x, int y, int z)
        {
            return (!(y < x))
            ? z
            : Tak4 (arg, count, arg1, arg2, Tak4 (count, arg2, arg1, arg, x - 1, y, z),
                    Tak4 (arg2, count, arg1, arg, y - 1, z, x),
                    Tak4 (count, arg, arg2, arg1, z - 1, x, y));
        }


        static object TakBox (int x, int y, int z)
        {
            return (!(y < x))
            ? z
            : TakBox ((int) TakBox (x - 1, y, z),
                     (int) TakBox (y - 1, z, x),
                     (int) TakBox (z - 1, x, y));
        }

        static void TakOut (out int answer, int x, int y, int z)
        {
            if (!(y < x)) {
                answer = z;
                return;
            }
            int x1;
            int y1;
            int z1;
            TakOut (out x1, x - 1, y, z);
            TakOut (out y1, y - 1, z, x);
            TakOut (out z1, z - 1, x, y);
            TakOut (out answer, x1, y1, z1);
        }

        static bool TakOut3a (out TakOut3d again, out int answer, int x, int y, int z)
        {
            if (!(y < x)) {
                again = null;
                answer = z;
                return false;
            }
            TakOut3d xagain;
            int x1;
            TakOut3d yagain;
            int y1;
            TakOut3d zagain;
            int z1;
            for (bool a = TakOut3b (out xagain, out x1, x, y, z); a; a = xagain (out xagain, out x1)) { };
            for (bool a = TakOut3b (out yagain, out y1, y, z, x); a; a = yagain (out yagain, out y1)) { };
            for (bool a = TakOut3b (out zagain, out z1, z, x, y); a; a = zagain (out zagain, out z1)) { };
            return TakOut3a (out again, out answer, x1, y1, z1);
        }

        static bool TakOut3b (out TakOut3d again, out int answer, int x, int y, int z)
        {
            //return TakOut3a (out again, out answer, x - 1, y, z);
            answer = 0;
            again = new TakOut3d (delegate (out TakOut3d aa, out int nn) { return TakOut3a (out aa, out nn, x - 1, y, z); });
            return true;
        }

        static void TakRef (ref int answer, int x, int y, int z)
        {
            if (!(y < x)) {
                answer = z;
                return;
            }
            int x1 = 0;
            int y1 = 0;
            int z1 = 0;
            TakRef (ref x1, x - 1, y, z);
            TakRef (ref y1, y - 1, z, x);
            TakRef (ref z1, z - 1, x, y);
            TakRef (ref answer, x1, y1, z1);
        }


        static TakValAnswer TakVala (int x, int y, int z)
        {
            return new TakValAnswer ((!(y < x))
                ? z
                : TakVala (TakVala (x - 1, y, z).value,
                          TakVala (y - 1, z, x).value,
                          TakVala (z - 1, x, y).value).value);
        }

        static TakValAnswer TakValb (int x, int y, int z)
        {
            if (!(y < x))
                return new TakValAnswer (z);
            else
            return 
                  TakValb (TakValb (x - 1, y, z).value,
                          TakValb (y - 1, z, x).value,
                          TakValb (z - 1, x, y).value);
        }

        static int TakGlobAnswer;
        static void TakGlob (int x, int y, int z)
        {
            if (!(y < x)) {
                TakGlobAnswer = z;
                return;
            }
            TakGlob (x - 1, y, z);
            int x1 = TakGlobAnswer;
            TakGlob (y - 1, z, x);
            int y1 = TakGlobAnswer;
            TakGlob (z - 1, x, y);
            int z1 = TakGlobAnswer;
            TakGlob (x1, y1, z1);
        }


        public static TakVal2Answer TakVal2 (int x, int y, int z)
        {
            if (!(y < x)) {
                return new TakVal2Answer (true, z);
            }
            return TakVal2 (TakVal2 (x - 1, y, z).value,
                            TakVal2 (y - 1, z, x).value,
                            TakVal2 (z - 1, x, y).value);
        }

        static Answer2 TakAnswer2 (int x, int y, int z)
        {
            if (!(y < x))
                return new Answer2 (null, z);
            Answer2 a1 = TakAnswer2 (x - 1, y, z);
            while (a1.Next != null) {
                a1 = a1.Next ();
            };
            Answer2 a2 = TakAnswer2 (y - 1, z, x);
            while (a2.Next != null) {
                a2 = a2.Next ();
            };
            Answer2 a3 = TakAnswer2 (z - 1, x, y);
            while (a3.Next != null) {
                a3 = a3.Next ();
            };
            return new Answer2 (
                new Step2 (delegate () { return TakAnswer2 ((int)a1.FinalAnswer, (int)a2.FinalAnswer, (int)a3.FinalAnswer); }),
                null);
        }

        static Answer2 TakAnswer2a (int x, int y, int z)
        {
            if (!(y < x))
                return new Answer2 (null, z);
            Answer2 a1 = TakAnswer2b (x, y, z);
            while (a1.Next != null) {
                a1 = a1.Next ();
            };
            Answer2 a2 = TakAnswer2b (y, z, x);
            while (a2.Next != null) {
                a2 = a2.Next ();
            };
            Answer2 a3 = TakAnswer2b (z, x, y);
            while (a3.Next != null) {
                a3 = a3.Next ();
            };
            return new Answer2 (
                new Step2 (delegate () { return TakAnswer2c ((int) a1.FinalAnswer, (int) a2.FinalAnswer, (int) a3.FinalAnswer); }),
                null);
        }

        static Answer2 TakAnswer2b (int x, int y, int z)
        {
            return TakAnswer2a (x - 1, y, z);
        }

        static Answer2 TakAnswer2c (int x, int y, int z)
        {
            return TakAnswer2a (x, y, z);
        }

        static Answer3 TakAnswer3 (int x, int y, int z)
        {
            if (!(y < x))
                return new Answer3 (null, z);
            Answer3 a1 = TakAnswer3 (x - 1, y, z);
            while (a1.Next != null) {
                a1 = a1.Next ();
            };
            if (a1.Unload != null) throw new NotImplementedException ();
            Answer3 a2 = TakAnswer3 (y - 1, z, x);
            while (a2.Next != null) {
                a2 = a2.Next ();
            };
            if (a2.Unload != null) throw new NotImplementedException ();
            Answer3 a3 = TakAnswer3 (z - 1, x, y);
            while (a3.Next != null) {
                a3 = a3.Next ();
            };
            if (a3.Unload != null) throw new NotImplementedException ();
            return new Answer3 (
                new Step3 (delegate () { return TakAnswer3 ((int) a1.FinalAnswer, (int) a2.FinalAnswer, (int) a3.FinalAnswer); }),
                null);
        }




        delegate void Thunk ();


        static Dictionary<long, int> runThunk (int samples, int iterations, Thunk thunk)
        {
            Dictionary<long,int> histogram = new Dictionary<long, int> ();
            for (int i = 0; i < samples; i++) {
                Stopwatch watch = Stopwatch.StartNew ();
                for (int j = 0; j < iterations; j++)
                    thunk ();
                long ticks = watch.ElapsedTicks;
                int oldCount;
                if (histogram.TryGetValue (ticks, out oldCount)) {
                    histogram [ticks] = oldCount + 1;
                }
                else {
                    histogram [ticks] = 1;
                }
            }
            return histogram;
        }

        static SCode bootstrap;

        public static void TakTrampInit ()
        {
            string originalDirectory = System.Environment.CurrentDirectory;

            try {
                Channel.Initialize (Console.In, Console.Out);
                System.Environment.CurrentDirectory = "C:\\jrm-code-project\\TakTest\\";
                Primitive.Initialize ();
                FixedObjectsVector.Initialize ();
                bootstrap = (Fasl.Fasload ("tak.bin") as SCode).Bind (new RootBindingEnvironment (Microcode.Environment.Global));
            }
            finally {
                System.Environment.CurrentDirectory = originalDirectory;
            }
        }

        //public static int TakTramp ()
        //{
        //        Interpreter interpreter = new Interpreter ();
        //        interpreter.Start (bootstrap);
        //        return 0;
        //}

        static int TakInterpNew ()
        {
            Control expr = bootstrap;
            Microcode.Environment env = Microcode.Environment.Global;
            object answer = null;
            while (expr.EvalStep (out answer, ref expr, ref env)) { };
            return (int) answer;
 
        }

        static int Tak5 (int count, int arg, int arg1, int arg2, int arg3, int x, int y, int z)
        {
            return (!(y < x))
            ? z
            : Tak5 (arg, count, arg1, arg2, arg3, Tak5 (count, arg, arg1, arg2, arg3, x - 1, y, z),
                    Tak5 (arg, count, arg1, arg2, arg3, y - 1, z, x),
                    Tak5 (count, arg, arg1, arg2, arg3, z - 1, x, y));
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

        static string appName;

        static void Identify (TextWriter oport, string prefix)
        {
            appName = AppDomain.CurrentDomain.FriendlyName;
            // You may conduct internal benchmark testing of the .NET Framework
            // component of the OS Components (".NET Component").  You may disclose
            // the results of any benchmark test of the .NET Component, provided
            // that you comply with the following terms: 
            // (1) you must disclose all the information necessary for replication 
            // of the tests, including complete and accurate details of your benchmark
            // testing methodology, the test scripts/cases, tuning parameters applied,
            // hardware and software platforms tested, the name and version number of
            // any third-party testing tool used to conduct the testing, and complete
            // source code for the benchmark suite/harness that is developed by or for
            // you and used to test both the .NET Component and the competing 
            // implementation(s);   


            foreach (ManagementObject mo in new ManagementObjectSearcher (new SelectQuery ("Win32_ComputerSystemProduct")).Get ()) {
                oport.WriteLine ("{0}{1} {2}", prefix,
                    mo.GetPropertyValue ("Vendor").ToString (),
                    mo.GetPropertyValue ("Version").ToString ());

            }
            oport.WriteLine ("{0}{1} processor{2}", prefix, System.Environment.ProcessorCount, System.Environment.ProcessorCount == 1 ? "" : "s");
            foreach (ManagementObject proc in new ManagementObjectSearcher (new SelectQuery ("Win32_Processor")).Get ()) {
                oport.WriteLine ("{0}    {1}", prefix, proc.GetPropertyValue ("Manufacturer").ToString ());
                oport.WriteLine ("{0}    {1}", prefix, proc.GetPropertyValue ("Name").ToString ());
                oport.WriteLine ("{0}    {1}", prefix, proc.GetPropertyValue ("Description").ToString ());
                oport.WriteLine ("{0}    {1} MHz", prefix, proc.GetPropertyValue ("CurrentClockSpeed").ToString ());
            }
            oport.WriteLine ("{0}Working set:  {1}", prefix, System.Environment.WorkingSet);
            oport.WriteLine ("{0}Overflow checking is {1}abled.", prefix, (CheckOverflowChecking () ? "en" : "dis"));
#if DEBUG
            oport.WriteLine ("{0}Compiled in DEBUG mode.", prefix);
#else
            oport.WriteLine ("{0}Compiled in RELEASE mode.",prefix);
#endif
            //Debug.Listeners.Add (new TextWriterTraceListener (Console.Out));
            //Debug.WriteLine ("DEBUG build");
            oport.WriteLine ("{0}Debugger is {1}attached.", prefix, Debugger.IsAttached ? "" : "not ");
            oport.WriteLine ("{0}Stopwatch is {1}high resolution.  {2} ticks per second.", prefix, Stopwatch.IsHighResolution ? "" : "NOT", Stopwatch.Frequency);
            oport.WriteLine ("{0}",prefix);

            // (2) you must disclose the date(s) that you conducted the benchmark tests, 
            // along with specific version information for all Microsoft software products
            // tested, including the .NET Component;

            oport.WriteLine ("{0}{1}", prefix, DateTime.Now.ToUniversalTime ());
            oport.WriteLine ("{0}{1}", prefix, System.Environment.OSVersion.ToString ());
            oport.WriteLine ("{0}CLR {1}", prefix, System.Environment.Version);
            oport.WriteLine ("{0}", prefix);
            // (4) it shall be sufficient if you make the disclosures provided for above
            // at a publicly available location such as a Web site, so long as every public
            // disclosure of the results of your benchmark test expressly identifies the
            // public site containing all required disclosures;
            oport.WriteLine ("{0}See http://eval.apply.googlepages.com/ for further information.", prefix);
            oport.WriteLine ("{0}", prefix);

            oport.WriteLine ("{0}{1}", prefix, System.Reflection.Assembly.GetExecutingAssembly ());
            System.Reflection.Assembly me = System.Reflection.Assembly.GetExecutingAssembly ();
            foreach (DebuggableAttribute debuggable in me.GetCustomAttributes (typeof (DebuggableAttribute), false)) {
                oport.WriteLine ("{0}Debugging Flags: {1}", prefix,debuggable.DebuggingFlags);
                oport.WriteLine ("{0}JIT Optimizer {1}abled", prefix,debuggable.IsJITOptimizerDisabled ? "dis" : "en");
                oport.WriteLine ("{0}JIT Tracking {1}abled", prefix,debuggable.IsJITTrackingEnabled ? "en" : "dis");
            }

        }

        static long EntryKey (KeyValuePair<long, int> kvp)
        {
            return kvp.Key;
        }

        static void DumpHistogram (TextWriter oport, Dictionary<long, int> histogram, double normalization)
        {
            int  total = 0;
            foreach (KeyValuePair<long,int> kvp in histogram.OrderBy<KeyValuePair<long, int>, long> (EntryKey)) {
                total += kvp.Value;
                double seconds = (double) kvp.Key / (double) Stopwatch.Frequency;
                double nspc = seconds / normalization;
                oport.WriteLine ("{0}, {1}, {2}, {3}, {4}", kvp.Key, kvp.Value, total, seconds, nspc);
            }
        }

        static void Main (string [] args)
        {
            Identify (Console.Out, "# ");
            Console.WriteLine ("set terminal windows");
            Console.WriteLine ("set logscale x");
            Console.WriteLine ("set grid");
            Console.Write ("plot \"-\" using 4:3 with lines");
            for (int i = 0; i < 3; i++)
                Console.Write (", \"-\" using 4:3 with lines");
            Console.WriteLine ("");

            int nruns = 21;
            //DumpHistogram ("c:\\jrm-code-project\\TakTest\\takval2.txt", runThunk (1000, 10, delegate () { 
            //    TakVal2 (18, 12, 6);
            //}), .000063609);
            DumpHistogram (Console.Out, runThunk (nruns, 10, delegate() { Tak (18, 12, 6); }), .000063609);
            Console.WriteLine ("e");
            //DumpHistogram (Console.Out, runThunk (nruns, 10, delegate () { Tak1a (0, 18, 12, 6); }), .000063609);
            //Console.WriteLine ("e");
            //DumpHistogram (Console.Out, runThunk (1000, 10, delegate () { Tak1b (0, 18, 12, 6); }), .000063609);
            //Console.WriteLine ("e");
            //DumpHistogram (Console.Out, runThunk (1000, 10, delegate () { Tak2 (0, 1, 18, 12, 6); }), .000063609);
            //Console.WriteLine ("e");
            //DumpHistogram (Console.Out, runThunk (1000, 10, delegate () { Tak3 (0, 1, 2, 18, 12, 6); }), .000063609);
            //Console.WriteLine ("e");
            DumpHistogram (Console.Out, runThunk (nruns, 10, delegate () { int answer; TakOut (out answer, 18, 12, 6); }), .000063609);
            Console.WriteLine ("e");
            DumpHistogram (Console.Out, runThunk (nruns, 10, delegate ()
            {
                int answer;
                TakOut3d again;
                for (bool a = TakOut3a (out again, out answer, 18, 12, 6); 
                    a;
                    a = again (out again, out answer)) { };

            }), .000063609);
            Console.WriteLine ("e");

            //DumpHistogram (Console.Out, runThunk (1000, 10, delegate () { int answer = 0; TakRef (ref answer, 18, 12, 6); }), .000063609);
            //Console.WriteLine ("e");
            //DumpHistogram (Console.Out, runThunk (nruns, 10, delegate () { TakBox (18, 12, 6); }), .000063609);
            //Console.WriteLine ("e");
            //DumpHistogram (Console.Out, runThunk (nruns, 10, delegate () { TakValb (18, 12, 6); }), .000063609);
            //Console.WriteLine ("e");
            //DumpHistogram (Console.Out, runThunk (nruns, 10, delegate () { TakGlob (18, 12, 6); }), .000063609);
            //Console.WriteLine ("e");
            //DumpHistogram (Console.Out, runThunk (nruns, 10, delegate () { Answer2 a = TakAnswer2 (18,12,6); while (a.Next != null) {a = a.Next();}; }), .000063609);
            //Console.WriteLine ("e");
            DumpHistogram (Console.Out, runThunk (nruns, 10, delegate () { Answer2 a = TakAnswer2a (18, 12, 6); while (a.Next != null) { a = a.Next (); }; }), .000063609);
            Console.WriteLine ("e");
            DumpHistogram (Console.Out, runThunk (nruns, 10, delegate () { Answer3 a = TakAnswer3 (18, 12, 6); while (a.Next != null) { a = a.Next (); }; }), .000063609);
            Console.WriteLine ("e");
            TakTrampInit ();
            DumpHistogram (Console.Out, runThunk (nruns, 1, delegate () { TakInterpNew (); }), .000063609);
            Console.WriteLine ("e");


            //DumpHistogram (Console.Out, runThunk (nruns, 10, delegate () { TakTramp (); }), .000063609);
            //Console.WriteLine ("e");

            //DumpHistogram (Console.Out, runThunk (1000, 10, delegate () { Tak4 (0, 1, 2, 3, 18, 12, 6); }), .000063609);

            Console.WriteLine ();
            Console.WriteLine ("# {0} exits", appName);
        }
    }
}
