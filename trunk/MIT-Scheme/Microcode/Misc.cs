using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using Microsoft.Win32;
using System.Text;

namespace Microcode
{
    static class Misc
    {

        public static UInt64 Abs64 (long n)
        {
            return (n < 0)
            ? (UInt64) (~n) + 1UL
            : (UInt64) n;
        }

        public static UInt64 Abs64 (int n)
        {
            return (n < 0)
            ? (UInt64) (~n) + 1UL
            : (UInt64) n;
        }

        public static UInt32 Abs32 (int n)
        {
            return (n < 0)
            ? (UInt32) (~n) + 1U
            : (UInt32) n;
        }

        public static object [] Remove (object item, object [] collection)
        {
            int count = 0;
            for (int i = 0; i < collection.Length; i++)
                if (collection [i] == item)
                    count += 1;
            if (count == 0)
                return collection;
            if (count == collection.Length)
                return new object [0];
            else {
                object [] answer = new object [collection.Length - count];
                int ptr = 0;
                for (int i = 0; i < collection.Length; i++)
                    if (collection [i] != item)
                        answer [ptr++] = collection [i];
                return answer;
            }
        }
     

        [SchemePrimitive ("BATCH-MODE?", 0, true)]
        public static bool IsBatchMode (out object answer)
        {
            answer = false;
            return false;
        }

        [SchemePrimitive ("COMPILED-ENTRY?", 1, true)]
        public static bool IsCompiledEntry (out object answer, object arg)
        {
            answer = false;
            return false;
        }

        [SchemePrimitive ("CONSTANT?", 1, true)]
        public static bool IsConstant (out object answer, object arg)
        {
            answer = System.GC.GetGeneration (arg) > 0;
            return false;
        }

        [SchemePrimitive ("DECODE-TIME", 2, false)]
        public static bool DecodeTime (out object answer, object arg0, object arg1)
        {
            object [] vec = (object []) arg0;
            long time = (long) arg1;
            DateTime ts = DateTime.FromBinary (time);
            vec [1] = ts.Second;
            vec [2] = ts.Minute;
            vec [3] = ts.Hour;
            vec [4] = ts.Day;
            vec [5] = ts.Month;
            vec [6] = ts.Year;
            vec [7] = (int) ts.DayOfWeek;
            vec [8] = Constant.sharpF;
            vec [9] = Constant.sharpF;
            answer = Constant.Unspecific;
            return false;
        }

        [SchemePrimitive ("DUMP-BAND", 2, false)]
        public static bool DumpBand (out object answer, object arg0, object arg1)
        {
            ControlPoint cp = (ControlPoint) arg0;  
            string filename = new string ((char []) arg1);

            System.IO.FileStream output = System.IO.File.OpenWrite ("C:\\jrm-code-project\\MIT-Scheme\\foo.band");
            System.Runtime.Serialization.Formatters.Binary.BinaryFormatter bfmt = new System.Runtime.Serialization.Formatters.Binary.BinaryFormatter ();
            bfmt.Serialize (output, new WorldState (cp, Environment.Global));
            output.Close ();

            // Create an initial unwinder state.  After reload, we resume at
            // WithinControlPointFrame.
            //UnwinderState env = new UnwinderState ((ControlPoint) cp, new RestoreBandFrame ());

            // Return from the primitive with instructions to unwind the stack.
            //answer = new TailCallInterpreter (Interpreter.UnwindStackExpression, env);
            //return true;

            // copacetic
            answer = Constant.sharpT;
            return false;
        }

        [SchemePrimitive ("ENCODED-TIME", 0, true)]
        public static bool EncodedTime (out object answer)
        {
            DateTime now = DateTime.UtcNow;
            answer = now.ToBinary ();
            return false;
        }

        [SchemePrimitive ("FILE-ACCESS", 2, false)]
        public static bool FileAccess (out object answer, object arg0, object arg1)
        {
            string filename = new String ((char []) arg0);
            int mode = (int) arg1;
            answer = System.IO.File.Exists (filename);
            return false;
        }

        [SchemePrimitive ("FILE-EXISTS?", 1, false)]
        public static bool IsFileExists (out object answer, object arg)
        {
            answer = System.IO.File.Exists (new String ((char []) arg));
            return false;
        }

        //[SchemePrimitive ("FIND-SYMBOL", 1)]
        //public static bool FindSymbol (out object answer, object arg)
        //{
        //    answer = (String.IsInterned (new String ((char []) arg)));
        //    return false;
        //}

        [SchemePrimitive ("FILE-MOD-TIME", 1, false)]
        public static bool FileModTime (out object answer, object arg)
        {
            String filename = new String ((char []) arg);
            answer = System.IO.File.GetLastWriteTimeUtc (filename).ToFileTimeUtc ();
            return false;
        }

        [SchemePrimitive ("FILE-TYPE-INDIRECT", 1, false)]
        public static bool FileTypeIndirect (out object answer, object arg)
        {
            String filename = new String ((char []) arg);
            System.IO.FileAttributes fa;
            try {
                fa = System.IO.File.GetAttributes (filename);
                if ((fa & System.IO.FileAttributes.Directory) == System.IO.FileAttributes.Directory)
                    answer = 1;
                else {
                    answer = 0;
                }
            }
            catch (System.IO.FileNotFoundException) {
                answer = false;
            }

            return false;
        }

        [SchemePrimitive ("GARBAGE-COLLECT", 1, true)]
        public static bool GarbageCollect (out object answer, object arg)
        {
            // Arg is the safety margin.
            answer = GC.GetTotalMemory (true);
            return false;
        }

        [SchemePrimitive ("GC-SPACE-STATUS", 0, true)]
        public static bool GcSpaceStatus (out object answer)
        {
            object [] status =  {4,
                29114368,
                33318628,
                33319140,
                33766628,
                58149296,
                58167296,
                28704768,
                29114232,
                28721152,
                29114368};
            //answer [0] = 4; // bytes per object
            //answer [1] = ; // constant-start
            //answer [2] = 0x200; // constant-alloc-next
            //answer [3] = 0x400; // constant-end
            //answer [4] = 0x1000; // heap-start
            //answer [5] = 0x20000000; // free
            //answer [6] = 0x40000000; // heap-alloc-limit
            //answer [7] = 0x50000000; // heap-end
            //answer [8] = 0x90000000; // stack start
            //answer [9] = 0x100000000; //stack pointer
            //answer [10] = 0x200000000; // stack guard
            //answer [11] = 0x400000000; // stack end
            answer = status;
            return false;
        }

        [SchemePrimitive ("GET-ENVIRONMENT-VARIABLE", 1, false)]
        public static bool GetEnvironmentVariable (out object answer, object arg)
        {
            char [] name = (char []) arg;
            string sname = new string (name);
            string temp = System.Environment.GetEnvironmentVariable (sname);
            answer = (temp == null
                ? (object) false
                : (object) temp.ToCharArray ());
            return false;
        }

        //[SchemePrimitive ("GET-NEXT-CONSTANT", 0)]
        //public static bool GetNextConstant (out object answer)
        //{
        //    answer = (0);
        //    return false;
        //}

        [SchemePrimitive ("GET-UNUSED-COMMAND-LINE", 0, true)]
        public static bool GetUnusedCommandLine (out object answer)
        {
            answer = Constant.sharpF;
            return false;
        }

        [SchemePrimitive ("HAVE-SELECT?", 0, true)]
        public static bool HaveSelect (out object answer)
        {
            answer = false;
            return false;
        }

        [SchemePrimitive ("INITIALIZE-C-COMPILED-BLOCK", 1, false)]
        public static bool InitializeCCompiledBlock (out object answer, object arg)
        {
            // what is arg?
            answer = false;
            return false;
        }

        [SchemePrimitive ("MICROCODE-IDENTIFY", 0, true)]
        public static bool MicrocodeIdentify (out object answer)
        {
            answer = new object [] {false,
                "15.1".ToCharArray(),
                false,
                80,
                20,
                '\n',
                53,
                Double.Epsilon,
            "NT".ToCharArray(),
            "MS Windows".ToCharArray(),
            "standard".ToCharArray(),
            "IA-32".ToCharArray(),
            "i386".ToCharArray(),
            false, false, false, false, false ,false, false};
            return false;
        }

        [SchemePrimitive ("MICROCODE-LIBRARY-PATH", 0, true)]
        public static bool MicrocodeLibraryPath (out object answer)
        {
            answer = new object [] { "C:\\Program Files\\MIT\\lib\\".ToCharArray () };
            return false;

        //    //return new object [] {"Program Files".ToCharArray(),
        //    //    "MIT".ToCharArray(),
        //    //    "scheme-7.7.1".ToCharArray(),
        //    //    "lib".ToCharArray()});
        }


        [SchemePrimitive ("MICROCODE-SYSTEM-CALL-NAMES", 0, true)]
        public static bool MicrocodeSystemCallNames (out object answer)
        {
            answer = FixedObjectsVector.SyscallNames;
            return false;
        }

        [SchemePrimitive ("MICROCODE-SYSTEM-ERROR-NAMES", 0, true)]
        public static bool MicrocodeSystemErrorNames (out object answer)
        {
            answer = FixedObjectsVector.SyserrNames;
            return false;
        }


        [SchemePrimitive ("MICROCODE-TABLES-FILENAME", 0, true)]
        public static bool MicrocodeTablesFilename ( out object answer)
        {
            answer = "utabmd.bin".ToCharArray ();
            return false;
        }

        [SchemePrimitive ("NT-GET-VOLUME-INFORMATION", 1, false)]
        public static bool NTGetVolumeInformation (out object answer, object arg)
        {
            if (((char []) arg).ToString ().Equals("c:\\", StringComparison.InvariantCultureIgnoreCase))
                answer = Constant.sharpF;
            else {
                object [] temp = { "".ToCharArray(),
                                   3260986805,
                                   255,
                                   2556159,
                                   "NTFS".ToCharArray()};
                answer = temp;
            }
                return false;
        }

        //[SchemePrimitive ("PURE?", 1)]
        //public static bool IsPure (out object answer, object arg)
        //{
        //    answer = (GC.GetGeneration (arg) >= 2);
        //    return false;
        //}

        [SchemePrimitive ("PRIMITIVE-FASDUMP", 3, false)]
        public static bool PrimitiveFasdump (out object answer, object arg0, object arg1, object arg2)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("PRIMITIVE-PURIFY", 3, true)]
        public static bool PrimitivePurify (out object answer, object obj, object isPure, object space)
        {
            answer = new Cons (true, 0x1000);
            return false;
        }

        static DateTime UnixEpochStart = new DateTime (1971, 1, 1, 0, 0, 0);

        /// <summary>
        /// Return the current real time in units of milliseconds.
        /// </summary>
        [SchemePrimitive ("REAL-TIME-CLOCK", 0, true)]
        public static bool RealTimeClock (out object answer)
        {
            answer = (long) DateTime.UtcNow.Subtract (UnixEpochStart).TotalMilliseconds;
            return false;
        }

        [SchemePrimitive ("RELOAD-RETRIEVE-STRING", 0, true)]
        public static bool ReloadRetrieveString (out object answer)
        {
            answer = "I'm the reload retrieve string!".ToCharArray ();
            return false;
        }

        [SchemePrimitive ("REQUEST-INTERRUPTS!", 1, false)]
        public static bool RequestInterrupts (out object answer, object arg)
        {
            answer = Constant.Unspecific;
            return false;
        }

        static Stopwatch systemTime = Stopwatch.StartNew ();
        [SchemePrimitive ("SYSTEM-CLOCK", 0, true)]
        public static bool SystemClock (out object answer)
        {
            answer = systemTime.ElapsedMilliseconds;
            return false;
        }

        [SchemePrimitive ("TERMINAL-BUFFERED", 1, true)]
        public static bool TerminalBuffered (out object answer, object arg)
        {
            answer = true;
            return false;
        }

        [SchemePrimitive ("TERMINAL-BUFFERED?", 1, true)]
        public static bool IsTerminalBuffered (out object answer, object arg)
        {
            answer = true;
            return false;
        }

        [SchemePrimitive ("TERMINAL-COOKED-OUTPUT", 1, false)]
        public static bool TerminalCookedOutput (out object answer, object arg)
        {
            answer = true;
            return false;
        }

        [SchemePrimitive ("TERMINAL-COOKED-OUTPUT?", 1, true)]
        public static bool TerminalCookedOutputP (out object answer, object arg)
        {
            answer = true;
            return false;
        }

        [SchemePrimitive ("TTY-X-SIZE", 0, true)]
        public static bool TtyXSize (out object answer)
        {
            answer = 80;
            return false;
        }

        [SchemePrimitive ("UNDER-EMACS?", 0, true)]
        public static bool IsUnderEmacs (out object answer)
        {
            answer = Constant.sharpF;
            return false;
        }

        [SchemePrimitive ("WIN32-PREDEFINED-REGISTRY-KEYS", 0, true)]
        public static bool Win32PredefinedRegistryKeys (out object answer)
        {
            answer = (
                new Cons (
                new Cons (string.Intern ("HKEY_CLASSES_ROOT"), null), //Registry.ClassesRoot),
                new Cons (
                new Cons (string.Intern ("HKEY_CURRENT_USER"), null), //Registry.CurrentUser),
                new Cons (
                new Cons (string.Intern ("HKEY_LOCAL_MACHINE"), null), //Registry.LocalMachine),
                new Cons (
                new Cons (string.Intern ("HKEY_USERS"), null), //Registry.Users),
                new Cons (
                new Cons (string.Intern ("HKEY_PERFORMANCE_DATA"), null), //Registry.PerformanceData),
                new Cons (
                new Cons (string.Intern ("HKEY_CURRENT_CONFIG"), null), //Registry.CurrentConfig),
                new Cons (
                new Cons (string.Intern ("HKEY_DYNAMIC_DATA"), null), //Registry.DynData),
                null))))))));
            return false;
        }

        [SchemePrimitive ("WORKING-DIRECTORY-PATHNAME", 0, true)]
        public static bool WorkingDirectoryPathname (out object answer)
        {
            answer = System.Environment.CurrentDirectory.ToCharArray ();
            return false;
        }

        [SchemePrimitive ("SET-WORKING-DIRECTORY-PATHNAME!", 1, false)]
        public static bool SetWorkingDirectoryPathname (out object answer, object arg)
        {
            answer = System.Environment.CurrentDirectory.ToCharArray ();
            System.Environment.CurrentDirectory = new String ((char []) arg);
            return false;
        }
    }
}
