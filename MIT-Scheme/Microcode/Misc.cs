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
        public static bool IsGensym (string thing)
        {
            return !Object.ReferenceEquals (thing, String.IsInterned (thing));
        }

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

        [SchemePrimitive("BATCH-MODE?", 0)]
        public static object IsBatchMode(Interpreter interpreter)
        {
            return interpreter.Return (false);
        }

        [SchemePrimitive("CONSTANT?", 1)]
        public static object IsConstant(Interpreter interpreter, object arg)
        {
            return interpreter.Return(System.GC.GetGeneration(arg) > 0);
        }

        [SchemePrimitive ("FILE-EXISTS?", 1)]
        public static object IsFileExists (Interpreter interpreter, object arg)
        {
            return interpreter.Return (System.IO.File.Exists (new String ((char []) (arg))));
        }

        [SchemePrimitive ("FIND-SYMBOL", 1)]
        public static object FindSymbol (Interpreter interpreter, object arg)
        {
            return interpreter.Return (String.IsInterned (new String ((char []) (arg))));
        }

        [SchemePrimitive ("FILE-MOD-TIME", 1)]
        public static object FileModTime (Interpreter interpreter, object arg)
        {
            String filename = new String ((char []) arg);
            return interpreter.Return (System.IO.File.GetLastWriteTimeUtc (filename).ToFileTimeUtc());
        }

        [SchemePrimitive ("FILE-TYPE-INDIRECT", 1)]
        public static object FileTypeIndirect (Interpreter interpreter, object arg)
        {
            String filename = new String ((char []) arg);
            System.IO.FileAttributes fa;
            try {
                fa = System.IO.File.GetAttributes (filename);
            }
            catch (System.IO.FileNotFoundException) {
                return interpreter.Return (false);
            }
            if ((fa & System.IO.FileAttributes.Directory) == System.IO.FileAttributes.Directory)
                return interpreter.Return(1);
            else {
                return interpreter.Return(0);
            }
        }

        [SchemePrimitive ("GARBAGE-COLLECT", 1)]
        public static object GarbageCollect (Interpreter interpreter, object arg)
        {
            // Arg is the safety margin.
            return interpreter.Return (GC.GetTotalMemory (true));
        }

        [SchemePrimitive ("GC-SPACE-STATUS", 0)]
        public static object GcSpaceStatus (Interpreter interpreter)
        {
            object [] answer = new object [12];
            answer [0] = 4; // bytes per object
            answer [1] = 0x100; // constant-start
            answer [2] = 0x200; // constant-alloc-next
            answer [3] = 0x400; // constant-end
            answer [4] = 0x1000; // heap-start
            answer [5] = 0x20000000; // free
            answer [6] = 0x40000000; // heap-alloc-limit
            answer [7] = 0x50000000; // heap-end
            answer [8] = 0x90000000; // stack start
            answer [9] = 0x100000000; //stack pointer
            answer [10] = 0x200000000; // stack guard
            answer [11] = 0x400000000; // stack end
            return interpreter.Return (answer);
        }

        [SchemePrimitive ("GET-ENVIRONMENT-VARIABLE", 1)]
        public static object GetEnvironmentVariable (Interpreter interpreter, object arg)
        {
            char [] name = (char []) arg;
            string sname = new string (name);
            string answer = System.Environment.GetEnvironmentVariable (sname);
            return interpreter.Return((answer == null) ? (object)false : (object)answer.ToCharArray ());
        }

        [SchemePrimitive ("GET-NEXT-CONSTANT", 0)]
        public static object GetNextConstant (Interpreter interpreter)
        {
            return interpreter.Return (0);
        }

        [SchemePrimitive ("HAVE-SELECT?", 0)]
        public static object HaveSelect (Interpreter interpreter)
        {
            return interpreter.Return (false);
        }


        [SchemePrimitive ("INITIALIZE-C-COMPILED-BLOCK", 1)]
        public static object InitializeCCompiledBlock (Interpreter interpreter, object arg)
        {
            // what is arg?
            return interpreter.Return (false);
        }

        [SchemePrimitive ("MICROCODE-IDENTIFY", 0)]
        public static object MicrocodeIdentify (Interpreter interpreter)
        {
            return interpreter.Return (new object [] {false,
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
            false, false, false, false, false ,false, false});
        }

        [SchemePrimitive ("MICROCODE-LIBRARY-PATH", 0)]
        public static object MicrocodeLibraryPath (Interpreter interpreter)
        {
            return interpreter.Return (new object [] { "C:\\Program Files\\MIT\\lib\\".ToCharArray () });

            //return interpreter.Return (new object [] {"Program Files".ToCharArray(),
            //    "MIT".ToCharArray(),
            //    "scheme-7.7.1".ToCharArray(),
            //    "lib".ToCharArray()});
        }


        [SchemePrimitive ("MICROCODE-SYSTEM-CALL-NAMES", 0)]
        public static object MicrocodeSystemCallNames (Interpreter interpreter)
        {
            return interpreter.Return (FixedObjectsVector.SyscallNames);
        }

        [SchemePrimitive ("MICROCODE-SYSTEM-ERROR-NAMES", 0)]
        public static object MicrocodeSystemErrorNames (Interpreter interpreter)
        {
            return interpreter.Return (FixedObjectsVector.SyserrNames);
        }


        [SchemePrimitive ("MICROCODE-TABLES-FILENAME", 0)]
        public static object MicrocodeTablesFilename (Interpreter interpreter)
        {
            return interpreter.Return ("utabmd.bin".ToCharArray ());
        }

        [SchemePrimitive ("NT-GET-VOLUME-INFORMATION", 1)]
        public static object NTGetVolumeInformation (Interpreter interpreter, object arg)
        {
            object [] answer = new object [5];
            answer [0] = "VOLUMEFOO".ToCharArray () ;
            answer [1] = 42;
            answer [2] = 255;
            answer [3] = 7;
            answer [4] = "NTFS".ToCharArray();
            return interpreter.Return (answer);
        }

        [SchemePrimitive ("PURE?", 1)]
        public static object IsPure (Interpreter interpreter, object arg)
        {
            return interpreter.Return (GC.GetGeneration (arg) >= 2);
        }



        static DateTime UnixEpochStart = new DateTime (1971, 1, 1, 0, 0, 0);

        /// <summary>
        /// Return the current real time in units of milliseconds.
        /// </summary>
        [SchemePrimitive ("REAL-TIME-CLOCK", 0)]
        public static object RealTimeClock (Interpreter interpreter)
        {
            return interpreter.Return ((long) (DateTime.UtcNow.Subtract (UnixEpochStart).TotalMilliseconds));
        }

        [SchemePrimitive ("REQUEST-INTERRUPTS!", 1)]
        public static object RequestInterrupts (Interpreter interpreter, object arg)
        {
            return interpreter.Return (Constant.Unspecific);
        }

        static Stopwatch systemTime = Stopwatch.StartNew ();
        [SchemePrimitive ("SYSTEM-CLOCK", 0)]
        public static object SystemClock (Interpreter interpreter)
        {
            return interpreter.Return (systemTime.ElapsedMilliseconds);
        }

        [SchemePrimitive ("TERMINAL-BUFFERED", 1)]
        public static object TerminalBuffered (Interpreter interpreter, object arg)
        {
            return interpreter.Return (true);
        }


        [SchemePrimitive ("TERMINAL-BUFFERED?", 1)]
        public static object IsTerminalBuffered (Interpreter interpreter, object arg)
        {
            return interpreter.Return (true);
        }


        [SchemePrimitive ("TERMINAL-COOKED-OUTPUT", 1)]
        public static object TerminalCookedOutput (Interpreter interpreter, object arg)
        {
            return interpreter.Return (true);
        }

        [SchemePrimitive ("TERMINAL-COOKED-OUTPUT?", 1)]
        public static object TerminalCookedOutputP (Interpreter interpreter, object arg)
        {
            return interpreter.Return (true);
        }

        [SchemePrimitive("TTY-X-SIZE", 0)]
        public static object TtyXSize(Interpreter interpreter)
        {
            return interpreter.Return(80);
        }


        [SchemePrimitive ("UNDER-EMACS?", 0)]
        public static object IsUnderEmacs (Interpreter interpreter)
        {
            return interpreter.Return (false);
        }

        [SchemePrimitive ("WIN32-PREDEFINED-REGISTRY-KEYS", 0)]
        public static object Win32PredefinedRegistryKeys (Interpreter interpreter)
        {
            return interpreter.Return (
                new Cons (
                new Cons (string.Intern ("HKEY_CLASSES_ROOT"), Registry.ClassesRoot),
                new Cons (
                new Cons (string.Intern ("HKEY_CURRENT_USER"), Registry.CurrentUser),
                new Cons (
                new Cons (string.Intern ("HKEY_LOCAL_MACHINE"), Registry.LocalMachine),
                new Cons (
                new Cons (string.Intern ("HKEY_USERS"), Registry.Users),
                new Cons (
                new Cons (string.Intern ("HKEY_PERFORMANCE_DATA"), Registry.PerformanceData),
                new Cons (
                new Cons (string.Intern ("HKEY_CURRENT_CONFIG"), Registry.CurrentConfig),
                new Cons (
                new Cons (string.Intern ("HKEY_DYNAMIC_DATA"), Registry.DynData),
                null))))))));
        }

        [SchemePrimitive ("WORKING-DIRECTORY-PATHNAME", 0)]
        public static object WorkingDirectoryPathname (Interpreter interpreter)
        {
            return interpreter.Return (System.Environment.CurrentDirectory.ToCharArray ());
        }

        [SchemePrimitive ("SET-WORKING-DIRECTORY-PATHNAME!", 1)]
        public static object SetWorkingDirectoryPathname (Interpreter interpreter, object arg)
        {
            char [] oldDirectory = System.Environment.CurrentDirectory.ToCharArray ();
            System.Environment.CurrentDirectory = new String ((char []) arg);
            return interpreter.Return (oldDirectory);
        }
    }
}
