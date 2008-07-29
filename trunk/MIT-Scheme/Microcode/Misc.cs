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

        [SchemePrimitive ("FILE-TYPE-INDIRECT", 1)]
        public static object FileTypeIndirect (Interpreter interpreter, object arg)
        {
            String filename = new String ((char []) arg);
            System.IO.FileAttributes fa = System.IO.File.GetAttributes (filename);
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("GARBAGE-COLLECT", 1)]
        public static object GarbageCollect (Interpreter interpreter, object arg)
        {
            // Arg is the safety margin.
            return interpreter.Return (GC.GetTotalMemory (true));
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
            return interpreter.Return (new object [] {"Program Files".ToCharArray(),
                "MIT".ToCharArray(),
                "scheme-7.7.1".ToCharArray(),
                "lib".ToCharArray()});
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



    }
}
