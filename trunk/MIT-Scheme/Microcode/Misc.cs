using System;
using System.Collections.Generic;
using System.Linq;
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

        [SchemePrimitive ("FILE-EXISTS?", 1)]
        public static object IsFileExists (Interpreter interpreter, object arg)
        {
            return interpreter.Return (System.IO.File.Exists (new String ((char []) (arg))));
        }

        [SchemePrimitive ("GET-NEXT-CONSTANT", 0)]
        public static object GetNextConstant (Interpreter interpreter)
        {
            return interpreter.Return (0);
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
            return interpreter.Return (new object [] {"JRM Scheme",
                0,
                0,
                80,
                20,
                '\n',
                53,
                Double.Epsilon,
            "NT".ToCharArray(),
            null,
            null,
            null});
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
    }
}
