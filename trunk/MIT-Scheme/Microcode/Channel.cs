using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class Channel
    {
        // Crude kludge to get initial prompt to work.
        static bool blocking = true;

        [SchemePrimitive ("CHANNEL-BLOCKING?", 1)]
        public static object IsChannelBlocking (Interpreter interpreter, object arg)
        {
            return interpreter.Return (blocking);
        }

        [SchemePrimitive ("CHANNEL-DESCRIPTOR", 1)]
        public static object PrimitiveChannelDescriptor (Interpreter interpreter, object arg)
        {
            return interpreter.Return (arg);
        }

        [SchemePrimitive ("CHANNEL-BLOCKING", 1)]
        public static object ChannelBlocking (Interpreter interpreter, object arg)
        {
            blocking = true;
            return interpreter.Return (true);
        }

        [SchemePrimitive ("CHANNEL-NONBLOCKING", 1)]
        public static object ChannelNonBlocking (Interpreter interpreter, object arg)
        {
            blocking = false;
            return interpreter.Return (true);
        }

        [SchemePrimitive ("CHANNEL-TYPE-NAME", 1)]
        public static object ChannelTypeName (Interpreter interpreter, object arg)
        {
            if (arg is System.IO.TextReader)
                return interpreter.Return ("terminal".ToCharArray ());
            else if (arg is System.IO.TextWriter)
                return interpreter.Return ("terminal".ToCharArray ());
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("CHANNEL-READ", 4)]
        public static object ChannelRead (Interpreter interpreter, object [] arglist)
        {
           System.IO.TextReader reader = (System.IO.TextReader) arglist [0];
            char [] buffer = (char []) arglist [1];
            int start = (int) arglist [2];
            int limit = (int) arglist [3];
            if (blocking == true)
            {
                int filled = reader.Read (buffer, start, limit - start);
                return interpreter.Return (filled);
            }
            else
            {
                return interpreter.Return (0);
            }
        }

        [SchemePrimitive ("CHANNEL-WRITE", 4)]
        public static object ChannelWrite (Interpreter interpreter, object [] arglist)
        {
            for (int i = ((int) (arglist [2])); i < ((int) (arglist [3])); i++)
            {
                ((System.IO.TextWriter) (arglist [0])).Write (((char []) (arglist [1])) [i]);
            }

            return interpreter.Return (arglist [3]);
        }

        [SchemePrimitive("TERMINAL-NONBUFFERED", 1)]
        public static object TerminalNonBuffered(Interpreter interpreter, object arg0)
        {
            return interpreter.Return(false);
        }

        [SchemePrimitive("TEST-SELECT-DESCRIPTOR", 3)]
        public static object TestSelectDescriptor(Interpreter interpreter, object arg0, object arg1, object arg2)
        {
            System.IO.TextWriter tw= arg0 as System.IO.TextWriter;
            System.IO.TextReader tr = arg0 as System.IO.TextReader;
            bool block = (bool)arg1;
            int direction = (int)arg2;
            return interpreter.Return(0);
        }

        [SchemePrimitive ("TTY-INPUT-CHANNEL", 0)]
        public static object TtyInputChannel (Interpreter interpreter)
        {
            return interpreter.Return (Console.In);
        }

        [SchemePrimitive ("TTY-OUTPUT-CHANNEL", 0)]
        public static object TtyOutputChannel (Interpreter interpreter)
        {
            return interpreter.Return (Console.Out);
        }
    }
}
