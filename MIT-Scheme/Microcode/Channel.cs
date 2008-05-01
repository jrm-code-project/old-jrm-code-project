using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class Channel
    {
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

        [SchemePrimitive ("CHANNEL-WRITE", 4)]
        public static object ChannelWrite (Interpreter interpreter, object [] arglist)
        {
            for (int i = ((int) (arglist [2])); i < ((int) (arglist [3])); i++)
            {
                ((System.IO.TextWriter) (arglist [0])).Write (((char []) (arglist [1])) [i]);
            }

            return interpreter.Return (arglist [3]);
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
