using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class Character
    {
        [SchemePrimitive ("MAKE-CHAR", 1)]
        public static object MakeChar (Interpreter interpreter, object argument)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("CHAR?", 1)]
        public static object IsChar (Interpreter interpreter, object arg)
        {
            return interpreter.Return (arg is char);
        }

        [SchemePrimitive ("CHAR->INTEGER", 1)]
        public static object CharToInteger (Interpreter interpreter, object arg)
        {
            return interpreter.Return ((int) (char) arg);
        }

        [SchemePrimitive ("INTEGER->CHAR", 1)]
        public static object IntegerToChar (Interpreter interpreter, object arg)
        {
            return interpreter.Return ((char) (int) arg);
        }

        [SchemePrimitive ("CHAR-UPCASE", 1)]
        public static object CharUpcase (Interpreter interpreter, object argument)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("CHAR-DOWNCASE", 1)]
        public static object CharDowncase (Interpreter interpreter, object argument)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("CHAR-BITS", 1)]
        public static object CharBits (Interpreter interpreter, object argument)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("CHAR-CODE", 1)]
        public static object CharCode (Interpreter interpreter, object argument)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("CHAR-ASCII?", 1)]
        public static object IsCharAscii (Interpreter interpreter, object argument)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("ASCII->CHAR", 1)]
        public static object AsciiToChar (Interpreter interpreter, object argument)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("CHAR->ASCII", 1)]
        public static object CharToAscii (Interpreter interpreter, object argument)
        {
            throw new NotImplementedException ();
        }
    }
}
