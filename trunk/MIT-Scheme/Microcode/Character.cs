using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class Character
    {
        [SchemePrimitive ("MAKE-CHAR", 2)]
        public static bool MakeChar (out object answer, object arg0, object arg1)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("CHAR?", 1)]
        public static bool IsChar (out object answer, object arg)
        {
            answer = arg is char;
            return false;
        }

        [SchemePrimitive ("CHAR->INTEGER", 1)]
        public static bool CharToInteger (out object answer, object arg)
        {
            answer = (int) (char) arg;
            return false;
        }

        [SchemePrimitive ("INTEGER->CHAR", 1)]
        public static bool IntegerToChar (out object answer, object arg)
        {
            answer = (char) (int) arg;
            return false;
        }

        [SchemePrimitive ("CHAR-UPCASE", 1)]
        public static bool CharUpcase (out object answer, object argument)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("CHAR-DOWNCASE", 1)]
        public static bool CharDowncase (out object answer, object argument)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("CHAR-BITS", 1)]
        public static bool CharBits ( out object answer, object argument)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("CHAR-CODE", 1)]
        public static bool CharCode (out object answer, object argument)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("CHAR-ASCII?", 1)]
        public static bool IsCharAscii (out object answer, object argument)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("ASCII->CHAR", 1)]
        public static bool AsciiToChar (out object answer, object argument)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("CHAR->ASCII", 1)]
        public static bool CharToAscii (out object answer, object argument)
        {
            throw new NotImplementedException ();
        }
    }
}
