using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static  class BitString
    {
        [SchemePrimitive ("MAKE-BIT-STRING", 2)]
        public static object MakeBitString (Interpreter interpreter, object arg0, object arg1)
        {
            // what is arg1?
            return interpreter.Return (new bool [(int) arg0]);
        }

        [SchemePrimitive ("BIT-STRING?", 1)]
        public static object IsBitString (Interpreter interpreter, object arg)
        {
            return interpreter.Return (arg is BitString);
        }
    }
}
