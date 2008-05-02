using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class Vector8b
    {
        [SchemePrimitive ("VECTOR-8B-REF", 2)]
        public static object Vector8bRef (Interpreter interpreter, object vec, object idx)
        {
            char old_value = ((char []) vec) [(int) idx];
            return interpreter.Return ((int) old_value);
        }

        [SchemePrimitive ("VECTOR-8B-SET!", 3)]
        public static object Vector8bSet (Interpreter interpreter, object vec, object idx, object val)
        {
            char old_value = ((char []) vec) [(int) idx];
            ((char []) vec) [(int) idx] = (char) (int) val;
            return interpreter.Return ((int) old_value);
        }
    }
}
