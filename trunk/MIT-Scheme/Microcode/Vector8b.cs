using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class Vector8b
    {
        [SchemePrimitive ("VECTOR-8B-REF", 2)]
        public static bool Vector8bRef (out object answer, object vec, object idx)
        {
            answer = (int) ((char []) vec) [(int) idx];
            return false;
        }

        [SchemePrimitive ("VECTOR-8B-SET!", 3)]
        public static bool Vector8bSet (out object answer, object vec, object idx, object val)
        {
            answer = (int) ((char []) vec) [(int) idx];
            ((char []) vec) [(int) idx] = (char) (int) val;
            return false;
        }
    }
}
