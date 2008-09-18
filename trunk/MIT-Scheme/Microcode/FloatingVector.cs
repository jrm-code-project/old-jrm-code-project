using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class FloatingVector
    {
        [SchemePrimitive ("FLOATING-VECTOR-CONS", 1, false)]
        public static bool FloatingVectorCons (out object answer, object arg)
        {
            answer = new double [(int) arg];
            return false;
        }

        [SchemePrimitive ("FLOATING-VECTOR-LENGTH", 1, false)]
        public static bool FloatingVectorLength (out object answer, object arg)
        {
            answer = ((double []) arg).Length;
            return false;
        }

        [SchemePrimitive ("FLOATING-VECTOR-REF", 2, false)]
        public static bool FloatingVectorRef (out object answer, object vec, object idx)
        {
            answer = ((double []) vec) [(int) idx];
            return false;
        }

        [SchemePrimitive ("FLOATING-VECTOR-SET!", 3, false)]
        public static bool FloatingVectorSet (out object answer, object vec, object idx, object val)
        {
            answer = ((double []) vec) [(int) idx];
            ((double []) vec) [(int) idx] = (double) val;
            return false;
        }
    }
}
