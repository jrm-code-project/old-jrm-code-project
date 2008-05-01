using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class FloatingVector
    {
        [SchemePrimitive ("FLOATING-VECTOR-CONS", 1)]
        public static object FloatingVectorCons (Interpreter interpreter, object arg)
        {
            double [] result = new double [(int) arg];
            return interpreter.Return (result);
        }

        [SchemePrimitive ("FLOATING-VECTOR-LENGTH", 1)]
        public static object FloatingVectorLength (Interpreter interpreter, object arg)
        {
            return interpreter.Return (((double []) arg).Length);
        }

        [SchemePrimitive ("FLOATING-VECTOR-REF", 2)]
        public static object FloatingVectorRef (Interpreter interpreter, object vec, object idx)
        {
            double old = ((double []) vec) [(int) idx];
            return interpreter.Return (old);
        }

        [SchemePrimitive ("FLOATING-VECTOR-SET!", 3)]
        public static object FloatingVectorSet (Interpreter interpreter, object vec, object idx, object val)
        {
            double old = ((double []) vec) [(int) idx];
            ((double []) vec) [(int) idx] = (double) val;
            return interpreter.Return (old);
        }
    }
}
