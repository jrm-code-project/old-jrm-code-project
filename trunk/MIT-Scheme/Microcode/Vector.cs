using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class Vector
    {
        [SchemePrimitive ("VECTOR", -1)]
        public static object MakeVector (Interpreter interpreter, object [] arglist)
        {
            return interpreter.Return (arglist.Clone ());
        }

        [SchemePrimitive ("VECTOR-CONS", 2)]
        public static object VectorCons (Interpreter interpreter, object size, object init)
        {
            object [] result = new object [(int) size];
            for (int i = 0; i < ((int) size); i++)
                result [i] = init;
            return interpreter.Return (result);
        }

        [SchemePrimitive ("VECTOR?", 1)]
        public static object IsVector (Interpreter interpreter, object arg)
        {
            return interpreter.Return (arg is object []);
        }

        [SchemePrimitive ("VECTOR-LENGTH", 1)]
        public static object VectorLength (Interpreter interpreter, object arg)
        {
            return interpreter.Return (((object []) arg).Length);
        }

        [SchemePrimitive ("VECTOR-REF", 2)]
        public static object VectorRef (Interpreter interpreter, object vec, object idx)
        {
            return interpreter.Return (((object []) vec) [(int) idx]);
        }

        [SchemePrimitive ("VECTOR-SET!", 3)]
        public static object VectorSet (Interpreter interpreter, object vec, object idx, object val)
        {
            object oldValue = ((object []) vec) [(int) idx];
            ((object []) vec) [(int) idx] = val;
            return interpreter.Return (oldValue);
        }

        [SchemePrimitive ("SUBVECTOR->LIST", 3)]
        public static object SubvectorToList (Interpreter interpreter, object avec, object astart, object aend)
        {
            object [] vector = (object []) avec;
            int start = (int) astart;
            int end = (int) aend;
            Cons answer = null;
            while (start < end)
            {
                answer = new Cons (vector [--end], answer);
            }
            return interpreter.Return (answer);
        }
    }
}
