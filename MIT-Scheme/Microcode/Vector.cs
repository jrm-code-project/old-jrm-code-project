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
            if (arg is object [] && (((object []) arg).Length == 4))
                return interpreter.Return (true);
            return interpreter.Return (arg is object []);
        }

        [SchemePrimitive ("VECTOR-LENGTH", 1)]
        public static object VectorLength (Interpreter interpreter, object arg)
        {
            if (arg is object []) {
                return interpreter.Return(((object []) arg).Length);
            }
            else if (arg is char []) {
                return interpreter.Return (((char []) arg).Length);
            }
            else
               throw new NotImplementedException();
        }

        [SchemePrimitive ("VECTOR-REF", 2)]
        public static object VectorRef (Interpreter interpreter, object vec, object idx)
        {
            if (vec is object []) {
                return interpreter.Return (((object []) vec) [(int) idx]);
            }
            else if (vec is char []) {
                return interpreter.Return (((char []) vec) [(int) idx]);
            }
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("VECTOR-SET!", 3)]
        public static object VectorSet (Interpreter interpreter, object vec, object idx, object val)
        {
            object oldValue = ((object []) vec) [(int) idx];
            ((object []) vec) [(int) idx] = val;
            return interpreter.Return (oldValue);
        }

        [SchemePrimitive ("SUBVECTOR-MOVE-RIGHT!", 5)]
        public static object SubvectorMoveRight (Interpreter interpreter, object [] arglist)
        {
            object [] ptr1 = (object []) (arglist [0]);
            int len1 = ptr1.Length;
            int end1 = (int) (arglist [2]);
            int start1 = (int) (arglist [1]);
            object [] ptr2 = (object []) (arglist [3]);
            int len2 = ptr2.Length;
            int start2 = (int) (arglist [4]);
            int length = end1 - start1;
            int end2 = start2 + length;

            int scan1 = end1;
            int scan2 = end2;
            int limit = scan1 - length;
            while (scan1 > limit)
                ptr2 [--scan2] = ptr1 [--scan1];
            return interpreter.Return ();
        }

        [SchemePrimitive ("SUBVECTOR-MOVE-LEFT!", 5)]
        public static object SubvectorMoveLeft (Interpreter interpreter, object [] arglist)
        {
            object [] ptr1 = (object []) (arglist [0]);
            int len1 = ptr1.Length;
            int end1 = (int) (arglist [2]);
            int start1 = (int) (arglist [1]);
            object [] ptr2 = (object []) (arglist [3]);
            int len2 = ptr2.Length;
            int start2 = (int) (arglist [4]);
            int length = end1 - start1;
            int end2 = start2 + length;

            int scan1 = start1;
            int scan2 = start2;
            int limit = scan1 + length;
            while (scan1 < limit)
                ptr2 [scan2++] = ptr1 [scan1++];
            return interpreter.Return ();
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
