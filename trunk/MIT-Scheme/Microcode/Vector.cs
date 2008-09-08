using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class Vector
    {
        [SchemePrimitive ("VECTOR", -1)]
        public static bool MakeVector (out object answer, object [] arglist)
        {
            answer = arglist;
            return false;
        }

        [SchemePrimitive ("VECTOR-CONS", 2)]
        public static bool VectorCons (out object answer, object size, object init)
        {
            object [] result = new object [(int) size];
            for (int i = 0; i < ((int) size); i++)
                result [i] = init;
            answer = result;
            return false;
        }

        [SchemePrimitive ("VECTOR?", 1)]
        public static bool IsVector (out object answer, object arg)
        {
            answer = arg is object [];
            return false;
        }

        [SchemePrimitive ("VECTOR-LENGTH", 1)]
        public static bool VectorLength (out object answer, object arg)
        {
            if (arg is object []) {
                answer = ((object []) arg).Length;
                return false;
            }
            //else if (arg is char []) {
            //    return ((char []) arg).Length;
            //}
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("VECTOR-REF", 2)]
        public static bool VectorRef (out object answer, object vec, object idx)
        {
            object [] ovec = vec as object [];
            if (ovec != null) {
                answer = ovec [(int) idx];
                return false;
            }
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("VECTOR-SET!", 3)]
        public static bool VectorSet (out object answer, object vec, object idx, object val)
        {
            object [] ovec = vec as object [];
            if (ovec != null) {
                answer = ovec [(int) idx];
                ovec [(int) idx] = val;
                return false;
            }
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("SUBVECTOR-MOVE-RIGHT!", 5)]
        public static bool SubvectorMoveRight (out object answer, object [] arglist)
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
            answer = false;
            return false;
        }

        [SchemePrimitive ("SUBVECTOR-MOVE-LEFT!", 5)]
        public static bool SubvectorMoveLeft (out object answer, object [] arglist)
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
            answer = false;
            return false;
        }

        [SchemePrimitive ("SUBVECTOR->LIST", 3)]
        public static bool SubvectorToList (out object answer, object avec, object astart, object aend)
        {
            object [] vector = (object []) avec;
            int start = (int) astart;
            int end = (int) aend;
            Cons list = null;
            while (start < end) {
                list = new Cons (vector [--end], list);
            }
            answer = list;
            return false;
        }
    }
}
