using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class Vector8b
    {

        [SchemePrimitive ("VECTOR-8B-FILL!", 4, false)]
        public static bool Vector8bFill (out object answer, object [] args)
        {
            char [] vvec = (char []) (args[0]);
            int istart = (int) (args[1]);
            int ilimit = (int) (args[2]);
            char bval = (char) (int) (args[3]);
            for (int i = istart; i < ilimit; i++)
                vvec [i] = bval;
            answer = Constant.Unspecific;
            return false;
        }

        [SchemePrimitive ("VECTOR-8B-REF", 2, false)]
        public static bool Vector8bRef (out object answer, object vec, object idx)
        {
            answer = (int) ((char []) vec) [(int) idx];
            return false;
        }

        [SchemePrimitive ("VECTOR-8B-SET!", 3, false)]
        public static bool Vector8bSet (out object answer, object vec, object idx, object val)
        {
            answer = (int) ((char []) vec) [(int) idx];
            ((char []) vec) [(int) idx] = (char) (int) val;
            return false;
        }

        [SchemePrimitive ("VECTOR-8B-FIND-PREVIOUS-CHAR", 4, false)]
        public static bool Vector8bFindPreviousChar (out object answer, object [] arglist)
        {
            //throw new NotImplementedException ();
            char [] str = (char []) (arglist [0]);
            int start = (int) (arglist [1]);
            int scan = (int) (arglist [2]);
            char target = (char) (int) (arglist [3]);

            while (start < scan) {
                int scan1 = scan - 1;
                if (str [scan1] == target) {
                    answer = scan1;
                    return false;
                }
                scan = scan1;
            }

            answer = false;
            return false;
        }
    }
}
