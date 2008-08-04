using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class SchemeString
    {
        [SchemePrimitive ("STRING?", 1)]
        public static object IsString (Interpreter interpreter, object arg)
        {
            return interpreter.Return (arg is char []);
        }

        [SchemePrimitive ("STRING->SYMBOL", 1)]
        public static object StringToSymbol (Interpreter interpreter, object arg)
        {
            char [] str = (char []) arg;
            return interpreter.Return (String.Intern (new String (str)));
        }

        [SchemePrimitive ("STRING-ALLOCATE", 1)]
        public static object StringAllocate (Interpreter interpreter, object arg)
        {
            return interpreter.Return (new char [((int) arg)]);
        }

        [SchemePrimitive ("STRING-HASH-MOD", 2)]
        public static object StringHashMod (Interpreter interpreter, object str, object modulus)
        {
            int tmp = new string ((char []) str).GetHashCode () % (int) modulus;
            if (tmp < 0) tmp += (int)modulus;
            return interpreter.Return (tmp);
        }

        [SchemePrimitive ("STRING-LENGTH", 1)]
        public static object StringLength (Interpreter interpreter, object arg)
        {
            return interpreter.Return (((char []) arg).Length);
        }

        [SchemePrimitive ("STRING-REF", 2)]
        public static object StringRef (Interpreter interpreter, object str, object idx)
        {
            return interpreter.Return (((char []) str) [(int) idx]);
        }

        [SchemePrimitive ("STRING-SET!", 3)]
        public static object StringSet (Interpreter interpreter, object str, object idx, object val)
        {
            char old_value = ((char []) str) [(int) idx];
            ((char []) str) [(int) idx] = (char) val;
            return interpreter.Return (old_value);
        }

        [SchemePrimitive ("SUBSTRING-MOVE-RIGHT!", 5)]
        public static object SubstringMoveRight (Interpreter interpreter, object [] arglist)
        {
            char [] ptr1 = (char []) (arglist [0]);
            int len1 = ptr1.Length;
            int end1 = (int) (arglist [2]);
            int start1 = (int) (arglist [1]);
            char [] ptr2 = (char []) (arglist [3]);
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

        [SchemePrimitive ("SUBSTRING-MOVE-LEFT!", 5)]
        public static object SubstringMoveLeft (Interpreter interpreter, object [] arglist)
        {
            char [] ptr1 = (char []) (arglist [0]);
            int len1 = ptr1.Length;
            int end1 = (int) (arglist [2]);
            int start1 = (int) (arglist [1]);
            char [] ptr2 = (char []) (arglist [3]);
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

        [SchemePrimitive ("SUBSTRING=?", 6)]
        public static object IsSubstringEqual (Interpreter interpreter, object [] arglist)
        {
            char [] left = (char []) (arglist [0]);
            int left_scan = (int) (arglist [1]);
            int left_limit = (int) (arglist [2]);
            char [] right = (char []) (arglist [3]);
            int right_scan = (int) (arglist [4]);
            int right_limit = (int) (arglist [5]);
            if ((left_limit - left_scan) != (right_limit - right_scan))
                return interpreter.Return (false);
            while (left_scan < left_limit)
            {
                if (left [left_scan++] != right [right_scan++])
                    return interpreter.Return (false);
            }
            if (right_scan == right_limit)
                return interpreter.Return (true);
            else
                return interpreter.Return (false);
        }

        [SchemePrimitive ("SUBSTRING-DOWNCASE!", 3)]
        public static object SubstringDowncase (Interpreter interpreter, object astr, object astart, object aend)
        {
            char [] str = (char []) astr;
            int start = (int) astart;
            int end = (int) aend;
            int length = (end - start);
            int scan = start;
            while ((length--) > 0)
            {
                char temp = str [scan];
                str [scan++] = Char.ToLower (temp);
            }
            return interpreter.Return ();
        }

        [SchemePrimitive ("SUBSTRING-FIND-NEXT-CHAR-IN-SET", 4)]
        public static object SubstringFindNextCharInSet (Interpreter interpreter, object [] arglist)
        {
            char [] str = (char []) (arglist [0]);
            int scan = (int) (arglist [1]);
            int limit = (int) (arglist [2]);
            char [] charset = (char []) (arglist [3]);

            while (scan < limit)
            {
                if (charset [str[scan++]] != '\0')
                    return interpreter.Return (scan - 1);
            }
            return interpreter.Return (false);
        }
    }
}
