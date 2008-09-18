using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class BitString
    {
        [SchemePrimitive ("MAKE-BIT-STRING", 2, false)]
        public static bool MakeBitString (out object answer, object arg0, object arg1)
        {
            answer = new bool [(int) arg0];
            return false;
        }

        [SchemePrimitive ("BIT-STRING?", 1, true)]
        public static bool IsBitString (out object answer, object arg)
        {
            answer = arg is bool [];
            return false;
        }
    }
}
