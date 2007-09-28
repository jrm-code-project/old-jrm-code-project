using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{
    sealed class Utility
    {
        private Utility ()
        {
        }

        static public void Ignore (object x)
        {
            return;
        }

        static public object GetArg (object list, object key, object defaultValue)
        {
            if (list == null)
                return defaultValue;
            Cons firstPair = (Cons) list;
            Cons secondPair = (Cons) (firstPair.Cdr);
            if (firstPair.Car == key)
                return secondPair.Car;
            else
                return GetArg (secondPair.Cdr, key, defaultValue);
        }
    }
}
