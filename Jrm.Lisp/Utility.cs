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

        static public Cons GetArgs (object [] ra, object key)
        {
            Cons reversedAnswer = null;
            for (int i = 0; i < ra.Length; i += 2) {
                if (ra [i] == key)
                    reversedAnswer = new Cons (ra [i + 1], reversedAnswer);
            }
            return CL.Reverse (reversedAnswer);
        }

        static public Cons RemArgs (object [] ra, object key)
        {
            Cons reversedAnswer = null;
            for (int i = 0; i < ra.Length; i += 2) {
                if (ra [i] != key)
                    reversedAnswer = new Cons (ra [i + 1], new Cons (ra [i], reversedAnswer));
            }
            return CL.Reverse (reversedAnswer);
        }

        static public Cons GetArgs (Cons list, object key)
        {
            if (list == null)
                return null;
            Cons firstPair = (Cons) list;
            Cons secondPair = (Cons) (firstPair.Cdr);
            if (firstPair.Car == key)
                return new Cons (secondPair.Car, GetArgs ((Cons) secondPair.Cdr, key));
            else
                return GetArgs ((Cons) secondPair.Cdr, key);
        }

        static public ConsList<T> GetArgs<T> (Cons list, object key)
        {
            if (list == null)
                return null;
            Cons firstPair = (Cons) list;
            Cons secondPair = (Cons) (firstPair.Cdr);
            if (firstPair.Car == key)
                return new ConsList<T> ((T) secondPair.Car, GetArgs<T> ((Cons) secondPair.Cdr, key));
            else
                return GetArgs<T> ((Cons) secondPair.Cdr, key);
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

        static public object GetArgStar (object list, ConsList<Symbol> keys, object defaultValue)
        {
            if (list == null)
                return defaultValue;
            Cons firstPair = (Cons) list;
            Cons secondPair = (Cons) (firstPair.Cdr);
            if (CL.Memq<Symbol> ((Symbol) firstPair.Car, keys) != null)
                return secondPair.Car;
            else
                return GetArgStar (secondPair.Cdr, keys, defaultValue);
        }
    }
}
