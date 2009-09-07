using System;
using System.Diagnostics;

namespace Microcode
{
    [Serializable]
    sealed class Cons: SchemeObject, ISystemPair
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.LIST; } }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object car;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object cdr;

        public Cons (object car, object cdr)
        {
            this.car = car;
            this.cdr = cdr;
        }

        public object Car
        {
            [DebuggerStepThrough]
            get
            {
                return this.car;
            }
            set
            {
                this.car = value;
            }
        }

        public object Cdr
        {
            [DebuggerStepThrough]
            get
            {
                return this.cdr;
            }
            set
            {
                this.cdr = value;
            }
        }

        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            [DebuggerStepThrough]
            get
            {
                return this.car;
            }

            set
            {
                this.car = value;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCdr
        {
            [DebuggerStepThrough]
            get
            {
                return this.cdr;
            }

            set
            {
                this.cdr = value ;
            }
        }

        #endregion

        public int Length ()
        {
            Cons ptr = this;
            int len = 0;
            while (ptr != null)
            {
                len += 1;
                ptr = (Cons) ptr.cdr;
            }
            return len;
        }

        public object [] ToVector ()
        {
            object [] result = new object [this.Length ()];
            Cons ptr = this;
            int idx = 0;
            while (ptr != null)
            {
                result [idx++] = ptr.car;
                ptr = (Cons) ptr.cdr;
            }
            return result;
        }

        // Too important to be interpreted
        [SchemePrimitive ("ASSQ", 2, false)]
        public static bool Assq (out object answer, object arg0, object arg1)
        {
            if (arg0 == null ||
                arg0 is bool ||
                arg0 is int ||
                arg0 is char ||
                arg0 is long)
                throw new NotImplementedException ();
            for (Cons tail = (Cons) arg1; tail != null; tail = (Cons) tail.Cdr) {
                Cons entry = (Cons) tail.Car;
                if (entry.Car == arg0) {
                    answer = entry;
                    return false;
                }
            }
            answer = false;
            return false;
        }

        [SchemePrimitive ("CAR", 1 ,false)]
        public static bool PrimitiveCar (out object answer, object arg0)
        {
            answer = ((Cons) arg0).Car;
            return false;
        }

        [SchemePrimitive ("CDR", 1, false)]
        public static bool PrimitiveCdr (out object answer, object arg0)
        {
            answer = ((Cons) arg0).Cdr;
            return false;
        }

        [SchemePrimitive ("GENERAL-CAR-CDR", 2, false)]
        public static bool GeneralCarCdr (out object answer, object arg0, object arg1)
        {
            object result = arg0;
            int pattern = (int) arg1;
            while (pattern > 1) {
                Cons pair = result as Cons;
                if (pair == null)
                    throw new NotImplementedException ();
                result = ((pattern & 0x01) == 0x01) ? pair.Car : pair.Cdr;
                pattern >>= 1;
            }
            answer = result;
            return false;
        }

        [SchemePrimitive ("CONS", 2, true)]
        public static bool PrimitiveCons (out object answer, object car, object cdr)
        {
            answer = new Cons (car, cdr);
            return false;
        }

        [SchemePrimitive ("LIST->VECTOR", 1, false)]
        public static bool ToVector (out object answer, object arg0)
        {
            answer = (arg0 == null)
                ? new object [0]
                : ((Cons) arg0).ToVector ();
            return false;
        }

        [SchemePrimitive ("PAIR?", 1, true)]
        public static bool IsPair (out object answer, object arg0)
        {
            answer = arg0 is Cons ? Constant.sharpT : Constant.sharpF;
            return false;
        }

        [SchemePrimitive ("SET-CAR!", 2, false)]
        public static bool SetCar (out object answer, object pair, object value)
        {
            answer = ((Cons) pair).Car;
            ((Cons) pair).Car = value;
            return false;
        }

        [SchemePrimitive ("SET-CDR!", 2, false)]
        public static bool SetCdr (out object answer, object pair, object value)
        {
            answer = ((Cons) pair).Cdr;
            ((Cons) pair).Cdr = value;
            return false;
        }
    }
}
