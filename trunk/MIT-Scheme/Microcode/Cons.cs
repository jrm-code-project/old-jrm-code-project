using System;
using System.Diagnostics;

namespace Microcode
{
    sealed class Cons: ISystemPair
    {
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

        [SchemePrimitive ("CAR", 1)]
        public static object PrimitiveCar (Interpreter interpreter, object arg0)
        {
            return interpreter.Return (((Cons) arg0).Car);
        }

        [SchemePrimitive ("CDR", 1)]
        public static object PrimitiveCdr (Interpreter interpreter, object arg0)
        {
            return interpreter.Return (((Cons) arg0).Cdr);
        }

        [SchemePrimitive ("GENERAL-CAR-CDR", 2)]
        public static object GeneralCarCdr (Interpreter interpreter, object arg0, object arg1)
        {
            object result = arg0;
            int pattern = (int) arg1;
            while (pattern > 1)
            {
                Cons pair = result as Cons;
                if (pair == null)
                    throw new NotImplementedException ();
                if ((pattern & 0x01) == 0x01)
                    result = pair.Car;
                else
                    result = pair.Cdr;
                pattern >>= 1;
            }
            return interpreter.Return (result);
        }

        [SchemePrimitive ("CONS", 2)]
        public static object PrimitiveCons (Interpreter interpreter, object car, object cdr)
        {
            return interpreter.Return (new Cons (car, cdr));
        }

        [SchemePrimitive ("LIST->VECTOR", 1)]
        public static object ToVector (Interpreter interpreter, Object arg0)
        {
            if (arg0 == null)
                return interpreter.Return (new object [0]);
            else
                return interpreter.Return (((Cons) arg0).ToVector ());
        }

        [SchemePrimitive ("PAIR?", 1)]
        public static object IsPair (Interpreter interpreter, object arg0)
        {
            return interpreter.Return (arg0 is Cons);
        }

        [SchemePrimitive ("SET-CAR!", 2)]
        public static object SetCar (Interpreter interpreter, object pair, object value)
        {
            object old = ((Cons) pair).Car;
            ((Cons) pair).Car = value;
            return interpreter.Return (old);
        }

        [SchemePrimitive ("SET-CDR!", 2)]
        public static object SetCdr (Interpreter interpreter, object pair, object value)
        {
            object old = ((Cons) pair).Cdr;
            ((Cons) pair).Cdr = value;
            return interpreter.Return (old);
        }
    }
}
