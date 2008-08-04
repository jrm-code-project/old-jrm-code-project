using System;
using System.Diagnostics;

namespace Microcode
{
    class WeakCons : ISystemPair
    {
        // WeakReference car;
        object car;
        object cdr;

        public WeakCons (object car, object cdr)
        {
            // this.car = new WeakReference (car);
            this.car = car;
            this.cdr = cdr;
        }

        public object Car
        {
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
            get
            {
                //return this.car.Target;
                return this.car;
            }
            set
            {
                // this.car.Target = value;
                this.car = value;
            }
        }
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCdr
        {
            get
            {
                return this.cdr;
            }
            set
            {
                this.cdr = value;
            }
        }

        #endregion
    }
}
