using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    class WeakCons : ISystemPair
    {
        WeakReference car;
        object cdr;

        public WeakCons (object car, object cdr)
        {
            this.car = new WeakReference (car);
            this.cdr = cdr;
        }

        #region ISystemPair Members

        public object SystemPairCar
        {
            get
            {
                return this.car.Target;
            }
            set
            {
                this.car.Target = value;
            }
        }

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
