using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{
    internal class ValueCell<T> : ILocation<T>
    {
        bool IsBound;
        T CurrentValue;
        public ValueCell ()
        {
        }
        public ValueCell (T initialValue)
        {
            this.IsBound = true;
            this.CurrentValue = initialValue;
        }

        public T Value
        {
            get
            {
                if (this.IsBound)
                    return this.CurrentValue;
                else
                    throw new NotImplementedException ();
            }

            set
            {
                this.IsBound = true;
                this.CurrentValue = value;
            }
        }
    }
}
