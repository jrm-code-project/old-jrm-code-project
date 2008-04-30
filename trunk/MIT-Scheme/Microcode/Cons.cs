using System.Diagnostics;

namespace Microcode
{
    sealed class Cons
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
    }
}
