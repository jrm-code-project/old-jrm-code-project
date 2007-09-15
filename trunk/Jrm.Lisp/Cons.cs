using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{
    [CLSCompliant(true)]
    public sealed class Cons
    {
        [System.Diagnostics.DebuggerBrowsable (System.Diagnostics.DebuggerBrowsableState.Never)]
        readonly object car;

        [System.Diagnostics.DebuggerBrowsable (System.Diagnostics.DebuggerBrowsableState.Never)]
        readonly object cdr;

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
        }

        public object Cdr
        {
            get
            {
                return this.cdr;
            }
        }
    }
}
