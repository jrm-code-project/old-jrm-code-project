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
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return this.car;
            }
        }

        public object Cdr
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return this.cdr;
            }
        }

        public static Cons SubvectorToList (object [] vector, int start, int limit)
        {
            if (vector == null) {
                if (start == limit)
                    return null;
                throw new ArgumentNullException ("vector");
            }
              
            Cons answer = null;
            int count = 1;
            for (int i = start; i < limit; i++) {
                answer = new Cons (vector [limit - count], answer);
                count += 1;
            }
            return answer;
        }
    }
}
