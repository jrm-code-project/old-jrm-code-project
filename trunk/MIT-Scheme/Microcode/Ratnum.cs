using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    sealed class Ratnum : ISystemPair
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object numerator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object denominator;

        public Ratnum (object numerator, object denominator)
        {
            this.numerator = numerator;
            this.denominator = denominator;
        }

        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            [DebuggerStepThrough]
            get
            {
                 return this.numerator;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCdr
        {
            [DebuggerStepThrough]
            get
            {
                return this.denominator;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        [SchemePrimitive ("RATNUM?", 1)]
        public static bool IsRatnum (out object answer, object arg)
        {
            answer = arg is Ratnum;
            return false;
        }
    }
}
