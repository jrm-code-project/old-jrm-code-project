using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    class Ratnum : ISystemPair
    {
        readonly object numerator;
        readonly object denominator;

        public Ratnum (object numerator, object denominator)
        {
            this.numerator = numerator;
            this.denominator = denominator;
        }

 

        [SchemePrimitive ("RATNUM?", 1)]
        public static object IsRatnum (Interpreter interpreter, object arg)
        {
            return interpreter.Return (arg is Ratnum);

        }

        #region ISystemPair Members

        public object SystemPairCar
        {
            get
            {
                 return this.numerator;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        public object SystemPairCdr
        {
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
    }
}
