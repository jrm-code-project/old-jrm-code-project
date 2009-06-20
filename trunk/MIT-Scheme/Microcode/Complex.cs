using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    sealed class Complex : SchemeObject
    {
        object realPart;
        object imagPart;

        public Complex (object realPart, object imagPart)
            : base (TC.COMPLEX)
        {
            this.realPart = realPart;
            this.imagPart = imagPart;
        }

        [SchemePrimitive ("COMPLEX?", 1, true)]
        public static bool IsComplex (out object answer, object arg)
        {
            answer = arg is Complex;
            return false;
        }
    }
}
