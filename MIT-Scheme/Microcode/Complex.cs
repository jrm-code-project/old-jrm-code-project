using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    class Complex
    {
        object realPart;
        object imagPart;

        public Complex (object realPart, object imagPart)
        {
            this.realPart = realPart;
            this.imagPart = imagPart;
        }

        [SchemePrimitive ("COMPLEX?", 1)]
        public static bool IsComplex (out object answer, object arg)
        {
            answer = arg is Complex;
            return false;
        }
    }
}
