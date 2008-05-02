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
        public static object IsComplex (Interpreter interpreter, object arg)
        {
            return interpreter.Return (arg is Complex);
        }
    }
}
