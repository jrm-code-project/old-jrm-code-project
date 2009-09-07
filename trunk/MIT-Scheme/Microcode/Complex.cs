using System;
using System.Diagnostics;

namespace Microcode
{
    [Serializable]
    sealed class Complex : SchemeObject
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.COMPLEX; } }

        public readonly object realPart;
        public readonly object imagPart;

        public Complex (object realPart, object imagPart)
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
