using System;
using System.Diagnostics;


namespace Microcode
{
    // Root of any object that isn't just lifted from
    // the underlying runtime.
    [Serializable]
    public class SchemeObject
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        TC typeCode;

        [DebuggerStepThrough]
        protected SchemeObject (TC typeCode)
        {
            this.typeCode = typeCode;
        }

        public TC TypeCode
        {
            [DebuggerStepThrough]
            get
            {
                return this.typeCode;
            }
        }
    }
}
