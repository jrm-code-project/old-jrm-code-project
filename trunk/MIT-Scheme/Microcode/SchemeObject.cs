
using System;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    // Root of any object that isn't just lifted from
    // the underlying runtime.
    [Serializable]
    public class SchemeObject
    {
#if DEBUG
        static long counter;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        protected readonly long objectSerialNumber;
#endif

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        TC typeCode;

        [DebuggerStepThrough]
        protected SchemeObject (TC typeCode)
        {
            this.typeCode = typeCode;
#if DEBUG
            this.objectSerialNumber = SCode.counter++;
#endif
        }

        public TC TypeCode
        {
            [DebuggerStepThrough]
            get
            {
                return this.typeCode;
            }
        }
#if DEBUG
        public long SerialNumber {
            [DebuggerStepThrough]
            get {
                return this.objectSerialNumber;
            }
        }
#endif
    }
}
