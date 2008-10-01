using System;
using System.Diagnostics;

namespace Microcode
{
    public class Hunk3 : ISystemHunk3
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object cxr0;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object cxr1;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object cxr2;

        internal Hunk3 (object cxr0, object cxr1, object cxr2)
        {
            this.cxr0 = cxr0;
            this.cxr1 = cxr1;
            this.cxr2 = cxr2;
        }

        internal object Cxr0
        {
            [DebuggerStepThrough]
            get { return this.cxr0; }
        }

        internal object Cxr1
        {
            [DebuggerStepThrough]
            get { return this.cxr1; }
        }

        internal object Cxr2
        {
            [DebuggerStepThrough]
            get { return this.cxr2; }
        }

        //[SchemePrimitive ("HUNK3-CONS", 3)]
        //public static PartialResult Hunk3Cons (object cxr0, object cxr1, object cxr2)
        //{
        //    return new PartialResult (new Hunk3 (cxr0, cxr1, cxr2));
        //}

        #region ISystemHunk3 Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr0
        {
            [DebuggerStepThrough]
            get
            {
                return this.cxr0;
            }
            [DebuggerStepThrough]
            set
            {
                this.cxr0 = value ;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr1
        {
            [DebuggerStepThrough]
            get
            {
                return this.cxr1;
            }
            [DebuggerStepThrough]
            set
            {
                this.cxr1 = value;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr2
        {
            [DebuggerStepThrough]
            get
            {
                return this.cxr2;
            }
            [DebuggerStepThrough]
            set
            {
                this.cxr2 = value;
            }
        }

        #endregion
    }
}
