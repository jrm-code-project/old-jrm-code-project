using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    class Hunk3 : ISystemHunk3
    {
        object cxr0;
        object cxr1;
        object cxr2;

        Hunk3 (object cxr0, object cxr1, object cxr2)
        {
            this.cxr0 = cxr0;
            this.cxr1 = cxr1;
            this.cxr2 = cxr2;
        }

        internal object Cxr0
        {
            get { return this.cxr0; }
        }

        internal object Cxr1
        {
            get { return this.cxr1; }
        }
        internal object Cxr2
        {
            get { return this.cxr2; }
        }

        [SchemePrimitive ("HUNK3-CONS", 3)]
        public static object Hunk3Cons (Interpreter interpreter, object cxr0, object cxr1, object cxr2)
        {
            return interpreter.Return (new Hunk3 (cxr0, cxr1, cxr2));
        }


        #region ISystemHunk3 Members

        public object SystemHunk3Cxr0
        {
            get
            {
                return this.cxr0;
            }
            set
            {
                this.cxr0 = value ;
            }
        }

        public object SystemHunk3Cxr1
        {
            get
            {
                return this.cxr1;
            }
            set
            {
                this.cxr1 = value;
            }
        }

        public object SystemHunk3Cxr2
        {
            get
            {
                return this.cxr2;
            }
            set
            {
                this.cxr2 = value;
            }
        }

        #endregion
    }
}
