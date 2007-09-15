using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{
    [CLSCompliant (true)]
    public class Symbol
    {
        [System.Diagnostics.DebuggerBrowsable (System.Diagnostics.DebuggerBrowsableState.Never)]
        readonly string name;

        public Symbol (string name)
        {
            this.name = name;
        }

        public string Name
        {
            get
            {
                return this.name;
            }
        }

        public override string ToString ()
        {
            return this.name;
        }
    }
}
