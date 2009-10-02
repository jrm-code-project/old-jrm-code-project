using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
   struct PartialResult
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode residual;

        public PartialResult (SCode residual)
        {
            this.residual = residual;
        }

        public SCode Residual
        {
            get
            {
                return this.residual;
            }
        }
    }
}
