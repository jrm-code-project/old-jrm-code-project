using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    public struct PartialResult
    {
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
