using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    /// <summary>
    /// Represents an object used to influence the interpreter
    /// flow of control.  SCode and other control objects are
    /// examples.
    /// </summary>
    public abstract class Control : SchemeObject
    {
        protected Control (TC typeCode) : base (typeCode) { }
        public abstract bool EvalStep (out object answer, ref Control expression, ref Environment environment);
    }
}
