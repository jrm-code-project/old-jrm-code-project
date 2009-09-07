using System;
using System.Diagnostics;

namespace Microcode
{
    /// <summary>
    /// Represents an object used to influence the interpreter
    /// flow of control.  SCode and other control objects are
    /// examples.
    /// </summary>
    [Serializable]
    public abstract class Control : SchemeObject
    {
        /// <summary>
        /// Causes one step of subproblem evaluation.  Caller should check the
        /// return value and re-invoke if it is true.
        /// </summary>
        /// <param name="answer">The result of the evaluation will be stored here.</param>
        /// <param name="expression">The form to be evaluated.</param>
        /// <param name="closureEnvironment">The closureEnvironment for evaluation.</param>
        /// <returns>True if further evaluation is necessary.</returns>
        public abstract bool EvalStep (out object answer, ref Control expression, ref Environment environment);
    }

    /// <summary>
    /// Represents an object used to influence the interpreter
    /// flow of control that is ideosyncratic to this interpreter.
    /// </summary>
    [Serializable]
    public abstract class SpecialControl : Control
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.SPECIAL; } }
    }
}
