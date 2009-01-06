using System;

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
        protected Control (TC typeCode) : base (typeCode) { }

        /// <summary>
        /// Causes one step of subproblem evaluation.  Caller should check the
        /// return value and re-invoke if it is true.
        /// </summary>
        /// <param name="answer">The result of the evaluation will be stored here.</param>
        /// <param name="expression">The form to be evaluated.</param>
        /// <param name="environment">The environment for evaluation.</param>
        /// <returns>True if further evaluation is necessary.</returns>
        public abstract bool EvalStep (out object answer, ref Control expression, ref Environment environment);
    }
}
