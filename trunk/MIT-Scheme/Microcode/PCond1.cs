using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PCond1 : Conditional
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

        protected readonly Type arg0Type;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly Primitive1 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        protected PrimitiveMethod1 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly SCode arg0;

        protected PCond1 (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.procedure = predicate.Operator;
            this.method = this.procedure.Method;
            this.arg0 = predicate.Operand;
#if DEBUG
            this.arg0Type = this.arg0.GetType ();
#endif
        }

        //static SCode SpecialMake (PrimitiveCarL predicate, SCode consequent, SCode alternative)
        //{
        //    return
        //         PCond1L.Make ((PrimitiveCombination1L) PrimitiveCombination1L.Make (predicate.Operator, (LexicalVariable) predicate.Operand), consequent, alternative);
        //}

        //static SCode InvertConditional (PrimitiveNot predicate, SCode consequent, SCode alternative)
        //{
        //    //Debug.Write ("\n; InvertConditional");
        //    return Conditional.Make (predicate.Operand, alternative, consequent);
        //}

        public static SCode Make (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
        {
            return
                //(Configuration.EnableInvertConditional && predicate is PrimitiveNot) ? InvertConditional ((PrimitiveNot) predicate, consequent, alternative) :
                //(predicate is PrimitiveCarL) ? SpecialMake ((PrimitiveCarL) predicate, consequent, alternative) :
                //(predicate is PrimitiveCdr) ? Unimplemented() :
                //(predicate is PrimitiveIsBigFixnum) ? PCondIsBigFixnum.Make ((PrimitiveIsBigFixnum) predicate, consequent, alternative) :
                //(predicate is PrimitiveIsBigFlonum) ? PCondIsBigFlonum.Make ((PrimitiveIsBigFlonum) predicate, consequent, alternative) :
                //(predicate is PrimitiveIsComplex) ? PCondIsComplex.Make ((PrimitiveIsComplex) predicate, consequent, alternative) :
                //(predicate is PrimitiveIsChar) ? PCondIsChar.Make ((PrimitiveIsChar) predicate, consequent, alternative) :
                //(predicate is PrimitiveIsFixnum) ? PCondIsFixnum.Make ((PrimitiveIsFixnum) predicate, consequent, alternative) :
                //(predicate is PrimitiveIsNegative) ? PCondIsNegative.Make ((PrimitiveIsNegative) predicate, consequent, alternative) :
                (predicate is PrimitiveIsNull) ? PCondIsNull.Make ((PrimitiveIsNull) predicate, consequent, alternative) :
                (predicate is PrimitiveIsPair) ? PCondIsPair.Make ((PrimitiveIsPair) predicate, consequent, alternative) :
                //(predicate is PrimitiveIsRatnum) ? PCondIsRatnum.Make ((PrimitiveIsRatnum) predicate, consequent, alternative) :
                //(predicate is PrimitiveIsRecord) ? PCondIsRecord.Make ((PrimitiveIsRecord) predicate, consequent, alternative) :
                //(predicate is PrimitiveIsSymbol) ? PCondIsSymbol.Make ((PrimitiveIsSymbol) predicate, consequent, alternative) :
                //(predicate is PrimitiveIsVector) ? PCondIsVector.Make ((PrimitiveIsVector) predicate, consequent, alternative) :
                //(predicate is PrimitiveCombination1L) ? PCond1L.Make ((PrimitiveCombination1L) predicate, consequent, alternative) :
                //(predicate is PrimitiveCombination1Q) ? Unimplemented () :
                //(consequent is LexicalVariable) ? PCond1SL.Make (predicate, (LexicalVariable) consequent, alternative) :
                //(consequent is Quotation) ? PCond1SQ.Make (predicate, (Quotation) consequent, alternative) :
                //(alternative is LexicalVariable) ? PCond1SSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                //(alternative is Quotation) ? PCond1SSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond1.EvalStep");
            NoteCalls (this.arg0);
            procedureHistogram.Note (this.procedure);
            arg0TypeHistogram.Note (this.arg0Type);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
                //answer = Interpreter.Unwind;
                //closureEnvironment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

//    [Serializable]
//    class PCond1L : PCond1
//    {
//        public readonly object predicateName;
//        public readonly int predicateDepth;
//        public readonly int predicateOffset;

//        protected PCond1L (PrimitiveCombination1L predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.predicateName = predicate.OperandName;
//            this.predicateDepth = predicate.OperandDepth;
//            this.predicateOffset = predicate.OperandOffset;
//        }

//        public static SCode Make (PrimitiveCombination1L predicate, SCode consequent, SCode alternative)
//        {
//            return
//                (predicate is PrimitiveCombination1A) ? PCond1A.Make ((PrimitiveCombination1A) predicate, consequent, alternative)
//                : (predicate is PrimitiveCombination1L1) ? PCond1L1.Make ((PrimitiveCombination1L1) predicate, consequent, alternative)
//                : (consequent is LexicalVariable) ? PCond1LL.Make (predicate, (LexicalVariable) consequent, alternative)
//                : (consequent is Quotation) ? PCond1LQ.Make (predicate, (Quotation) consequent, alternative)
//                : (alternative is LexicalVariable) ? PCond1LSL.Make (predicate, consequent, (LexicalVariable) alternative)
//                : (alternative is Quotation) ? PCond1LSQ.Make (predicate, consequent, (Quotation) alternative)
//                : new PCond1L (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1L.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
//                throw new NotImplementedException ();

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//            SCode.location = this.procedure.Name.ToString();
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1A : PCond1L
//    {
//        protected PCond1A (PrimitiveCombination1A predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1A predicate, SCode consequent, SCode alternative)
//        {
//            return
//                (predicate is PrimitiveCombination1A0) ? PCond1A0.Make ((PrimitiveCombination1A0) predicate, consequent, alternative)
//                : (predicate is PrimitiveCombination1A1) ? PCond1A1.Make ((PrimitiveCombination1A1) predicate, consequent, alternative)
//                : (consequent is LexicalVariable) ? PCond1AL.Make (predicate, (LexicalVariable) consequent, alternative)
//                : (consequent is Quotation) ? PCond1AQ.Make (predicate, (Quotation) consequent, alternative)
//                : (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
//                : (alternative is Quotation) ? PCond1ASQ.Make (predicate, consequent, (Quotation) alternative)
//                : new PCond1A (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            #region EvalStepBody
//#if DEBUG
//            Warm ("PCond1A.EvalStep");

//#endif
//            object ev0 = closureEnvironment.ArgumentValue (this.predicateOffset);

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);

//#endif
//                expression = this.consequent;
//                return true;
//            }
//            #endregion

//        }
//    }

//    [Serializable]
//    class PCond1A0 : PCond1A
//    {
//        protected PCond1A0 (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A0.EvalStep");
//#endif

//            object ev0 = closureEnvironment.Argument0Value;


//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
//        {
//            return
//       (consequent is LexicalVariable) ? PCond1A0L.Make (predicate, (LexicalVariable) consequent, alternative)
//       : (consequent is Quotation) ? PCond1A0Q.Make (predicate, (Quotation) consequent, alternative)
//       : (alternative is LexicalVariable) ? PCond1A0SL.Make (predicate, consequent, (LexicalVariable) alternative)
//       : (alternative is Quotation) ? PCond1A0SQ.Make (predicate, consequent, (Quotation) alternative)
//       : new PCond1A0 (predicate, consequent, alternative);
//        }
//    }

//    [Serializable]
//    class PCond1A0L : PCond1A0
//    {
//        public readonly object consequentName;
//        public readonly int consequentDepth;
//        public readonly int consequentOffset;

//        protected PCond1A0L (PrimitiveCombination1A0 predicate, LexicalVariable consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentName = consequent.Name;
//            this.consequentDepth = consequent.Depth;
//            this.consequentOffset = consequent.Offset;
//        }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, LexicalVariable consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument) ? PCond1A0A.Make (predicate, (Argument) consequent, alternative) :
//                (consequent is LexicalVariable1) ? PCond1A0L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
//                (alternative is LexicalVariable) ? PCond1A0LL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCond1A0LQ.Make (predicate, consequent, (Quotation) alternative):
//                new PCond1A0L (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A0L.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1A0A : PCond1A0L
//    {

//        protected PCond1A0A (PrimitiveCombination1A0 predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, Argument consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument0) ? PCond1A0A0.Make (predicate, (Argument0) consequent, alternative) :
//                (consequent is Argument1) ? PCond1A0A1.Make (predicate, (Argument1) consequent, alternative) :
//                (alternative is LexicalVariable) ? PCond1A0LL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCond1A0LQ.Make (predicate, consequent, (Quotation) alternative) :
//                new PCond1A0A (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("PCond1A0L.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1A0A0 : PCond1A0A
//    {
//        protected PCond1A0A0 (PrimitiveCombination1A0 predicate, Argument0 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, Argument0 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented() :
//                (alternative is Quotation) ? PCond1A0A0Q.Make (predicate, consequent, (Quotation) alternative) :
//                new PCond1A0A0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A0A0.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }
//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = ev0;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCond1A0A0Q : PCond1A0A0
//    {
//        public readonly object alternativeValue;
//        PCond1A0A0Q (PrimitiveCombination1A0 predicate, Argument0 consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, Argument0 consequent, Quotation alternative)
//        {
//            return
//                new PCond1A0A0Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A0A0Q.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            object temp;
//            if (this.method (out temp, ev0)) {
//                TailCallInterpreter tci = temp as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            answer = ((temp is bool) && (bool) temp == false) ? this.alternativeValue : ev0;
//            return false;
//        }
//    }

//    [Serializable]
//    class PCond1A0A1 : PCond1A0A
//    {
//        protected PCond1A0A1 (PrimitiveCombination1A0 predicate, Argument1 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, Argument1 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? PCond1A0A1Q.Make (predicate, consequent, (Quotation) alternative) :
//                new PCond1A0A1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A0A1.EvalStep");
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, closureEnvironment.Argument0Value)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }
//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = closureEnvironment.Argument1Value;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCond1A0A1Q : PCond1A0A1
//    {
//        public readonly object alternativeValue;
//        PCond1A0A1Q (PrimitiveCombination1A0 predicate, Argument1 consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, Argument1 consequent, Quotation alternative)
//        {
//            return
//                new PCond1A0A1Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("PCond1A0A0Q.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            object temp;
//            if (this.method (out temp, ev0)) {
//                TailCallInterpreter tci = temp as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            answer = ((temp is bool) && (bool) temp == false) ? this.alternativeValue : ev0;
//            return false;
//        }
//    }

//    [Serializable]
//    class PCond1A0L1 : PCond1A0L
//    {
//        protected PCond1A0L1 (PrimitiveCombination1A0 predicate, LexicalVariable1 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, LexicalVariable1 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented():
//                (alternative is Quotation) ? Unimplemented():
//                new PCond1A0L1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A0L1.EvalStep");
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, closureEnvironment.Argument0Value)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }
//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }



//    [Serializable]
//    class PCond1A0LL : PCond1A0L
//    {
//        protected PCond1A0LL (PrimitiveCombination1A0 predicate, LexicalVariable consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        { }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, LexicalVariable consequent, LexicalVariable alternative)
//        {
//            return new PCond1A0LL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class PCond1A0LQ : PCond1A0L
//    {
//        public readonly object alternativeValue;

//        protected PCond1A0LQ (PrimitiveCombination1A0 predicate, LexicalVariable consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, LexicalVariable consequent, Quotation alternative)
//        {
//            return new PCond1A0LQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A0LQ.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {

//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1A0Q : PCond1A0
//    {
//        public readonly object consequentValue;

//        protected PCond1A0Q (PrimitiveCombination1A0 predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }
//        internal static SCode Make (PrimitiveCombination1A0 predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCond1A0QL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCond1A0QQ.Make (predicate, consequent, (Quotation) alternative):
//                new PCond1A0Q (predicate, consequent, alternative);

//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A0Q.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = this.consequentValue;
//                return false;
//            }
//        }
//    }
 
//    [Serializable]
//    class PCond1A0QL : PCond1A0Q
//    {
//        public readonly object alternativeName;
//        public readonly int alternativeDepth;
//        public readonly int alternativeOffset;

//        protected PCond1A0QL (PrimitiveCombination1A0 predicate, Quotation consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, Quotation consequent, LexicalVariable alternative)
//        {
//            return
//                (alternative is Argument) ? PCond1A0QA.Make (predicate, consequent, (Argument) alternative):
//                (alternative is LexicalVariable1) ? Unimplemented():
//                new PCond1A0QL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A0QL.EvalStep");
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, closureEnvironment.Argument0Value)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//                answer = this.consequentValue;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1A0QA : PCond1A0QL
//    {

//        protected PCond1A0QA (PrimitiveCombination1A0 predicate, Quotation consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, Quotation consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCond1A0QA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? Unimplemented () :
//                new PCond1A0QA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("PCond1A0QA.EvalStep");
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, closureEnvironment.Argument0Value)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//                answer = this.consequentValue;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCond1A0QA0 : PCond1A0QA
//    {
//        PCond1A0QA0 (PrimitiveCombination1A0 predicate, Quotation consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, Quotation consequent, Argument0 alternative)
//        {
//            return
//                new PCond1A0QA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            Primitive.hotPrimitives.Note (this.procedure);
//            SCode.location = "PCond1A0QA0.EvalStep";
//#endif
//            object ev0 = closureEnvironment.Argument0Value;
//            object temp;
//            if (this.method (out temp, ev0)) {
//                TailCallInterpreter tci = temp as TailCallInterpreter;
//                if (tci != null) {
//                    temp = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out temp, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            answer = ((temp is bool) && (bool) temp == false) ? ev0 : this.consequentValue;
//            return false;
//        }
//    }
 
//    [Serializable]
//    sealed class PCond1A0QQ : PCond1A0Q
//    {
//        public readonly object alternativeValue;

//        PCond1A0QQ (PrimitiveCombination1A0 predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }
//        internal static SCode Make (PrimitiveCombination1A0 predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            return new PCond1A0QQ (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A0QQ.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;
//            answer = (ev0 is bool && (bool) ev0 == false) ? this.alternativeValue : this.consequentValue;
//            return false;
//        }


//    }

//    [Serializable]
//    class PCond1A0SL : PCond1A0
//    {
//        public readonly object alternativeName;
//        public readonly int alternativeDepth;
//        public readonly int alternativeOffset;

//        protected PCond1A0SL (PrimitiveCombination1A0 predicate, SCode consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }
//        internal static SCode Make (PrimitiveCombination1A0 predicate, SCode consequent, LexicalVariable alternative)
//        {
//            return 
//                (alternative is Argument) ? PCond1A0SA.Make (predicate, consequent, (Argument) alternative) :
//                (alternative is LexicalVariable1) ? PCond1A0SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
//                new PCond1A0SL (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A0SL.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);

//#endif
//                expression = this.consequent;
//                return true;
//            }


//        }


//    }

//    [Serializable]
//    class PCond1A0SA : PCond1A0SL
//    {

//        protected PCond1A0SA (PrimitiveCombination1A0 predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCond1A0SA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? PCond1A0SA1.Make (predicate, consequent, (Argument1) alternative) :
//                new PCond1A0SA (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("PCond1A0SL.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);

//#endif
//                expression = this.consequent;
//                return true;
//            }


//        }


//    }

//    [Serializable]
//    sealed class PCond1A0SA0 : PCond1A0SA
//    {

//        PCond1A0SA0 (PrimitiveCombination1A0 predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new PCond1A0SA0 (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A0SA0.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                answer = ev0;
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCond1A0SA1 : PCond1A0SA
//    {

//        PCond1A0SA1 (PrimitiveCombination1A0 predicate, SCode consequent, Argument1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, SCode consequent, Argument1 alternative)
//        {
//            return
//                new PCond1A0SA1 (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A0SA1.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                answer = closureEnvironment.Argument1Value;
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }



//    [Serializable]
//    sealed class PCond1A0SL1 : PCond1A0SL
//    {
//        PCond1A0SL1 (PrimitiveCombination1A0 predicate, SCode consequent, LexicalVariable1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, SCode consequent, LexicalVariable1 alternative)
//        {
//            return
//                new PCond1A0SL1 (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A0SL1.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                if (closureEnvironment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1A0SQ : PCond1A0
//    {
//        public readonly object alternativeValue;

//        protected PCond1A0SQ (PrimitiveCombination1A0 predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveCombination1A0 predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCond1A0SQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A0SQ.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {

//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1A1 : PCond1A
//    {
//        protected PCond1A1 (PrimitiveCombination1A1 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A1.EvalStep");

//#endif
//            object ev0 = closureEnvironment.Argument1Value;

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);

//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }

//        internal static SCode Make (PrimitiveCombination1A1 predicate, SCode consequent, SCode alternative)
//        {
//            return
//                (consequent is LexicalVariable) ? PCond1A1L.Make (predicate, (LexicalVariable) consequent, alternative)
//                : (consequent is Quotation) ? PCond1A1Q.Make (predicate, (Quotation) consequent, alternative)
//                : (alternative is LexicalVariable) ? PCond1A1SL.Make (predicate, consequent, (LexicalVariable) alternative)
//                : (alternative is Quotation) ? PCond1A1SQ.Make (predicate, consequent, (Quotation) alternative)
//                : new PCond1A1 (predicate, consequent, alternative);
//        }
//    }

//    [Serializable]
//    class PCond1A1L : PCond1A1
//    {
//        public readonly object consequentName;
//        public readonly int consequentDepth;
//        public readonly int consequentOffset;

//        protected PCond1A1L (PrimitiveCombination1A1 predicate, LexicalVariable consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentName = consequent.Name;
//            this.consequentDepth = consequent.Depth;
//            this.consequentOffset = consequent.Offset;

//        }

//        internal static SCode Make (PrimitiveCombination1A1 predicate, LexicalVariable consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument) ? PCond1A1A.Make (predicate, (Argument) consequent, alternative) :
//                (consequent is LexicalVariable1) ? PCond1A1L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
//                (alternative is LexicalVariable) ? PCond1A1LL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCond1A1LQ.Make (predicate, consequent, (Quotation) alternative) :
//                new PCond1A1L (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            #region EvalStepBody
//#if DEBUG
//            Warm ("PCond1A1L.EvalStep");

//#endif
//            object ev0 = closureEnvironment.Argument1Value;
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            #endregion
//        }
//    }

//    [Serializable]
//    class PCond1A1A : PCond1A1L
//    {
//        protected PCond1A1A (PrimitiveCombination1A1 predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1A1 predicate, Argument consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument0) ? PCond1A1A0.Make (predicate, (Argument0) consequent, alternative) :
//                (consequent is Argument1) ? PCond1A1A1.Make (predicate, (Argument1) consequent, alternative) :
//                (alternative is LexicalVariable) ? Unimplemented() :
//                (alternative is Quotation) ? Unimplemented() :
//                new PCond1A1A (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//            #region EvalStepBody
//#if DEBUG
//            Warm ("PCond1A1L.EvalStep");

//#endif
//            object ev0 = closureEnvironment.Argument1Value;
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            #endregion

//        }


//    }

//    [Serializable]
//    class PCond1A1A0 : PCond1A1A
//    {
//        protected PCond1A1A0 (PrimitiveCombination1A1 predicate, Argument0 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1A1 predicate, Argument0 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? Unimplemented () :
//                new PCond1A1A0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A1A0.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument1Value;
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = closureEnvironment.Argument0Value;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1A1A1 : PCond1A1A
//    {
//        protected PCond1A1A1 (PrimitiveCombination1A1 predicate, Argument1 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1A1 predicate, Argument1 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? Unimplemented () :
//                new PCond1A1A1 (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A1A1.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument1Value;
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = ev0;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1A1L1 : PCond1A1L
//    {
//        protected PCond1A1L1 (PrimitiveCombination1A1 predicate, LexicalVariable1 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1A1 predicate, LexicalVariable1 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? Unimplemented () :
//                new PCond1A1L1 (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//            #region EvalStepBody
//#if DEBUG
//            Warm ("PCond1A1L.EvalStep");

//#endif
//            object ev0 = closureEnvironment.Argument1Value;
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            #endregion

//        }


//    }



//    [Serializable]
//    class PCond1A1LL : PCond1A1L
//    {
//        protected PCond1A1LL (PrimitiveCombination1A1 predicate, LexicalVariable consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        { }

//        internal static SCode Make (PrimitiveCombination1A1 predicate, LexicalVariable consequent, LexicalVariable alternative)
//        {
//            return new PCond1A1LL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class PCond1A1LQ : PCond1A1L
//    {
//        public readonly object alternativeValue;

//        protected PCond1A1LQ (PrimitiveCombination1A1 predicate, LexicalVariable consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveCombination1A1 predicate, LexicalVariable consequent, Quotation alternative)
//        {
//            return new PCond1A1LQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            object ev0 = closureEnvironment.Argument1Value;

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {

//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1A1Q : PCond1A1
//    {
//        public readonly object consequentValue;
//        protected PCond1A1Q (PrimitiveCombination1A1 predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }
//        internal static SCode Make (PrimitiveCombination1A1 predicate, Quotation consequent, SCode alternative)
//        {
//            return
//        (alternative is LexicalVariable) ? PCond1A1QL.Make (predicate, consequent, (LexicalVariable) alternative)
//        : (alternative is Quotation) ? PCond1A1QQ.Make (predicate, consequent, (Quotation) alternative)
//        : new PCond1A1Q (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A1Q.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument1Value;

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = this.consequentValue;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1A1QL : PCond1A1Q
//    {
//        protected PCond1A1QL (PrimitiveCombination1A1 predicate, Quotation consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        { }
//        internal static SCode Make (PrimitiveCombination1A1 predicate, Quotation quotation, LexicalVariable alternative)
//        {

//            throw new NotImplementedException ();
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }


//    }

//    [Serializable]
//    class PCond1A1QQ : PCond1A1Q
//    {
//        protected PCond1A1QQ (PrimitiveCombination1A1 predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        { }
//        internal static SCode Make (PrimitiveCombination1A1 predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            throw new NotImplementedException ();
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }


//    }

//    [Serializable]
//    class PCond1A1SL : PCond1A1
//    {
//        public readonly object alternativeName;
//        public readonly int alternativeDepth;
//        public readonly int alternativeOffset;

//        protected PCond1A1SL (PrimitiveCombination1A1 predicate, SCode consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }
//        internal static SCode Make (PrimitiveCombination1A1 predicate, SCode consequent, LexicalVariable alternative)
//        {
//            return 
//                (alternative is Argument) ? PCond1A1SA.Make (predicate, consequent, (Argument) alternative) :
//                (alternative is LexicalVariable1) ? Unimplemented():
//                new PCond1A1SL (predicate, consequent, alternative);

//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            #region EvalStepBody
//#if DEBUG
//            Warm ("PCond1A1SL.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument1Value;
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);

//#endif
//                expression = this.consequent;
//                return true;
//            }
//            #endregion

//        }


//    }

//    [Serializable]
//    class PCond1A1SA : PCond1A1SL
//    {

//        protected PCond1A1SA (PrimitiveCombination1A1 predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1A1 predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCond1A1SA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? PCond1A1SA1.Make (predicate, consequent, (Argument1) alternative) :
//                new PCond1A1SA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            Unimplemented ();
//            #region EvalStepBody
//#if DEBUG
//            Warm ("PCond1A1SL.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument1Value;
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);

//#endif
//                expression = this.consequent;
//                return true;
//            }
//            #endregion

//        }


//    }

//    [Serializable]
//    sealed class PCond1A1SA0 : PCond1A1SA
//    {
//        PCond1A1SA0 (PrimitiveCombination1A1 predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1A1 predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new PCond1A1SA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A1SA0.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument1Value;
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                answer = closureEnvironment.Argument0Value;
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCond1A1SA1 : PCond1A1SA
//    {
//        PCond1A1SA1 (PrimitiveCombination1A1 predicate, SCode consequent, Argument1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1A1 predicate, SCode consequent, Argument1 alternative)
//        {
//            return
//                new PCond1A1SA1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A1SA0.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument1Value;
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                answer = closureEnvironment.Argument1Value;
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCond1A1SQ : PCond1A1
//    {
//        public readonly object alternativeValue;

//        PCond1A1SQ (PrimitiveCombination1A1 predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveCombination1A1 predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCond1A1SQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1A1SQ.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument1Value;

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {

//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1AL : PCond1A
//    {
//        public readonly object consequentName;
//        public readonly int consequentDepth;
//        public readonly int consequentOffset;

//        protected PCond1AL (PrimitiveCombination1A predicate, LexicalVariable consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentName = consequent.Name;
//            this.consequentDepth = consequent.Depth;
//            this.consequentOffset = consequent.Offset;
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            #region EvalStepBody
//#if DEBUG
//            Warm ();

//#endif
//            object ev0 = closureEnvironment.ArgumentValue (this.predicateOffset);
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);

//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            #endregion
//        }

//        internal static SCode Make (PrimitiveCombination1A predicate, LexicalVariable consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument) ? PCond1AA.Make (predicate, (Argument) consequent, alternative) :
//                (consequent is LexicalVariable1) ? Unimplemented():
//                (alternative is LexicalVariable) ? PCond1ALL.Make (predicate, consequent, (LexicalVariable) alternative)
//                : (alternative is Quotation) ? PCond1ALQ.Make (predicate, consequent, (Quotation) alternative)
//                : new PCond1AL (predicate, consequent, alternative);
//        }
//    }

//    [Serializable]
//    class PCond1AA : PCond1AL
//    {
//        protected PCond1AA (PrimitiveCombination1A predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        { }

//        static public SCode Make (PrimitiveCombination1A predicate, Argument consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument0) ? PCond1AA0.Make (predicate, (Argument0) consequent, alternative) :
//                (consequent is Argument1) ? PCond1AA1.Make (predicate, (Argument1) consequent, alternative) :
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? Unimplemented () :
//                new PCond1AA (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1AA.EvalStep");
//#endif
//            object ev0 = closureEnvironment.ArgumentValue (this.predicateOffset);
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = closureEnvironment.ArgumentValue (this.consequentOffset);
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1AA0 : PCond1AA
//    {
//        protected PCond1AA0 (PrimitiveCombination1A predicate, Argument0 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        { }

//        static public SCode Make (PrimitiveCombination1A predicate, Argument0 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? Unimplemented () :
//                new PCond1AA0 (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1AA0.EvalStep");
//#endif
//            object ev0 = closureEnvironment.ArgumentValue (this.predicateOffset);
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = closureEnvironment.Argument0Value;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1AA1 : PCond1AA
//    {
//        protected PCond1AA1 (PrimitiveCombination1A predicate, Argument1 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        { }

//        static public SCode Make (PrimitiveCombination1A predicate, Argument1 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? Unimplemented () :
//                new PCond1AA1 (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }

//    }



//    [Serializable]
//    class PCond1AL1 : PCond1AL
//    {
//        protected PCond1AL1 (PrimitiveCombination1A predicate, LexicalVariable1 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        { }
//        static public SCode Make (PrimitiveCombination1A predicate, LexicalVariable1 consequent, SCode alternative)
//        {
//            throw new NotImplementedException ();
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class PCond1ALL : PCond1AL
//    {
//        protected PCond1ALL (PrimitiveCombination1A predicate, LexicalVariable consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        { }
//        static public SCode Make (PrimitiveCombination1A predicate, LexicalVariable consequent, LexicalVariable alternative)
//        {
//            throw new NotImplementedException ();
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class PCond1ALQ : PCond1AL
//    {
//        protected PCond1ALQ (PrimitiveCombination1A predicate, LexicalVariable consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        { }
//        static public SCode Make (PrimitiveCombination1A predicate, LexicalVariable consequent, Quotation alternative)
//        {
//            throw new NotImplementedException ();
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class PCond1AQ : PCond1A
//    {
//        public readonly object consequentValue;

//        protected PCond1AQ (PrimitiveCombination1A predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1AQ.EvalStep");
//#endif
//            object ev0 = closureEnvironment.ArgumentValue (this.predicateOffset);

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = this.consequentValue;
//                return false;
//            }

//        }

//        internal static SCode Make (PrimitiveCombination1A predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCond1AQL.Make (predicate, consequent, (LexicalVariable) alternative)
//                : (alternative is Quotation) ? PCond1AQQ.Make (predicate, consequent, (Quotation) alternative)
//                : new PCond1AQ (predicate, consequent, alternative);
//        }
//    }

//    [Serializable]
//    class PCond1AQL : PCond1AQ
//    {
//        protected PCond1AQL (PrimitiveCombination1A predicate, Quotation consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }

//        internal static SCode Make (PrimitiveCombination1A predicate, Quotation consequent, LexicalVariable alternative)
//        {
//            return new PCond1AQL (predicate, consequent, alternative);
//        }
//    }

//    [Serializable]
//    class PCond1AQQ : PCond1AQ
//    {
//        protected PCond1AQQ (PrimitiveCombination1A predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }

//        internal static SCode Make (PrimitiveCombination1A predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            return new PCond1AQQ (predicate, consequent, alternative);
//        }
//    }

//    [Serializable]
//    class PCond1ASL : PCond1A
//    {
//        protected PCond1ASL (PrimitiveCombination1A predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }

//        internal static PCond1A Make (PrimitiveCombination1A predicate, SCode consequent, LexicalVariable lexicalVariable)
//        {
//            throw new NotImplementedException ();
//        }
//    }
 
//    [Serializable]
//    class PCond1ASQ : PCond1A
//    {
//        public readonly object alternativeValue;

//        protected PCond1ASQ (PrimitiveCombination1A predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1ASQ.EvalStep");
//#endif
//            object ev0 = closureEnvironment.ArgumentValue (predicateOffset);

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {

//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }

//        internal static PCond1A Make (PrimitiveCombination1A predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCond1ASQ (predicate, consequent, alternative);
//        }
//    }

//    [Serializable]
//    class PCond1L1 : PCond1L
//    {
//        protected PCond1L1 (PrimitiveCombination1L1 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1L1 predicate, SCode consequent, SCode alternative)
//        {
//            return
//       (consequent is LexicalVariable) ? PCond1L1L.Make (predicate, (LexicalVariable) consequent, alternative)
//       : (consequent is Quotation) ? PCond1L1Q.Make (predicate, (Quotation) consequent, alternative)
//       : (alternative is LexicalVariable) ? PCond1L1SL.Make (predicate, consequent, (LexicalVariable) alternative)
//       : (alternative is Quotation) ? PCond1L1SQ.Make (predicate, consequent, (Quotation) alternative)
//       : new PCond1L1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            #region EvalStepBody
//#if DEBUG
//            Warm ("PCond1L1.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);

//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//            #endregion

//        }
//    }

//    [Serializable]
//    class PCond1L1L : PCond1L1
//    {
//        public readonly object consequentName;
//        public readonly int consequentDepth;
//        public readonly int consequentOffset;

//        protected PCond1L1L (PrimitiveCombination1L1 predicate, LexicalVariable consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentName = consequent.Name;
//            this.consequentDepth = consequent.Depth;
//            this.consequentOffset = consequent.Offset;

//        }

//        internal static SCode Make (PrimitiveCombination1L1 predicate, LexicalVariable consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument) ? PCond1L1A.Make (predicate, (Argument) consequent, alternative) :
//                (consequent is LexicalVariable1) ? PCond1L1L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
//                (alternative is LexicalVariable) ? PCond1L1LL.Make (predicate, consequent, (LexicalVariable) alternative)
//                : (alternative is Quotation) ? PCond1L1LQ.Make (predicate, consequent, (Quotation) alternative)
//                : new PCond1L1L (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            #region EvalStepBody
//#if DEBUG
//            Warm ();
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);

//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            #endregion

//        }


//    }

//    [Serializable]
//    class PCond1L1A : PCond1L1L
//    {
//        protected PCond1L1A (PrimitiveCombination1L1 predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1L1 predicate, Argument consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument0) ? PCond1L1A0.Make (predicate, (Argument0) consequent, alternative) :
//                (consequent is Argument1) ? PCond1L1A1.Make (predicate, (Argument1) consequent, alternative) :
//                (alternative is LexicalVariable) ? Unimplemented():
//                (alternative is Quotation) ? Unimplemented():
//                new PCond1L1A (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            Unimplemented ();
//            #region EvalStepBody
//#if DEBUG
//            Warm ();
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);

//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            #endregion

//        }


//    }

//    [Serializable]
//    class PCond1L1A0 : PCond1L1A
//    {
//        protected PCond1L1A0 (PrimitiveCombination1L1 predicate, Argument0 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1L1 predicate, Argument0 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? Unimplemented () :
//                new PCond1L1A0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1L1A0.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = closureEnvironment.Argument0Value;
//                return false;
//            }
//        }
//    }



//    [Serializable]
//    class PCond1L1A1 : PCond1L1A
//    {
//        protected PCond1L1A1 (PrimitiveCombination1L1 predicate, Argument1 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1L1 predicate, Argument1 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? Unimplemented () :
//                new PCond1L1A1 (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            Unimplemented ();
//            #region EvalStepBody
//#if DEBUG
//            Warm ();
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);

//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            #endregion

//        }


//    }



//    [Serializable]
//    class PCond1L1L1 : PCond1L1L
//    {
       
//        protected PCond1L1L1 (PrimitiveCombination1L1 predicate, LexicalVariable1 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveCombination1L1 predicate, LexicalVariable1 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented() :
//                (alternative is Quotation) ? Unimplemented():
//                new PCond1L1L1 (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1L1L1.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);

//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }

//        }


//    }



//    [Serializable]
//    class PCond1L1LL : PCond1L1L
//    {
//        protected PCond1L1LL (PrimitiveCombination1L1 predicate, LexicalVariable consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        { }

//        internal static SCode Make (PrimitiveCombination1L1 predicate, LexicalVariable consequent, LexicalVariable alternative)
//        {
//            return new PCond1L1LL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class PCond1L1LQ : PCond1L1L
//    {
//        public readonly object alternativeValue;

//        protected PCond1L1LQ (PrimitiveCombination1L1 predicate, LexicalVariable consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveCombination1L1 predicate, LexicalVariable consequent, Quotation alternative)
//        {
//            return new PCond1L1LQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            object ev0 = closureEnvironment.Argument1Value;

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {

//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1L1Q : PCond1L1
//    {
//        public readonly object consequentValue;

//        protected PCond1L1Q (PrimitiveCombination1L1 predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }
//        internal static SCode Make (PrimitiveCombination1L1 predicate, Quotation consequent, SCode alternative)
//        {
//            return
//        (alternative is LexicalVariable) ? PCond1L1QL.Make (predicate, consequent, (LexicalVariable) alternative)
//        : (alternative is Quotation) ? PCond1L1QQ.Make (predicate, consequent, (Quotation) alternative)
//        : new PCond1L1Q (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            #region EvalStepBody
//#if DEBUG
//            Warm ("PCond1LQ.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = this.consequentValue;
//                return false;
//            }
//            #endregion

//        }
//    }

//    [Serializable]
//    class PCond1L1QL : PCond1L1Q
//    {
//        protected PCond1L1QL (PrimitiveCombination1L1 predicate, Quotation consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        { }
//        internal static SCode Make (PrimitiveCombination1L1 predicate, Quotation quotation, LexicalVariable alternative)
//        {

//            throw new NotImplementedException ();
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }


//    }

//    [Serializable]
//    class PCond1L1QQ : PCond1L1Q
//    {
//        protected PCond1L1QQ (PrimitiveCombination1L1 predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        { }
//        internal static SCode Make (PrimitiveCombination1L1 predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            return new PCond1L1QQ (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }


//    }

//    [Serializable]
//    class PCond1L1SL : PCond1L1
//    {
//        public readonly object alternativeName;
//        public readonly int alternativeDepth;
//        public readonly int alternativeOffset;

//        protected PCond1L1SL (PrimitiveCombination1L1 predicate, SCode consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }
//        internal static SCode Make (PrimitiveCombination1L1 predicate, SCode consequent, LexicalVariable alternative)
//        {
//            return new PCond1L1SL (predicate, consequent, alternative);

//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1L1SL.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//            SCode.location = this.procedure.Name.ToString();
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }


//    }

//    [Serializable]
//    sealed class PCond1L1SQ : PCond1L1
//    {
//        public readonly object alternativeValue;

//        PCond1L1SQ (PrimitiveCombination1L1 predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveCombination1L1 predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCond1L1SQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1L1SQ.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1LL : PCond1L
//    {
//        public readonly object consequentName;
//        public readonly int consequentDepth;
//        public readonly int consequentOffset;

//        protected PCond1LL (PrimitiveCombination1L predicate, LexicalVariable consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentName = consequent.Name;
//            this.consequentDepth = consequent.Depth;
//            this.consequentOffset = consequent.Offset;
//        }

//        public static SCode Make (PrimitiveCombination1L predicate, LexicalVariable consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument) ? PCond1LA.Make (predicate, (Argument) consequent, alternative) :
//                (consequent is LexicalVariable1) ? Unimplemented():
//                (alternative is LexicalVariable) ? PCond1LLL.Make (predicate, consequent, alternative):
//                (alternative is Quotation) ? PCond1LLQ.Make (predicate, consequent, alternative):
//                new PCond1LL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1LL.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
//                throw new NotImplementedException ();

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//            SCode.location = this.procedure.Name.ToString();
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1LA : PCond1LL
//    {
//        protected PCond1LA (PrimitiveCombination1L predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1L predicate, Argument consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument0) ? PCond1LA0.Make (predicate, (Argument0) consequent, alternative) :
//                (consequent is Argument1) ? Unimplemented () :
//                (alternative is LexicalVariable) ? Unimplemented() :
//                (alternative is Quotation) ? Unimplemented() :
//                new PCond1LA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("PCond1LL.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
//                throw new NotImplementedException ();

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//            SCode.location = this.procedure.Name.ToString();
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1LA0 : PCond1LA
//    {
//        protected PCond1LA0 (PrimitiveCombination1L predicate, Argument0 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1L predicate, Argument0 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? Unimplemented () :
//                new PCond1LA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("PCond1LL.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
//                throw new NotImplementedException ();

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//            SCode.location = this.procedure.Name.ToString();
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return true;
//            }
//        }
//    }



//    [Serializable]
//    class PCond1LLL : PCond1LL
//    {
//        protected PCond1LLL (PrimitiveCombination1L predicate, LexicalVariable consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1L predicate, LexicalVariable consequent, LexicalVariable alternative)
//        {
//            return 
//                (alternative is Argument) ? Unimplemented():
//                (alternative is LexicalVariable1) ? Unimplemented():
//                new PCond1LLL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class PCond1LLQ : PCond1LL
//    {
//        protected PCond1LLQ (PrimitiveCombination1L predicate, LexicalVariable consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1L predicate, LexicalVariable consequent, Quotation alternative)
//        {
//            return new PCond1LLQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class PCond1LQ : PCond1L
//    {
//        public readonly object consequentValue;
//        protected PCond1LQ (PrimitiveCombination1L predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }

//        public static SCode Make (PrimitiveCombination1L predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCond1LQL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCond1LQQ.Make (predicate, consequent, (Quotation) alternative) :
//              new PCond1LQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1LQ.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
//                throw new NotImplementedException ();

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//            SCode.location = this.procedure.Name.ToString();
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = this.consequentValue;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1LQL : PCond1LQ
//    {
//        protected PCond1LQL (PrimitiveCombination1L predicate, Quotation consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1L predicate, Quotation consequent, LexicalVariable alternative)
//        {
//            return 
//                (alternative is Argument) ? Unimplemented():
//                (alternative is LexicalVariable1) ? Unimplemented():
//                new PCond1LQL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class PCond1LQQ : PCond1LQ
//    {
//        protected PCond1LQQ (PrimitiveCombination1L predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1L predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            return new PCond1LQQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class PCond1LSL : PCond1L
//    {
//        public readonly object alternativeName;
//        public readonly int alternativeDepth;
//        public readonly int alternativeOffset;

//        protected PCond1LSL (PrimitiveCombination1L predicate, SCode consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }

//        public static SCode Make (PrimitiveCombination1L predicate, SCode consequent, LexicalVariable alternative)
//        {
//            return 
//                (alternative is Argument) ? PCond1LSA.Make (predicate, consequent, (Argument) alternative) :
//                (alternative is LexicalVariable1) ? Unimplemented():
//                new PCond1LSL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1LSL.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
//                throw new NotImplementedException ();

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//            SCode.location = this.procedure.Name.ToString();
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1LSA : PCond1LSL
//    {
//        protected PCond1LSA (PrimitiveCombination1L predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1L predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCond1LSA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? PCond1LSA1.Make (predicate, consequent, (Argument1) alternative) :
//                new PCond1LSA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("PCond1LSL.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
//                throw new NotImplementedException ();

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//            SCode.location = this.procedure.Name.ToString ();
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1LSA0 : PCond1LSA
//    {
//        protected PCond1LSA0 (PrimitiveCombination1L predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1L predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new PCond1LSA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1LSA0.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
//                throw new NotImplementedException ();
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//            SCode.location = this.procedure.Name.ToString ();
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                answer = closureEnvironment.Argument0Value;
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1LSA1 : PCond1LSA
//    {
//        protected PCond1LSA1 (PrimitiveCombination1L predicate, SCode consequent, Argument1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1L predicate, SCode consequent, Argument1 alternative)
//        {
//            return
//                new PCond1LSA1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("PCond1LSL.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
//                throw new NotImplementedException ();

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//            SCode.location = this.procedure.Name.ToString ();
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }



//    [Serializable]
//    class PCond1LSQ : PCond1L
//    {
//        public readonly object alternativeValue;

//        protected PCond1LSQ (PrimitiveCombination1L predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (PrimitiveCombination1L predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCond1LSQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            #region EvalStepBody
//#if DEBUG
//            Warm ("PCondL1SQ.EvalStep");

//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
//                throw new NotImplementedException ();
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);

//#endif
//                expression = this.consequent;
//                return true;
//            }
//            #endregion

//        }
//    }

//    [Serializable]
//    class PCond1SL : PCond1
//    {
//        public readonly object consequentName;
//        public readonly int consequentDepth;
//        public readonly int consequentOffset;

//        protected PCond1SL (PrimitiveCombination1 predicate, LexicalVariable consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentName = consequent.Name;
//            this.consequentDepth = consequent.Depth;
//            this.consequentOffset = consequent.Offset;
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, LexicalVariable consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument) ? PCond1SA.Make (predicate, (Argument) consequent, alternative) :
//                (consequent is LexicalVariable1) ? PCond1SL1.Make (predicate, (LexicalVariable1) consequent, alternative) :
//                (alternative is LexicalVariable) ? PCond1SLL.Make (predicate, consequent, (LexicalVariable) alternative):
//                (alternative is Quotation) ? PCond1SLQ.Make (predicate, consequent, (Quotation) alternative):
//                new PCond1SL (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            #region EvalStepBody
//#if DEBUG
//            Warm ("PCond1SL.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            #endregion
//        }
//    }

//    [Serializable]
//    class PCond1SA : PCond1SL
//    {
//        protected PCond1SA (PrimitiveCombination1 predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, Argument consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument0) ? PCond1SA0.Make (predicate, (Argument0) consequent, alternative) :
//                (consequent is Argument1) ? PCond1SA1.Make (predicate, (Argument1) consequent, alternative) :
//                (alternative is LexicalVariable) ? Unimplemented():
//                (alternative is Quotation) ? PCond1SAQ.Make (predicate, consequent, (Quotation) alternative) :
//                new PCond1SA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1SA.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = closureEnvironment.ArgumentValue (this.consequentOffset);
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1SA0 : PCond1SA
//    {
//        protected PCond1SA0 (PrimitiveCombination1 predicate, Argument0 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, Argument0 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? PCond1SA0Q.Make (predicate, consequent, (Quotation) alternative) :
//                new PCond1SA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1SA0.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = closureEnvironment.Argument0Value;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCond1SA0Q : PCond1SA0
//    {
//        public readonly object alternativeValue;
//        PCond1SA0Q (PrimitiveCombination1 predicate, Argument0 consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, Argument0 consequent, Quotation alternative)
//        {
//            return
//                new PCond1SA0Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1SA0Q.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            answer = ((answer is bool) && (bool) answer == false) ? this.alternativeValue : closureEnvironment.Argument0Value;
//            return false;
//        }
//    }

//    [Serializable]
//    class PCond1SA1 : PCond1SA
//    {
//        protected PCond1SA1 (PrimitiveCombination1 predicate, Argument1 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, Argument1 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? Unimplemented () :
//                new PCond1SA1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1SA1.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = closureEnvironment.Argument1Value;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCond1SAQ : PCond1SA
//    {
//        public readonly object alternativeValue;
//        PCond1SAQ (PrimitiveCombination1 predicate, Argument consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, Argument consequent, Quotation alternative)
//        {
//            return
//                new PCond1SAQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1SAQ.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            answer = ((answer is bool) && (bool) answer == false) ? 
//                this.alternativeValue : 
//                closureEnvironment.ArgumentValue (this.consequentOffset) ;
//            return false;
//        }
//    }

//    [Serializable]
//    class PCond1SL1 : PCond1SL
//    {
//        protected PCond1SL1 (PrimitiveCombination1 predicate, LexicalVariable1 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, LexicalVariable1 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? Unimplemented () :
//                new PCond1SL1 (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.arg0);
//            SCode.location = "PCond1SL1.EvalStep";
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1SLL : PCond1SL
//    {
//        public readonly object alternativeName;
//        public readonly int alternativeDepth;
//        public readonly int alternativeOffset;

//        protected PCond1SLL (PrimitiveCombination1 predicate, LexicalVariable consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, LexicalVariable consequent, LexicalVariable alternative)
//        {
//            return
//                (alternative is Argument) ? Unimplemented () :
//                (alternative is LexicalVariable1) ? PCond1SLL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
//                new PCond1SLL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    sealed class PCond1SLL1 : PCond1SLL
//    {

//        PCond1SLL1 (PrimitiveCombination1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
//        {
//            return
//                new PCond1SLL1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }



//    [Serializable]
//    sealed class PCond1SLQ : PCond1SL
//    {
//        public readonly object alternativeValue;

//        PCond1SLQ (PrimitiveCombination1 predicate, LexicalVariable consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }


//        public static SCode Make (PrimitiveCombination1 predicate, LexicalVariable consequent, Quotation alternative)
//        {
//            return new PCond1SLQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.arg0);
//            SCode.location = "PCond1SLQ.EvalStep";
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };

//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//            SCode.location = this.procedure.Name.ToString();
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }
//#if DEBUG
//                        SCode.location = "PCond1SLQ.EvalStep.1";
//#endif
//            if ((answer is bool) && (bool) answer == false) {
//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1SQ : PCond1
//    {
//        public readonly object consequentValue;

//        protected PCond1SQ (PrimitiveCombination1 predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCond1SQL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCond1SQQ.Make (predicate, consequent, (Quotation) alternative) :
//                new PCond1SQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1SQ.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                NoteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = this.consequentValue;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1SQL : PCond1SQ
//    {
//        public readonly object alternativeName;
//        public readonly int alternativeDepth;
//        public readonly int alternativeOffset;

//        protected PCond1SQL (PrimitiveCombination1 predicate, Quotation consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, Quotation consequent, LexicalVariable alternative)
//        {
//            return
//                (alternative is Argument) ? PCond1SQA.Make (predicate, consequent, (Argument) alternative) :
//                (alternative is LexicalVariable1) ? PCond1SQL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
//                 new PCond1SQL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class PCond1SQA : PCond1SQL
//    {
//        protected PCond1SQA (PrimitiveCombination1 predicate, Quotation consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, Quotation consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCond1SQA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? PCond1SQA1.Make (predicate, consequent, (Argument1) alternative) :
//                new PCond1SQA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    sealed class PCond1SQA0 : PCond1SQA
//    {
//        PCond1SQA0 (PrimitiveCombination1 predicate, Quotation consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }


//        public static SCode Make (PrimitiveCombination1 predicate, Quotation consequent, Argument0 alternative)
//        {
//            return
//                  new PCond1SQA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1SQA0.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            answer = ((answer is bool) && (bool) answer == false) ? closureEnvironment.Argument0Value : this.consequentValue;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PCond1SQA1 : PCond1SQA
//    {
//        PCond1SQA1 (PrimitiveCombination1 predicate, Quotation consequent, Argument1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, Quotation consequent, Argument1 alternative)
//        {
//            return
//                  new PCond1SQA1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    sealed class PCond1SQL1 : PCond1SQL
//    {
//        PCond1SQL1 (PrimitiveCombination1 predicate, Quotation consequent, LexicalVariable1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, Quotation consequent, LexicalVariable1 alternative)
//        {
//            return
//                  new PCond1SQL1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    sealed class PCond1SQQ : PCond1SQ
//    {
//        public readonly object alternativeValue;

//        PCond1SQQ (PrimitiveCombination1 predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, Quotation consequent, Quotation alternative)
//        {
//            //if (consequent.Quoted == alternative.Quoted) {
//            //    Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//            //    return Sequence2.Make (predicate, consequent);
//            //}
//            //else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//            //    Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//            //    return Sequence2.Make (predicate, alternative);
//            //}
//            //else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//            //    Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//            //    return Sequence2.Make (predicate, consequent);
//            //}

//            return new PCond1SQQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            #region EvalStepBody
//#if DEBUG
//            Warm ("PCond1SQQ.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//                answer = this.consequentValue;
//                return false;
//            }
//            #endregion
//        }
//    }

//    [Serializable]
//    class PCond1SSL : PCond1
//    {
//        public readonly object alternativeName;
//        public readonly int alternativeDepth;
//        public readonly int alternativeOffset;

//        protected PCond1SSL (PrimitiveCombination1 predicate, SCode consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, SCode consequent, LexicalVariable alternative)
//        {
//            return 
//                (alternative is Argument) ? PCond1SSA.Make (predicate, consequent, (Argument) alternative) :
//                (alternative is LexicalVariable1) ? PCond1SSL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
//                new PCond1SSL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1SSL.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }


//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PCond1SSA : PCond1SSL
//    {
//        protected PCond1SSA (PrimitiveCombination1 predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCond1SSA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? PCond1SSA1.Make (predicate, consequent, (Argument1) alternative) :
//                new PCond1SSA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1SSA.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                answer = closureEnvironment.ArgumentValue (this.alternativeOffset);
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCond1SSA0 : PCond1SSA
//    {
//        PCond1SSA0 (PrimitiveCombination1 predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                 new PCond1SSA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1SSA0.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }


//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                answer = closureEnvironment.Argument0Value;
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCond1SSA1 : PCond1SSA
//    {
//        PCond1SSA1 (PrimitiveCombination1 predicate, SCode consequent, Argument1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, SCode consequent, Argument1 alternative)
//        {
//            return
//                 new PCond1SSA1 (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1SSA1.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }


//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                answer = closureEnvironment.Argument1Value;
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCond1SSL1 : PCond1SSL
//    {
//        PCond1SSL1 (PrimitiveCombination1 predicate, SCode consequent, LexicalVariable1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, SCode consequent, LexicalVariable1 alternative)
//        {
//            return
//                 new PCond1SSL1 (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1SSL1.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }


//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                if (closureEnvironment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCond1SSQ : PCond1
//    {
//        public readonly object alternativeValue;

//        PCond1SSQ (PrimitiveCombination1 predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCond1SSQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PCond1SSQ.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }
}
