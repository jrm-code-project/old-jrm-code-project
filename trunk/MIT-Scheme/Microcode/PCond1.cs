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

        //static SCode InvertConditional (PrimitiveNot predicate, SCode consequent, SCode alternative)
        //{
        //    //Debug.Write ("\n; InvertConditional");
        //    return Conditional.Make (predicate.Operand, alternative, consequent);
        //}

        public static SCode Make (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
        {
            return
                //(Configuration.EnableInvertConditional && predicate is PrimitiveNot) ? InvertConditional ((PrimitiveNot) predicate, consequent, alternative) :
                (Configuration.EnableInlinePCond1 && predicate is PrimitiveCar) ? PCondCar.Make ((PrimitiveCar) predicate, consequent, alternative) :
                (Configuration.EnableInlinePCond1 && predicate is PrimitiveIsType<long>) ? PCondIsType<long>.Make ((PrimitiveIsType<long>) predicate, consequent, alternative) :
                (Configuration.EnableInlinePCond1 && predicate is PrimitiveIsType<double>) ? PCondIsType<double>.Make ((PrimitiveIsType<double>) predicate, consequent, alternative) :
                (Configuration.EnableInlinePCond1 && predicate is PrimitiveIsType<Complex>) ? PCondIsType<Complex>.Make ((PrimitiveIsType<Complex>) predicate, consequent, alternative) :
                (Configuration.EnableInlinePCond1 && predicate is PrimitiveIsType<char>) ? PCondIsType<char>.Make ((PrimitiveIsType<char>) predicate, consequent, alternative) :
                (Configuration.EnableInlinePCond1 && predicate is PrimitiveIsType<char []>) ? PCondIsType<char []>.Make ((PrimitiveIsType<char []>) predicate, consequent, alternative) :
                (Configuration.EnableInlinePCond1 && predicate is PrimitiveIsType<int>) ? PCondIsType<int>.Make ((PrimitiveIsType<int>) predicate, consequent, alternative) :
                (Configuration.EnableInlinePCond1 && predicate is PrimitiveIsNull) ? PCondIsNull.Make ((PrimitiveIsNull) predicate, consequent, alternative) :
                (Configuration.EnableInlinePCond1 && predicate is PrimitiveIsType<Cons>) ? PCondIsType<Cons>.Make ((PrimitiveIsType<Cons>) predicate, consequent, alternative) :
                (Configuration.EnableInlinePCond1 && predicate is PrimitiveIsType<Entity>) ? PCondIsType<Entity>.Make ((PrimitiveIsType<Entity>) predicate, consequent, alternative) :
                (Configuration.EnableInlinePCond1 && predicate is PrimitiveIsType<Ratnum>) ? PCondIsType<Ratnum>.Make ((PrimitiveIsType<Ratnum>) predicate, consequent, alternative) :
                (Configuration.EnableInlinePCond1 && predicate is PrimitiveIsType<Record>) ? PCondIsType<Record>.Make ((PrimitiveIsType<Record>) predicate, consequent, alternative) :
                ////(predicate is PrimitiveIsSymbol) ? PCondIsSymbol.Make ((PrimitiveIsSymbol) predicate, consequent, alternative) :
                (Configuration.EnableInlinePCond1 && predicate is PrimitiveIsType<object []>) ? PCondIsType<object []>.Make ((PrimitiveIsType<object []>) predicate, consequent, alternative) :
                (Configuration.EnableInlinePCond1 && predicate is PrimitiveIsType<WeakCons>) ? PCondIsType<WeakCons>.Make ((PrimitiveIsType<WeakCons>) predicate, consequent, alternative) :

                (predicate.Operand is Argument) ? PCond1A.Make (predicate, consequent, alternative) :
                (predicate.Operand is Quotation) ? PCond1Q.Make (predicate, consequent, alternative) :
                (predicate.Operand is StaticVariable) ? PCond1S.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? PCond1XA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond1XQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCond1XS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCond1XXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond1XXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCond1XXS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            procedureHistogram.Note (this.procedure);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCond1";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCond1";
#endif
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
#if DEBUG
            SCode.location = "PCond1";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondCar : PCond1
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondCar (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCar predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand is Argument) ? PCondCarA.Make (predicate, consequent, alternative) :
                //(consequent is Argument) ? PCondIsNullXA.Make (predicate, (Argument) consequent, alternative) :
                //(consequent is Quotation) ? PCondIsNullXQ.Make (predicate, (Quotation) consequent, alternative) :
                //(consequent is StaticVariable) ? PCondIsNullXS.Make (predicate, (StaticVariable) consequent, alternative) :
                //(alternative is Argument) ? PCondIsNullXXA.Make (predicate, consequent, (Argument) alternative) :
                //(alternative is Quotation) ? PCondIsNullXXQ.Make (predicate, consequent, (Quotation) alternative) :
                //(alternative is StaticVariable) ? PCondIsNullXXS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondCar (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCondCar";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCondCar";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, closureEnvironment));
                //answer = Interpreter.Unwind;
                //closureEnvironment = env;
                //return false;
            }
            Cons ev0Pair = ev0 as Cons;
            object temp = ev0Pair.Car;

            if (temp is Boolean && (bool) temp == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondCar";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            } else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondCar";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondCarA : PCondCar
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int predicateArgumentOffset;

        protected PCondCarA (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateArgumentOffset = ((Argument) predicate.Operand).Offset;
        }

        public static new SCode Make (PrimitiveCar predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand is Argument0) ? PCondCarA0.Make (predicate, consequent, alternative) :
                (predicate.Operand is Argument1) ? PCondCarA1.Make (predicate, consequent, alternative) :
                //(consequent is Argument) ? PCond1AA.Make (predicate, (Argument) consequent, alternative) :
                //(consequent is Quotation) ? PCond1AQ.Make (predicate, (Quotation) consequent, alternative) :
                ////: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCond1AXQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondCarA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondCarA";
#endif
            object ev0 = environment.ArgumentValue (this.predicateArgumentOffset);

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
#if DEBUG
            SCode.location = "PCondCarA";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondCarA";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondCarA";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondCarA0 : PCondCarA
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondCarA0 (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
        {
            return
                //(consequent is Argument) ? PCond1A0A.Make (predicate, (Argument) consequent, alternative) :
                //(consequent is Quotation) ? PCond1A0Q.Make (predicate, (Quotation) consequent, alternative) :
                //(consequent is StaticVariable) ? PCond1A0S.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCondCarA0XA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondCarA0XQ.Make (predicate, consequent, (Quotation) alternative) :
                //(alternative is StaticVariable) ? PCond1A0XS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondCarA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCondCarA0";
#endif
            object ev0 = environment.Argument0Value;

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
#if DEBUG
            SCode.location = "PCondCarA0";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondCarA0";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondCarA0";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondCarA0XA : PCondCarA0
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();

#endif
        public readonly int alternativeOffset;

        protected PCondCarA0XA (PrimitiveCombination1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondCarA0XA0.Make (predicate, consequent, (Argument0) alternative):
                (alternative is Argument1) ? PCondCarA0XA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCondCarA0XA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCondCarA0XA";
#endif
            object ev0 = environment.Argument0Value;

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
#if DEBUG
            SCode.location = "PCondCarA0XA";
#endif

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondCarA0XA";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondCarA0XA0 : PCondCarA0XA
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();

#endif
        protected PCondCarA0XA0 (PrimitiveCombination1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondCarA0XA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCondCarA0XA0";
#endif
            object ev0 = environment.Argument0Value;

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
#if DEBUG
            SCode.location = "PCondCarA0XA0";
#endif

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondCarA0XA0";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondCarA0XA1 : PCondCarA0XA
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();

#endif
        protected PCondCarA0XA1 (PrimitiveCombination1 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCondCarA0XA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCondCarA0XA1";
#endif
            object ev0 = environment.Argument0Value;

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
#if DEBUG
            SCode.location = "PCondCarA0XA1";
#endif

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondCarA0XA1";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondCarA0XQ : PCondCarA0
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();

#endif
        public readonly object alternativeValue;

        protected PCondCarA0XQ (PrimitiveCombination1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondCarA0XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCondCarA0XQ";
#endif
            object ev0 = environment.Argument0Value;

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
#if DEBUG
            SCode.location = "PCondCarA0XQ";
#endif

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondCarA0XQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondCarA1 : PCond1A
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondCarA1 (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
        {
            return
                //(consequent is Argument) ? PCond1A1A.Make (predicate, (Argument) consequent, alternative) :
                //(consequent is Quotation) ? PCond1A1Q.Make (predicate, (Quotation) consequent, alternative) :
                //(alternative is Argument) ? PCond1A1XA.Make (predicate, consequent, (Argument) alternative) :
                //(alternative is Quotation) ? PCond1A1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondCarA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCondCarA1";
#endif
            object ev0 = environment.Argument1Value;

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
#if DEBUG
            SCode.location = "PCondCarA1";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondCarA1";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondCarA1";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1A : PCond1
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int predicateArgumentOffset;

        protected PCond1A (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateArgumentOffset = ((Argument) predicate.Operand).Offset;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand is Argument0) ? PCond1A0.Make (predicate, consequent, alternative) :
                (predicate.Operand is Argument1) ? PCond1A1.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? PCond1AA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond1AQ.Make (predicate, (Quotation) consequent, alternative) :
                //: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                (alternative is Quotation) ? PCond1AXQ.Make (predicate, consequent, (Quotation) alternative):
                new PCond1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A";
#endif
            object ev0 = environment.ArgumentValue (this.predicateArgumentOffset);

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
#if DEBUG
            SCode.location = "PCond1A";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1A";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1A";
#endif
                expression = this.consequent;
                return true;
            }
        }
     }

    [Serializable]
    class PCond1A0 : PCond1A
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond1A0 (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond1A0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond1A0Q.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCond1A0S.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCond1A0XA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond1A0XQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCond1A0XS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A0";
#endif
            object ev0 = environment.Argument0Value;

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
#if DEBUG
            SCode.location = "PCond1A0";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1A0";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1A0";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1A0A : PCond1A0
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCond1A0A (PrimitiveCombination1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond1A0A0.Make (predicate, (Argument0) consequent, alternative) :
                 (consequent is Argument1) ? PCond1A0A1.Make (predicate, (Argument1) consequent, alternative) :
                //: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCond1A1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A0A";
#endif
            object ev0 = environment.Argument0Value;

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
#if DEBUG
            SCode.location = "PCond1A0A";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1A0A";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCond1A0A0 : PCond1A0A
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond1A0A0 (PrimitiveCombination1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCond1A1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond1A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A0A0";
#endif
            object ev0 = environment.Argument0Value;

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
#if DEBUG
            SCode.location = "PCond1A0A0";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1A0A0";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = ev0;
                return false;
            }
        }
    }

    [Serializable]
    class PCond1A0A1 : PCond1A0A
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond1A0A1 (PrimitiveCombination1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Argument1 consequent, SCode alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCond1A1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond1A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A0A1";
#endif
            object ev0 = environment.Argument0Value;

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
#if DEBUG
            SCode.location = "PCond1A0A1";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1A0A1";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCond1A0Q : PCond1A0
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond1A0Q (PrimitiveCombination1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Quotation consequent, SCode alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCond1A1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond1A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A0Q";
#endif
            object ev0 = environment.Argument0Value;

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
#if DEBUG
            SCode.location = "PCond1A0Q";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1A0Q";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond1A0S : PCond1A0
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;

        protected PCond1A0S (PrimitiveCombination1 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCond1A1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond1A0S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A0S";
#endif
            object ev0 = environment.Argument0Value;

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
#if DEBUG
            SCode.location = "PCond1A0S";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1A0S";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCond1A0XA : PCond1A0
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCond1A0XA (PrimitiveCombination1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond1A0XA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond1A0XA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond1A0XA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A0XA";
#endif
            object ev0 = environment.Argument0Value;

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
#if DEBUG
            SCode.location = "PCond1A0XA";
#endif

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1A0XA";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1A0XA0 : PCond1A0XA
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond1A0XA0 (PrimitiveCombination1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond1A0XA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A0XA0";
#endif
            object ev0 = environment.Argument0Value;

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
#if DEBUG
            SCode.location = "PCond1A0XA0";
#endif

            if ((answer is bool) && (bool) answer == false) {
                answer = ev0;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1A0XA0";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1A0XA1 : PCond1A0XA
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond1A0XA1 (PrimitiveCombination1 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCond1A0XA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A0XA1";
#endif
            object ev0 = environment.Argument0Value;

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
#if DEBUG
            SCode.location = "PCond1A0XA1";
#endif

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1A0XA1";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1A0XQ : PCond1A0
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond1A0XQ (PrimitiveCombination1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Quotation alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCond1A1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond1A0XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A0XQ";
#endif
            object ev0 = environment.Argument0Value;

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
#if DEBUG
            SCode.location = "PCond1A0XQ";
#endif

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;

            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1A0XQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1A0XS : PCond1A0
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCond1A0XS (PrimitiveCombination1 predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, StaticVariable alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCond1A1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond1A0XS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A0XS";
#endif
            object ev0 = environment.Argument0Value;

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
#if DEBUG
            SCode.location = "PCond1A0XS";
#endif

            if ((answer is bool) && (bool) answer == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1A0XQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1A1 : PCond1A
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond1A1 (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond1A1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond1A1Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is Argument) ? PCond1A1XA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond1A1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A1";
#endif
            object ev0 = environment.Argument1Value;

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
#if DEBUG
            SCode.location = "PCond1A1";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1A1";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1A1";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1A1A : PCond1A1
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCond1A1A (PrimitiveCombination1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond1A1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond1A1A1.Make (predicate, (Argument1) consequent, alternative) :
                //: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCond1A1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond1A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A1A";
#endif
            object ev0 = environment.Argument1Value;

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
#if DEBUG
            SCode.location = "PCond1A1A";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1A1A";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCond1A1A0 : PCond1A1A
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond1A1A0 (PrimitiveCombination1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Argument0 consequent, SCode alternative)
        {
            return

                //: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCond1A1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond1A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A1A0";
#endif
            object ev0 = environment.Argument1Value;

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
#if DEBUG
            SCode.location = "PCond1A1A0";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1A1A0";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCond1A1A1 : PCond1A1A
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond1A1A1 (PrimitiveCombination1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Argument1 consequent, SCode alternative)
        {
            return

                //: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCond1A1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond1A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A1A1";
#endif
            object ev0 = environment.Argument1Value;

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
#if DEBUG
            SCode.location = "PCond1A1A1";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1A1A1";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = ev0;
                return false;
            }
        }
    }


    [Serializable]
    class PCond1A1Q : PCond1A1
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond1A1Q (PrimitiveCombination1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Quotation consequent, SCode alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCond1A1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond1A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A1Q";
#endif
            object ev0 = environment.Argument1Value;

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
#if DEBUG
            SCode.location = "PCond1A1Q";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1A1";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond1A1XA : PCond1A1
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCond1A1XA (PrimitiveCombination1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond1A1XA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond1A1XA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond1A1XA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A1XA";
#endif
            object ev0 = environment.Argument1Value;

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
#if DEBUG
            SCode.location = "PCond1A1XA";
#endif

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1A1XA";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1A1XA0 : PCond1A1XA
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond1A1XA0 (PrimitiveCombination1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond1A1XA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A1XA0";
#endif
            object ev0 = environment.Argument1Value;

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
#if DEBUG
            SCode.location = "PCond1A1XA0";
#endif

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1A1XA0";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1A1XA1 : PCond1A1XA
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond1A1XA1 (PrimitiveCombination1 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCond1A1XA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A1XA1";
#endif
            object ev0 = environment.Argument1Value;

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
#if DEBUG
            SCode.location = "PCond1A1XA1";
#endif

            if ((answer is bool) && (bool) answer == false) {
                answer = ev0;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1A1XA1";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }


    [Serializable]
    class PCond1A1XQ : PCond1A1
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond1A1XQ (PrimitiveCombination1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Quotation alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCond1A1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond1A1XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1A1XQ";
#endif
            object ev0 = environment.Argument1Value;

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
#if DEBUG
            SCode.location = "PCond1A1XQ";
#endif

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;

            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1A1XQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1AA : PCond1A
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCond1AA (PrimitiveCombination1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond1AA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond1AA1.Make (predicate, (Argument1) consequent, alternative) :
                new PCond1AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1AA";
#endif
            object ev0 = environment.ArgumentValue (this.predicateArgumentOffset);

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
#if DEBUG
            SCode.location = "PCond1AA";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1AA";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCond1AA0 : PCond1AA
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCond1AA0 (PrimitiveCombination1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                new PCond1AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1AA0";
#endif
            object ev0 = environment.ArgumentValue (this.predicateArgumentOffset);

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
#if DEBUG
            SCode.location = "PCond1AA0";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1AA0";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCond1AA1 : PCond1AA
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCond1AA1 (PrimitiveCombination1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Argument1 consequent, SCode alternative)
        {
            return
                new PCond1AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1AA1";
#endif
            object ev0 = environment.ArgumentValue (this.predicateArgumentOffset);

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
#if DEBUG
            SCode.location = "PCond1AA1";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1AA1";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCond1AQ : PCond1A
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCond1AQ (PrimitiveCombination1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Quotation consequent, SCode alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //: (alternative is Quotation) ? PCond1ASQ.Make (predicate, consequent, (Quotation) alternative)
                new PCond1AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1AQ";
#endif
            object ev0 = environment.ArgumentValue (this.predicateArgumentOffset);

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
#if DEBUG
            SCode.location = "PCond1AQ";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1AQ";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond1AXQ : PCond1A
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCond1AXQ (PrimitiveCombination1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond1AXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1AXQ";
#endif
            object ev0 = environment.ArgumentValue (this.predicateArgumentOffset);

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
#if DEBUG
            SCode.location = "PCond1AXQ";
#endif

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;

            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1AXQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }


    [Serializable]
    class PCond1Q : PCond1
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object predicateArgumentValue;

        protected PCond1Q (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateArgumentValue = ((Quotation) predicate.Operand).Quoted;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
        {
            return
                //(predicate.Operand is A0) ? PCond1A0.Make ((PrimitiveCombination1A0) predicate, consequent, alternative) :
                //(predicate.Operand is A1) ? PCond1A1.Make ((PrimitiveCombination1A1) predicate, consequent, alternative) :
                //: (consequent is LexicalVariable) ? PCond1AL.Make (predicate, (LexicalVariable) consequent, alternative)
                //: (consequent is Quotation) ? PCond1AQ.Make (predicate, (Quotation) consequent, alternative)
                //: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //: (alternative is Quotation) ? PCond1ASQ.Make (predicate, consequent, (Quotation) alternative)
                new PCond1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("-");
//            procedureHistogram.Note (this.procedure);
//            SCode.location = "PCond1A";
//#endif
//            object ev0 = environment.ArgumentValue (this.predicateArgumentOffset);

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
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
//            SCode.location = "PCond1A";
//#endif

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                SCode.location = "-";
//                NoteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//                SCode.location = "PCond1A";
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                SCode.location = "-";
//                NoteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//                SCode.location = "PCond1A";
//#endif
//                expression = this.consequent;
//                return true;
//            }
        }
    }

    [Serializable]
    class PCond1S : PCond1
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol predicateRandName;
        public readonly int predicateRandOffset;

        protected PCond1S (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateRandName = ((StaticVariable) predicate.Operand).Name;
            this.predicateRandOffset = ((StaticVariable) predicate.Operand).Offset;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, SCode alternative)
        {
            return
                //(predicate.Operand is A0) ? PCond1A0.Make ((PrimitiveCombination1A0) predicate, consequent, alternative) :
                //(predicate.Operand is A1) ? PCond1A1.Make ((PrimitiveCombination1A1) predicate, consequent, alternative) :
                (consequent is Argument) ? PCond1SA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond1SQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCond1SS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Quotation) ? PCond1SXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCond1SXS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond1S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1S";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.predicateRandName, this.predicateRandOffset))
                throw new NotImplementedException ();

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
#if DEBUG
            SCode.location = "PCond1S";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1S";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1S";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1SA : PCond1S
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCond1SA (PrimitiveCombination1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond1SA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond1SA1.Make (predicate, (Argument1) consequent, alternative) :
                new PCond1SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1SA";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.predicateRandName, this.predicateRandOffset))
                throw new NotImplementedException ();

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
#if DEBUG
            SCode.location = "PCond1SA";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1SA";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCond1SA0 : PCond1SA
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCond1SA0 (PrimitiveCombination1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                new PCond1SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1SA0";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.predicateRandName, this.predicateRandOffset))
                throw new NotImplementedException ();

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
#if DEBUG
            SCode.location = "PCond1SA0";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1SA0";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    class PCond1SA1 : PCond1SA
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCond1SA1 (PrimitiveCombination1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Argument1 consequent, SCode alternative)
        {
            return
                new PCond1SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1SA1";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.predicateRandName, this.predicateRandOffset))
                throw new NotImplementedException ();

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
#if DEBUG
            SCode.location = "PCond1SA1";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1SA1";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }



    [Serializable]
    class PCond1SQ : PCond1S
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCond1SQ (PrimitiveCombination1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Quotation consequent, SCode alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCond1SXQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1SQ";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.predicateRandName, this.predicateRandOffset))
                throw new NotImplementedException ();

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
#if DEBUG
            SCode.location = "PCond1SQ";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1SQ";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond1SS : PCond1S
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;

        protected PCond1SS (PrimitiveCombination1 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCond1ASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCond1SXQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond1SS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1SS";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.predicateRandName, this.predicateRandOffset))
                throw new NotImplementedException ();

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
#if DEBUG
            SCode.location = "PCond1SS";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1SS";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCond1SXQ : PCond1S
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCond1SXQ (PrimitiveCombination1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond1SXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1SXQ";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.predicateRandName, this.predicateRandOffset))
                throw new NotImplementedException ();

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
#if DEBUG
            SCode.location = "PCond1SXQ";
#endif

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1SXQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1SXS : PCond1S
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected PCond1SXS (PrimitiveCombination1 predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCond1SXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond1SXS";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.predicateRandName, this.predicateRandOffset))
                throw new NotImplementedException ();

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
#if DEBUG
            SCode.location = "PCond1SXS";
#endif

            if ((answer is bool) && (bool) answer == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1SXS";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1XA : PCond1
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCond1XA (PrimitiveCombination1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond1XA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond1XA1.Make (predicate, (Argument1) consequent, alternative) :
                new PCond1XA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            procedureHistogram.Note (this.procedure);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCond1XA";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCond1XA";
#endif
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
#if DEBUG
            SCode.location = "PCond1XA";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1XA";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCond1XA0 : PCond1XA
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond1XA0 (PrimitiveCombination1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                new PCond1XA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            procedureHistogram.Note (this.procedure);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCond1XA0";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCond1XA0";
#endif
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
#if DEBUG
            SCode.location = "PCond1XA0";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1XA0";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCond1XA1 : PCond1XA
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond1XA1 (PrimitiveCombination1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Argument1 consequent, SCode alternative)
        {
            return
                new PCond1XA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            procedureHistogram.Note (this.procedure);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCond1XA1";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCond1XA1";
#endif
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
#if DEBUG
            SCode.location = "PCond1XA1";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1XA1";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCond1XQ : PCond1
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object consequentValue;
        protected PCond1XQ (PrimitiveCombination1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, Quotation consequent, SCode alternative)
        {
            return
                new PCond1XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            procedureHistogram.Note (this.procedure);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCond1XQ";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCond1XQ";
#endif
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
#if DEBUG
            SCode.location = "PCond1XQ";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1XQ";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond1XS : PCond1
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;

        protected PCond1XS (PrimitiveCombination1 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                new PCond1XS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            procedureHistogram.Note (this.procedure);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCond1XS";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCond1XS";
#endif
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
#if DEBUG
            SCode.location = "PCond1XS";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond1XS";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCond1XXA : PCond1
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        public readonly int alternativeOffset;

        protected PCond1XXA (PrimitiveCombination1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond1XXA0.Make (predicate, consequent, (Argument0) alternative) :
                 (alternative is Argument1) ? PCond1XXA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond1XXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            procedureHistogram.Note (this.procedure);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCond1XXA";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCond1XXA";
#endif
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
#if DEBUG
            SCode.location = "PCond1XXA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1XXA";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1XXA0 : PCond1XXA
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        public readonly int alternativeOffset;

        protected PCond1XXA0 (PrimitiveCombination1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond1XXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            procedureHistogram.Note (this.procedure);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCond1XXA0";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCond1XXA0";
#endif
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
#if DEBUG
            SCode.location = "PCond1XXA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1XXA0";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1XXA1 : PCond1XXA
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCond1XXA1 (PrimitiveCombination1 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCond1XXA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            procedureHistogram.Note (this.procedure);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCond1XXA1";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCond1XXA1";
#endif
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
#if DEBUG
            SCode.location = "PCond1XXA1";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1XXA1";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1XXQ : PCond1
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object alternativeValue;
        protected PCond1XXQ (PrimitiveCombination1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond1XXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            procedureHistogram.Note (this.procedure);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCond1XXQ";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCond1XXQ";
#endif
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
#if DEBUG
            SCode.location = "PCond1XXQ";
#endif

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1XXQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond1XXS : PCond1
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected PCond1XXS (PrimitiveCombination1 predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination1 predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCond1XXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            procedureHistogram.Note (this.procedure);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCond1XXS";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCond1XXS";
#endif
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
#if DEBUG
            SCode.location = "PCond1XXS";
#endif
            if ((answer is bool) && (bool) answer == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond1XXS";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

}
