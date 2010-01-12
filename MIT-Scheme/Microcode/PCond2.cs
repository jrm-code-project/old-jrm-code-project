using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Microcode
{
    [Serializable]
    class PCond2 : Conditional
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

        protected Type rand0Type;
        protected Type rand1Type;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly Primitive2 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        protected PrimitiveMethod2 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly SCode rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly SCode rand1;

        protected PCond2 (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.procedure = predicate.Rator;
            this.method = this.procedure.Method;
            this.rand0 = predicate.Operand0;
            this.rand1 = predicate.Operand1;
#if DEBUG
            rand0Type = rand0.GetType ();
            rand1Type = rand1.GetType ();
#endif
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                //(predicate is PrimitiveGreaterThanFixnum) ? PCondGreaterThanFixnum.Make ((PrimitiveGreaterThanFixnum) predicate, consequent, alternative) :
                //(predicate is PrimitiveIsEq) ? PCondIsEq.Make ((PrimitiveIsEq) predicate, consequent, alternative) :
                //(predicate is PrimitiveLessThanFixnum) ? PCondLessThanFixnum.Make ((PrimitiveLessThanFixnum) predicate, consequent, alternative) :
                //(predicate is PrimitiveIsObjectType) ? PCondIsObjectType.Make ((PrimitiveIsObjectType) predicate, consequent, alternative) :
                //(predicate is PrimitiveRecordRef) ? PCondRecordRef.Make ((PrimitiveRecordRef) predicate, consequent, alternative) :
                //(predicate is PrimitiveVectorRef) ? PCondVectorRef.Make ((PrimitiveVectorRef) predicate, consequent, alternative) :
                (predicate.Operand0 is Argument) ? PCond2A.Make (predicate, consequent, alternative) :
                (predicate.Operand0 is Quotation) ? PCond2Q.Make (predicate, consequent, alternative) :
                (predicate.Operand0 is StaticVariable) ? PCond2S.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is Argument) ? PCond2XA.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is Quotation) ? PCond2XQ.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is StaticVariable) ? PCond2XS.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? PCond2XXA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond2XXQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented():
                (alternative is Argument) ? PCond2XXXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2XXXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented():
                new PCond2 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2";
#endif
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

    [Serializable]
    class PCond2A : PCond2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;
        protected PCond2A (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Offset = ((Argument) predicate.Operand0).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand0 is Argument0) ? PCond2A0.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is Argument) ? PCond2AA.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is Quotation) ? PCond2AQ.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is StaticVariable) ? PCond2AS.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? PCond2AXA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond2AXQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? PCond2AXXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2AXXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2A";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2A";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A";
#endif
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

    [Serializable]
    class PCond2A0 : PCond2A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2A0 (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument) ? PCond2A0A.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is Quotation) ? PCond2A0Q.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is StaticVariable) ? PCond2A0S.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? PCond2A0XA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond2A0XQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? PCond2A0XXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2A0XXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCond2A0XXS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond2A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2A0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2A0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0";
#endif
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

    [Serializable]
    class PCond2A0A : PCond2A0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;
        protected PCond2A0A (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? Unimplemented() :
                (consequent is Argument) ? PCond2A0AA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond2A0AQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2A0AXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0A";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0A";
#endif
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

    [Serializable]
    class PCond2A0AA : PCond2A0A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCond2A0AA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2A0AA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? PCond2A0AAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? Unimplemented() :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2A0AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0AA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0AA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2A0AA0 : PCond2A0AA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2A0AA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCond2A0AA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2A0AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0AA0";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0AA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2A0AA0A : PCond2A0AA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2A0AA0A (PrimitiveCombination2 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented() :
                new PCond2A0AA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0AA0A";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0AA0A";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2A0AAA : PCond2A0AA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2A0AAA (PrimitiveCombination2 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0AAA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2A0AAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0AAA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0AAA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCond2A0AAA0 : PCond2A0AAA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        protected PCond2A0AAA0 (PrimitiveCombination2 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2A0AAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0AAA0";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0AAA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCond2A0AQ : PCond2A0A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond2A0AQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented() :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2A0AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0AQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0AQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2A0AXQ : PCond2A0A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2A0AXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2A0AXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0AXQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0AXQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
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

    [Serializable]
    class PCond2A0S : PCond2A0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        protected PCond2A0S (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2A0SA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond2A0SQ.Make (predicate, (Quotation) consequent, alternative):
                (consequent is StaticVariable) ? PCond2A0SS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCond2A0SXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2A0SXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCond2A0SXS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond2A0S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0S";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0S";
#endif
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

    [Serializable]
    class PCond2A0SA : PCond2A0S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCond2A0SA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2A0SA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented() :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2A0SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0SA";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0SA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2A0SA0 : PCond2A0SA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2A0SA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? PCond2A0SA0S.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond2A0SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0SA0";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0SA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2A0SA0S : PCond2A0SA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected PCond2A0SA0S (PrimitiveCombination2 predicate, Argument0 consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, StaticVariable alternative)
        {
            return
                new PCond2A0SA0S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0SA0S";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0SA0S";
#endif
            if ((answer is bool) && (bool) answer == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }


    [Serializable]
    class PCond2A0SQ : PCond2A0S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond2A0SQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented() :
                (alternative is Quotation) ? PCond2A0SQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2A0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0SQ";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0SQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2A0SQQ : PCond2A0SQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly object alternativeValue;
        protected PCond2A0SQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2A0SQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0SQQ";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0SQQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2A0SS : PCond2A0S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;
        protected PCond2A0SS (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2A0SS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0SS";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0SS";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2A0SXA : PCond2A0S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2A0SXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ?PCond2A0SXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2A0SXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0SXA";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0SXA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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

    [Serializable]
    class PCond2A0SXA0 : PCond2A0SXA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2A0SXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond2A0SXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0SXA0";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0SXA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
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


    [Serializable]
    class PCond2A0SXQ : PCond2A0S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2A0SXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2A0SXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0SXQ";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0SXQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
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

    [Serializable]
    class PCond2A0SXS : PCond2A0S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCond2A0SXS (PrimitiveCombination2 predicate, SCode consequent,StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCond2A0SXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0SXS";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0SXS";
#endif
            if ((answer is bool) && (bool) answer == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
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

    [Serializable]
    class PCond2A0XA : PCond2A0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCond2A0XA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2A0XA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2A0XA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2A0XA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2A0XA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0XA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2A0XA0 : PCond2A0XA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2A0XA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2A0XA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2A0XA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2A0XA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0XA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2A0XQ : PCond2A0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond2A0XQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCond2A0XQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2A0XQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2A0XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2A0XQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2A0XQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0XQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2A0XQA : PCond2A0XQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2A0XQA (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0XQA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2A0XQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2A0XQA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2A0XQA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0XQA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2A0XQA0 : PCond2A0XQA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2A0XQA0 (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2A0XQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2A0XQA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2A0XQA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0XQA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2A0XQQ : PCond2A0XQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCond2A0XQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2A0XQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2A0XQQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2A0XQQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0XQQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }


    [Serializable]
    class PCond2A0Q : PCond2A0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;
        protected PCond2A0Q (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = ((Quotation) predicate.Operand1).Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2A0QA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond2A0QQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCond2A0QS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCond2A0QXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2A0QXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCond2A0QXS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond2A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0Q";
#endif
            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2A0Q";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2A0Q";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond2A0Q";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2A0QA : PCond2A0Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCond2A0QA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2A0QA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented() :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2A0QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0QA";
#endif
            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2A0QA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2A0QA";
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
    class PCond2A0QA0 : PCond2A0QA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2A0QA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCond2A0QA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2A0QA0Q.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2A0QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0QA";
#endif
            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2A0QA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2A0QA0";
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
    class PCond2A0QA0A : PCond2A0QA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2A0QA0A (PrimitiveCombination2 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented():
                new PCond2A0QA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0QAA";
#endif
            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2A0QA0A";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }


    [Serializable]
    class PCond2A0QA0Q : PCond2A0QA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly object alternativeValue;
        protected PCond2A0QA0Q (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2A0QA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0QAQ";
#endif
            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2A0QA0Q";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2A0QQ : PCond2A0Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond2A0QQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCond2A0QQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2A0QQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2A0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0QQ";
#endif
            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2A0QQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2A0QQ";
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
    class PCond2A0QQA : PCond2A0QQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2A0QQA (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0QQA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2A0QQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0QQA";
#endif
            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2A0QQA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2A0QQA0 : PCond2A0QQA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        protected PCond2A0QQA0 (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2A0QQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0QQA0";
#endif
            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2A0QQA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2A0QQQ : PCond2A0QQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly object alternativeValue;
        protected PCond2A0QQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2A0QQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0QQQ";
#endif
            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2A0QQQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2A0QS : PCond2A0Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;
        protected PCond2A0QS (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? PCond2A0QSS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond2A0QS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0QS";
#endif
            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2A0QS";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2A0QS";
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
    class PCond2A0QSS : PCond2A0QS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCond2A0QSS (PrimitiveCombination2 predicate, StaticVariable consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, StaticVariable alternative)
        {
            return
                new PCond2A0QSS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0QSS";
#endif
            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2A0QSS";
#endif
            if ((answer is bool) && (bool) answer == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }


    [Serializable]
    class PCond2A0QXA : PCond2A0Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2A0QXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0QXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2A0QXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0QXA";
#endif
            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2A0QXQ";
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
                SCode.location = "PCond2A0QXQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2A0QXA0 : PCond2A0QXA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2A0QXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond2A0QXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0QXA0";
#endif
            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2A0QXA0";
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
                SCode.location = "PCond2A0QXA0";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }


    [Serializable]
    class PCond2A0QXQ : PCond2A0Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2A0QXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2A0QXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0QXQ";
#endif
            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2A0QXQ";
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
                SCode.location = "PCond2A0QXQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2A0QXS : PCond2A0Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCond2A0QXS (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCond2A0QXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0QXS";
#endif
            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2A0QXS";
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
                SCode.location = "PCond2A0QXQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }


    [Serializable]
    class PCond2A0XXA : PCond2A0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        internal PCond2A0XXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0XXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2A0XXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2A0XXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2A0XXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0XXA";
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
                SCode.location = "PCond2A0XXA";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2A0XXA0 : PCond2A0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        internal PCond2A0XXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond2A0XXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2A0XXA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2A0XXA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0XXA0";
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
                SCode.location = "PCond2A0XXA0";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }


    [Serializable]
    sealed class PCond2A0XXQ : PCond2A0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        internal PCond2A0XXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2A0XXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2A0XXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2A0XXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0XXQ";
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
                SCode.location = "PCond2A0XXQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    sealed class PCond2A0XXS : PCond2A0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        internal PCond2A0XXS (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName  = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCond2A0XXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2A0XXS";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2A0XXS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2A0XXS";
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
                SCode.location = "PCond2A0XXS";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2AA : PCond2A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;
        protected PCond2AA (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? PCond2AA0.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? PCond2AAA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond2AAQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? PCond2AAXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2AAXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2AA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2AA";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond2AA";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2AA0 : PCond2AA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2AA0 (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? Unimplemented () :
                (consequent is Quotation) ? PCond2AA0Q.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2AA0XQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AA0";
#endif
            object ev1 = environment.Argument0Value;
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2AA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2AA0";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond2AA0";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCond2AA0Q : PCond2AA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond2AA0Q (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2AA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AA0Q";
#endif
            object ev1 = environment.Argument0Value;
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2AA0Q";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2AA0Q";
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
    class PCond2AA0XQ : PCond2AA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2AA0XQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2AA0XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AA0XQ";
#endif
            object ev1 = environment.Argument0Value;
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2AA0XQ";
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
                SCode.location = "PCond2AA0";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCond2AAA : PCond2AA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCond2AAA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? Unimplemented():
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2AAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AAA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2AAA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2AAA";
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

    class PCond2AAQ : PCond2AA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond2AAQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2AAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AAQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2AAQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2AAQ";
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
    class PCond2AAXA : PCond2AA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2AAXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented() :
                new PCond2AAXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AAXA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2AAXA";
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
                SCode.location = "PCond2AAXQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }


    [Serializable]
    class PCond2AAXQ : PCond2AA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2AAXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2AAXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AAXQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2AAXQ";
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
                SCode.location = "PCond2AAXQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }


    [Serializable]
    class PCond2AQ : PCond2A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;
        protected PCond2AQ (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = ((Quotation) predicate.Operand1).Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2AQA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond2AQQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? PCond2AQXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2AQXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AQ";
#endif
            object ev0 = environment.ArgumentValue(this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2AQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2AQ";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond2AQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2AQA : PCond2AQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCond2AQA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2AQA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented() :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2AQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AQA";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2AQA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2AQ";
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
    class PCond2AQA0 : PCond2AQA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2AQA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2AQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AQA0";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2AQA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2AQA0";
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
    class PCond2AQQ : PCond2AQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond2AQQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCond2AQQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2AQQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2AQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AQQ";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2AQQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2AQQ";
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
    class PCond2AQQA : PCond2AQQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2AQQA (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AQQA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2AQQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AQQA";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2AQQA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2AQQA0 : PCond2AQQA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        protected PCond2AQQA0 (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2AQQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AQQA0";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2AQQA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2AQQQ : PCond2AQQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
#endif
        public readonly object alternativeValue;
        protected PCond2AQQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2AQQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AQQQ";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2AQQQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2AQXA : PCond2AQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2AQXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AQXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2AQXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AQXA";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2AQXA";
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
                SCode.location = "PCond2AQXA";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }



    [Serializable]
    class PCond2AQXA0 : PCond2AQXA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCond2AQXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond2AQXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AQXA0";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2AQXA0";
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
                SCode.location = "PCond2AQXA0";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2AQXQ : PCond2AQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2AQXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2AQXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AQXQ";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, this.rand1Value)) {
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
            SCode.location = "PCond2AQXQ";
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
                SCode.location = "PCond2AQXQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2AS : PCond2A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        protected PCond2AS (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2ASA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond2ASQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? PCond2ASXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2ASXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2AS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
               throw new NotImplementedException();

            object ev0 = environment.ArgumentValue(this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2AS";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2AS";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond2AS";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2ASA : PCond2AS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCond2ASA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2ASA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented() :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2ASA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2ASA";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
               throw new NotImplementedException();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2ASA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2AS";
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
    class PCond2ASA0 : PCond2ASA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2ASA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2ASA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2ASA0";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
               throw new NotImplementedException();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2ASA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2ASA0";
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
    class PCond2ASQ : PCond2AS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond2ASQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCond2ASQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2ASQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2ASQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2ASQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
               throw new NotImplementedException();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2ASQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2ASQ";
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
    class PCond2ASQA : PCond2ASQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2ASQA (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2ASQA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2ASQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2ASQA";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
               throw new NotImplementedException();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2ASQA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2ASQA0 : PCond2ASQA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        protected PCond2ASQA0 (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2ASQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2ASQA0";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
               throw new NotImplementedException();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2ASQA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2ASQQ : PCond2ASQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
#endif
        public readonly object alternativeValue;
        protected PCond2ASQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2ASQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2ASQQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
               throw new NotImplementedException();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2ASQQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2ASXA : PCond2AS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2ASXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2ASXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2ASXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2ASXA";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
               throw new NotImplementedException();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2ASXA";
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
                SCode.location = "PCond2ASXA";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }



    [Serializable]
    class PCond2ASXA0 : PCond2ASXA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCond2ASXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond2ASXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2ASXA0";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
               throw new NotImplementedException();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2ASXA0";
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
                SCode.location = "PCond2ASXA0";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2ASXQ : PCond2AS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2ASXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2ASXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2ASXQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
               throw new NotImplementedException();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2ASXQ";
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
                SCode.location = "PCond2ASXQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2AXA : PCond2A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCond2AXA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2AXA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2AXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2AXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2AXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2AXA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2AXA0 : PCond2AXA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2AXA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2AXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2AXA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2AXA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2AXA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = environment.Argument0Value ;
                return false;
            }
        }
    }


    [Serializable]
    class PCond2AXQ : PCond2A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond2AXQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCond2AXQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2AXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2AXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2AXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2AXQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2AXQA : PCond2AXQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2AXQA (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AXQA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2AXQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2AXQA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2AXQA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2AXQA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2AXQA0 : PCond2AXQA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2AXQA0 (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2AXQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2AXQA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2AXQA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2AXQA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2AXXA : PCond2A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int  alternativeOffset;
        protected PCond2AXXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented():
                new PCond2AXXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2AXXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2AXXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2AXXA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;

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


    [Serializable]
    class PCond2AXXQ : PCond2A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2AXXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2AXXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2AXXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2AXXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2AXXQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;

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


    [Serializable]
    class PCond2Q : PCond2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;
        protected PCond2Q (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Value = ((Quotation) predicate.Operand0).Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument) ? PCond2QA.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is Quotation) ? Unimplemented () :
                (predicate.Operand1 is StaticVariable) ? PCond2QS.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? PCond2QXA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond2QXQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCond2QXS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCond2QXXA.Make (predicate, consequent, (Argument) alternative):
                (alternative is Quotation) ? PCond2QXXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2Q";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2Q";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2Q";
#endif
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

    [Serializable]
    class PCond2QA : PCond2Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;
        protected PCond2QA (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? PCond2QA0.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? Unimplemented () :
                (consequent is Quotation) ? PCond2QAQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCond2QAS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCond2QAXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2QAXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QA";
#endif
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

    [Serializable]
    class PCond2QA0 : PCond2QA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2QA0 (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2QA0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond2QA0Q.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCond2QA0S.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCond2QA0XA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2QA0XQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QA0";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QA0";
#endif
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

    [Serializable]
    class PCond2QA0A : PCond2QA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCond2QA0A (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2QA0A0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented():
                (alternative is Quotation) ? PCond2QA0AQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2QA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QA0A";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QA0A";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2QA0A0 : PCond2QA0A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCond2QA0A0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2QA0A0Q.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2QA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QA0A0";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QA0A0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2QA0A0Q : PCond2QA0A0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCond2QA0A0Q (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2QA0A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QA0A0Q";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QA0A0Q";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2QA0AQ : PCond2QA0A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCond2QA0AQ (PrimitiveCombination2 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2QA0AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QA0AQ";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QA0AQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCond2QA0Q : PCond2QA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond2QA0Q (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCond2QA0QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2QA0QQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2QA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QA0Q";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QA0Q";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2QA0QA : PCond2QA0Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2QA0QA (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2QA0QA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2QA0QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QA0QA";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QA0QA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2QA0QA0 : PCond2QA0QA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        protected PCond2QA0QA0 (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2QA0QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QA0QA0";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QA0QA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2QA0QQ : PCond2QA0Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly object alternativeValue;
        protected PCond2QA0QQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2QA0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QA0QQ";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QA0QQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2QA0S : PCond2QA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;

        protected PCond2QA0S (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () : //PCond2QA0AQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2QA0S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QA0S";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QA0S";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2QA0XA : PCond2QA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2QA0XA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented():
                new PCond2QA0XA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QA0XA";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QA0XA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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


    [Serializable]
    class PCond2QA0XQ : PCond2QA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2QA0XQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2QA0XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QA0XQ";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QA0XQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
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

    [Serializable]
    class PCond2QAQ : PCond2QA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond2QAQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCond2QAQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2QAQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2QAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QAQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QAQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2QAQA : PCond2QAQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2QAQA (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ?  Unimplemented() : //PCond2QAQA0.Make (predicate, consequent, alternative) :
                new PCond2QAQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QAQA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QAQA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }


    [Serializable]
    class PCond2QAQQ : PCond2QAQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly object alternativeValue;
        protected PCond2QAQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2QAQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QAQQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QAQQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2QAS : PCond2QA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;

        protected PCond2QAS (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented():
                (alternative is Quotation) ? Unimplemented():
                (alternative is StaticVariable) ? Unimplemented():
                new PCond2QAS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QAS";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QAS";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2QAXA : PCond2QA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2QAXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented():
                new PCond2QAXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QAXA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QAXA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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

    [Serializable]
    class PCond2QAXQ : PCond2QA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2QAXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2QAXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QAXQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QAXQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
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

    [Serializable]
    class PCond2QS : PCond2Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        protected PCond2QS (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? Unimplemented () :
                (consequent is Quotation) ? PCond2QSQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCond2QSS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2QSXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2QS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QS";
#endif
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

    [Serializable]
    class PCond2QSQ : PCond2QS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond2QSQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2QSQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2QSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QSQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QSQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2QSQQ : PCond2QSQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly object alternativeValue;
        protected PCond2QSQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2QSQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QSQQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QSQQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2QSS : PCond2QS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;

        protected PCond2QSS (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () : //PCond2QSQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2QSS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QSS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QSS";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2QSXQ : PCond2QS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2QSXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2QSXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2QSXQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QSXQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
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

    [Serializable]
    class PCond2QXA : PCond2Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCond2QXA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2QXA0.Make (predicate, (Argument0) consequent, alternative):
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented() :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2QXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2QXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2QXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QXA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2QXA0 : PCond2QXA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCond2QXA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2QXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2QXA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2QXA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QXA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2QXQ : PCond2Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond2QXQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCond2QXQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2QXQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2QXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2QXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2QXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QXQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2QXQA : PCond2QXQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2QXQA (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented() :
                new PCond2QXQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2QXQA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2QXQA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QXQA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2QXQQ : PCond2QXQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2QXQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2QXQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2QXQQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2QXQQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QXQQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2QXS : PCond2Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;
        protected PCond2QXS (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? PCond2QXSS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond2QXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2QXS";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2QXS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QXS";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2QXSS : PCond2QXS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCond2QXSS (PrimitiveCombination2 predicate, StaticVariable consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, StaticVariable alternative)
        {
            return
                new PCond2QXSS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2QXSS";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2QXSS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QXSS";
#endif
            if ((answer is bool) && (bool) answer == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCond2QXXA : PCond2Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2QXXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2QXXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2QXXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2QXXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2QXXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QXXA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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

    [Serializable]
    class PCond2QXXA0 : PCond2QXXA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2QXXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond2QXXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2QXXA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2QXXA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QXXA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
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


    [Serializable]
    class PCond2QXXQ : PCond2Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2QXXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2QXXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2QXXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2QXXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2QXXQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
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


    [Serializable]
    class PCond2S : PCond2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;
        protected PCond2S (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Name = ((StaticVariable) predicate.Operand0).Name;
            this.rand0Offset = ((StaticVariable) predicate.Operand0).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument) ? PCond2SA.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is Quotation) ? PCond2SQ.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is StaticVariable) ? PCond2SS.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? PCond2SXA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond2SXQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCond2SXS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCond2SXXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2SXXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCond2SXXS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond2S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2S";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2S";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2S";
#endif
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

    [Serializable]
    class PCond2SA : PCond2S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;
        protected PCond2SA (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? PCond2SA0.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? Unimplemented():
                (consequent is Quotation) ? Unimplemented () :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2SAXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SA";
#endif
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

    [Serializable]
    class PCond2SA0 : PCond2SA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCond2SA0 (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2SA0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond2SA0Q.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCond2SA0S.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCond2SA0XA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2SA0XQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SA0";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SA0";
#endif
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

    [Serializable]
    class PCond2SA0A : PCond2SA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCond2SA0A (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2SA0A0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2SA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SA0A";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SA0A";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2SA0A0 : PCond2SA0A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2SA0A0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2SA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SA0A0";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SA0A0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2SA0Q : PCond2SA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond2SA0Q (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2SA0QQ.Make(predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2SA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SA0Q";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SA0Q";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2SA0QQ : PCond2SA0Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly object alternativeValue;
        protected PCond2SA0QQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2SA0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SA0QQ";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SA0QQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2SA0S : PCond2SA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;
        protected PCond2SA0S (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2SA0S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SA0S";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SA0S";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2SA0XA : PCond2SA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2SA0XA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SA0XA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2SA0XA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SA0XA";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SA0XA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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

    [Serializable]
    class PCond2SA0XA0 : PCond2SA0XA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2SA0XA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond2SA0XA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SA0XA0";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SA0XA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
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

    [Serializable]
    class PCond2SA0XQ : PCond2SA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2SA0XQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2SA0XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SA0XQ";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SA0XQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue; 
                return false;
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

    [Serializable]
    class PCond2SAXQ : PCond2SA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2SAXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2SAXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SAXQ";
#endif
            object ev1 = environment.ArgumentValue(this.rand1Offset);

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SAXQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
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


    [Serializable]
    class PCond2SQ : PCond2S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;
        protected PCond2SQ (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = ((Quotation) predicate.Operand1).Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2SQA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond2SQQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCond2SQS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCond2SQXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2SQXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCond2SQXS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond2SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SQ";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SQ";
#endif
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

    [Serializable]
    class PCond2SQA : PCond2SQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCond2SQA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2SQA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented():
                (alternative is StaticVariable) ? Unimplemented () : //PCond2SQQS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond2SQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SQA";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SQA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2SQA0 : PCond2SQA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2SQA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () : //PCond2SQQS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond2SQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SQA0";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SQA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2SQQ : PCond2SQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond2SQQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2SQQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCond2SQQS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond2SQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SQQ";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SQQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2SQQQ : PCond2SQQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly object alternativeValue;

        protected PCond2SQQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2SQQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SQQQ";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SQQQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }


    [Serializable]
    class PCond2SQQS : PCond2SQQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCond2SQQS (PrimitiveCombination2 predicate, Quotation consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, StaticVariable alternative)
        {
            return
                new PCond2SQQS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SQQS";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SQQS";
#endif
            if ((answer is bool) && (bool) answer == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2SQS : PCond2SQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;
        protected PCond2SQS (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2SQSQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented() : //PCond2SQQS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond2SQS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SQS";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SQS";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2SQSQ : PCond2SQS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
#endif
        public readonly object alternativeValue;
        protected PCond2SQSQ (PrimitiveCombination2 predicate, StaticVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, Quotation alternative)
        {
            return
                new PCond2SQSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SQSQ";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SQSQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }


    [Serializable]
    class PCond2SQXA : PCond2SQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2SQXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SQXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2SQXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SQXA";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SQXA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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

    [Serializable]
    class PCond2SQXA0 : PCond2SQXA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2SQXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond2SQXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SQXA0";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SQXA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
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

    [Serializable]
    class PCond2SQXQ : PCond2SQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCond2SQXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2SQXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SQXQ";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SQXQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
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

    [Serializable]
    class PCond2SQXS : PCond2SQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCond2SQXS (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCond2SQXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SQXS";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SQXS";
#endif
            if ((answer is bool) && (bool) answer == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
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

    [Serializable]
    class PCond2SS : PCond2S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        protected PCond2SS (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2SSA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented () :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2SSXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCond2SSXS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond2SS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SA";
#endif
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

    [Serializable]
    class PCond2SSA : PCond2SS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCond2SSA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2SSA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented():
                (alternative is StaticVariable) ? Unimplemented():
                new PCond2SSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SSA";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SSA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2SSA0 : PCond2SSA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2SSA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2SSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SSA0";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SSA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2SSXQ : PCond2SS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCond2SSXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2SSXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SSXQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SSXQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
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

    [Serializable]
    class PCond2SSXS : PCond2SS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected PCond2SSXS (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCond2SSXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2SSXS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SSXS";
#endif
            if ((answer is bool) && (bool) answer == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
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


    [Serializable]
    class PCond2SXA : PCond2S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        public readonly int consequentOffset;
        protected PCond2SXA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2SXA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2SXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2SXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2SXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SXA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2SXA0 : PCond2SXA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCond2SXA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2SXA0Q.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCond2SXA0S.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond2SXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2SXA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2SXA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SXA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2SXA0Q : PCond2SXA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2SXA0Q (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2SXA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2SXA0Q";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2SXA0Q";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SXA0Q";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2SXA0S : PCond2SXA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCond2SXA0S (PrimitiveCombination2 predicate, Argument0 consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, StaticVariable alternative)
        {
            return
                new PCond2SXA0S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2SXA0S";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2SXA0S";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SXA0S";
#endif
            if ((answer is bool) && (bool) answer == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }


    [Serializable]
    class PCond2SXQ : PCond2S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object consequentValue;
        protected PCond2SXQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2SXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2SXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2SXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SXQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2SXS : PCond2S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;
        protected PCond2SXS (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2SXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2SXS";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2SXS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SXS";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2SXXA : PCond2S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2SXXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SXXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2SXXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2SXXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2SXXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SXXA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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


    [Serializable]
    class PCond2SXXA0 : PCond2SXXA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCond2SXXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond2SXXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2SXXA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2SXXA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SXXA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
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


    [Serializable]
    class PCond2SXXQ : PCond2S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2SXXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2SXXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2SXXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2SXXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SXXQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
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

    [Serializable]
    class PCond2SXXS : PCond2S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCond2SXXS (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCond2SXXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2SXXS";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2SXXS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2SXXS";
#endif
            if ((answer is bool) && (bool) answer == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
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


    [Serializable]
    class PCond2XA : PCond2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;

        protected PCond2XA (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? PCond2XA0.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? PCond2XAA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond2XAQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? Unimplemented() :
                (alternative is Quotation) ? PCond2XAXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2XA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2XA";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond2XA";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2XA0 : PCond2XA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2XA0 (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? Unimplemented() : //PCond2XAA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented() : //PCond2XAQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2XA0XQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2XA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XA0";
#endif
            object ev1 = environment.Argument0Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2XA0";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond2XA0";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2XA0XQ : PCond2XA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2XA0XQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2XA0XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XA0XQ";
#endif
            object ev1 = environment.Argument0Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XA0XQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XA0XQ";
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
                SCode.location = "PCond2XA0";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }


    [Serializable]
    class PCond2XAA : PCond2XA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCond2XAA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2XAA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2XAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XAA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XAA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XAA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2XAA";
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
    class PCond2XAA0 : PCond2XAA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2XAA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2XAA0Q.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2XAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XAA0";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XAA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XAA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2XAA0";
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
    class PCond2XAA0Q : PCond2XAA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2XAA0Q (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2XAA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XAA0Q";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XAA0Q";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XAA0Q";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }


    [Serializable]
    class PCond2XAQ : PCond2XA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCond2XAQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2XAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XAQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XAQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XAQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2XAQ";
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
    class PCond2XAXQ : PCond2XA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2XAXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2XAXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XAXQ";
#endif
            object ev1 = environment.ArgumentValue(this.rand1Offset);

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XAXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XAXQ";
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
                SCode.location = "PCond2XAXQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }


    [Serializable]
    class PCond2XQ : PCond2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PCond2XQ (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = ((Quotation) predicate.Operand1).Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2XQA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond2XQQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? PCond2XQXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2XQXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XQ";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2";
#endif
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

    [Serializable]
    class PCond2XQA : PCond2XQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCond2XQA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2XQA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented() :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2XQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XQA";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XQA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XQA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2XQA0 : PCond2XQA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCond2XQA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2XQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XQA0";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XQA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XQA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2XQQ : PCond2XQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCond2XQQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2XQQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCond2XQQS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCond2XQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XQQ";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XQQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XQQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2XQQQ : PCond2XQQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCond2XQQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2XQQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XQQQ";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XQQQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XQQQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2XQQS : PCond2XQQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected PCond2XQQS (PrimitiveCombination2 predicate, Quotation consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, StaticVariable alternative)
        {
            return
                new PCond2XQQS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XQQS";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XQQS";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XQQS";
#endif
            if ((answer is bool) && (bool) answer == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }


    [Serializable]
    class PCond2XQXA : PCond2XQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;

        protected PCond2XQXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2XQXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2XQXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XQXA";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XQXA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XQXA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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

    [Serializable]
    class PCond2XQXA0 : PCond2XQXA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCond2XQXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond2XQXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XQXA0";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XQXA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XQXA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
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

    [Serializable]
    class PCond2XQXQ : PCond2XQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCond2XQXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2XQXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XQXQ";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XQXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XQXQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
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

    [Serializable]
    class PCond2XS : PCond2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PCond2XS (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2XSA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCond2XSQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCond2XSS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCond2XSXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCond2XSXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2XS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XS";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XS";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2XS";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCond2XS";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2XSA : PCond2XS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCond2XSA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2XSA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2XSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XSA";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XSA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XSA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2XSA";
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
    class PCond2XSA0 : PCond2XSA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCond2XSA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2XSA0Q.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2XSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XSA0";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XSA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XSA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2XSA0";
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
    class PCond2XSA0Q : PCond2XSA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2XSA0Q (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2XSA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XSA0Q";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XSA0Q";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XSA0Q";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2XSQ : PCond2XS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCond2XSQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2XSQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2XSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XSQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XSQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XSQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2XSQ";
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
    class PCond2XSQQ : PCond2XSQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCond2XSQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2XSQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XSQQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XSQQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XSQQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2XSS : PCond2XS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;

        public readonly int consequentOffset;
        protected PCond2XSS (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2XSSQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2XSS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XSS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XSS";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XSS";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCond2XSS";
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
    class PCond2XSSQ : PCond2XSS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2XSSQ (PrimitiveCombination2 predicate, StaticVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, Quotation alternative)
        {
            return
                new PCond2XSSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XSSQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XSSQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XSSQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }


    [Serializable]
    class PCond2XSXA : PCond2XS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;

        protected PCond2XSXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument) ? PCond2XSXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCond2XSXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XSXA";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XSXA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XSXA";
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
                SCode.location = "PCond2XSXQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2XSXA0 : PCond2XSXA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCond2XSXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond2XSXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XSXA0";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XSXA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XSXA0";
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
                SCode.location = "PCond2XSXA0";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2XSXQ : PCond2XS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCond2XSXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2XSXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2XSXQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XSXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XSXQ";
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
                SCode.location = "PCond2XSXQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2XXA : PCond2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCond2XXA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2XXA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2XXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2XXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XXA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XXA";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2XXA0 : PCond2XXA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCond2XXA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCond2XXA0Q.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCond2XXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2XXA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XXA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XXA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XXA0";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2XXA0Q : PCond2XXA0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2XXA0Q (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2XXA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2XXA0Q";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XXA0Q";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XXA0Q";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XXA0Q";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }


    [Serializable]
    class PCond2XXQ : PCond2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCond2XXQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented():
                (alternative is Quotation) ? PCond2XXQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented():
                new PCond2XXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2XXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XXQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class PCond2XXQQ : PCond2XXQ
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2XXQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2XXQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2XXQQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XXQQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XXQQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XXQQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCond2XXXA : PCond2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCond2XXXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented():
                new PCond2XXXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2XXXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XXXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XXXA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XXXA";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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


    [Serializable]
    class PCond2XXXQ : PCond2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCond2XXXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2XXXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCond2XXXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XXXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCond2XXXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
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
            SCode.location = "PCond2XXXQ";
#endif
            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
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

}
