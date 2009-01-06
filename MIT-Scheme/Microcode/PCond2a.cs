using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Microcode
{
    class PCond2SL : PCond2
    {
        // (s l s s)
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCond2SL (PrimitiveCombination2SL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveCombination2SL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveCombination2SA) ? PCond2SA.Make ((PrimitiveCombination2SA) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2SL1) ? PCond2SL1.Make ((PrimitiveCombination2SL1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCond2SLL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2SLQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SLSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SLSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2SL.EvalStep");
            noteCalls (this.rand0);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCond2SLL : PCond2SL
    {
        // (s l l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2SLL (PrimitiveCombination2SL predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2SL predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveCombination2SA) ? PCond2SA.Make ((PrimitiveCombination2SA) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2SL1) ? PCond2SL.Make ((PrimitiveCombination2SL1) predicate, consequent, alternative) :
                (consequent is Argument) ? PCond2SLA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2SLL1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SLLL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SLLQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SLA : PCond2SLL
    {
        // (s l a s)

        protected PCond2SLA (PrimitiveCombination2SL predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2SLA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2SLA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SLAL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SLAQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
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

    class PCond2SLAL : PCond2SLA
    {
        // (s l a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SLAL (PrimitiveCombination2SL predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SLAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SLAL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SLAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLAL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    class PCond2SLAA : PCond2SLAL
    {
        // (s l a l1)

        protected PCond2SLAA (PrimitiveCombination2SL predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SLAA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SLAA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SLAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLAA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = environment.ArgumentValue (((answer is bool) && (bool) answer == false) ?
                this.alternativeOffset :
                this.consequentOffset);
            return false;
        }
    }

    sealed class PCond2SLAA0 : PCond2SLAA
    {
        // (s l a a0)

        PCond2SLAA0 (PrimitiveCombination2SL predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2SLAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLAA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument0Value :
                environment.ArgumentValue (this.consequentOffset);
            return false;
        }
    }

    sealed class PCond2SLAA1 : PCond2SLAA
    {
        // (s l a a1)

        PCond2SLAA1 (PrimitiveCombination2SL predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2SLAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLAA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument1Value :
                environment.ArgumentValue (this.consequentOffset);
	    return false;
        }
    }

    sealed class PCond2SLAL1 : PCond2SLAL
    {
        // (s l a l1)

        PCond2SLAL1 (PrimitiveCombination2SL predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SLAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLAL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    sealed class PCond2SLAQ : PCond2SLA
    {
        // (s l a Q)
        public readonly object alternativeValue;
        PCond2SLAQ (PrimitiveCombination2SL predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2SLAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLAQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.ArgumentValue (this.consequentOffset);
            return false;
        }
    }

    class PCond2SLA0 : PCond2SLA
    {
        // (s l a0 s)

        protected PCond2SLA0 (PrimitiveCombination2SL predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SLA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SLA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
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

    class PCond2SLA0L : PCond2SLA0
    {
        // (s l a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SLA0L (PrimitiveCombination2SL predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SLA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SLA0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SLA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLA0L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    class PCond2SLA0A : PCond2SLA0L
    {
        // (s l a0 l1)

        protected PCond2SLA0A (PrimitiveCombination2SL predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SLA0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SLA0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SLA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLA0A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.ArgumentValue(this.alternativeOffset) :
                environment.Argument0Value;
            return false;
        }
    }

    sealed class PCond2SLA0A0 : PCond2SLA0A
    {
        // (s l a0 a0)

        PCond2SLA0A0 (PrimitiveCombination2SL predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2SLA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLA0A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = environment.Argument0Value;
            return false;
        }
    }

    sealed class PCond2SLA0A1 : PCond2SLA0A
    {
        // (s l a0 a1)

        PCond2SLA0A1 (PrimitiveCombination2SL predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2SLA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLA0A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument1Value :
                environment.Argument0Value;
	    return false;
        }
    }

    sealed class PCond2SLA0L1 : PCond2SLA0L
    {
        // (s l a0 l1)

        PCond2SLA0L1 (PrimitiveCombination2SL predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SLA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLA0L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    sealed class PCond2SLA0Q : PCond2SLA0
    {
        // (s l a0 Q)
        public readonly object alternativeValue;
        PCond2SLA0Q (PrimitiveCombination2SL predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2SLA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLA0Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.Argument0Value;
            return false;
        }
    }

    class PCond2SLA1 : PCond2SLA
    {
        // (s l a1 s)

        protected PCond2SLA1 (PrimitiveCombination2SL predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SLA1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SLA1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SLA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
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

    class PCond2SLA1L : PCond2SLA1
    {
        // (s l a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SLA1L (PrimitiveCombination2SL predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SLA1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SLA1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SLA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLA1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    class PCond2SLA1A : PCond2SLA1L
    {
        // (s l a1 l1)

        protected PCond2SLA1A (PrimitiveCombination2SL predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SLA1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SLA1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SLA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLA1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.ArgumentValue(this.alternativeOffset) :
                environment.Argument1Value;
            return false;
        }
    }

    sealed class PCond2SLA1A0 : PCond2SLA1A
    {
        // (s l a1 a0)

        PCond2SLA1A0 (PrimitiveCombination2SL predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2SLA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLA1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument0Value :
                environment.Argument1Value;
            return false;
        }
    }

    sealed class PCond2SLA1A1 : PCond2SLA1A
    {
        // (s l a1 a1)

        PCond2SLA1A1 (PrimitiveCombination2SL predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2SLA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLA1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = environment.Argument1Value;
	    return false;
        }
    }

    sealed class PCond2SLA1L1 : PCond2SLA1L
    {
        // (s l a1 l1)

        PCond2SLA1L1 (PrimitiveCombination2SL predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SLA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLA1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    sealed class PCond2SLA1Q : PCond2SLA1
    {
        // (s l a1 Q)
        public readonly object alternativeValue;
        PCond2SLA1Q (PrimitiveCombination2SL predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2SLA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLA1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.Argument1Value;
            return false;
        }
    }

    class PCond2SLL1 : PCond2SLL
    {
        // (s l l1 s)

        protected PCond2SLL1 (PrimitiveCombination2SL predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SLL1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SLL1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SLL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SLL1L : PCond2SLL1
    {
        // (s l l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SLL1L (PrimitiveCombination2SL predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SL predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SLL1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SLL1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SLL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLL1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SLL1A : PCond2SLL1L
    {
        // (s l l1 l1)

        protected PCond2SLL1A (PrimitiveCombination2SL predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SLL1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SLL1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SLL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLL1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SLL1A0 : PCond2SLL1A
    {
        // (s l l1 a0)

        PCond2SLL1A0 (PrimitiveCombination2SL predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2SLL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLL1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SLL1A1 : PCond2SLL1A
    {
        // (s l l1 a1)

        PCond2SLL1A1 (PrimitiveCombination2SL predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2SLL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLL1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SLL1L1 : PCond2SLL1L
    {
        // (s l l1 l1)

        PCond2SLL1L1 (PrimitiveCombination2SL predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SLL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLL1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SLL1Q : PCond2SLL1
    {
        // (s l l1 Q)
        public readonly object alternativeValue;
        PCond2SLL1Q (PrimitiveCombination2SL predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SL predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2SLL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLL1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SLLL : PCond2SLL
    {
        // (s l l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SLLL (PrimitiveCombination2SL predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SL predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SLLA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SLLL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SLLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLLL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SLLA : PCond2SLLL
    {
        // (s l l l1)

        protected PCond2SLLA (PrimitiveCombination2SL predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SLLA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SLLA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SLLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLLA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SLLA0 : PCond2SLLA
    {
        // (s l l a0)

        PCond2SLLA0 (PrimitiveCombination2SL predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2SLLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLLA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SLLA1 : PCond2SLLA
    {
        // (s l l a1)

        PCond2SLLA1 (PrimitiveCombination2SL predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2SLLA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLLA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SLLL1 : PCond2SLLL
    {
        // (s l l l1)

        PCond2SLLL1 (PrimitiveCombination2SL predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SLLL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLLL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SLLQ : PCond2SLL
    {
        // (s l l Q)
        public readonly object alternativeValue;
        PCond2SLLQ (PrimitiveCombination2SL predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SL predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2SLLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLLQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SLQ : PCond2SL
    {
        // (s l q s)
        public readonly object consequentValue;

        protected PCond2SLQ (PrimitiveCombination2SL predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SLQL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SLQQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "Pcond2SSQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
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

    class PCond2SLQL : PCond2SLQ
    {
        // (s l q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2SLQL (PrimitiveCombination2SL predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SLQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SLQL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SLQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLQL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    class PCond2SLQA : PCond2SLQL
    {
        // (s l q a)
        protected PCond2SLQA (PrimitiveCombination2SL predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SLQA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SLQA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SLQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLQA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ? 
                environment.ArgumentValue (alternativeOffset) : 
                this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SLQA0 : PCond2SLQA
    {
        // (s l q a0)
        PCond2SLQA0 (PrimitiveCombination2SL predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2SLQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLQA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ? environment.Argument0Value : this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SLQA1 : PCond2SLQA
    {
        // (s l q a1)
        PCond2SLQA1 (PrimitiveCombination2SL predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2SLQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLQA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ? environment.Argument1Value : this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SLQL1 : PCond2SLQL
    {
        // (s l q l1)
        PCond2SLQL1 (PrimitiveCombination2SL predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SLQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLQL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    sealed class PCond2SLQQ : PCond2SLQ
    {
        // (s l q q)
        public readonly object alternativeValue;

        PCond2SLQQ (PrimitiveCombination2SL predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SL predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2SLQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLQQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ? this.alternativeValue : this.consequentValue;
            return false;
        }
    }

    class PCond2SLSL : PCond2SL
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2SLSL (PrimitiveCombination2SL predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2SL predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SLSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SLSL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SLSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLSL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCond2SLSA : PCond2SLSL
    {
        protected PCond2SLSA (PrimitiveCombination2SL predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SL predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SLSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SLSA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SLSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLSA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SLSA0 : PCond2SLSA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2SLSA0 (PrimitiveCombination2SL predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SL predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2SLSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2SLSA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SLSA1 : PCond2SLSA
    {
        PCond2SLSA1 (PrimitiveCombination2SL predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SL predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2SLSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLSA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SLSL1 : PCond2SLSL
    {
        PCond2SLSL1 (PrimitiveCombination2SL predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SL predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2SLSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLSL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SLSQ : PCond2SL
    {
        // (s l s q)
        public readonly object alternativeValue;

        PCond2SLSQ (PrimitiveCombination2SL predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SL predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2SLSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SLSQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCond2SA : PCond2SL
    {
        // (s a s s)
        protected PCond2SA (PrimitiveCombination2SA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveCombination2SA0) ? PCond2SA0.Make ((PrimitiveCombination2SA0) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2SA1) ? PCond2SA1.Make ((PrimitiveCombination2SA1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCond2SAL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2SAQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SASL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SASQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2SA.EvalStep");
            noteCalls (this.rand0);
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCond2SAL : PCond2SA
    {
        // (s a l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2SAL (PrimitiveCombination2SA predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2SAA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2SAL1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SALL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SALQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAL.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SAA : PCond2SAL
    {
        // (s a a s)

        protected PCond2SAA (PrimitiveCombination2SA predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2SAA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2SAA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SAAL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SAAQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAA.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                noteCalls (this.alternative);
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

    class PCond2SAAL : PCond2SAA
    {
        // (s a a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SAAL (PrimitiveCombination2SA predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SAAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SAAL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SAAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAAL.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    class PCond2SAAA : PCond2SAAL
    {
        // (s a a l1)

        protected PCond2SAAA (PrimitiveCombination2SA predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SAAA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SAAA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SAAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAAA.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = environment.ArgumentValue (((answer is bool) && (bool) answer == false) ?
                this.alternativeOffset :
                this.consequentOffset);
            return false;
        }
    }

    sealed class PCond2SAAA0 : PCond2SAAA
    {
        // (s a a a0)

        PCond2SAAA0 (PrimitiveCombination2SA predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2SAAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAAA0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument0Value :
                environment.ArgumentValue (this.consequentOffset);
            return false;
        }
    }

    sealed class PCond2SAAA1 : PCond2SAAA
    {
        // (s a a a1)

        PCond2SAAA1 (PrimitiveCombination2SA predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2SAAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAAA1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument1Value :
                environment.ArgumentValue (this.consequentOffset);
	    return false;
        }
    }

    sealed class PCond2SAAL1 : PCond2SAAL
    {
        // (s a a l1)

        PCond2SAAL1 (PrimitiveCombination2SA predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SAAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAAL1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    sealed class PCond2SAAQ : PCond2SAA
    {
        // (s a a Q)
        public readonly object alternativeValue;
        PCond2SAAQ (PrimitiveCombination2SA predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2SAAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAAQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.ArgumentValue (this.consequentOffset);
            return false;
        }
    }

    class PCond2SAA0 : PCond2SAA
    {
        // (s a a0 s)

        protected PCond2SAA0 (PrimitiveCombination2SA predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SAA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SAA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAA0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                noteCalls (this.alternative);
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

    class PCond2SAA0L : PCond2SAA0
    {
        // (s a a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SAA0L (PrimitiveCombination2SA predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SAA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SAA0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SAA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAA0L.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    class PCond2SAA0A : PCond2SAA0L
    {
        // (s a a0 l1)

        protected PCond2SAA0A (PrimitiveCombination2SA predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SAA0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SAA0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SAA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAA0A.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.ArgumentValue(this.alternativeOffset) :
                environment.Argument0Value;
            return false;
        }
    }

    sealed class PCond2SAA0A0 : PCond2SAA0A
    {
        // (s a a0 a0)

        PCond2SAA0A0 (PrimitiveCombination2SA predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2SAA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAA0A0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = environment.Argument0Value;
            return false;
        }
    }

    sealed class PCond2SAA0A1 : PCond2SAA0A
    {
        // (s a a0 a1)

        PCond2SAA0A1 (PrimitiveCombination2SA predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2SAA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAA0A1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument1Value :
                environment.Argument0Value;
	    return false;
        }
    }

    sealed class PCond2SAA0L1 : PCond2SAA0L
    {
        // (s a a0 l1)

        PCond2SAA0L1 (PrimitiveCombination2SA predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SAA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAA0L1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    sealed class PCond2SAA0Q : PCond2SAA0
    {
        // (s a a0 Q)
        public readonly object alternativeValue;
        PCond2SAA0Q (PrimitiveCombination2SA predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2SAA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAA0Q.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.Argument0Value;
            return false;
        }
    }

    class PCond2SAA1 : PCond2SAA
    {
        // (s a a1 s)

        protected PCond2SAA1 (PrimitiveCombination2SA predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SAA1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SAA1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAA1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                noteCalls (this.alternative);
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

    class PCond2SAA1L : PCond2SAA1
    {
        // (s a a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SAA1L (PrimitiveCombination2SA predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SAA1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SAA1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SAA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAA1L.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    class PCond2SAA1A : PCond2SAA1L
    {
        // (s a a1 l1)

        protected PCond2SAA1A (PrimitiveCombination2SA predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SAA1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SAA1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SAA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAA1A.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.ArgumentValue(this.alternativeOffset) :
                environment.Argument1Value;
            return false;
        }
    }

    sealed class PCond2SAA1A0 : PCond2SAA1A
    {
        // (s a a1 a0)

        PCond2SAA1A0 (PrimitiveCombination2SA predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2SAA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAA1A0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument0Value :
                environment.Argument1Value;
            return false;
        }
    }

    sealed class PCond2SAA1A1 : PCond2SAA1A
    {
        // (s a a1 a1)

        PCond2SAA1A1 (PrimitiveCombination2SA predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2SAA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAA1A1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = environment.Argument1Value;
	    return false;
        }
    }

    sealed class PCond2SAA1L1 : PCond2SAA1L
    {
        // (s a a1 l1)

        PCond2SAA1L1 (PrimitiveCombination2SA predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SAA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAA1L1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    sealed class PCond2SAA1Q : PCond2SAA1
    {
        // (s a a1 Q)
        public readonly object alternativeValue;
        PCond2SAA1Q (PrimitiveCombination2SA predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2SAA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAA1Q.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.Argument1Value;
            return false;
        }
    }

    class PCond2SAL1 : PCond2SAL
    {
        // (s a l1 s)

        protected PCond2SAL1 (PrimitiveCombination2SA predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SAL1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SAL1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAL1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SAL1L : PCond2SAL1
    {
        // (s a l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SAL1L (PrimitiveCombination2SA predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SAL1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SAL1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SAL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAL1L.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SAL1A : PCond2SAL1L
    {
        // (s a l1 l1)

        protected PCond2SAL1A (PrimitiveCombination2SA predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SAL1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SAL1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SAL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAL1A.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SAL1A0 : PCond2SAL1A
    {
        // (s a l1 a0)

        PCond2SAL1A0 (PrimitiveCombination2SA predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2SAL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAL1A0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                answer = environment.Argument0Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SAL1A1 : PCond2SAL1A
    {
        // (s a l1 a1)

        PCond2SAL1A1 (PrimitiveCombination2SA predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2SAL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAL1A1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                answer = environment.Argument1Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SAL1L1 : PCond2SAL1L
    {
        // (s a l1 l1)

        PCond2SAL1L1 (PrimitiveCombination2SA predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SAL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAL1L1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SAL1Q : PCond2SAL1
    {
        // (s a l1 Q)
        public readonly object alternativeValue;
        PCond2SAL1Q (PrimitiveCombination2SA predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2SAL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAL1Q.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                answer = this.alternativeValue;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SALL : PCond2SAL
    {
        // (s a l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SALL (PrimitiveCombination2SA predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SALA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SALL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SALL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SALL.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SALA : PCond2SALL
    {
        // (s a l l1)

        protected PCond2SALA (PrimitiveCombination2SA predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SALA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SALA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SALA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SALA.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SALA0 : PCond2SALA
    {
        // (s a l a0)

        PCond2SALA0 (PrimitiveCombination2SA predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2SALA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SALA0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                answer = environment.Argument0Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SALA1 : PCond2SALA
    {
        // (s a l a1)

        PCond2SALA1 (PrimitiveCombination2SA predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2SALA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SALA1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                answer = environment.Argument1Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SALL1 : PCond2SALL
    {
        // (s a l l1)

        PCond2SALL1 (PrimitiveCombination2SA predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SALL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SALL1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SALQ : PCond2SAL
    {
        // (s a l Q)
        public readonly object alternativeValue;
        PCond2SALQ (PrimitiveCombination2SA predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2SALQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SALQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                answer = this.alternativeValue;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SAQ : PCond2SA
    {
        // (s a q s)
        public readonly object consequentValue;

        protected PCond2SAQ (PrimitiveCombination2SA predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SAQL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SAQQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "Pcond2SSQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                noteCalls (this.alternative);
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

    class PCond2SAQL : PCond2SAQ
    {
        // (s a q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2SAQL (PrimitiveCombination2SA predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SAQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SAQL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SAQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAQL.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    class PCond2SAQA : PCond2SAQL
    {
        // (s a q a)
        protected PCond2SAQA (PrimitiveCombination2SA predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SAQA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SAQA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SAQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAQA.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ? 
                environment.ArgumentValue (alternativeOffset) : 
                this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SAQA0 : PCond2SAQA
    {
        // (s a q a0)
        PCond2SAQA0 (PrimitiveCombination2SA predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2SAQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAQA0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ? environment.Argument0Value : this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SAQA1 : PCond2SAQA
    {
        // (s a q a1)
        PCond2SAQA1 (PrimitiveCombination2SA predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2SAQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAQA1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ? environment.Argument1Value : this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SAQL1 : PCond2SAQL
    {
        // (s a q l1)
        PCond2SAQL1 (PrimitiveCombination2SA predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SAQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAQL1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    sealed class PCond2SAQQ : PCond2SAQ
    {
        // (s a q q)
        public readonly object alternativeValue;

        PCond2SAQQ (PrimitiveCombination2SA predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2SAQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SAQQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
                    if (tci != null) {
                        answer = null; // dispose of the evidence
                        // set up the interpreter for a tail call
                        Control cExpression = tci.Expression;
                        Environment cEnvironment = tci.Environment;
                        while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                    }
                }

            answer = ((answer is bool) && (bool) answer == false) ? this.alternativeValue : this.consequentValue;
            return false;
        }
    }

    class PCond2SASL : PCond2SA
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2SASL (PrimitiveCombination2SA predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2SA predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SASA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SASL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SASL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SASL.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCond2SASA : PCond2SASL
    {
        protected PCond2SASA (PrimitiveCombination2SA predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SA predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SASA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SASA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SASA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SASA.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SASA0 : PCond2SASA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2SASA0 (PrimitiveCombination2SA predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SA predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2SASA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2SASA0.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SASA1 : PCond2SASA
    {
        PCond2SASA1 (PrimitiveCombination2SA predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SA predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2SASA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SASA1.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                answer = environment.Argument1Value;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SASL1 : PCond2SASL
    {
        PCond2SASL1 (PrimitiveCombination2SA predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SA predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2SASL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SASL1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SASQ : PCond2SA
    {
        // (s a s q)
        public readonly object alternativeValue;

        PCond2SASQ (PrimitiveCombination2SA predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2SASQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SASQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue (this.rand1Offset))) {
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
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCond2SA0 : PCond2SA
    {
        // (s a0 s s)
        protected PCond2SA0 (PrimitiveCombination2SA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2SA0L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2SA0Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SA0SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SA0SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2SA0.EvalStep");
            noteCalls (this.rand0);
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCond2SA0L : PCond2SA0
    {
        // (s a0 l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2SA0L (PrimitiveCombination2SA0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2SA0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2SA0L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SA0LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SA0LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0L.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SA0A : PCond2SA0L
    {
        // (s a0 a s)

        protected PCond2SA0A (PrimitiveCombination2SA0 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2SA0A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2SA0A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SA0AL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SA0AQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0A.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                noteCalls (this.alternative);
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

    class PCond2SA0AL : PCond2SA0A
    {
        // (s a0 a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SA0AL (PrimitiveCombination2SA0 predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SA0AA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SA0AL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SA0AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0AL.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    class PCond2SA0AA : PCond2SA0AL
    {
        // (s a0 a l1)

        protected PCond2SA0AA (PrimitiveCombination2SA0 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SA0AA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SA0AA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SA0AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0AA.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = environment.ArgumentValue (((answer is bool) && (bool) answer == false) ?
                this.alternativeOffset :
                this.consequentOffset);
            return false;
        }
    }

    sealed class PCond2SA0AA0 : PCond2SA0AA
    {
        // (s a0 a a0)

        PCond2SA0AA0 (PrimitiveCombination2SA0 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2SA0AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0AA0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument0Value :
                environment.ArgumentValue (this.consequentOffset);
            return false;
        }
    }

    sealed class PCond2SA0AA1 : PCond2SA0AA
    {
        // (s a0 a a1)

        PCond2SA0AA1 (PrimitiveCombination2SA0 predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2SA0AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0AA1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument1Value :
                environment.ArgumentValue (this.consequentOffset);
	    return false;
        }
    }

    sealed class PCond2SA0AL1 : PCond2SA0AL
    {
        // (s a0 a l1)

        PCond2SA0AL1 (PrimitiveCombination2SA0 predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SA0AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0AL1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    sealed class PCond2SA0AQ : PCond2SA0A
    {
        // (s a0 a Q)
        public readonly object alternativeValue;
        PCond2SA0AQ (PrimitiveCombination2SA0 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2SA0AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0AQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.ArgumentValue (this.consequentOffset);
            return false;
        }
    }

    class PCond2SA0A0 : PCond2SA0A
    {
        // (s a0 a0 s)

        protected PCond2SA0A0 (PrimitiveCombination2SA0 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SA0A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SA0A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0A0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                noteCalls (this.alternative);
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

    class PCond2SA0A0L : PCond2SA0A0
    {
        // (s a0 a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SA0A0L (PrimitiveCombination2SA0 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SA0A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SA0A0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SA0A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0A0L.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    class PCond2SA0A0A : PCond2SA0A0L
    {
        // (s a0 a0 l1)

        protected PCond2SA0A0A (PrimitiveCombination2SA0 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SA0A0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SA0A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SA0A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0A0A.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.ArgumentValue(this.alternativeOffset) :
                environment.Argument0Value;
            return false;
        }
    }

    sealed class PCond2SA0A0A0 : PCond2SA0A0A
    {
        // (s a0 a0 a0)

        PCond2SA0A0A0 (PrimitiveCombination2SA0 predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2SA0A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0A0A0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = environment.Argument0Value;
            return false;
        }
    }

    sealed class PCond2SA0A0A1 : PCond2SA0A0A
    {
        // (s a0 a0 a1)

        PCond2SA0A0A1 (PrimitiveCombination2SA0 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2SA0A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0A0A1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument1Value :
                environment.Argument0Value;
	    return false;
        }
    }

    sealed class PCond2SA0A0L1 : PCond2SA0A0L
    {
        // (s a0 a0 l1)

        PCond2SA0A0L1 (PrimitiveCombination2SA0 predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SA0A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0A0L1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    sealed class PCond2SA0A0Q : PCond2SA0A0
    {
        // (s a0 a0 Q)
        public readonly object alternativeValue;
        PCond2SA0A0Q (PrimitiveCombination2SA0 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2SA0A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0A0Q.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.Argument0Value;
            return false;
        }
    }

    class PCond2SA0A1 : PCond2SA0A
    {
        // (s a0 a1 s)

        protected PCond2SA0A1 (PrimitiveCombination2SA0 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SA0A1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SA0A1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0A1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                noteCalls (this.alternative);
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

    class PCond2SA0A1L : PCond2SA0A1
    {
        // (s a0 a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SA0A1L (PrimitiveCombination2SA0 predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SA0A1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SA0A1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SA0A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0A1L.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    class PCond2SA0A1A : PCond2SA0A1L
    {
        // (s a0 a1 l1)

        protected PCond2SA0A1A (PrimitiveCombination2SA0 predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SA0A1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SA0A1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SA0A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0A1A.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.ArgumentValue(this.alternativeOffset) :
                environment.Argument1Value;
            return false;
        }
    }

    sealed class PCond2SA0A1A0 : PCond2SA0A1A
    {
        // (s a0 a1 a0)

        PCond2SA0A1A0 (PrimitiveCombination2SA0 predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2SA0A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0A1A0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument0Value :
                environment.Argument1Value;
            return false;
        }
    }

    sealed class PCond2SA0A1A1 : PCond2SA0A1A
    {
        // (s a0 a1 a1)

        PCond2SA0A1A1 (PrimitiveCombination2SA0 predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2SA0A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0A1A1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = environment.Argument1Value;
	    return false;
        }
    }

    sealed class PCond2SA0A1L1 : PCond2SA0A1L
    {
        // (s a0 a1 l1)

        PCond2SA0A1L1 (PrimitiveCombination2SA0 predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SA0A1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0A1L1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    sealed class PCond2SA0A1Q : PCond2SA0A1
    {
        // (s a0 a1 Q)
        public readonly object alternativeValue;
        PCond2SA0A1Q (PrimitiveCombination2SA0 predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2SA0A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0A1Q.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.Argument1Value;
            return false;
        }
    }

    class PCond2SA0L1 : PCond2SA0L
    {
        // (s a0 l1 s)

        protected PCond2SA0L1 (PrimitiveCombination2SA0 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SA0L1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SA0L1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0L1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SA0L1L : PCond2SA0L1
    {
        // (s a0 l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SA0L1L (PrimitiveCombination2SA0 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SA0L1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SA0L1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SA0L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0L1L.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SA0L1A : PCond2SA0L1L
    {
        // (s a0 l1 l1)

        protected PCond2SA0L1A (PrimitiveCombination2SA0 predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SA0L1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SA0L1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SA0L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0L1A.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SA0L1A0 : PCond2SA0L1A
    {
        // (s a0 l1 a0)

        PCond2SA0L1A0 (PrimitiveCombination2SA0 predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2SA0L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0L1A0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                answer = environment.Argument0Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SA0L1A1 : PCond2SA0L1A
    {
        // (s a0 l1 a1)

        PCond2SA0L1A1 (PrimitiveCombination2SA0 predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2SA0L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0L1A1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                answer = environment.Argument1Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SA0L1L1 : PCond2SA0L1L
    {
        // (s a0 l1 l1)

        PCond2SA0L1L1 (PrimitiveCombination2SA0 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SA0L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0L1L1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SA0L1Q : PCond2SA0L1
    {
        // (s a0 l1 Q)
        public readonly object alternativeValue;
        PCond2SA0L1Q (PrimitiveCombination2SA0 predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2SA0L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0L1Q.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                answer = this.alternativeValue;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SA0LL : PCond2SA0L
    {
        // (s a0 l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SA0LL (PrimitiveCombination2SA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SA0LA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SA0LL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SA0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0LL.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SA0LA : PCond2SA0LL
    {
        // (s a0 l l1)

        protected PCond2SA0LA (PrimitiveCombination2SA0 predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SA0LA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SA0LA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SA0LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0LA.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SA0LA0 : PCond2SA0LA
    {
        // (s a0 l a0)

        PCond2SA0LA0 (PrimitiveCombination2SA0 predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2SA0LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0LA0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                answer = environment.Argument0Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SA0LA1 : PCond2SA0LA
    {
        // (s a0 l a1)

        PCond2SA0LA1 (PrimitiveCombination2SA0 predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2SA0LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0LA1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                answer = environment.Argument1Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SA0LL1 : PCond2SA0LL
    {
        // (s a0 l l1)

        PCond2SA0LL1 (PrimitiveCombination2SA0 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SA0LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0LL1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SA0LQ : PCond2SA0L
    {
        // (s a0 l Q)
        public readonly object alternativeValue;
        PCond2SA0LQ (PrimitiveCombination2SA0 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2SA0LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0LQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                answer = this.alternativeValue;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SA0Q : PCond2SA0
    {
        // (s a0 q s)
        public readonly object consequentValue;

        protected PCond2SA0Q (PrimitiveCombination2SA0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SA0QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SA0QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "Pcond2SSQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                noteCalls (this.alternative);
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

    class PCond2SA0QL : PCond2SA0Q
    {
        // (s a0 q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2SA0QL (PrimitiveCombination2SA0 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SA0QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SA0QL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SA0QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0QL.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    class PCond2SA0QA : PCond2SA0QL
    {
        // (s a0 q a)
        protected PCond2SA0QA (PrimitiveCombination2SA0 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SA0QA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SA0QA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SA0QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0QA.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ? 
                environment.ArgumentValue (alternativeOffset) : 
                this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SA0QA0 : PCond2SA0QA
    {
        // (s a0 q a0)
        PCond2SA0QA0 (PrimitiveCombination2SA0 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2SA0QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0QA0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ? environment.Argument0Value : this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SA0QA1 : PCond2SA0QA
    {
        // (s a0 q a1)
        PCond2SA0QA1 (PrimitiveCombination2SA0 predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2SA0QA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0QA1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ? environment.Argument1Value : this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SA0QL1 : PCond2SA0QL
    {
        // (s a0 q l1)
        PCond2SA0QL1 (PrimitiveCombination2SA0 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SA0QL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0QL1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    sealed class PCond2SA0QQ : PCond2SA0Q
    {
        // (s a0 q q)
        public readonly object alternativeValue;

        PCond2SA0QQ (PrimitiveCombination2SA0 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2SA0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0QQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, ev0, environment.Argument0Value)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
                    if (tci != null) {
                        answer = null; // dispose of the evidence
                        // set up the interpreter for a tail call
                        Control cExpression = tci.Expression;
                        Environment cEnvironment = tci.Environment;
                        while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                    }
                }

            answer = ((answer is bool) && (bool) answer == false) ? this.alternativeValue : this.consequentValue;
            return false;
        }
    }

    class PCond2SA0SL : PCond2SA0
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2SA0SL (PrimitiveCombination2SA0 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2SA0 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SA0SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SA0SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SA0SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0SL.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCond2SA0SA : PCond2SA0SL
    {
        protected PCond2SA0SA (PrimitiveCombination2SA0 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SA0 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SA0SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SA0SA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SA0SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0SA.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SA0SA0 : PCond2SA0SA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2SA0SA0 (PrimitiveCombination2SA0 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SA0 predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2SA0SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2SA0SA0.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SA0SA1 : PCond2SA0SA
    {
        PCond2SA0SA1 (PrimitiveCombination2SA0 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SA0 predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2SA0SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0SA1.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                answer = environment.Argument1Value;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SA0SL1 : PCond2SA0SL
    {
        PCond2SA0SL1 (PrimitiveCombination2SA0 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SA0 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2SA0SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0SL1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SA0SQ : PCond2SA0
    {
        // (s a0 s q)
        public readonly object alternativeValue;

        PCond2SA0SQ (PrimitiveCombination2SA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA0 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2SA0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA0SQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCond2SA1 : PCond2SA
    {
        // (s a1 s s)
        protected PCond2SA1 (PrimitiveCombination2SA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2SA1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2SA1Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SA1SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SA1SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2SA1.EvalStep");
            noteCalls (this.rand0);
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCond2SA1L : PCond2SA1
    {
        // (s a1 l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2SA1L (PrimitiveCombination2SA1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2SA1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2SA1L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SA1LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SA1LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1L.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SA1A : PCond2SA1L
    {
        // (s a1 a s)

        protected PCond2SA1A (PrimitiveCombination2SA1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2SA1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2SA1A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SA1AL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SA1AQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1A.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                noteCalls (this.alternative);
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

    class PCond2SA1AL : PCond2SA1A
    {
        // (s a1 a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SA1AL (PrimitiveCombination2SA1 predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SA1AA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SA1AL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SA1AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1AL.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    class PCond2SA1AA : PCond2SA1AL
    {
        // (s a1 a l1)

        protected PCond2SA1AA (PrimitiveCombination2SA1 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SA1AA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SA1AA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SA1AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1AA.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = environment.ArgumentValue (((answer is bool) && (bool) answer == false) ?
                this.alternativeOffset :
                this.consequentOffset);
            return false;
        }
    }

    sealed class PCond2SA1AA0 : PCond2SA1AA
    {
        // (s a1 a a0)

        PCond2SA1AA0 (PrimitiveCombination2SA1 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2SA1AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1AA0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument0Value :
                environment.ArgumentValue (this.consequentOffset);
            return false;
        }
    }

    sealed class PCond2SA1AA1 : PCond2SA1AA
    {
        // (s a1 a a1)

        PCond2SA1AA1 (PrimitiveCombination2SA1 predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2SA1AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1AA1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument1Value :
                environment.ArgumentValue (this.consequentOffset);
	    return false;
        }
    }

    sealed class PCond2SA1AL1 : PCond2SA1AL
    {
        // (s a1 a l1)

        PCond2SA1AL1 (PrimitiveCombination2SA1 predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SA1AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1AL1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    sealed class PCond2SA1AQ : PCond2SA1A
    {
        // (s a1 a Q)
        public readonly object alternativeValue;
        PCond2SA1AQ (PrimitiveCombination2SA1 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2SA1AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1AQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.ArgumentValue (this.consequentOffset);
            return false;
        }
    }

    class PCond2SA1A0 : PCond2SA1A
    {
        // (s a1 a0 s)

        protected PCond2SA1A0 (PrimitiveCombination2SA1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SA1A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SA1A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1A0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                noteCalls (this.alternative);
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

    class PCond2SA1A0L : PCond2SA1A0
    {
        // (s a1 a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SA1A0L (PrimitiveCombination2SA1 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SA1A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SA1A0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SA1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1A0L.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    class PCond2SA1A0A : PCond2SA1A0L
    {
        // (s a1 a0 l1)

        protected PCond2SA1A0A (PrimitiveCombination2SA1 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SA1A0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SA1A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SA1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1A0A.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.ArgumentValue(this.alternativeOffset) :
                environment.Argument0Value;
            return false;
        }
    }

    sealed class PCond2SA1A0A0 : PCond2SA1A0A
    {
        // (s a1 a0 a0)

        PCond2SA1A0A0 (PrimitiveCombination2SA1 predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2SA1A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1A0A0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = environment.Argument0Value;
            return false;
        }
    }

    sealed class PCond2SA1A0A1 : PCond2SA1A0A
    {
        // (s a1 a0 a1)

        PCond2SA1A0A1 (PrimitiveCombination2SA1 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2SA1A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1A0A1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument1Value :
                environment.Argument0Value;
	    return false;
        }
    }

    sealed class PCond2SA1A0L1 : PCond2SA1A0L
    {
        // (s a1 a0 l1)

        PCond2SA1A0L1 (PrimitiveCombination2SA1 predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SA1A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1A0L1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    sealed class PCond2SA1A0Q : PCond2SA1A0
    {
        // (s a1 a0 Q)
        public readonly object alternativeValue;
        PCond2SA1A0Q (PrimitiveCombination2SA1 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2SA1A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1A0Q.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.Argument0Value;
            return false;
        }
    }

    class PCond2SA1A1 : PCond2SA1A
    {
        // (s a1 a1 s)

        protected PCond2SA1A1 (PrimitiveCombination2SA1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SA1A1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SA1A1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1A1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                noteCalls (this.alternative);
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

    class PCond2SA1A1L : PCond2SA1A1
    {
        // (s a1 a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SA1A1L (PrimitiveCombination2SA1 predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SA1A1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SA1A1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SA1A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1A1L.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    class PCond2SA1A1A : PCond2SA1A1L
    {
        // (s a1 a1 l1)

        protected PCond2SA1A1A (PrimitiveCombination2SA1 predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SA1A1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SA1A1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SA1A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1A1A.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.ArgumentValue(this.alternativeOffset) :
                environment.Argument1Value;
            return false;
        }
    }

    sealed class PCond2SA1A1A0 : PCond2SA1A1A
    {
        // (s a1 a1 a0)

        PCond2SA1A1A0 (PrimitiveCombination2SA1 predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2SA1A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1A1A0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument0Value :
                environment.Argument1Value;
            return false;
        }
    }

    sealed class PCond2SA1A1A1 : PCond2SA1A1A
    {
        // (s a1 a1 a1)

        PCond2SA1A1A1 (PrimitiveCombination2SA1 predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2SA1A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1A1A1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = environment.Argument1Value;
	    return false;
        }
    }

    sealed class PCond2SA1A1L1 : PCond2SA1A1L
    {
        // (s a1 a1 l1)

        PCond2SA1A1L1 (PrimitiveCombination2SA1 predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SA1A1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1A1L1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    sealed class PCond2SA1A1Q : PCond2SA1A1
    {
        // (s a1 a1 Q)
        public readonly object alternativeValue;
        PCond2SA1A1Q (PrimitiveCombination2SA1 predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2SA1A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1A1Q.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.Argument1Value;
            return false;
        }
    }

    class PCond2SA1L1 : PCond2SA1L
    {
        // (s a1 l1 s)

        protected PCond2SA1L1 (PrimitiveCombination2SA1 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SA1L1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SA1L1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1L1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SA1L1L : PCond2SA1L1
    {
        // (s a1 l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SA1L1L (PrimitiveCombination2SA1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SA1L1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SA1L1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SA1L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1L1L.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SA1L1A : PCond2SA1L1L
    {
        // (s a1 l1 l1)

        protected PCond2SA1L1A (PrimitiveCombination2SA1 predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SA1L1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SA1L1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SA1L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1L1A.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SA1L1A0 : PCond2SA1L1A
    {
        // (s a1 l1 a0)

        PCond2SA1L1A0 (PrimitiveCombination2SA1 predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2SA1L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1L1A0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                answer = environment.Argument0Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SA1L1A1 : PCond2SA1L1A
    {
        // (s a1 l1 a1)

        PCond2SA1L1A1 (PrimitiveCombination2SA1 predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2SA1L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1L1A1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                answer = environment.Argument1Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SA1L1L1 : PCond2SA1L1L
    {
        // (s a1 l1 l1)

        PCond2SA1L1L1 (PrimitiveCombination2SA1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SA1L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1L1L1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SA1L1Q : PCond2SA1L1
    {
        // (s a1 l1 Q)
        public readonly object alternativeValue;
        PCond2SA1L1Q (PrimitiveCombination2SA1 predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2SA1L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1L1Q.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                answer = this.alternativeValue;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SA1LL : PCond2SA1L
    {
        // (s a1 l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SA1LL (PrimitiveCombination2SA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SA1LA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SA1LL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SA1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1LL.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SA1LA : PCond2SA1LL
    {
        // (s a1 l l1)

        protected PCond2SA1LA (PrimitiveCombination2SA1 predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SA1LA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SA1LA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SA1LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1LA.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SA1LA0 : PCond2SA1LA
    {
        // (s a1 l a0)

        PCond2SA1LA0 (PrimitiveCombination2SA1 predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2SA1LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1LA0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                answer = environment.Argument0Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SA1LA1 : PCond2SA1LA
    {
        // (s a1 l a1)

        PCond2SA1LA1 (PrimitiveCombination2SA1 predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2SA1LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1LA1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                answer = environment.Argument1Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SA1LL1 : PCond2SA1LL
    {
        // (s a1 l l1)

        PCond2SA1LL1 (PrimitiveCombination2SA1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SA1LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1LL1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SA1LQ : PCond2SA1L
    {
        // (s a1 l Q)
        public readonly object alternativeValue;
        PCond2SA1LQ (PrimitiveCombination2SA1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2SA1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1LQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                answer = this.alternativeValue;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SA1Q : PCond2SA1
    {
        // (s a1 q s)
        public readonly object consequentValue;

        protected PCond2SA1Q (PrimitiveCombination2SA1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SA1QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SA1QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "Pcond2SSQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                noteCalls (this.alternative);
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

    class PCond2SA1QL : PCond2SA1Q
    {
        // (s a1 q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2SA1QL (PrimitiveCombination2SA1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SA1QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SA1QL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SA1QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1QL.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    class PCond2SA1QA : PCond2SA1QL
    {
        // (s a1 q a)
        protected PCond2SA1QA (PrimitiveCombination2SA1 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SA1QA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SA1QA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SA1QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1QA.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ? 
                environment.ArgumentValue (alternativeOffset) : 
                this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SA1QA0 : PCond2SA1QA
    {
        // (s a1 q a0)
        PCond2SA1QA0 (PrimitiveCombination2SA1 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2SA1QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1QA0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ? environment.Argument0Value : this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SA1QA1 : PCond2SA1QA
    {
        // (s a1 q a1)
        PCond2SA1QA1 (PrimitiveCombination2SA1 predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2SA1QA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1QA1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ? environment.Argument1Value : this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SA1QL1 : PCond2SA1QL
    {
        // (s a1 q l1)
        PCond2SA1QL1 (PrimitiveCombination2SA1 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SA1QL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1QL1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    sealed class PCond2SA1QQ : PCond2SA1Q
    {
        // (s a1 q q)
        public readonly object alternativeValue;

        PCond2SA1QQ (PrimitiveCombination2SA1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2SA1QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1QQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, ev0, environment.Argument1Value)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
                    if (tci != null) {
                        answer = null; // dispose of the evidence
                        // set up the interpreter for a tail call
                        Control cExpression = tci.Expression;
                        Environment cEnvironment = tci.Environment;
                        while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                    }
                }

            answer = ((answer is bool) && (bool) answer == false) ? this.alternativeValue : this.consequentValue;
            return false;
        }
    }

    class PCond2SA1SL : PCond2SA1
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2SA1SL (PrimitiveCombination2SA1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2SA1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SA1SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SA1SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SA1SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1SL.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCond2SA1SA : PCond2SA1SL
    {
        protected PCond2SA1SA (PrimitiveCombination2SA1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SA1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SA1SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SA1SA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SA1SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1SA.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SA1SA0 : PCond2SA1SA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2SA1SA0 (PrimitiveCombination2SA1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SA1 predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2SA1SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2SA1SA0.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SA1SA1 : PCond2SA1SA
    {
        PCond2SA1SA1 (PrimitiveCombination2SA1 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SA1 predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2SA1SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1SA1.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                answer = environment.Argument1Value;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SA1SL1 : PCond2SA1SL
    {
        PCond2SA1SL1 (PrimitiveCombination2SA1 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SA1 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2SA1SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1SL1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SA1SQ : PCond2SA1
    {
        // (s a1 s q)
        public readonly object alternativeValue;

        PCond2SA1SQ (PrimitiveCombination2SA1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SA1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2SA1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SA1SQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
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
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCond2SL1 : PCond2SL
    {
        // (s l1 s s)
        protected PCond2SL1 (PrimitiveCombination2SL1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2SL1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2SL1Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SL1SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SL1SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2SL1.EvalStep");
            noteCalls (this.rand0);
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCond2SL1L : PCond2SL1
    {
        // (s l1 l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2SL1L (PrimitiveCombination2SL1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2SL1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2SL1L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SL1LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SL1LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SL1A : PCond2SL1L
    {
        // (s l1 a s)

        protected PCond2SL1A (PrimitiveCombination2SL1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2SL1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2SL1A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SL1AL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SL1AQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
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

    class PCond2SL1AL : PCond2SL1A
    {
        // (s l1 a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SL1AL (PrimitiveCombination2SL1 predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SL1AA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SL1AL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SL1AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1AL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    class PCond2SL1AA : PCond2SL1AL
    {
        // (s l1 a l1)

        protected PCond2SL1AA (PrimitiveCombination2SL1 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SL1AA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SL1AA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SL1AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1AA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = environment.ArgumentValue (((answer is bool) && (bool) answer == false) ?
                this.alternativeOffset :
                this.consequentOffset);
            return false;
        }
    }

    sealed class PCond2SL1AA0 : PCond2SL1AA
    {
        // (s l1 a a0)

        PCond2SL1AA0 (PrimitiveCombination2SL1 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2SL1AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1AA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument0Value :
                environment.ArgumentValue (this.consequentOffset);
            return false;
        }
    }

    sealed class PCond2SL1AA1 : PCond2SL1AA
    {
        // (s l1 a a1)

        PCond2SL1AA1 (PrimitiveCombination2SL1 predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2SL1AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1AA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument1Value :
                environment.ArgumentValue (this.consequentOffset);
	    return false;
        }
    }

    sealed class PCond2SL1AL1 : PCond2SL1AL
    {
        // (s l1 a l1)

        PCond2SL1AL1 (PrimitiveCombination2SL1 predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SL1AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1AL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    sealed class PCond2SL1AQ : PCond2SL1A
    {
        // (s l1 a Q)
        public readonly object alternativeValue;
        PCond2SL1AQ (PrimitiveCombination2SL1 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2SL1AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1AQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.ArgumentValue (this.consequentOffset);
            return false;
        }
    }

    class PCond2SL1A0 : PCond2SL1A
    {
        // (s l1 a0 s)

        protected PCond2SL1A0 (PrimitiveCombination2SL1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SL1A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SL1A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
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

    class PCond2SL1A0L : PCond2SL1A0
    {
        // (s l1 a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SL1A0L (PrimitiveCombination2SL1 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SL1A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SL1A0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SL1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1A0L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    class PCond2SL1A0A : PCond2SL1A0L
    {
        // (s l1 a0 l1)

        protected PCond2SL1A0A (PrimitiveCombination2SL1 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SL1A0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SL1A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SL1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1A0A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.ArgumentValue(this.alternativeOffset) :
                environment.Argument0Value;
            return false;
        }
    }

    sealed class PCond2SL1A0A0 : PCond2SL1A0A
    {
        // (s l1 a0 a0)

        PCond2SL1A0A0 (PrimitiveCombination2SL1 predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2SL1A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1A0A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = environment.Argument0Value;
            return false;
        }
    }

    sealed class PCond2SL1A0A1 : PCond2SL1A0A
    {
        // (s l1 a0 a1)

        PCond2SL1A0A1 (PrimitiveCombination2SL1 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2SL1A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1A0A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument1Value :
                environment.Argument0Value;
	    return false;
        }
    }

    sealed class PCond2SL1A0L1 : PCond2SL1A0L
    {
        // (s l1 a0 l1)

        PCond2SL1A0L1 (PrimitiveCombination2SL1 predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SL1A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1A0L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    sealed class PCond2SL1A0Q : PCond2SL1A0
    {
        // (s l1 a0 Q)
        public readonly object alternativeValue;
        PCond2SL1A0Q (PrimitiveCombination2SL1 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2SL1A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1A0Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.Argument0Value;
            return false;
        }
    }

    class PCond2SL1A1 : PCond2SL1A
    {
        // (s l1 a1 s)

        protected PCond2SL1A1 (PrimitiveCombination2SL1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SL1A1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SL1A1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
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

    class PCond2SL1A1L : PCond2SL1A1
    {
        // (s l1 a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SL1A1L (PrimitiveCombination2SL1 predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SL1A1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SL1A1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SL1A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1A1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    class PCond2SL1A1A : PCond2SL1A1L
    {
        // (s l1 a1 l1)

        protected PCond2SL1A1A (PrimitiveCombination2SL1 predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SL1A1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SL1A1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SL1A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1A1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.ArgumentValue(this.alternativeOffset) :
                environment.Argument1Value;
            return false;
        }
    }

    sealed class PCond2SL1A1A0 : PCond2SL1A1A
    {
        // (s l1 a1 a0)

        PCond2SL1A1A0 (PrimitiveCombination2SL1 predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2SL1A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1A1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument0Value :
                environment.Argument1Value;
            return false;
        }
    }

    sealed class PCond2SL1A1A1 : PCond2SL1A1A
    {
        // (s l1 a1 a1)

        PCond2SL1A1A1 (PrimitiveCombination2SL1 predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2SL1A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1A1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = environment.Argument1Value;
	    return false;
        }
    }

    sealed class PCond2SL1A1L1 : PCond2SL1A1L
    {
        // (s l1 a1 l1)

        PCond2SL1A1L1 (PrimitiveCombination2SL1 predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SL1A1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1A1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    sealed class PCond2SL1A1Q : PCond2SL1A1
    {
        // (s l1 a1 Q)
        public readonly object alternativeValue;
        PCond2SL1A1Q (PrimitiveCombination2SL1 predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2SL1A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1A1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.Argument1Value;
            return false;
        }
    }

    class PCond2SL1L1 : PCond2SL1L
    {
        // (s l1 l1 s)

        protected PCond2SL1L1 (PrimitiveCombination2SL1 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SL1L1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SL1L1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SL1L1L : PCond2SL1L1
    {
        // (s l1 l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SL1L1L (PrimitiveCombination2SL1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SL1L1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SL1L1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SL1L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1L1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SL1L1A : PCond2SL1L1L
    {
        // (s l1 l1 l1)

        protected PCond2SL1L1A (PrimitiveCombination2SL1 predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SL1L1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SL1L1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SL1L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1L1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SL1L1A0 : PCond2SL1L1A
    {
        // (s l1 l1 a0)

        PCond2SL1L1A0 (PrimitiveCombination2SL1 predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2SL1L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1L1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SL1L1A1 : PCond2SL1L1A
    {
        // (s l1 l1 a1)

        PCond2SL1L1A1 (PrimitiveCombination2SL1 predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2SL1L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1L1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SL1L1L1 : PCond2SL1L1L
    {
        // (s l1 l1 l1)

        PCond2SL1L1L1 (PrimitiveCombination2SL1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SL1L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1L1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SL1L1Q : PCond2SL1L1
    {
        // (s l1 l1 Q)
        public readonly object alternativeValue;
        PCond2SL1L1Q (PrimitiveCombination2SL1 predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2SL1L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1L1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SL1LL : PCond2SL1L
    {
        // (s l1 l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SL1LL (PrimitiveCombination2SL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SL1LA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SL1LL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SL1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1LL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SL1LA : PCond2SL1LL
    {
        // (s l1 l l1)

        protected PCond2SL1LA (PrimitiveCombination2SL1 predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SL1LA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SL1LA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SL1LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1LA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SL1LA0 : PCond2SL1LA
    {
        // (s l1 l a0)

        PCond2SL1LA0 (PrimitiveCombination2SL1 predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2SL1LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1LA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SL1LA1 : PCond2SL1LA
    {
        // (s l1 l a1)

        PCond2SL1LA1 (PrimitiveCombination2SL1 predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2SL1LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1LA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SL1LL1 : PCond2SL1LL
    {
        // (s l1 l l1)

        PCond2SL1LL1 (PrimitiveCombination2SL1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SL1LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1LL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SL1LQ : PCond2SL1L
    {
        // (s l1 l Q)
        public readonly object alternativeValue;
        PCond2SL1LQ (PrimitiveCombination2SL1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2SL1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1LQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SL1Q : PCond2SL1
    {
        // (s l1 q s)
        public readonly object consequentValue;

        protected PCond2SL1Q (PrimitiveCombination2SL1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SL1QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SL1QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "Pcond2SSQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
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

    class PCond2SL1QL : PCond2SL1Q
    {
        // (s l1 q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2SL1QL (PrimitiveCombination2SL1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SL1QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SL1QL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SL1QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1QL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    class PCond2SL1QA : PCond2SL1QL
    {
        // (s l1 q a)
        protected PCond2SL1QA (PrimitiveCombination2SL1 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SL1QA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SL1QA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SL1QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1QA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ? 
                environment.ArgumentValue (alternativeOffset) : 
                this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SL1QA0 : PCond2SL1QA
    {
        // (s l1 q a0)
        PCond2SL1QA0 (PrimitiveCombination2SL1 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2SL1QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1QA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ? environment.Argument0Value : this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SL1QA1 : PCond2SL1QA
    {
        // (s l1 q a1)
        PCond2SL1QA1 (PrimitiveCombination2SL1 predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2SL1QA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1QA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ? environment.Argument1Value : this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SL1QL1 : PCond2SL1QL
    {
        // (s l1 q l1)
        PCond2SL1QL1 (PrimitiveCombination2SL1 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SL1QL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1QL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    sealed class PCond2SL1QQ : PCond2SL1Q
    {
        // (s l1 q q)
        public readonly object alternativeValue;

        PCond2SL1QQ (PrimitiveCombination2SL1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2SL1QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1QQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ? this.alternativeValue : this.consequentValue;
            return false;
        }
    }

    class PCond2SL1SL : PCond2SL1
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2SL1SL (PrimitiveCombination2SL1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2SL1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SL1SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SL1SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SL1SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1SL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCond2SL1SA : PCond2SL1SL
    {
        protected PCond2SL1SA (PrimitiveCombination2SL1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SL1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SL1SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SL1SA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SL1SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1SA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SL1SA0 : PCond2SL1SA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2SL1SA0 (PrimitiveCombination2SL1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SL1 predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2SL1SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2SL1SA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SL1SA1 : PCond2SL1SA
    {
        PCond2SL1SA1 (PrimitiveCombination2SL1 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SL1 predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2SL1SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1SA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SL1SL1 : PCond2SL1SL
    {
        PCond2SL1SL1 (PrimitiveCombination2SL1 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SL1 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2SL1SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1SL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SL1SQ : PCond2SL1
    {
        // (s l1 s q)
        public readonly object alternativeValue;

        PCond2SL1SQ (PrimitiveCombination2SL1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SL1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2SL1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL1SQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCond2SQ : PCond2
    {
        // (s q s s)
        public readonly object rand1Value;

        protected PCond2SQ (PrimitiveCombination2SQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2SQL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2SQQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SQSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SQSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("PCond2SQ.EvalStep");
            noteCalls (this.rand0);
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
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

                     if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                    noteCalls (this.alternative);
#endif
                    expression = this.alternative;
                    return true;
            }
            else {
#if DEBUG
                    noteCalls (this.consequent);
#endif
                    expression = this.consequent;
                    return true;
            }
        }
    }

    class PCond2SQL : PCond2SQ
    {
        // (s q l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2SQL (PrimitiveCombination2SQ predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2SQA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2SQL1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SQLL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SQLQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQL.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SQA : PCond2SQL
    {
        // (s q a s)

        protected PCond2SQA (PrimitiveCombination2SQ predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2SQA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2SQA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2SQAL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SQAQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQA.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
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

    class PCond2SQAL : PCond2SQA
    {
        // (s q a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SQAL (PrimitiveCombination2SQ predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SQAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SQAL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SQAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQAL.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    class PCond2SQAA : PCond2SQAL
    {
        // (s q a l1)

        protected PCond2SQAA (PrimitiveCombination2SQ predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SQAA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SQAA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SQAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQAA.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = environment.ArgumentValue (((answer is bool) && (bool) answer == false) ?
                this.alternativeOffset :
                this.consequentOffset);
            return false;
        }
    }

    sealed class PCond2SQAA0 : PCond2SQAA
    {
        // (s q a a0)

        PCond2SQAA0 (PrimitiveCombination2SQ predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2SQAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQAA0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument0Value :
                environment.ArgumentValue (this.consequentOffset);
            return false;
        }
    }

    sealed class PCond2SQAA1 : PCond2SQAA
    {
        // (s q a a1)

        PCond2SQAA1 (PrimitiveCombination2SQ predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2SQAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQAA1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument1Value :
                environment.ArgumentValue (this.consequentOffset);
	    return false;
        }
    }

    sealed class PCond2SQAL1 : PCond2SQAL
    {
        // (s q a l1)

        PCond2SQAL1 (PrimitiveCombination2SQ predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SQAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQAL1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    sealed class PCond2SQAQ : PCond2SQA
    {
        // (s q a Q)
        public readonly object alternativeValue;
        PCond2SQAQ (PrimitiveCombination2SQ predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2SQAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQAQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.ArgumentValue (this.consequentOffset);
            return false;
        }
    }

    class PCond2SQA0 : PCond2SQA
    {
        // (s q a0 s)

        protected PCond2SQA0 (PrimitiveCombination2SQ predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SQA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SQA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQA0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
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

    class PCond2SQA0L : PCond2SQA0
    {
        // (s q a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SQA0L (PrimitiveCombination2SQ predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SQA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SQA0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SQA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQA0L.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    class PCond2SQA0A : PCond2SQA0L
    {
        // (s q a0 l1)

        protected PCond2SQA0A (PrimitiveCombination2SQ predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SQA0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SQA0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SQA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQA0A.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.ArgumentValue(this.alternativeOffset) :
                environment.Argument0Value;
            return false;
        }
    }

    sealed class PCond2SQA0A0 : PCond2SQA0A
    {
        // (s q a0 a0)

        PCond2SQA0A0 (PrimitiveCombination2SQ predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2SQA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQA0A0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = environment.Argument0Value;
            return false;
        }
    }

    sealed class PCond2SQA0A1 : PCond2SQA0A
    {
        // (s q a0 a1)

        PCond2SQA0A1 (PrimitiveCombination2SQ predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2SQA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQA0A1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument1Value :
                environment.Argument0Value;
	    return false;
        }
    }

    sealed class PCond2SQA0L1 : PCond2SQA0L
    {
        // (s q a0 l1)

        PCond2SQA0L1 (PrimitiveCombination2SQ predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SQA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQA0L1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    sealed class PCond2SQA0Q : PCond2SQA0
    {
        // (s q a0 Q)
        public readonly object alternativeValue;
        PCond2SQA0Q (PrimitiveCombination2SQ predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2SQA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQA0Q.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.Argument0Value;
            return false;
        }
    }

    class PCond2SQA1 : PCond2SQA
    {
        // (s q a1 s)

        protected PCond2SQA1 (PrimitiveCombination2SQ predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SQA1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SQA1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQA1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
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

    class PCond2SQA1L : PCond2SQA1
    {
        // (s q a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SQA1L (PrimitiveCombination2SQ predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SQA1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SQA1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SQA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQA1L.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    class PCond2SQA1A : PCond2SQA1L
    {
        // (s q a1 l1)

        protected PCond2SQA1A (PrimitiveCombination2SQ predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SQA1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SQA1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SQA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQA1A.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.ArgumentValue(this.alternativeOffset) :
                environment.Argument1Value;
            return false;
        }
    }

    sealed class PCond2SQA1A0 : PCond2SQA1A
    {
        // (s q a1 a0)

        PCond2SQA1A0 (PrimitiveCombination2SQ predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2SQA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQA1A0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                environment.Argument0Value :
                environment.Argument1Value;
            return false;
        }
    }

    sealed class PCond2SQA1A1 : PCond2SQA1A
    {
        // (s q a1 a1)

        PCond2SQA1A1 (PrimitiveCombination2SQ predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2SQA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQA1A1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = environment.Argument1Value;
	    return false;
        }
    }

    sealed class PCond2SQA1L1 : PCond2SQA1L
    {
        // (s q a1 l1)

        PCond2SQA1L1 (PrimitiveCombination2SQ predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SQA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQA1L1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    sealed class PCond2SQA1Q : PCond2SQA1
    {
        // (s q a1 Q)
        public readonly object alternativeValue;
        PCond2SQA1Q (PrimitiveCombination2SQ predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2SQA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQA1Q.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ?
                this.alternativeValue :
                environment.Argument1Value;
            return false;
        }
    }

    class PCond2SQL1 : PCond2SQL
    {
        // (s q l1 s)

        protected PCond2SQL1 (PrimitiveCombination2SQ predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SQL1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SQL1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQL1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SQL1L : PCond2SQL1
    {
        // (s q l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SQL1L (PrimitiveCombination2SQ predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SQL1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SQL1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SQL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQL1L.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SQL1A : PCond2SQL1L
    {
        // (s q l1 l1)

        protected PCond2SQL1A (PrimitiveCombination2SQ predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SQL1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SQL1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SQL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQL1A.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SQL1A0 : PCond2SQL1A
    {
        // (s q l1 a0)

        PCond2SQL1A0 (PrimitiveCombination2SQ predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2SQL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQL1A0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SQL1A1 : PCond2SQL1A
    {
        // (s q l1 a1)

        PCond2SQL1A1 (PrimitiveCombination2SQ predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2SQL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQL1A1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SQL1L1 : PCond2SQL1L
    {
        // (s q l1 l1)

        PCond2SQL1L1 (PrimitiveCombination2SQ predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SQL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQL1L1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SQL1Q : PCond2SQL1
    {
        // (s q l1 Q)
        public readonly object alternativeValue;
        PCond2SQL1Q (PrimitiveCombination2SQ predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2SQL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQL1Q.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SQLL : PCond2SQL
    {
        // (s q l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2SQLL (PrimitiveCombination2SQ predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SQLA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SQLL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SQLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQLL.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCond2SQLA : PCond2SQLL
    {
        // (s q l l1)

        protected PCond2SQLA (PrimitiveCombination2SQ predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SQLA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SQLA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SQLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQLA.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SQLA0 : PCond2SQLA
    {
        // (s q l a0)

        PCond2SQLA0 (PrimitiveCombination2SQ predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2SQLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQLA0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SQLA1 : PCond2SQLA
    {
        // (s q l a1)

        PCond2SQLA1 (PrimitiveCombination2SQ predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2SQLA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQLA1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SQLL1 : PCond2SQLL
    {
        // (s q l l1)

        PCond2SQLL1 (PrimitiveCombination2SQ predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SQLL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQLL1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2SQLQ : PCond2SQL
    {
        // (s q l Q)
        public readonly object alternativeValue;
        PCond2SQLQ (PrimitiveCombination2SQ predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2SQLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQLQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCond2SQQ : PCond2SQ
    {
        // (s q q s)
        public readonly object consequentValue;

        protected PCond2SQQ (PrimitiveCombination2SQ predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2SQQL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2SQQQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2SQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "Pcond2SSQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
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

    class PCond2SQQL : PCond2SQQ
    {
        // (s q q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2SQQL (PrimitiveCombination2SQ predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SQQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SQQL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SQQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQQL.EvalStep";
#endif
#if DEBUG
            noteCalls (this.rand0);
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    class PCond2SQQA : PCond2SQQL
    {
        // (s q q a)
        protected PCond2SQQA (PrimitiveCombination2SQ predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SQQA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SQQA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SQQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQQA.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ? 
                environment.ArgumentValue (alternativeOffset) : 
                this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SQQA0 : PCond2SQQA
    {
        // (s q q a0)
        PCond2SQQA0 (PrimitiveCombination2SQ predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2SQQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQQA0.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ? environment.Argument0Value : this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SQQA1 : PCond2SQQA
    {
        // (s q q a1)
        PCond2SQQA1 (PrimitiveCombination2SQ predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2SQQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQQA1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ? environment.Argument1Value : this.consequentValue;
            return false;
        }
    }

    sealed class PCond2SQQL1 : PCond2SQQL
    {
        // (s q q l1)
        PCond2SQQL1 (PrimitiveCombination2SQ predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2SQQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQQL1.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    sealed class PCond2SQQQ : PCond2SQQ
    {
        // (s q q q)
        public readonly object alternativeValue;

        PCond2SQQQ (PrimitiveCombination2SQ predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2SQQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQQQ.EvalStep";
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            answer = ((answer is bool) && (bool) answer == false) ? this.alternativeValue : this.consequentValue;
            return false;
        }
    }

    class PCond2SQSL : PCond2SQ
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2SQSL (PrimitiveCombination2SQ predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2SQ predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SQSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2SQSL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2SQSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQSL.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCond2SQSA : PCond2SQSL
    {
        protected PCond2SQSA (PrimitiveCombination2SQ predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SQ predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2SQSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2SQSA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2SQSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQSA.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue(this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SQSA0 : PCond2SQSA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2SQSA0 (PrimitiveCombination2SQ predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SQ predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2SQSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCond2SQSA0.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SQSA1 : PCond2SQSA
    {
        PCond2SQSA1 (PrimitiveCombination2SQ predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SQ predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2SQSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQSA1.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2SQSL1 : PCond2SQSL
    {
        PCond2SQSL1 (PrimitiveCombination2SQ predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2SQ predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2SQSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQSL1.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
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

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    sealed class PCond2SQSQ : PCond2SQ
    {
        public readonly object alternativeValue;

        PCond2SQSQ (PrimitiveCombination2SQ predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        static public SCode Make (PrimitiveCombination2SQ predicate, SCode consequent, Quotation alternative)
        {
            return new PCond2SQSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQSQ.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, environment, this.rand1Value));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString();
#endif
            object predValue;

            if (this.method (out predValue, ev0, this.rand1Value)) {
                TailCallInterpreter tci = predValue as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
                    expression = tci.Expression;
                    environment = tci.Environment;
                    return true;
                }
                else
                    throw new NotImplementedException ();
            }

            if (predValue is bool && (bool) predValue == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }


}
