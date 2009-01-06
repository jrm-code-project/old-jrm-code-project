using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    class PCond2L1 : PCond2L
    {
        // (l1 s s s)
        protected PCond2L1 (PrimitiveCombination2L1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveCombination2L1L) ? PCond2L1L.Make ((PrimitiveCombination2L1L) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2L1Q) ? PCond2L1Q.Make ((PrimitiveCombination2L1Q) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCond2L1SL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2L1SQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1SSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1SSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2L1.EvalStep");
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();

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

    class PCond2L1SL : PCond2L1
    {
        // (l1 s l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2L1SL (PrimitiveCombination2L1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2L1SA.Make (predicate, (Argument) consequent, alternative)
                : (consequent is LexicalVariable1) ? PCond2L1SL1.Make (predicate, (LexicalVariable1) consequent, alternative)
                : (alternative is LexicalVariable) ? PCond2L1SLL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2L1SLQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2L1SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1SLL : PCond2L1SL
    {
        // (l1 s l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1SLL (PrimitiveCombination2L1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1SLA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2L1SLL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2L1SLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1SLA : PCond2L1SLL
    {
        // (l1 s l a)
        protected PCond2L1SLA (PrimitiveCombination2L1 predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1SLA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2L1SLA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2L1SLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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
                answer = environment.ArgumentValue (alternativeOffset);
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2L1SLA0 : PCond2L1SLA
    {
        // (l1 s l a0)
        PCond2L1SLA0 (PrimitiveCombination2L1 predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2L1SLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1SLA1 : PCond2L1SLA
    {
        // (l1 s l a1)
        PCond2L1SLA1 (PrimitiveCombination2L1 predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2L1SLA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1SLL1 : PCond2L1SLL
    {
        // (l1 s l l1)
        PCond2L1SLL1 (PrimitiveCombination2L1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1SLL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1SLQ : PCond2L1SL
    {
        // (l1 s l q)
        public readonly object alternativeValue;

        PCond2L1SLQ (PrimitiveCombination2L1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2L1SLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1SA : PCond2L1SL
    {
        // (l1 s a s)
        protected PCond2L1SA (PrimitiveCombination2L1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2L1SA0.Make (predicate, (Argument0) consequent, alternative)
                : (consequent is Argument1) ? PCond2L1SA1.Make (predicate, (Argument1) consequent, alternative)
                : (alternative is LexicalVariable) ? PCond2L1SAL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2L1SAQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2L1SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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
                answer = environment.ArgumentValue (consequentOffset);
                return false;
            }
        }
    }

    class PCond2L1SAL : PCond2L1SA
    {
        // (l1 s a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1SAL (PrimitiveCombination2L1 predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1SAA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2L1SAL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2L1SAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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
                answer = environment.ArgumentValue (consequentOffset);
                return false;
            }
        }
    }

    class PCond2L1SAA : PCond2L1SAL
    {
        // (l1 s a a)
        protected PCond2L1SAA (PrimitiveCombination2L1 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1SAA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2L1SAA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2L1SAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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
                answer = environment.ArgumentValue (alternativeOffset);
                return false;
            }
            else {
                answer = environment.ArgumentValue (consequentOffset);
                return false;
            }
        }
    }

    sealed class PCond2L1SAA0 : PCond2L1SAA
    {
        // (l1 s a a0)
        PCond2L1SAA0 (PrimitiveCombination2L1 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2L1SAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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
                answer = environment.ArgumentValue (consequentOffset);
                return false;
            }
        }
    }

    sealed class PCond2L1SAA1 : PCond2L1SAA
    {
        // (l1 s a a1)
        PCond2L1SAA1 (PrimitiveCombination2L1 predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2L1SAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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
                answer = environment.ArgumentValue (consequentOffset);
                return false;
            }
        }
    }

    sealed class PCond2L1SAL1 : PCond2L1SAL
    {
        // (l1 s a l1)
        PCond2L1SAL1 (PrimitiveCombination2L1 predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1SAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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
                answer = environment.ArgumentValue (consequentOffset);
                return false;
            }
        }
    }

    sealed class PCond2L1SAQ : PCond2L1SA
    {
        // (l1 s a q)
        public readonly object alternativeValue;

        PCond2L1SAQ (PrimitiveCombination2L1 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2L1SAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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
                answer = environment.ArgumentValue (consequentOffset);
                return false;
            }
        }
    }

    class PCond2L1SA0 : PCond2L1SA
    {
        // (l1 s a0 s)
        protected PCond2L1SA0 (PrimitiveCombination2L1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1SA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1SA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2L1SA0.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
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

    class PCond2L1SA0L : PCond2L1SA0
    {
        // (l1 s a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1SA0L (PrimitiveCombination2L1 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1SA0A.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2L1SA0L1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2L1SA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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

    class PCond2L1SA0A : PCond2L1SA0L
    {
        // (l1 s a0 a)
        protected PCond2L1SA0A (PrimitiveCombination2L1 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1SA0A0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2L1SA0A1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2L1SA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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
                answer = environment.ArgumentValue (alternativeOffset);
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    sealed class PCond2L1SA0A0 : PCond2L1SA0A
    {
        // (l1 s a0 a0)
        PCond2L1SA0A0 (PrimitiveCombination2L1 predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2L1SA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    sealed class PCond2L1SA0A1 : PCond2L1SA0A
    {
        // (l1 s a0 a1)
        PCond2L1SA0A1 (PrimitiveCombination2L1 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2L1SA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    sealed class PCond2L1SA0L1 : PCond2L1SA0L
    {
        // (l1 s a0 l1)
        PCond2L1SA0L1 (PrimitiveCombination2L1 predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1SA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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

    sealed class PCond2L1SA0Q : PCond2L1SA0
    {
        // (l1 s a0 q)
        public readonly object alternativeValue;

        PCond2L1SA0Q (PrimitiveCombination2L1 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2L1SA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2L1SA0Q.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
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

            answer = ((answer is bool) && (bool) answer == false) ? this.alternativeValue : environment.Argument0Value;
            return false;
        }
    }

    class PCond2L1SA1 : PCond2L1SA
    {
        // (l1 s a1 s)
        protected PCond2L1SA1 (PrimitiveCombination2L1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1SA1L.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2L1SA1Q.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2L1SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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

    class PCond2L1SA1L : PCond2L1SA1
    {
        // (l1 s a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1SA1L (PrimitiveCombination2L1 predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1SA1A.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2L1SA1L1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2L1SA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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

    class PCond2L1SA1A : PCond2L1SA1L
    {
        // (l1 s a1 a)
        protected PCond2L1SA1A (PrimitiveCombination2L1 predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1SA1A0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2L1SA1A1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2L1SA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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
                answer = environment.ArgumentValue (alternativeOffset);
                return false;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    sealed class PCond2L1SA1A0 : PCond2L1SA1A
    {
        // (l1 s a1 a0)
        PCond2L1SA1A0 (PrimitiveCombination2L1 predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2L1SA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    sealed class PCond2L1SA1A1 : PCond2L1SA1A
    {
        // (l1 s a1 a1)
        PCond2L1SA1A1 (PrimitiveCombination2L1 predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2L1SA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    sealed class PCond2L1SA1L1 : PCond2L1SA1L
    {
        // (l1 s a1 l1)
        PCond2L1SA1L1 (PrimitiveCombination2L1 predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1SA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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

    sealed class PCond2L1SA1Q : PCond2L1SA1
    {
        // (l1 s a1 q)
        public readonly object alternativeValue;

        PCond2L1SA1Q (PrimitiveCombination2L1 predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2L1SA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    class PCond2L1SL1 : PCond2L1SL
    {
        // (l1 s l1 s)
        protected PCond2L1SL1 (PrimitiveCombination2L1 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1SL1L.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2L1SL1Q.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2L1SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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

    class PCond2L1SL1L : PCond2L1SL1
    {
        // (l1 s l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1SL1L (PrimitiveCombination2L1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1SL1A.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2L1SL1L1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2L1SL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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

    class PCond2L1SL1A : PCond2L1SL1L
    {
        // (l1 s l1 a)
        protected PCond2L1SL1A (PrimitiveCombination2L1 predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1SL1A0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2L1SL1A1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2L1SL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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
                answer = environment.ArgumentValue (alternativeOffset);
                return false;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCond2L1SL1A0 : PCond2L1SL1A
    {
        // (l1 s l1 a0)
        PCond2L1SL1A0 (PrimitiveCombination2L1 predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2L1SL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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

    sealed class PCond2L1SL1A1 : PCond2L1SL1A
    {
        // (l1 s l1 a1)
        PCond2L1SL1A1 (PrimitiveCombination2L1 predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2L1SL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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

    sealed class PCond2L1SL1L1 : PCond2L1SL1L
    {
        // (l1 s l1 l1)
        PCond2L1SL1L1 (PrimitiveCombination2L1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1SL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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

    sealed class PCond2L1SL1Q : PCond2L1SL1
    {
        // (l1 s l1 q)
        public readonly object alternativeValue;

        PCond2L1SL1Q (PrimitiveCombination2L1 predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2L1SL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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

    class PCond2L1SQ : PCond2L1
    {
        // (l1 s q s)
        public readonly object consequentValue;

        protected PCond2L1SQ (PrimitiveCombination2L1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1SQL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2L1SQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2L1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2L1SQ.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
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

    class PCond2L1SQL : PCond2L1SQ
    {
        // (l1 s q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1SQL (PrimitiveCombination2L1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1SQA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2L1SQL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2L1SQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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

    class PCond2L1SQA : PCond2L1SQL
    {
        // (l1 s q a)
        protected PCond2L1SQA (PrimitiveCombination2L1 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1SQA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2L1SQA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2L1SQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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
                answer = environment.ArgumentValue (alternativeOffset);
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    sealed class PCond2L1SQA0 : PCond2L1SQA
    {
        // (l1 s q a0)
        PCond2L1SQA0 (PrimitiveCombination2L1 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2L1SQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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
                answer = this.consequentValue;
                return false;
            }
        }
    }

    sealed class PCond2L1SQA1 : PCond2L1SQA
    {
        // (l1 s q a1)
        PCond2L1SQA1 (PrimitiveCombination2L1 predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2L1SQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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
                answer = this.consequentValue;
                return false;
            }
        }
    }

    sealed class PCond2L1SQL1 : PCond2L1SQL
    {
        // (l1 s q l1)
        PCond2L1SQL1 (PrimitiveCombination2L1 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1SQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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

    sealed class PCond2L1SQQ : PCond2L1SQ
    {
        // (l1 s q q)
        public readonly object alternativeValue;

        PCond2L1SQQ (PrimitiveCombination2L1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2L1SQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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
                answer = this.consequentValue;
                return false;
            }
        }
    }

    class PCond2L1SSL : PCond2L1
    {
        // (l1 s s l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1SSL (PrimitiveCombination2L1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1SSA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2L1SSL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2L1SSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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

    class PCond2L1SSA : PCond2L1SSL
    {
        // (l1 s s a)
        protected PCond2L1SSA (PrimitiveCombination2L1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1SSA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2L1SSA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2L1SSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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
                answer = environment.ArgumentValue (alternativeOffset);
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

    sealed class PCond2L1SSA0 : PCond2L1SSA
    {
        // (l1 s s a0)
        PCond2L1SSA0 (PrimitiveCombination2L1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond2L1SSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2L1SSA0.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
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
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCond2L1SSA1 : PCond2L1SSA
    {
        // (l1 s s a1)
        PCond2L1SSA1 (PrimitiveCombination2L1 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCond2L1SSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
#if DEBUG
            noteCalls (this.rand1);
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

                        object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();



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

    sealed class PCond2L1SSL1 : PCond2L1SSL
    {
        // (l1 s s l1)
        PCond2L1SSL1 (PrimitiveCombination2L1 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1SSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2L1SSL1.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
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

    sealed class PCond2L1SSQ : PCond2L1
    {
        // (l1 s s q)
        public readonly object alternativeValue;

        PCond2L1SSQ (PrimitiveCombination2L1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2L1SSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2L1SSQ.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
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

    class PCond2L1L : PCond2L1
    {
        // (l1 l s s)
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCond2L1L (PrimitiveCombination2L1L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveCombination2L1A) ? PCond2L1A.Make ((PrimitiveCombination2L1A) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2L1L1) ? PCond2L1L1.Make ((PrimitiveCombination2L1L1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCond2L1LL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2L1LQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1LSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1LSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2L1L.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();

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

    class PCond2L1LL : PCond2L1L
    {
        // (l1 l l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2L1LL (PrimitiveCombination2L1L predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2L1LA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2L1LL1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1LLL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1LLQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LA : PCond2L1LL
    {
        // (l1 l a s)

        protected PCond2L1LA (PrimitiveCombination2L1L predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2L1LA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2L1LA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1LAL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1LAQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LAL : PCond2L1LA
    {
        // (l1 l a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1LAL (PrimitiveCombination2L1L predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1LAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1LAL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1LAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LAL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LAA : PCond2L1LAL
    {
        // (l1 l a l1)

        protected PCond2L1LAA (PrimitiveCombination2L1L predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1LAA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1LAA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1LAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LAA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LAA0 : PCond2L1LAA
    {
        // (l1 l a a0)

        PCond2L1LAA0 (PrimitiveCombination2L1L predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2L1LAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LAA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LAA1 : PCond2L1LAA
    {
        // (l1 l a a1)

        PCond2L1LAA1 (PrimitiveCombination2L1L predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2L1LAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LAA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LAL1 : PCond2L1LAL
    {
        // (l1 l a l1)

        PCond2L1LAL1 (PrimitiveCombination2L1L predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1LAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LAL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LAQ : PCond2L1LA
    {
        // (l1 l a Q)
        public readonly object alternativeValue;
        PCond2L1LAQ (PrimitiveCombination2L1L predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2L1LAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LAQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LA0 : PCond2L1LA
    {
        // (l1 l a0 s)

        protected PCond2L1LA0 (PrimitiveCombination2L1L predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1LA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1LA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LA0L : PCond2L1LA0
    {
        // (l1 l a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1LA0L (PrimitiveCombination2L1L predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1LA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1LA0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1LA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LA0L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LA0A : PCond2L1LA0L
    {
        // (l1 l a0 l1)

        protected PCond2L1LA0A (PrimitiveCombination2L1L predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1LA0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1LA0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1LA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LA0A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LA0A0 : PCond2L1LA0A
    {
        // (l1 l a0 a0)

        PCond2L1LA0A0 (PrimitiveCombination2L1L predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2L1LA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LA0A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LA0A1 : PCond2L1LA0A
    {
        // (l1 l a0 a1)

        PCond2L1LA0A1 (PrimitiveCombination2L1L predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2L1LA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LA0A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LA0L1 : PCond2L1LA0L
    {
        // (l1 l a0 l1)

        PCond2L1LA0L1 (PrimitiveCombination2L1L predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1LA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LA0L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LA0Q : PCond2L1LA0
    {
        // (l1 l a0 Q)
        public readonly object alternativeValue;
        PCond2L1LA0Q (PrimitiveCombination2L1L predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2L1LA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LA0Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LA1 : PCond2L1LA
    {
        // (l1 l a1 s)

        protected PCond2L1LA1 (PrimitiveCombination2L1L predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1LA1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1LA1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LA1L : PCond2L1LA1
    {
        // (l1 l a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1LA1L (PrimitiveCombination2L1L predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1LA1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1LA1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1LA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LA1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LA1A : PCond2L1LA1L
    {
        // (l1 l a1 l1)

        protected PCond2L1LA1A (PrimitiveCombination2L1L predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1LA1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1LA1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1LA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LA1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LA1A0 : PCond2L1LA1A
    {
        // (l1 l a1 a0)

        PCond2L1LA1A0 (PrimitiveCombination2L1L predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2L1LA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LA1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LA1A1 : PCond2L1LA1A
    {
        // (l1 l a1 a1)

        PCond2L1LA1A1 (PrimitiveCombination2L1L predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2L1LA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LA1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LA1L1 : PCond2L1LA1L
    {
        // (l1 l a1 l1)

        PCond2L1LA1L1 (PrimitiveCombination2L1L predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1LA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LA1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LA1Q : PCond2L1LA1
    {
        // (l1 l a1 Q)
        public readonly object alternativeValue;
        PCond2L1LA1Q (PrimitiveCombination2L1L predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2L1LA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LA1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LL1 : PCond2L1LL
    {
        // (l1 l l1 s)

        protected PCond2L1LL1 (PrimitiveCombination2L1L predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1LL1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1LL1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LL1L : PCond2L1LL1
    {
        // (l1 l l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1LL1L (PrimitiveCombination2L1L predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1LL1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1LL1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1LL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LL1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LL1A : PCond2L1LL1L
    {
        // (l1 l l1 l1)

        protected PCond2L1LL1A (PrimitiveCombination2L1L predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1LL1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1LL1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1LL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LL1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LL1A0 : PCond2L1LL1A
    {
        // (l1 l l1 a0)

        PCond2L1LL1A0 (PrimitiveCombination2L1L predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2L1LL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LL1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LL1A1 : PCond2L1LL1A
    {
        // (l1 l l1 a1)

        PCond2L1LL1A1 (PrimitiveCombination2L1L predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2L1LL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LL1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LL1L1 : PCond2L1LL1L
    {
        // (l1 l l1 l1)

        PCond2L1LL1L1 (PrimitiveCombination2L1L predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1LL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LL1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LL1Q : PCond2L1LL1
    {
        // (l1 l l1 Q)
        public readonly object alternativeValue;
        PCond2L1LL1Q (PrimitiveCombination2L1L predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2L1LL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LL1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LLL : PCond2L1LL
    {
        // (l1 l l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1LLL (PrimitiveCombination2L1L predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1LLA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1LLL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1LLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LLL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LLA : PCond2L1LLL
    {
        // (l1 l l l1)

        protected PCond2L1LLA (PrimitiveCombination2L1L predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1LLA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1LLA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1LLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LLA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LLA0 : PCond2L1LLA
    {
        // (l1 l l a0)

        PCond2L1LLA0 (PrimitiveCombination2L1L predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2L1LLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LLA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LLA1 : PCond2L1LLA
    {
        // (l1 l l a1)

        PCond2L1LLA1 (PrimitiveCombination2L1L predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2L1LLA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LLA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LLL1 : PCond2L1LLL
    {
        // (l1 l l l1)

        PCond2L1LLL1 (PrimitiveCombination2L1L predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1LLL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LLL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LLQ : PCond2L1LL
    {
        // (l1 l l Q)
        public readonly object alternativeValue;
        PCond2L1LLQ (PrimitiveCombination2L1L predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2L1LLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LLQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LQ : PCond2L1L
    {
        // (l1 l q s)
        public readonly object consequentValue;

        protected PCond2L1LQ (PrimitiveCombination2L1L predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1LQL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1LQQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LQL : PCond2L1LQ
    {
        // (l1 l q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1LQL (PrimitiveCombination2L1L predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1LQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1LQL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1LQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LQL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LQA : PCond2L1LQL
    {
        // (l1 l q a)
        protected PCond2L1LQA (PrimitiveCombination2L1L predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1LQA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1LQA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1LQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LQA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LQA0 : PCond2L1LQA
    {
        // (l1 l q a0)
        PCond2L1LQA0 (PrimitiveCombination2L1L predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2L1LQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LQA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LQA1 : PCond2L1LQA
    {
        // (l1 l q a1)
        PCond2L1LQA1 (PrimitiveCombination2L1L predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2L1LQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LQA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LQL1 : PCond2L1LQL
    {
        // (l1 l q l1)
        PCond2L1LQL1 (PrimitiveCombination2L1L predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1LQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LQL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LQQ : PCond2L1LQ
    {
        // (l1 l q q)
        public readonly object alternativeValue;

        PCond2L1LQQ (PrimitiveCombination2L1L predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2L1LQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2L1LQQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LSL : PCond2L1L
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1LSL (PrimitiveCombination2L1L predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2L1L predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1LSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1LSL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1LSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LSL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1LSA : PCond2L1LSL
    {
        protected PCond2L1LSA (PrimitiveCombination2L1L predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1L predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1LSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1LSA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1LSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LSA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LSA0 : PCond2L1LSA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2L1LSA0 (PrimitiveCombination2L1L predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1L predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2L1LSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2L1LSA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LSA1 : PCond2L1LSA
    {
        PCond2L1LSA1 (PrimitiveCombination2L1L predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1L predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2L1LSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LSA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    sealed class PCond2L1LSL1 : PCond2L1LSL
    {
        PCond2L1LSL1 (PrimitiveCombination2L1L predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1L predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2L1LSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LSL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();

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

    sealed class PCond2L1LSQ : PCond2L1L
    {
        // (l1 l s q)
        public readonly object alternativeValue;

        PCond2L1LSQ (PrimitiveCombination2L1L predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1L predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2L1LSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1LSQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();


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

    class PCond2L1A : PCond2L1L
    {
        // (s a s s)
        protected PCond2L1A (PrimitiveCombination2L1A predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveCombination2L1A0) ? PCond2L1A0.Make ((PrimitiveCombination2L1A0) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2L1A1) ? PCond2L1A1.Make ((PrimitiveCombination2L1A1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCond2L1AL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2L1AQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1ASL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1ASQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG

            Warm ("PCond2L1A.EvalStep");
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();

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

    class PCond2L1AL : PCond2L1A
    {
        // (s a l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2L1AL (PrimitiveCombination2L1A predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2L1AA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2L1AL1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1ALL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1ALQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();

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

    class PCond2L1AA : PCond2L1AL
    {
        // (s a a s)

        protected PCond2L1AA (PrimitiveCombination2L1A predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2L1AA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2L1AA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1AAL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1AAQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1AAL : PCond2L1AA
    {
        // (s a a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1AAL (PrimitiveCombination2L1A predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1AAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1AAL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1AAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AAL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1AAA : PCond2L1AAL
    {
        // (s a a l1)

        protected PCond2L1AAA (PrimitiveCombination2L1A predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1AAA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1AAA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1AAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AAA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AAA0 : PCond2L1AAA
    {
        // (s a a a0)

        PCond2L1AAA0 (PrimitiveCombination2L1A predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2L1AAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AAA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AAA1 : PCond2L1AAA
    {
        // (s a a a1)

        PCond2L1AAA1 (PrimitiveCombination2L1A predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2L1AAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AAA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AAL1 : PCond2L1AAL
    {
        // (s a a l1)

        PCond2L1AAL1 (PrimitiveCombination2L1A predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1AAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AAL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AAQ : PCond2L1AA
    {
        // (s a a Q)
        public readonly object alternativeValue;
        PCond2L1AAQ (PrimitiveCombination2L1A predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2L1AAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AAQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1AA0 : PCond2L1AA
    {
        // (s a a0 s)

        protected PCond2L1AA0 (PrimitiveCombination2L1A predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1AA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1AA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1AA0L : PCond2L1AA0
    {
        // (s a a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1AA0L (PrimitiveCombination2L1A predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1AA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1AA0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1AA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AA0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1AA0A : PCond2L1AA0L
    {
        // (s a a0 l1)

        protected PCond2L1AA0A (PrimitiveCombination2L1A predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1AA0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1AA0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1AA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AA0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AA0A0 : PCond2L1AA0A
    {
        // (s a a0 a0)

        PCond2L1AA0A0 (PrimitiveCombination2L1A predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2L1AA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AA0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AA0A1 : PCond2L1AA0A
    {
        // (s a a0 a1)

        PCond2L1AA0A1 (PrimitiveCombination2L1A predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2L1AA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AA0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AA0L1 : PCond2L1AA0L
    {
        // (s a a0 l1)

        PCond2L1AA0L1 (PrimitiveCombination2L1A predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1AA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AA0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AA0Q : PCond2L1AA0
    {
        // (s a a0 Q)
        public readonly object alternativeValue;
        PCond2L1AA0Q (PrimitiveCombination2L1A predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2L1AA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AA0Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1AA1 : PCond2L1AA
    {
        // (s a a1 s)

        protected PCond2L1AA1 (PrimitiveCombination2L1A predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1AA1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1AA1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1AA1L : PCond2L1AA1
    {
        // (s a a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1AA1L (PrimitiveCombination2L1A predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1AA1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1AA1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1AA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AA1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1AA1A : PCond2L1AA1L
    {
        // (s a a1 l1)

        protected PCond2L1AA1A (PrimitiveCombination2L1A predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1AA1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1AA1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1AA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AA1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AA1A0 : PCond2L1AA1A
    {
        // (s a a1 a0)

        PCond2L1AA1A0 (PrimitiveCombination2L1A predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2L1AA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AA1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AA1A1 : PCond2L1AA1A
    {
        // (s a a1 a1)

        PCond2L1AA1A1 (PrimitiveCombination2L1A predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2L1AA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AA1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AA1L1 : PCond2L1AA1L
    {
        // (s a a1 l1)

        PCond2L1AA1L1 (PrimitiveCombination2L1A predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1AA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AA1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AA1Q : PCond2L1AA1
    {
        // (s a a1 Q)
        public readonly object alternativeValue;
        PCond2L1AA1Q (PrimitiveCombination2L1A predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2L1AA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AA1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1AL1 : PCond2L1AL
    {
        // (s a l1 s)

        protected PCond2L1AL1 (PrimitiveCombination2L1A predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1AL1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1AL1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1AL1L : PCond2L1AL1
    {
        // (s a l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1AL1L (PrimitiveCombination2L1A predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1AL1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1AL1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1AL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AL1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1AL1A : PCond2L1AL1L
    {
        // (s a l1 l1)

        protected PCond2L1AL1A (PrimitiveCombination2L1A predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1AL1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1AL1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1AL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AL1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AL1A0 : PCond2L1AL1A
    {
        // (s a l1 a0)

        PCond2L1AL1A0 (PrimitiveCombination2L1A predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2L1AL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AL1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AL1A1 : PCond2L1AL1A
    {
        // (s a l1 a1)

        PCond2L1AL1A1 (PrimitiveCombination2L1A predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2L1AL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AL1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AL1L1 : PCond2L1AL1L
    {
        // (s a l1 l1)

        PCond2L1AL1L1 (PrimitiveCombination2L1A predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1AL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AL1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AL1Q : PCond2L1AL1
    {
        // (s a l1 Q)
        public readonly object alternativeValue;
        PCond2L1AL1Q (PrimitiveCombination2L1A predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2L1AL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AL1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1ALL : PCond2L1AL
    {
        // (s a l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1ALL (PrimitiveCombination2L1A predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1ALA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1ALL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1ALL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1ALL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1ALA : PCond2L1ALL
    {
        // (s a l l1)

        protected PCond2L1ALA (PrimitiveCombination2L1A predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1ALA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1ALA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1ALA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1ALA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1ALA0 : PCond2L1ALA
    {
        // (s a l a0)

        PCond2L1ALA0 (PrimitiveCombination2L1A predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2L1ALA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1ALA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1ALA1 : PCond2L1ALA
    {
        // (s a l a1)

        PCond2L1ALA1 (PrimitiveCombination2L1A predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2L1ALA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1ALA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1ALL1 : PCond2L1ALL
    {
        // (s a l l1)

        PCond2L1ALL1 (PrimitiveCombination2L1A predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1ALL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1ALL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1ALQ : PCond2L1AL
    {
        // (s a l Q)
        public readonly object alternativeValue;
        PCond2L1ALQ (PrimitiveCombination2L1A predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2L1ALQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1ALQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1AQ : PCond2L1A
    {
        // (s a q s)
        public readonly object consequentValue;

        protected PCond2L1AQ (PrimitiveCombination2L1A predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1AQL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1AQQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1AQL : PCond2L1AQ
    {
        // (s a q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1AQL (PrimitiveCombination2L1A predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1AQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1AQL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1AQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AQL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1AQA : PCond2L1AQL
    {
        // (s a q a)
        protected PCond2L1AQA (PrimitiveCombination2L1A predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1AQA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1AQA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1AQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AQA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AQA0 : PCond2L1AQA
    {
        // (s a q a0)
        PCond2L1AQA0 (PrimitiveCombination2L1A predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2L1AQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AQA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AQA1 : PCond2L1AQA
    {
        // (s a q a1)
        PCond2L1AQA1 (PrimitiveCombination2L1A predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2L1AQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AQA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AQL1 : PCond2L1AQL
    {
        // (s a q l1)
        PCond2L1AQL1 (PrimitiveCombination2L1A predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1AQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1AQL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1AQQ : PCond2L1AQ
    {
        // (s a q q)
        public readonly object alternativeValue;

        PCond2L1AQQ (PrimitiveCombination2L1A predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2L1AQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2L1AQQ.EvalStep";
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1ASL : PCond2L1A
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1ASL (PrimitiveCombination2L1A predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2L1A predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1ASA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1ASL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1ASL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1ASL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1ASA : PCond2L1ASL
    {
        protected PCond2L1ASA (PrimitiveCombination2L1A predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1A predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1ASA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1ASA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1ASA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1ASA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1ASA0 : PCond2L1ASA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2L1ASA0 (PrimitiveCombination2L1A predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1A predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2L1ASA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2L1ASA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1ASA1 : PCond2L1ASA
    {
        PCond2L1ASA1 (PrimitiveCombination2L1A predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1A predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2L1ASA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1ASA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1ASL1 : PCond2L1ASL
    {
        PCond2L1ASL1 (PrimitiveCombination2L1A predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1A predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2L1ASL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1ASL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1ASQ : PCond2L1A
    {
        // (s a s q)
        public readonly object alternativeValue;

        PCond2L1ASQ (PrimitiveCombination2L1A predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2L1ASQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1ASQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0 : PCond2L1A
    {
        // (s a0 s s)
        protected PCond2L1A0 (PrimitiveCombination2L1A0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2L1A0L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2L1A0Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1A0SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1A0SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2L1A0.EvalStep");
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0L : PCond2L1A0
    {
        // (s a0 l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2L1A0L (PrimitiveCombination2L1A0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2L1A0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2L1A0L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1A0LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1A0LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0A : PCond2L1A0L
    {
        // (s a0 a s)

        protected PCond2L1A0A (PrimitiveCombination2L1A0 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2L1A0A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2L1A0A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1A0AL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1A0AQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0AL : PCond2L1A0A
    {
        // (s a0 a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1A0AL (PrimitiveCombination2L1A0 predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1A0AA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1A0AL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1A0AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0AL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0AA : PCond2L1A0AL
    {
        // (s a0 a l1)

        protected PCond2L1A0AA (PrimitiveCombination2L1A0 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1A0AA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1A0AA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1A0AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0AA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0AA0 : PCond2L1A0AA
    {
        // (s a0 a a0)

        PCond2L1A0AA0 (PrimitiveCombination2L1A0 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2L1A0AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0AA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0AA1 : PCond2L1A0AA
    {
        // (s a0 a a1)

        PCond2L1A0AA1 (PrimitiveCombination2L1A0 predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2L1A0AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0AA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0AL1 : PCond2L1A0AL
    {
        // (s a0 a l1)

        PCond2L1A0AL1 (PrimitiveCombination2L1A0 predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1A0AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0AL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0AQ : PCond2L1A0A
    {
        // (s a0 a Q)
        public readonly object alternativeValue;
        PCond2L1A0AQ (PrimitiveCombination2L1A0 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2L1A0AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0AQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0A0 : PCond2L1A0A
    {
        // (s a0 a0 s)

        protected PCond2L1A0A0 (PrimitiveCombination2L1A0 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1A0A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1A0A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0A0L : PCond2L1A0A0
    {
        // (s a0 a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1A0A0L (PrimitiveCombination2L1A0 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1A0A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1A0A0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1A0A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0A0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0A0A : PCond2L1A0A0L
    {
        // (s a0 a0 l1)

        protected PCond2L1A0A0A (PrimitiveCombination2L1A0 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1A0A0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1A0A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1A0A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0A0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0A0A0 : PCond2L1A0A0A
    {
        // (s a0 a0 a0)

        PCond2L1A0A0A0 (PrimitiveCombination2L1A0 predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2L1A0A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0A0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0A0A1 : PCond2L1A0A0A
    {
        // (s a0 a0 a1)

        PCond2L1A0A0A1 (PrimitiveCombination2L1A0 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2L1A0A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0A0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0A0L1 : PCond2L1A0A0L
    {
        // (s a0 a0 l1)

        PCond2L1A0A0L1 (PrimitiveCombination2L1A0 predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1A0A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0A0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0A0Q : PCond2L1A0A0
    {
        // (s a0 a0 Q)
        public readonly object alternativeValue;
        PCond2L1A0A0Q (PrimitiveCombination2L1A0 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2L1A0A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0A0Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0A1 : PCond2L1A0A
    {
        // (s a0 a1 s)

        protected PCond2L1A0A1 (PrimitiveCombination2L1A0 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1A0A1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1A0A1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0A1L : PCond2L1A0A1
    {
        // (s a0 a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1A0A1L (PrimitiveCombination2L1A0 predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1A0A1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1A0A1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1A0A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0A1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0A1A : PCond2L1A0A1L
    {
        // (s a0 a1 l1)

        protected PCond2L1A0A1A (PrimitiveCombination2L1A0 predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1A0A1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1A0A1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1A0A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0A1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0A1A0 : PCond2L1A0A1A
    {
        // (s a0 a1 a0)

        PCond2L1A0A1A0 (PrimitiveCombination2L1A0 predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2L1A0A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0A1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0A1A1 : PCond2L1A0A1A
    {
        // (s a0 a1 a1)

        PCond2L1A0A1A1 (PrimitiveCombination2L1A0 predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2L1A0A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0A1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0A1L1 : PCond2L1A0A1L
    {
        // (s a0 a1 l1)

        PCond2L1A0A1L1 (PrimitiveCombination2L1A0 predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1A0A1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0A1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0A1Q : PCond2L1A0A1
    {
        // (s a0 a1 Q)
        public readonly object alternativeValue;
        PCond2L1A0A1Q (PrimitiveCombination2L1A0 predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2L1A0A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0A1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0L1 : PCond2L1A0L
    {
        // (s a0 l1 s)

        protected PCond2L1A0L1 (PrimitiveCombination2L1A0 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1A0L1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1A0L1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0L1L : PCond2L1A0L1
    {
        // (s a0 l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1A0L1L (PrimitiveCombination2L1A0 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1A0L1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1A0L1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1A0L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0L1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0L1A : PCond2L1A0L1L
    {
        // (s a0 l1 l1)

        protected PCond2L1A0L1A (PrimitiveCombination2L1A0 predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1A0L1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1A0L1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1A0L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0L1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0L1A0 : PCond2L1A0L1A
    {
        // (s a0 l1 a0)

        PCond2L1A0L1A0 (PrimitiveCombination2L1A0 predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2L1A0L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0L1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0L1A1 : PCond2L1A0L1A
    {
        // (s a0 l1 a1)

        PCond2L1A0L1A1 (PrimitiveCombination2L1A0 predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2L1A0L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0L1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0L1L1 : PCond2L1A0L1L
    {
        // (s a0 l1 l1)

        PCond2L1A0L1L1 (PrimitiveCombination2L1A0 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1A0L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0L1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0L1Q : PCond2L1A0L1
    {
        // (s a0 l1 Q)
        public readonly object alternativeValue;
        PCond2L1A0L1Q (PrimitiveCombination2L1A0 predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2L1A0L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0L1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0LL : PCond2L1A0L
    {
        // (s a0 l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1A0LL (PrimitiveCombination2L1A0 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1A0LA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1A0LL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1A0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0LL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0LA : PCond2L1A0LL
    {
        // (s a0 l l1)

        protected PCond2L1A0LA (PrimitiveCombination2L1A0 predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1A0LA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1A0LA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1A0LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0LA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0LA0 : PCond2L1A0LA
    {
        // (s a0 l a0)

        PCond2L1A0LA0 (PrimitiveCombination2L1A0 predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2L1A0LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0LA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0LA1 : PCond2L1A0LA
    {
        // (s a0 l a1)

        PCond2L1A0LA1 (PrimitiveCombination2L1A0 predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2L1A0LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0LA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0LL1 : PCond2L1A0LL
    {
        // (s a0 l l1)

        PCond2L1A0LL1 (PrimitiveCombination2L1A0 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1A0LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0LL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0LQ : PCond2L1A0L
    {
        // (s a0 l Q)
        public readonly object alternativeValue;
        PCond2L1A0LQ (PrimitiveCombination2L1A0 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2L1A0LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0LQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0Q : PCond2L1A0
    {
        // (s a0 q s)
        public readonly object consequentValue;

        protected PCond2L1A0Q (PrimitiveCombination2L1A0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1A0QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1A0QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0QL : PCond2L1A0Q
    {
        // (s a0 q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1A0QL (PrimitiveCombination2L1A0 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1A0QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1A0QL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1A0QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0QL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0QA : PCond2L1A0QL
    {
        // (s a0 q a)
        protected PCond2L1A0QA (PrimitiveCombination2L1A0 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1A0QA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1A0QA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1A0QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0QA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0QA0 : PCond2L1A0QA
    {
        // (s a0 q a0)
        PCond2L1A0QA0 (PrimitiveCombination2L1A0 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2L1A0QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0QA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0QA1 : PCond2L1A0QA
    {
        // (s a0 q a1)
        PCond2L1A0QA1 (PrimitiveCombination2L1A0 predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2L1A0QA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0QA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0QL1 : PCond2L1A0QL
    {
        // (s a0 q l1)
        PCond2L1A0QL1 (PrimitiveCombination2L1A0 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1A0QL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0QL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0QQ : PCond2L1A0Q
    {
        // (s a0 q q)
        public readonly object alternativeValue;

        PCond2L1A0QQ (PrimitiveCombination2L1A0 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2L1A0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2L1A0QQ.EvalStep";
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0SL : PCond2L1A0
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1A0SL (PrimitiveCombination2L1A0 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2L1A0 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1A0SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1A0SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1A0SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0SL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A0SA : PCond2L1A0SL
    {
        protected PCond2L1A0SA (PrimitiveCombination2L1A0 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1A0 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1A0SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1A0SA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1A0SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0SA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0SA0 : PCond2L1A0SA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2L1A0SA0 (PrimitiveCombination2L1A0 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1A0 predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2L1A0SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2L1A0SA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0SA1 : PCond2L1A0SA
    {
        PCond2L1A0SA1 (PrimitiveCombination2L1A0 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1A0 predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2L1A0SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0SA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0SL1 : PCond2L1A0SL
    {
        PCond2L1A0SL1 (PrimitiveCombination2L1A0 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1A0 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2L1A0SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0SL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A0SQ : PCond2L1A0
    {
        // (s a0 s q)
        public readonly object alternativeValue;

        PCond2L1A0SQ (PrimitiveCombination2L1A0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A0 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2L1A0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A0SQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1 : PCond2L1A
    {
        // (s a1 s s)
        protected PCond2L1A1 (PrimitiveCombination2L1A1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2L1A1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2L1A1Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1A1SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1A1SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2L1A1.EvalStep");
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1L : PCond2L1A1
    {
        // (s a1 l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2L1A1L (PrimitiveCombination2L1A1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2L1A1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2L1A1L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1A1LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1A1LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1A : PCond2L1A1L
    {
        // (s a1 a s)

        protected PCond2L1A1A (PrimitiveCombination2L1A1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2L1A1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2L1A1A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1A1AL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1A1AQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1AL : PCond2L1A1A
    {
        // (s a1 a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1A1AL (PrimitiveCombination2L1A1 predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1A1AA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1A1AL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1A1AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1AL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1AA : PCond2L1A1AL
    {
        // (s a1 a l1)

        protected PCond2L1A1AA (PrimitiveCombination2L1A1 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1A1AA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1A1AA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1A1AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1AA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1AA0 : PCond2L1A1AA
    {
        // (s a1 a a0)

        PCond2L1A1AA0 (PrimitiveCombination2L1A1 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2L1A1AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1AA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1AA1 : PCond2L1A1AA
    {
        // (s a1 a a1)

        PCond2L1A1AA1 (PrimitiveCombination2L1A1 predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2L1A1AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1AA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1AL1 : PCond2L1A1AL
    {
        // (s a1 a l1)

        PCond2L1A1AL1 (PrimitiveCombination2L1A1 predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1A1AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1AL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1AQ : PCond2L1A1A
    {
        // (s a1 a Q)
        public readonly object alternativeValue;
        PCond2L1A1AQ (PrimitiveCombination2L1A1 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2L1A1AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1AQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1A0 : PCond2L1A1A
    {
        // (s a1 a0 s)

        protected PCond2L1A1A0 (PrimitiveCombination2L1A1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1A1A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1A1A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1A0L : PCond2L1A1A0
    {
        // (s a1 a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1A1A0L (PrimitiveCombination2L1A1 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1A1A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1A1A0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1A1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1A0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1A0A : PCond2L1A1A0L
    {
        // (s a1 a0 l1)

        protected PCond2L1A1A0A (PrimitiveCombination2L1A1 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1A1A0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1A1A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1A1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1A0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1A0A0 : PCond2L1A1A0A
    {
        // (s a1 a0 a0)

        PCond2L1A1A0A0 (PrimitiveCombination2L1A1 predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2L1A1A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1A0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1A0A1 : PCond2L1A1A0A
    {
        // (s a1 a0 a1)

        PCond2L1A1A0A1 (PrimitiveCombination2L1A1 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2L1A1A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1A0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1A0L1 : PCond2L1A1A0L
    {
        // (s a1 a0 l1)

        PCond2L1A1A0L1 (PrimitiveCombination2L1A1 predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1A1A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1A0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1A0Q : PCond2L1A1A0
    {
        // (s a1 a0 Q)
        public readonly object alternativeValue;
        PCond2L1A1A0Q (PrimitiveCombination2L1A1 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2L1A1A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1A0Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1A1 : PCond2L1A1A
    {
        // (s a1 a1 s)

        protected PCond2L1A1A1 (PrimitiveCombination2L1A1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1A1A1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1A1A1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1A1L : PCond2L1A1A1
    {
        // (s a1 a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1A1A1L (PrimitiveCombination2L1A1 predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1A1A1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1A1A1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1A1A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1A1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1A1A : PCond2L1A1A1L
    {
        // (s a1 a1 l1)

        protected PCond2L1A1A1A (PrimitiveCombination2L1A1 predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1A1A1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1A1A1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1A1A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1A1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1A1A0 : PCond2L1A1A1A
    {
        // (s a1 a1 a0)

        PCond2L1A1A1A0 (PrimitiveCombination2L1A1 predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2L1A1A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1A1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1A1A1 : PCond2L1A1A1A
    {
        // (s a1 a1 a1)

        PCond2L1A1A1A1 (PrimitiveCombination2L1A1 predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2L1A1A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1A1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1A1L1 : PCond2L1A1A1L
    {
        // (s a1 a1 l1)

        PCond2L1A1A1L1 (PrimitiveCombination2L1A1 predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1A1A1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1A1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1A1Q : PCond2L1A1A1
    {
        // (s a1 a1 Q)
        public readonly object alternativeValue;
        PCond2L1A1A1Q (PrimitiveCombination2L1A1 predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2L1A1A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1A1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1L1 : PCond2L1A1L
    {
        // (s a1 l1 s)

        protected PCond2L1A1L1 (PrimitiveCombination2L1A1 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1A1L1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1A1L1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1A1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1L1L : PCond2L1A1L1
    {
        // (s a1 l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1A1L1L (PrimitiveCombination2L1A1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1A1L1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1A1L1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1A1L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1L1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1L1A : PCond2L1A1L1L
    {
        // (s a1 l1 l1)

        protected PCond2L1A1L1A (PrimitiveCombination2L1A1 predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1A1L1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1A1L1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1A1L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1L1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1L1A0 : PCond2L1A1L1A
    {
        // (s a1 l1 a0)

        PCond2L1A1L1A0 (PrimitiveCombination2L1A1 predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2L1A1L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1L1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1L1A1 : PCond2L1A1L1A
    {
        // (s a1 l1 a1)

        PCond2L1A1L1A1 (PrimitiveCombination2L1A1 predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2L1A1L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1L1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1L1L1 : PCond2L1A1L1L
    {
        // (s a1 l1 l1)

        PCond2L1A1L1L1 (PrimitiveCombination2L1A1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1A1L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1L1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1L1Q : PCond2L1A1L1
    {
        // (s a1 l1 Q)
        public readonly object alternativeValue;
        PCond2L1A1L1Q (PrimitiveCombination2L1A1 predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2L1A1L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1L1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1LL : PCond2L1A1L
    {
        // (s a1 l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1A1LL (PrimitiveCombination2L1A1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1A1LA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1A1LL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1A1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1LL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1LA : PCond2L1A1LL
    {
        // (s a1 l l1)

        protected PCond2L1A1LA (PrimitiveCombination2L1A1 predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1A1LA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1A1LA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1A1LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1LA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1LA0 : PCond2L1A1LA
    {
        // (s a1 l a0)

        PCond2L1A1LA0 (PrimitiveCombination2L1A1 predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2L1A1LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1LA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1LA1 : PCond2L1A1LA
    {
        // (s a1 l a1)

        PCond2L1A1LA1 (PrimitiveCombination2L1A1 predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2L1A1LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1LA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1LL1 : PCond2L1A1LL
    {
        // (s a1 l l1)

        PCond2L1A1LL1 (PrimitiveCombination2L1A1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1A1LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1LL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1LQ : PCond2L1A1L
    {
        // (s a1 l Q)
        public readonly object alternativeValue;
        PCond2L1A1LQ (PrimitiveCombination2L1A1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2L1A1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1LQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1Q : PCond2L1A1
    {
        // (s a1 q s)
        public readonly object consequentValue;

        protected PCond2L1A1Q (PrimitiveCombination2L1A1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1A1QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1A1QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1QL : PCond2L1A1Q
    {
        // (s a1 q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1A1QL (PrimitiveCombination2L1A1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1A1QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1A1QL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1A1QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1QL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1QA : PCond2L1A1QL
    {
        // (s a1 q a)
        protected PCond2L1A1QA (PrimitiveCombination2L1A1 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1A1QA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1A1QA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1A1QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1QA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1QA0 : PCond2L1A1QA
    {
        // (s a1 q a0)
        PCond2L1A1QA0 (PrimitiveCombination2L1A1 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2L1A1QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1QA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1QA1 : PCond2L1A1QA
    {
        // (s a1 q a1)
        PCond2L1A1QA1 (PrimitiveCombination2L1A1 predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2L1A1QA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1QA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1QL1 : PCond2L1A1QL
    {
        // (s a1 q l1)
        PCond2L1A1QL1 (PrimitiveCombination2L1A1 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1A1QL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1QL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1QQ : PCond2L1A1Q
    {
        // (s a1 q q)
        public readonly object alternativeValue;

        PCond2L1A1QQ (PrimitiveCombination2L1A1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2L1A1QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2L1A1QQ.EvalStep";
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1SL : PCond2L1A1
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1A1SL (PrimitiveCombination2L1A1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2L1A1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1A1SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1A1SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1A1SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1SL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1A1SA : PCond2L1A1SL
    {
        protected PCond2L1A1SA (PrimitiveCombination2L1A1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1A1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1A1SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1A1SA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1A1SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1SA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1SA0 : PCond2L1A1SA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2L1A1SA0 (PrimitiveCombination2L1A1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1A1 predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2L1A1SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2L1A1SA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1SA1 : PCond2L1A1SA
    {
        PCond2L1A1SA1 (PrimitiveCombination2L1A1 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1A1 predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2L1A1SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1SA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1SL1 : PCond2L1A1SL
    {
        PCond2L1A1SL1 (PrimitiveCombination2L1A1 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1A1 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2L1A1SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1SL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1A1SQ : PCond2L1A1
    {
        // (s a1 s q)
        public readonly object alternativeValue;

        PCond2L1A1SQ (PrimitiveCombination2L1A1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1A1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2L1A1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1A1SQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1L1 : PCond2L1L
    {
        // (l1 l1 s s)
        protected PCond2L1L1 (PrimitiveCombination2L1L1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2L1L1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2L1L1Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1L1SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1L1SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2L1L1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1L : PCond2L1L1
    {
        // (l1 l1 l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2L1L1L (PrimitiveCombination2L1L1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2L1L1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2L1L1L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1L1LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1L1LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1A : PCond2L1L1L
    {
        // (l1 l1 a s)

        protected PCond2L1L1A (PrimitiveCombination2L1L1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2L1L1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2L1L1A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1L1AL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1L1AQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1AL : PCond2L1L1A
    {
        // (l1 l1 a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1L1AL (PrimitiveCombination2L1L1 predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1L1AA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1L1AL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1L1AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1AL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1AA : PCond2L1L1AL
    {
        // (l1 l1 a l1)

        protected PCond2L1L1AA (PrimitiveCombination2L1L1 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1L1AA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1L1AA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1L1AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1AA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1AA0 : PCond2L1L1AA
    {
        // (l1 l1 a a0)

        PCond2L1L1AA0 (PrimitiveCombination2L1L1 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2L1L1AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1AA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1AA1 : PCond2L1L1AA
    {
        // (l1 l1 a a1)

        PCond2L1L1AA1 (PrimitiveCombination2L1L1 predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2L1L1AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1AA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1AL1 : PCond2L1L1AL
    {
        // (l1 l1 a l1)

        PCond2L1L1AL1 (PrimitiveCombination2L1L1 predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1L1AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1AL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1AQ : PCond2L1L1A
    {
        // (l1 l1 a Q)
        public readonly object alternativeValue;
        PCond2L1L1AQ (PrimitiveCombination2L1L1 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2L1L1AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1AQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1A0 : PCond2L1L1A
    {
        // (l1 l1 a0 s)

        protected PCond2L1L1A0 (PrimitiveCombination2L1L1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1L1A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1L1A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1A0L : PCond2L1L1A0
    {
        // (l1 l1 a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1L1A0L (PrimitiveCombination2L1L1 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1L1A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1L1A0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1L1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1A0L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1A0A : PCond2L1L1A0L
    {
        // (l1 l1 a0 l1)

        protected PCond2L1L1A0A (PrimitiveCombination2L1L1 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1L1A0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1L1A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1L1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1A0A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1A0A0 : PCond2L1L1A0A
    {
        // (l1 l1 a0 a0)

        PCond2L1L1A0A0 (PrimitiveCombination2L1L1 predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2L1L1A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1A0A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1A0A1 : PCond2L1L1A0A
    {
        // (l1 l1 a0 a1)

        PCond2L1L1A0A1 (PrimitiveCombination2L1L1 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2L1L1A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1A0A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1A0L1 : PCond2L1L1A0L
    {
        // (l1 l1 a0 l1)

        PCond2L1L1A0L1 (PrimitiveCombination2L1L1 predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1L1A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1A0L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1A0Q : PCond2L1L1A0
    {
        // (l1 l1 a0 Q)
        public readonly object alternativeValue;
        PCond2L1L1A0Q (PrimitiveCombination2L1L1 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2L1L1A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1A0Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1A1 : PCond2L1L1A
    {
        // (l1 l1 a1 s)

        protected PCond2L1L1A1 (PrimitiveCombination2L1L1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1L1A1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1L1A1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1A1L : PCond2L1L1A1
    {
        // (l1 l1 a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1L1A1L (PrimitiveCombination2L1L1 predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1L1A1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1L1A1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1L1A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1A1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1A1A : PCond2L1L1A1L
    {
        // (l1 l1 a1 l1)

        protected PCond2L1L1A1A (PrimitiveCombination2L1L1 predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1L1A1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1L1A1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1L1A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1A1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1A1A0 : PCond2L1L1A1A
    {
        // (l1 l1 a1 a0)

        PCond2L1L1A1A0 (PrimitiveCombination2L1L1 predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2L1L1A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1A1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1A1A1 : PCond2L1L1A1A
    {
        // (l1 l1 a1 a1)

        PCond2L1L1A1A1 (PrimitiveCombination2L1L1 predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2L1L1A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1A1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1A1L1 : PCond2L1L1A1L
    {
        // (l1 l1 a1 l1)

        PCond2L1L1A1L1 (PrimitiveCombination2L1L1 predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1L1A1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1A1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1A1Q : PCond2L1L1A1
    {
        // (l1 l1 a1 Q)
        public readonly object alternativeValue;
        PCond2L1L1A1Q (PrimitiveCombination2L1L1 predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2L1L1A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1A1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1L1 : PCond2L1L1L
    {
        // (l1 l1 l1 s)

        protected PCond2L1L1L1 (PrimitiveCombination2L1L1 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1L1L1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1L1L1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1L1L : PCond2L1L1L1
    {
        // (l1 l1 l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1L1L1L (PrimitiveCombination2L1L1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1L1L1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1L1L1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1L1L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1L1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1L1A : PCond2L1L1L1L
    {
        // (l1 l1 l1 l1)

        protected PCond2L1L1L1A (PrimitiveCombination2L1L1 predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1L1L1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1L1L1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1L1L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1L1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1L1A0 : PCond2L1L1L1A
    {
        // (l1 l1 l1 a0)

        PCond2L1L1L1A0 (PrimitiveCombination2L1L1 predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2L1L1L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1L1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1L1A1 : PCond2L1L1L1A
    {
        // (l1 l1 l1 a1)

        PCond2L1L1L1A1 (PrimitiveCombination2L1L1 predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2L1L1L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1L1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1L1L1 : PCond2L1L1L1L
    {
        // (l1 l1 l1 l1)

        PCond2L1L1L1L1 (PrimitiveCombination2L1L1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1L1L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1L1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1L1Q : PCond2L1L1L1
    {
        // (l1 l1 l1 Q)
        public readonly object alternativeValue;
        PCond2L1L1L1Q (PrimitiveCombination2L1L1 predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2L1L1L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1L1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1LL : PCond2L1L1L
    {
        // (l1 l1 l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1L1LL (PrimitiveCombination2L1L1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1L1LA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1L1LL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1L1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1LL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1LA : PCond2L1L1LL
    {
        // (l1 l1 l l1)

        protected PCond2L1L1LA (PrimitiveCombination2L1L1 predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1L1LA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1L1LA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1L1LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1LA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1LA0 : PCond2L1L1LA
    {
        // (l1 l1 l a0)

        PCond2L1L1LA0 (PrimitiveCombination2L1L1 predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2L1L1LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1LA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1LA1 : PCond2L1L1LA
    {
        // (l1 l1 l a1)

        PCond2L1L1LA1 (PrimitiveCombination2L1L1 predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2L1L1LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1LA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1LL1 : PCond2L1L1LL
    {
        // (l1 l1 l l1)

        PCond2L1L1LL1 (PrimitiveCombination2L1L1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1L1LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1LL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1LQ : PCond2L1L1L
    {
        // (l1 l1 l Q)
        public readonly object alternativeValue;
        PCond2L1L1LQ (PrimitiveCombination2L1L1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2L1L1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1LQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1Q : PCond2L1L1
    {
        // (l1 l1 q s)
        public readonly object consequentValue;

        protected PCond2L1L1Q (PrimitiveCombination2L1L1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1L1QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1L1QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1QL : PCond2L1L1Q
    {
        // (l1 l1 q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1L1QL (PrimitiveCombination2L1L1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1L1QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1L1QL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1L1QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1QL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1QA : PCond2L1L1QL
    {
        // (l1 l1 q a)
        protected PCond2L1L1QA (PrimitiveCombination2L1L1 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1L1QA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1L1QA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1L1QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1QA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1QA0 : PCond2L1L1QA
    {
        // (l1 l1 q a0)
        PCond2L1L1QA0 (PrimitiveCombination2L1L1 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2L1L1QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1QA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1QA1 : PCond2L1L1QA
    {
        // (l1 l1 q a1)
        PCond2L1L1QA1 (PrimitiveCombination2L1L1 predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2L1L1QA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1QA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1QL1 : PCond2L1L1QL
    {
        // (l1 l1 q l1)
        PCond2L1L1QL1 (PrimitiveCombination2L1L1 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1L1QL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1QL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1QQ : PCond2L1L1Q
    {
        // (l1 l1 q q)
        public readonly object alternativeValue;

        PCond2L1L1QQ (PrimitiveCombination2L1L1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2L1L1QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2L1L1QQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
                if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1SL : PCond2L1L1
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1L1SL (PrimitiveCombination2L1L1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2L1L1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1L1SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1L1SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1L1SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1SL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1L1SA : PCond2L1L1SL
    {
        protected PCond2L1L1SA (PrimitiveCombination2L1L1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1L1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1L1SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1L1SA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1L1SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1SA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1SA0 : PCond2L1L1SA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2L1L1SA0 (PrimitiveCombination2L1L1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1L1 predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2L1L1SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2L1L1SA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1SA1 : PCond2L1L1SA
    {
        PCond2L1L1SA1 (PrimitiveCombination2L1L1 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1L1 predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2L1L1SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1SA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1SL1 : PCond2L1L1SL
    {
        PCond2L1L1SL1 (PrimitiveCombination2L1L1 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1L1 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2L1L1SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1SL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    sealed class PCond2L1L1SQ : PCond2L1L1
    {
        // (l1 l1 s q)
        public readonly object alternativeValue;

        PCond2L1L1SQ (PrimitiveCombination2L1L1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1L1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2L1L1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1L1SQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            if (this.method (out answer, ev0, ev1)) {
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

    class PCond2L1Q : PCond2L1
    {
        // (s q s s)
        public readonly object rand1Value;

        protected PCond2L1Q (PrimitiveCombination2L1Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2L1QL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2L1QQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1QSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1QSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("PCond2L1Q.EvalStep");
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QL : PCond2L1Q
    {
        // (s q l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2L1QL (PrimitiveCombination2L1Q predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2L1QA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2L1QL1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1QLL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1QLQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QA : PCond2L1QL
    {
        // (s q a s)

        protected PCond2L1QA (PrimitiveCombination2L1Q predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2L1QA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2L1QA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2L1QAL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1QAQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QAL : PCond2L1QA
    {
        // (s q a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1QAL (PrimitiveCombination2L1Q predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1QAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1QAL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1QAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QAL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QAA : PCond2L1QAL
    {
        // (s q a l1)

        protected PCond2L1QAA (PrimitiveCombination2L1Q predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1QAA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1QAA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1QAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QAA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QAA0 : PCond2L1QAA
    {
        // (s q a a0)

        PCond2L1QAA0 (PrimitiveCombination2L1Q predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2L1QAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QAA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QAA1 : PCond2L1QAA
    {
        // (s q a a1)

        PCond2L1QAA1 (PrimitiveCombination2L1Q predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2L1QAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QAA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QAL1 : PCond2L1QAL
    {
        // (s q a l1)

        PCond2L1QAL1 (PrimitiveCombination2L1Q predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1QAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QAL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QAQ : PCond2L1QA
    {
        // (s q a Q)
        public readonly object alternativeValue;
        PCond2L1QAQ (PrimitiveCombination2L1Q predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2L1QAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QAQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QA0 : PCond2L1QA
    {
        // (s q a0 s)

        protected PCond2L1QA0 (PrimitiveCombination2L1Q predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1QA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1QA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QA0L : PCond2L1QA0
    {
        // (s q a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1QA0L (PrimitiveCombination2L1Q predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1QA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1QA0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1QA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QA0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QA0A : PCond2L1QA0L
    {
        // (s q a0 l1)

        protected PCond2L1QA0A (PrimitiveCombination2L1Q predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1QA0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1QA0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1QA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QA0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QA0A0 : PCond2L1QA0A
    {
        // (s q a0 a0)

        PCond2L1QA0A0 (PrimitiveCombination2L1Q predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2L1QA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QA0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QA0A1 : PCond2L1QA0A
    {
        // (s q a0 a1)

        PCond2L1QA0A1 (PrimitiveCombination2L1Q predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2L1QA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QA0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QA0L1 : PCond2L1QA0L
    {
        // (s q a0 l1)

        PCond2L1QA0L1 (PrimitiveCombination2L1Q predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1QA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QA0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QA0Q : PCond2L1QA0
    {
        // (s q a0 Q)
        public readonly object alternativeValue;
        PCond2L1QA0Q (PrimitiveCombination2L1Q predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2L1QA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QA0Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QA1 : PCond2L1QA
    {
        // (s q a1 s)

        protected PCond2L1QA1 (PrimitiveCombination2L1Q predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1QA1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1QA1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1QA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QA1L : PCond2L1QA1
    {
        // (s q a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1QA1L (PrimitiveCombination2L1Q predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1QA1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1QA1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1QA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QA1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QA1A : PCond2L1QA1L
    {
        // (s q a1 l1)

        protected PCond2L1QA1A (PrimitiveCombination2L1Q predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1QA1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1QA1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1QA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QA1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QA1A0 : PCond2L1QA1A
    {
        // (s q a1 a0)

        PCond2L1QA1A0 (PrimitiveCombination2L1Q predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2L1QA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QA1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QA1A1 : PCond2L1QA1A
    {
        // (s q a1 a1)

        PCond2L1QA1A1 (PrimitiveCombination2L1Q predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2L1QA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QA1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QA1L1 : PCond2L1QA1L
    {
        // (s q a1 l1)

        PCond2L1QA1L1 (PrimitiveCombination2L1Q predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1QA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QA1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QA1Q : PCond2L1QA1
    {
        // (s q a1 Q)
        public readonly object alternativeValue;
        PCond2L1QA1Q (PrimitiveCombination2L1Q predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2L1QA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QA1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QL1 : PCond2L1QL
    {
        // (s q l1 s)

        protected PCond2L1QL1 (PrimitiveCombination2L1Q predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1QL1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1QL1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1QL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QL1L : PCond2L1QL1
    {
        // (s q l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1QL1L (PrimitiveCombination2L1Q predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1QL1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1QL1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1QL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QL1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QL1A : PCond2L1QL1L
    {
        // (s q l1 l1)

        protected PCond2L1QL1A (PrimitiveCombination2L1Q predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1QL1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1QL1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1QL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QL1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QL1A0 : PCond2L1QL1A
    {
        // (s q l1 a0)

        PCond2L1QL1A0 (PrimitiveCombination2L1Q predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2L1QL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QL1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QL1A1 : PCond2L1QL1A
    {
        // (s q l1 a1)

        PCond2L1QL1A1 (PrimitiveCombination2L1Q predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2L1QL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QL1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QL1L1 : PCond2L1QL1L
    {
        // (s q l1 l1)

        PCond2L1QL1L1 (PrimitiveCombination2L1Q predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1QL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QL1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QL1Q : PCond2L1QL1
    {
        // (s q l1 Q)
        public readonly object alternativeValue;
        PCond2L1QL1Q (PrimitiveCombination2L1Q predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2L1QL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QL1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QLL : PCond2L1QL
    {
        // (s q l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2L1QLL (PrimitiveCombination2L1Q predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1QLA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1QLL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1QLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QLL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QLA : PCond2L1QLL
    {
        // (s q l l1)

        protected PCond2L1QLA (PrimitiveCombination2L1Q predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1QLA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1QLA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1QLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QLA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QLA0 : PCond2L1QLA
    {
        // (s q l a0)

        PCond2L1QLA0 (PrimitiveCombination2L1Q predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2L1QLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QLA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QLA1 : PCond2L1QLA
    {
        // (s q l a1)

        PCond2L1QLA1 (PrimitiveCombination2L1Q predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2L1QLA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QLA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QLL1 : PCond2L1QLL
    {
        // (s q l l1)

        PCond2L1QLL1 (PrimitiveCombination2L1Q predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1QLL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QLL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QLQ : PCond2L1QL
    {
        // (s q l Q)
        public readonly object alternativeValue;
        PCond2L1QLQ (PrimitiveCombination2L1Q predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2L1QLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QLQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QQ : PCond2L1Q
    {
        // (s q q s)
        public readonly object consequentValue;

        protected PCond2L1QQ (PrimitiveCombination2L1Q predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2L1QQL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2L1QQQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L1QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QQL : PCond2L1QQ
    {
        // (s q q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1QQL (PrimitiveCombination2L1Q predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1QQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1QQL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1QQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QQL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QQA : PCond2L1QQL
    {
        // (s q q a)
        protected PCond2L1QQA (PrimitiveCombination2L1Q predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1QQA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1QQA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1QQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QQA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QQA0 : PCond2L1QQA
    {
        // (s q q a0)
        PCond2L1QQA0 (PrimitiveCombination2L1Q predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2L1QQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QQA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QQA1 : PCond2L1QQA
    {
        // (s q q a1)
        PCond2L1QQA1 (PrimitiveCombination2L1Q predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2L1QQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QQA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QQL1 : PCond2L1QQL
    {
        // (s q q l1)
        PCond2L1QQL1 (PrimitiveCombination2L1Q predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2L1QQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QQL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QQQ : PCond2L1QQ
    {
        // (s q q q)
        public readonly object alternativeValue;

        PCond2L1QQQ (PrimitiveCombination2L1Q predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L1Q predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2L1QQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2L1QQQ.EvalStep";
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QSL : PCond2L1Q
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2L1QSL (PrimitiveCombination2L1Q predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2L1Q predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2L1QSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2L1QSL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2L1QSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QSL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    class PCond2L1QSA : PCond2L1QSL
    {
        protected PCond2L1QSA (PrimitiveCombination2L1Q predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1Q predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2L1QSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2L1QSA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2L1QSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QSA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QSA0 : PCond2L1QSA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2L1QSA0 (PrimitiveCombination2L1Q predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1Q predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2L1QSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2L1QSA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QSA1 : PCond2L1QSA
    {
        PCond2L1QSA1 (PrimitiveCombination2L1Q predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1Q predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2L1QSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QSA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QSL1 : PCond2L1QSL
    {
        PCond2L1QSL1 (PrimitiveCombination2L1Q predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L1Q predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2L1QSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QSL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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

    sealed class PCond2L1QSQ : PCond2L1Q
    {
        public readonly object alternativeValue;

        PCond2L1QSQ (PrimitiveCombination2L1Q predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        static public SCode Make (PrimitiveCombination2L1Q predicate, SCode consequent, Quotation alternative)
        {
            return new PCond2L1QSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2L1QQQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString();
#endif
            object predValue;

            object ev0;
	    if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
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
