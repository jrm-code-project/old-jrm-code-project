using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    class PCond2L : PCond2
    {
        // (l s s s)
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PCond2L (PrimitiveCombination2L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Name = predicate.rand0Name;
            this.rand0Depth = predicate.rand0Depth;
            this.rand0Offset = predicate.rand0Offset;
        }

        public static SCode Make (PrimitiveCombination2L predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveCombination2A) ? PCond2A.Make ((PrimitiveCombination2A) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2L1) ? PCond2L1.Make ((PrimitiveCombination2L1) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2LL) ? PCond2LL.Make ((PrimitiveCombination2LL) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2LQ) ? PCond2LQ.Make ((PrimitiveCombination2LQ) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCond2LSL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2LSQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LSSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LSSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2L.EvalStep");
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSL : PCond2L
    {
        // (l s l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2LSL (PrimitiveCombination2L predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2L predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2LSA.Make (predicate, (Argument) consequent, alternative)
                : (consequent is LexicalVariable1) ? PCond2LSL1.Make (predicate, (LexicalVariable1) consequent, alternative)
                : (alternative is LexicalVariable) ? PCond2LSLL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2LSLQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2LSL (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSLL : PCond2LSL
    {
        // (l s l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LSLL (PrimitiveCombination2L predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LSLA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2LSLL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2LSLL (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSLA : PCond2LSLL
    {
        // (l s l a)
        protected PCond2LSLA (PrimitiveCombination2L predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LSLA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2LSLA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2LSLA (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSLA0 : PCond2LSLA
    {
        // (l s l a0)
        PCond2LSLA0 (PrimitiveCombination2L predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2LSLA0 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSLA1 : PCond2LSLA
    {
        // (l s l a1)
        PCond2LSLA1 (PrimitiveCombination2L predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2LSLA1 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSLL1 : PCond2LSLL
    {
        // (l s l l1)
        PCond2LSLL1 (PrimitiveCombination2L predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LSLL1 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSLQ : PCond2LSL
    {
        // (l s l q)
        public readonly object alternativeValue;

        PCond2LSLQ (PrimitiveCombination2L predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2LSLQ (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSA : PCond2LSL
    {
        // (l s a s)
        protected PCond2LSA (PrimitiveCombination2L predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2LSA0.Make (predicate, (Argument0) consequent, alternative)
                : (consequent is Argument1) ? PCond2LSA1.Make (predicate, (Argument1) consequent, alternative)
                : (alternative is LexicalVariable) ? PCond2LSAL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2LSAQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2LSA (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSAL : PCond2LSA
    {
        // (l s a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LSAL (PrimitiveCombination2L predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LSAA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2LSAL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2LSAL (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSAA : PCond2LSAL
    {
        // (l s a a)
        protected PCond2LSAA (PrimitiveCombination2L predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LSAA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2LSAA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2LSAA (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSAA0 : PCond2LSAA
    {
        // (l s a a0)
        PCond2LSAA0 (PrimitiveCombination2L predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2LSAA0 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSAA1 : PCond2LSAA
    {
        // (l s a a1)
        PCond2LSAA1 (PrimitiveCombination2L predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2LSAA1 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSAL1 : PCond2LSAL
    {
        // (l s a l1)
        PCond2LSAL1 (PrimitiveCombination2L predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LSAL1 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSAQ : PCond2LSA
    {
        // (l s a q)
        public readonly object alternativeValue;

        PCond2LSAQ (PrimitiveCombination2L predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2LSAQ (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSA0 : PCond2LSA
    {
        // (l s a0 s)
        protected PCond2LSA0 (PrimitiveCombination2L predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LSA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LSA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2LSA0.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSA0L : PCond2LSA0
    {
        // (l s a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LSA0L (PrimitiveCombination2L predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LSA0A.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2LSA0L1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2LSA0L (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSA0A : PCond2LSA0L
    {
        // (l s a0 a)
        protected PCond2LSA0A (PrimitiveCombination2L predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LSA0A0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2LSA0A1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2LSA0A (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSA0A0 : PCond2LSA0A
    {
        // (l s a0 a0)
        PCond2LSA0A0 (PrimitiveCombination2L predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2LSA0A0 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSA0A1 : PCond2LSA0A
    {
        // (l s a0 a1)
        PCond2LSA0A1 (PrimitiveCombination2L predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2LSA0A1 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSA0L1 : PCond2LSA0L
    {
        // (l s a0 l1)
        PCond2LSA0L1 (PrimitiveCombination2L predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LSA0L1 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSA0Q : PCond2LSA0
    {
        // (l s a0 q)
        public readonly object alternativeValue;

        PCond2LSA0Q (PrimitiveCombination2L predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2LSA0Q (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    class PCond2LSA1 : PCond2LSA
    {
        // (l s a1 s)
        protected PCond2LSA1 (PrimitiveCombination2L predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LSA1L.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2LSA1Q.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2LSA1 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSA1L : PCond2LSA1
    {
        // (l s a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LSA1L (PrimitiveCombination2L predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LSA1A.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2LSA1L1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2LSA1L (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSA1A : PCond2LSA1L
    {
        // (l s a1 a)
        protected PCond2LSA1A (PrimitiveCombination2L predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LSA1A0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2LSA1A1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2LSA1A (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSA1A0 : PCond2LSA1A
    {
        // (l s a1 a0)
        PCond2LSA1A0 (PrimitiveCombination2L predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2LSA1A0 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSA1A1 : PCond2LSA1A
    {
        // (l s a1 a1)
        PCond2LSA1A1 (PrimitiveCombination2L predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2LSA1A1 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSA1L1 : PCond2LSA1L
    {
        // (l s a1 l1)
        PCond2LSA1L1 (PrimitiveCombination2L predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LSA1L1 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSA1Q : PCond2LSA1
    {
        // (l s a1 q)
        public readonly object alternativeValue;

        PCond2LSA1Q (PrimitiveCombination2L predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2LSA1Q (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSL1 : PCond2LSL
    {
        // (l s l1 s)
        protected PCond2LSL1 (PrimitiveCombination2L predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LSL1L.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2LSL1Q.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2LSL1 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSL1L : PCond2LSL1
    {
        // (l s l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LSL1L (PrimitiveCombination2L predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LSL1A.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2LSL1L1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2LSL1L (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSL1A : PCond2LSL1L
    {
        // (l s l1 a)
        protected PCond2LSL1A (PrimitiveCombination2L predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LSL1A0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2LSL1A1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2LSL1A (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSL1A0 : PCond2LSL1A
    {
        // (l s l1 a0)
        PCond2LSL1A0 (PrimitiveCombination2L predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2LSL1A0 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSL1A1 : PCond2LSL1A
    {
        // (l s l1 a1)
        PCond2LSL1A1 (PrimitiveCombination2L predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2LSL1A1 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSL1L1 : PCond2LSL1L
    {
        // (l s l1 l1)
        PCond2LSL1L1 (PrimitiveCombination2L predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LSL1L1 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSL1Q : PCond2LSL1
    {
        // (l s l1 q)
        public readonly object alternativeValue;

        PCond2LSL1Q (PrimitiveCombination2L predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2LSL1Q (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSQ : PCond2L
    {
        // (l s q s)
        public readonly object consequentValue;

        protected PCond2LSQ (PrimitiveCombination2L predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LSQL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2LSQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2LSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2LSQ.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSQL : PCond2LSQ
    {
        // (l s q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LSQL (PrimitiveCombination2L predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LSQA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2LSQL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2LSQL (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSQA : PCond2LSQL
    {
        // (l s q a)
        protected PCond2LSQA (PrimitiveCombination2L predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LSQA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2LSQA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2LSQA (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSQA0 : PCond2LSQA
    {
        // (l s q a0)
        PCond2LSQA0 (PrimitiveCombination2L predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2LSQA0 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSQA1 : PCond2LSQA
    {
        // (l s q a1)
        PCond2LSQA1 (PrimitiveCombination2L predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2LSQA1 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSQL1 : PCond2LSQL
    {
        // (l s q l1)
        PCond2LSQL1 (PrimitiveCombination2L predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LSQL1 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSQQ : PCond2LSQ
    {
        // (l s q q)
        public readonly object alternativeValue;

        PCond2LSQQ (PrimitiveCombination2L predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2LSQQ (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSSL : PCond2L
    {
        // (l s s l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LSSL (PrimitiveCombination2L predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2L predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LSSA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2LSSL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2LSSL (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LSSA : PCond2LSSL
    {
        // (l s s a)
        protected PCond2LSSA (PrimitiveCombination2L predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LSSA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2LSSA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2LSSA (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSSA0 : PCond2LSSA
    {
        // (l s s a0)
        PCond2LSSA0 (PrimitiveCombination2L predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond2LSSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2LSSA0.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSSA1 : PCond2LSSA
    {
        // (l s s a1)
        PCond2LSSA1 (PrimitiveCombination2L predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCond2LSSA1 (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSSL1 : PCond2LSSL
    {
        // (l s s l1)
        PCond2LSSL1 (PrimitiveCombination2L predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2L predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LSSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2LSSL1.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LSSQ : PCond2L
    {
        // (l s s q)
        public readonly object alternativeValue;

        PCond2LSSQ (PrimitiveCombination2L predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2L predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2LSSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2LSSQ.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL : PCond2L
    {
        // (l l s s)
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCond2LL (PrimitiveCombination2LL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveCombination2LL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveCombination2LA) ? PCond2LA.Make ((PrimitiveCombination2LA) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2LL1) ? PCond2LL1.Make ((PrimitiveCombination2LL1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCond2LLL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2LLQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LLSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LLSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2LL.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLL : PCond2LL
    {
        // (l l l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2LLL (PrimitiveCombination2LL predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2LL predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2LLA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2LLL1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LLLL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LLLQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLA : PCond2LLL
    {
        // (l l a s)

        protected PCond2LLA (PrimitiveCombination2LL predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2LLA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2LLA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LLAL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LLAQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLAL : PCond2LLA
    {
        // (l l a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LLAL (PrimitiveCombination2LL predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LLAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LLAL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LLAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLAL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLAA : PCond2LLAL
    {
        // (l l a l1)

        protected PCond2LLAA (PrimitiveCombination2LL predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LLAA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LLAA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LLAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLAA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLAA0 : PCond2LLAA
    {
        // (l l a a0)

        PCond2LLAA0 (PrimitiveCombination2LL predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2LLAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLAA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLAA1 : PCond2LLAA
    {
        // (l l a a1)

        PCond2LLAA1 (PrimitiveCombination2LL predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2LLAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLAA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLAL1 : PCond2LLAL
    {
        // (l l a l1)

        PCond2LLAL1 (PrimitiveCombination2LL predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LLAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLAL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLAQ : PCond2LLA
    {
        // (l l a Q)
        public readonly object alternativeValue;
        PCond2LLAQ (PrimitiveCombination2LL predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2LLAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLAQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLA0 : PCond2LLA
    {
        // (l l a0 s)

        protected PCond2LLA0 (PrimitiveCombination2LL predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LLA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LLA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLA0L : PCond2LLA0
    {
        // (l l a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LLA0L (PrimitiveCombination2LL predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LLA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LLA0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LLA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLA0L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLA0A : PCond2LLA0L
    {
        // (l l a0 l1)

        protected PCond2LLA0A (PrimitiveCombination2LL predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LLA0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LLA0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LLA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLA0A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLA0A0 : PCond2LLA0A
    {
        // (l l a0 a0)

        PCond2LLA0A0 (PrimitiveCombination2LL predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2LLA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLA0A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLA0A1 : PCond2LLA0A
    {
        // (l l a0 a1)

        PCond2LLA0A1 (PrimitiveCombination2LL predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2LLA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLA0A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLA0L1 : PCond2LLA0L
    {
        // (l l a0 l1)

        PCond2LLA0L1 (PrimitiveCombination2LL predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LLA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLA0L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLA0Q : PCond2LLA0
    {
        // (l l a0 Q)
        public readonly object alternativeValue;
        PCond2LLA0Q (PrimitiveCombination2LL predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2LLA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLA0Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLA1 : PCond2LLA
    {
        // (l l a1 s)

        protected PCond2LLA1 (PrimitiveCombination2LL predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LLA1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LLA1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LLA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLA1L : PCond2LLA1
    {
        // (l l a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LLA1L (PrimitiveCombination2LL predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LLA1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LLA1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LLA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLA1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLA1A : PCond2LLA1L
    {
        // (l l a1 l1)

        protected PCond2LLA1A (PrimitiveCombination2LL predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LLA1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LLA1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LLA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLA1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLA1A0 : PCond2LLA1A
    {
        // (l l a1 a0)

        PCond2LLA1A0 (PrimitiveCombination2LL predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2LLA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLA1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLA1A1 : PCond2LLA1A
    {
        // (l l a1 a1)

        PCond2LLA1A1 (PrimitiveCombination2LL predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2LLA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLA1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLA1L1 : PCond2LLA1L
    {
        // (l l a1 l1)

        PCond2LLA1L1 (PrimitiveCombination2LL predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LLA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLA1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLA1Q : PCond2LLA1
    {
        // (l l a1 Q)
        public readonly object alternativeValue;
        PCond2LLA1Q (PrimitiveCombination2LL predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2LLA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLA1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLL1 : PCond2LLL
    {
        // (l l l1 s)

        protected PCond2LLL1 (PrimitiveCombination2LL predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LLL1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LLL1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LLL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLL1L : PCond2LLL1
    {
        // (l l l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LLL1L (PrimitiveCombination2LL predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LL predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LLL1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LLL1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LLL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLL1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLL1A : PCond2LLL1L
    {
        // (l l l1 l1)

        protected PCond2LLL1A (PrimitiveCombination2LL predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LLL1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LLL1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LLL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLL1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLL1A0 : PCond2LLL1A
    {
        // (l l l1 a0)

        PCond2LLL1A0 (PrimitiveCombination2LL predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2LLL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLL1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLL1A1 : PCond2LLL1A
    {
        // (l l l1 a1)

        PCond2LLL1A1 (PrimitiveCombination2LL predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2LLL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLL1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLL1L1 : PCond2LLL1L
    {
        // (l l l1 l1)

        PCond2LLL1L1 (PrimitiveCombination2LL predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LLL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLL1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLL1Q : PCond2LLL1
    {
        // (l l l1 Q)
        public readonly object alternativeValue;
        PCond2LLL1Q (PrimitiveCombination2LL predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LL predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2LLL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLL1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLLL : PCond2LLL
    {
        // (l l l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LLLL (PrimitiveCombination2LL predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LL predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LLLA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LLLL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LLLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLLL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLLA : PCond2LLLL
    {
        // (l l l l1)

        protected PCond2LLLA (PrimitiveCombination2LL predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LLLA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LLLA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LLLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLLA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLLA0 : PCond2LLLA
    {
        // (l l l a0)

        PCond2LLLA0 (PrimitiveCombination2LL predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2LLLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLLA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLLA1 : PCond2LLLA
    {
        // (l l l a1)

        PCond2LLLA1 (PrimitiveCombination2LL predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2LLLA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLLA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLLL1 : PCond2LLLL
    {
        // (l l l l1)

        PCond2LLLL1 (PrimitiveCombination2LL predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LLLL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLLL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLLQ : PCond2LLL
    {
        // (l l l Q)
        public readonly object alternativeValue;
        PCond2LLLQ (PrimitiveCombination2LL predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LL predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2LLLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLLQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLQ : PCond2LL
    {
        // (l l q s)
        public readonly object consequentValue;

        protected PCond2LLQ (PrimitiveCombination2LL predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LLQL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LLQQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LLQ (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLQL : PCond2LLQ
    {
        // (l l q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LLQL (PrimitiveCombination2LL predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LLQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LLQL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LLQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLQL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLQA : PCond2LLQL
    {
        // (l l q a)
        protected PCond2LLQA (PrimitiveCombination2LL predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LLQA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LLQA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LLQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLQA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLQA0 : PCond2LLQA
    {
        // (l l q a0)
        PCond2LLQA0 (PrimitiveCombination2LL predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2LLQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLQA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLQA1 : PCond2LLQA
    {
        // (l l q a1)
        PCond2LLQA1 (PrimitiveCombination2LL predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2LLQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLQA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLQL1 : PCond2LLQL
    {
        // (l l q l1)
        PCond2LLQL1 (PrimitiveCombination2LL predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LLQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLQL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLQQ : PCond2LLQ
    {
        // (l l q q)
        public readonly object alternativeValue;

        PCond2LLQQ (PrimitiveCombination2LL predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LL predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2LLQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2LLQQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLSL : PCond2LL
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LLSL (PrimitiveCombination2LL predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2LL predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LLSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LLSL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LLSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLSL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LLSA : PCond2LLSL
    {
        protected PCond2LLSA (PrimitiveCombination2LL predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LL predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LLSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LLSA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LLSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLSA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLSA0 : PCond2LLSA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2LLSA0 (PrimitiveCombination2LL predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LL predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2LLSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2LLSA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLSA1 : PCond2LLSA
    {
        PCond2LLSA1 (PrimitiveCombination2LL predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LL predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2LLSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLSA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLSL1 : PCond2LLSL
    {
        PCond2LLSL1 (PrimitiveCombination2LL predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LL predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2LLSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLSL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LLSQ : PCond2LL
    {
        // (l l s q)
        public readonly object alternativeValue;

        PCond2LLSQ (PrimitiveCombination2LL predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LL predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2LLSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LLSQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA : PCond2LL
    {
        // (s a s s)
        protected PCond2LA (PrimitiveCombination2LA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveCombination2LA0) ? PCond2LA0.Make ((PrimitiveCombination2LA0) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2LA1) ? PCond2LA1.Make ((PrimitiveCombination2LA1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCond2LAL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2LAQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LASL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LASQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG

            Warm ("PCond2LA.EvalStep");
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LAL : PCond2LA
    {
        // (s a l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2LAL (PrimitiveCombination2LA predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2LAA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2LAL1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LALL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LALQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LAA : PCond2LAL
    {
        // (s a a s)

        protected PCond2LAA (PrimitiveCombination2LA predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2LAA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2LAA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LAAL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LAAQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LAAL : PCond2LAA
    {
        // (s a a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LAAL (PrimitiveCombination2LA predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LAAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LAAL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LAAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAAL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LAAA : PCond2LAAL
    {
        // (s a a l1)

        protected PCond2LAAA (PrimitiveCombination2LA predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LAAA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LAAA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LAAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAAA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAAA0 : PCond2LAAA
    {
        // (s a a a0)

        PCond2LAAA0 (PrimitiveCombination2LA predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2LAAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAAA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAAA1 : PCond2LAAA
    {
        // (s a a a1)

        PCond2LAAA1 (PrimitiveCombination2LA predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2LAAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAAA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAAL1 : PCond2LAAL
    {
        // (s a a l1)

        PCond2LAAL1 (PrimitiveCombination2LA predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LAAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAAL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAAQ : PCond2LAA
    {
        // (s a a Q)
        public readonly object alternativeValue;
        PCond2LAAQ (PrimitiveCombination2LA predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2LAAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAAQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LAA0 : PCond2LAA
    {
        // (s a a0 s)

        protected PCond2LAA0 (PrimitiveCombination2LA predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LAA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LAA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LAA0L : PCond2LAA0
    {
        // (s a a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LAA0L (PrimitiveCombination2LA predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LAA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LAA0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LAA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAA0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LAA0A : PCond2LAA0L
    {
        // (s a a0 l1)

        protected PCond2LAA0A (PrimitiveCombination2LA predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LAA0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LAA0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LAA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAA0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAA0A0 : PCond2LAA0A
    {
        // (s a a0 a0)

        PCond2LAA0A0 (PrimitiveCombination2LA predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2LAA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAA0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAA0A1 : PCond2LAA0A
    {
        // (s a a0 a1)

        PCond2LAA0A1 (PrimitiveCombination2LA predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2LAA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAA0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAA0L1 : PCond2LAA0L
    {
        // (s a a0 l1)

        PCond2LAA0L1 (PrimitiveCombination2LA predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LAA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAA0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAA0Q : PCond2LAA0
    {
        // (s a a0 Q)
        public readonly object alternativeValue;
        PCond2LAA0Q (PrimitiveCombination2LA predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2LAA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAA0Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LAA1 : PCond2LAA
    {
        // (s a a1 s)

        protected PCond2LAA1 (PrimitiveCombination2LA predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LAA1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LAA1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LAA1L : PCond2LAA1
    {
        // (s a a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LAA1L (PrimitiveCombination2LA predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LAA1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LAA1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LAA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAA1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LAA1A : PCond2LAA1L
    {
        // (s a a1 l1)

        protected PCond2LAA1A (PrimitiveCombination2LA predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LAA1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LAA1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LAA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAA1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAA1A0 : PCond2LAA1A
    {
        // (s a a1 a0)

        PCond2LAA1A0 (PrimitiveCombination2LA predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2LAA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAA1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAA1A1 : PCond2LAA1A
    {
        // (s a a1 a1)

        PCond2LAA1A1 (PrimitiveCombination2LA predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2LAA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAA1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAA1L1 : PCond2LAA1L
    {
        // (s a a1 l1)

        PCond2LAA1L1 (PrimitiveCombination2LA predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LAA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAA1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAA1Q : PCond2LAA1
    {
        // (s a a1 Q)
        public readonly object alternativeValue;
        PCond2LAA1Q (PrimitiveCombination2LA predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2LAA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAA1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LAL1 : PCond2LAL
    {
        // (s a l1 s)

        protected PCond2LAL1 (PrimitiveCombination2LA predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LAL1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LAL1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LAL1L : PCond2LAL1
    {
        // (s a l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LAL1L (PrimitiveCombination2LA predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LAL1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LAL1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LAL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAL1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LAL1A : PCond2LAL1L
    {
        // (s a l1 l1)

        protected PCond2LAL1A (PrimitiveCombination2LA predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LAL1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LAL1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LAL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAL1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAL1A0 : PCond2LAL1A
    {
        // (s a l1 a0)

        PCond2LAL1A0 (PrimitiveCombination2LA predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2LAL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAL1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAL1A1 : PCond2LAL1A
    {
        // (s a l1 a1)

        PCond2LAL1A1 (PrimitiveCombination2LA predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2LAL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAL1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAL1L1 : PCond2LAL1L
    {
        // (s a l1 l1)

        PCond2LAL1L1 (PrimitiveCombination2LA predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LAL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAL1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAL1Q : PCond2LAL1
    {
        // (s a l1 Q)
        public readonly object alternativeValue;
        PCond2LAL1Q (PrimitiveCombination2LA predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2LAL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAL1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LALL : PCond2LAL
    {
        // (s a l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LALL (PrimitiveCombination2LA predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LALA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LALL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LALL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LALL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LALA : PCond2LALL
    {
        // (s a l l1)

        protected PCond2LALA (PrimitiveCombination2LA predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LALA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LALA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LALA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LALA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LALA0 : PCond2LALA
    {
        // (s a l a0)

        PCond2LALA0 (PrimitiveCombination2LA predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2LALA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LALA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LALA1 : PCond2LALA
    {
        // (s a l a1)

        PCond2LALA1 (PrimitiveCombination2LA predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2LALA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LALA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LALL1 : PCond2LALL
    {
        // (s a l l1)

        PCond2LALL1 (PrimitiveCombination2LA predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LALL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LALL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LALQ : PCond2LAL
    {
        // (s a l Q)
        public readonly object alternativeValue;
        PCond2LALQ (PrimitiveCombination2LA predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2LALQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LALQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LAQ : PCond2LA
    {
        // (s a q s)
        public readonly object consequentValue;

        protected PCond2LAQ (PrimitiveCombination2LA predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LAQL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LAQQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LAQL : PCond2LAQ
    {
        // (s a q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LAQL (PrimitiveCombination2LA predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LAQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LAQL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LAQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAQL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LAQA : PCond2LAQL
    {
        // (s a q a)
        protected PCond2LAQA (PrimitiveCombination2LA predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LAQA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LAQA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LAQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAQA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAQA0 : PCond2LAQA
    {
        // (s a q a0)
        PCond2LAQA0 (PrimitiveCombination2LA predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2LAQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAQA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAQA1 : PCond2LAQA
    {
        // (s a q a1)
        PCond2LAQA1 (PrimitiveCombination2LA predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2LAQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAQA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAQL1 : PCond2LAQL
    {
        // (s a q l1)
        PCond2LAQL1 (PrimitiveCombination2LA predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LAQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LAQL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LAQQ : PCond2LAQ
    {
        // (s a q q)
        public readonly object alternativeValue;

        PCond2LAQQ (PrimitiveCombination2LA predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2LAQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2LAQQ.EvalStep";
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LASL : PCond2LA
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LASL (PrimitiveCombination2LA predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2LA predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LASA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LASL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LASL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LASL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LASA : PCond2LASL
    {
        protected PCond2LASA (PrimitiveCombination2LA predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LA predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LASA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LASA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LASA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LASA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LASA0 : PCond2LASA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2LASA0 (PrimitiveCombination2LA predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LA predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2LASA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2LASA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LASA1 : PCond2LASA
    {
        PCond2LASA1 (PrimitiveCombination2LA predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LA predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2LASA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LASA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LASL1 : PCond2LASL
    {
        PCond2LASL1 (PrimitiveCombination2LA predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LA predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2LASL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LASL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LASQ : PCond2LA
    {
        // (s a s q)
        public readonly object alternativeValue;

        PCond2LASQ (PrimitiveCombination2LA predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2LASQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LASQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0 : PCond2LA
    {
        // (s a0 s s)
        protected PCond2LA0 (PrimitiveCombination2LA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2LA0L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2LA0Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LA0SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LA0SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2LA0.EvalStep");
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0L : PCond2LA0
    {
        // (s a0 l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2LA0L (PrimitiveCombination2LA0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2LA0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2LA0L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LA0LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LA0LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0A : PCond2LA0L
    {
        // (s a0 a s)

        protected PCond2LA0A (PrimitiveCombination2LA0 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2LA0A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2LA0A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LA0AL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LA0AQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0AL : PCond2LA0A
    {
        // (s a0 a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LA0AL (PrimitiveCombination2LA0 predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LA0AA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LA0AL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LA0AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0AL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0AA : PCond2LA0AL
    {
        // (s a0 a l1)

        protected PCond2LA0AA (PrimitiveCombination2LA0 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LA0AA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LA0AA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LA0AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0AA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0AA0 : PCond2LA0AA
    {
        // (s a0 a a0)

        PCond2LA0AA0 (PrimitiveCombination2LA0 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2LA0AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0AA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0AA1 : PCond2LA0AA
    {
        // (s a0 a a1)

        PCond2LA0AA1 (PrimitiveCombination2LA0 predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2LA0AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0AA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0AL1 : PCond2LA0AL
    {
        // (s a0 a l1)

        PCond2LA0AL1 (PrimitiveCombination2LA0 predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LA0AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0AL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0AQ : PCond2LA0A
    {
        // (s a0 a Q)
        public readonly object alternativeValue;
        PCond2LA0AQ (PrimitiveCombination2LA0 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2LA0AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0AQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0A0 : PCond2LA0A
    {
        // (s a0 a0 s)

        protected PCond2LA0A0 (PrimitiveCombination2LA0 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LA0A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LA0A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0A0L : PCond2LA0A0
    {
        // (s a0 a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LA0A0L (PrimitiveCombination2LA0 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LA0A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LA0A0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LA0A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0A0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0A0A : PCond2LA0A0L
    {
        // (s a0 a0 l1)

        protected PCond2LA0A0A (PrimitiveCombination2LA0 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LA0A0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LA0A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LA0A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0A0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0A0A0 : PCond2LA0A0A
    {
        // (s a0 a0 a0)

        PCond2LA0A0A0 (PrimitiveCombination2LA0 predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2LA0A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0A0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0A0A1 : PCond2LA0A0A
    {
        // (s a0 a0 a1)

        PCond2LA0A0A1 (PrimitiveCombination2LA0 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2LA0A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0A0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0A0L1 : PCond2LA0A0L
    {
        // (s a0 a0 l1)

        PCond2LA0A0L1 (PrimitiveCombination2LA0 predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LA0A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0A0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0A0Q : PCond2LA0A0
    {
        // (s a0 a0 Q)
        public readonly object alternativeValue;
        PCond2LA0A0Q (PrimitiveCombination2LA0 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2LA0A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0A0Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0A1 : PCond2LA0A
    {
        // (s a0 a1 s)

        protected PCond2LA0A1 (PrimitiveCombination2LA0 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LA0A1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LA0A1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0A1L : PCond2LA0A1
    {
        // (s a0 a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LA0A1L (PrimitiveCombination2LA0 predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LA0A1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LA0A1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LA0A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0A1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0A1A : PCond2LA0A1L
    {
        // (s a0 a1 l1)

        protected PCond2LA0A1A (PrimitiveCombination2LA0 predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LA0A1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LA0A1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LA0A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0A1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0A1A0 : PCond2LA0A1A
    {
        // (s a0 a1 a0)

        PCond2LA0A1A0 (PrimitiveCombination2LA0 predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2LA0A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0A1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0A1A1 : PCond2LA0A1A
    {
        // (s a0 a1 a1)

        PCond2LA0A1A1 (PrimitiveCombination2LA0 predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2LA0A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0A1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0A1L1 : PCond2LA0A1L
    {
        // (s a0 a1 l1)

        PCond2LA0A1L1 (PrimitiveCombination2LA0 predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LA0A1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0A1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0A1Q : PCond2LA0A1
    {
        // (s a0 a1 Q)
        public readonly object alternativeValue;
        PCond2LA0A1Q (PrimitiveCombination2LA0 predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2LA0A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0A1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0L1 : PCond2LA0L
    {
        // (s a0 l1 s)

        protected PCond2LA0L1 (PrimitiveCombination2LA0 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LA0L1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LA0L1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0L1L : PCond2LA0L1
    {
        // (s a0 l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LA0L1L (PrimitiveCombination2LA0 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LA0L1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LA0L1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LA0L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0L1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0L1A : PCond2LA0L1L
    {
        // (s a0 l1 l1)

        protected PCond2LA0L1A (PrimitiveCombination2LA0 predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LA0L1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LA0L1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LA0L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0L1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0L1A0 : PCond2LA0L1A
    {
        // (s a0 l1 a0)

        PCond2LA0L1A0 (PrimitiveCombination2LA0 predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2LA0L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0L1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0L1A1 : PCond2LA0L1A
    {
        // (s a0 l1 a1)

        PCond2LA0L1A1 (PrimitiveCombination2LA0 predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2LA0L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0L1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0L1L1 : PCond2LA0L1L
    {
        // (s a0 l1 l1)

        PCond2LA0L1L1 (PrimitiveCombination2LA0 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LA0L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0L1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0L1Q : PCond2LA0L1
    {
        // (s a0 l1 Q)
        public readonly object alternativeValue;
        PCond2LA0L1Q (PrimitiveCombination2LA0 predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2LA0L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0L1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0LL : PCond2LA0L
    {
        // (s a0 l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LA0LL (PrimitiveCombination2LA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LA0LA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LA0LL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LA0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0LL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0LA : PCond2LA0LL
    {
        // (s a0 l l1)

        protected PCond2LA0LA (PrimitiveCombination2LA0 predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LA0LA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LA0LA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LA0LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0LA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0LA0 : PCond2LA0LA
    {
        // (s a0 l a0)

        PCond2LA0LA0 (PrimitiveCombination2LA0 predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2LA0LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0LA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0LA1 : PCond2LA0LA
    {
        // (s a0 l a1)

        PCond2LA0LA1 (PrimitiveCombination2LA0 predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2LA0LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0LA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0LL1 : PCond2LA0LL
    {
        // (s a0 l l1)

        PCond2LA0LL1 (PrimitiveCombination2LA0 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LA0LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0LL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0LQ : PCond2LA0L
    {
        // (s a0 l Q)
        public readonly object alternativeValue;
        PCond2LA0LQ (PrimitiveCombination2LA0 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2LA0LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0LQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0Q : PCond2LA0
    {
        // (s a0 q s)
        public readonly object consequentValue;

        protected PCond2LA0Q (PrimitiveCombination2LA0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LA0QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LA0QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0QL : PCond2LA0Q
    {
        // (s a0 q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LA0QL (PrimitiveCombination2LA0 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LA0QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LA0QL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LA0QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0QL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0QA : PCond2LA0QL
    {
        // (s a0 q a)
        protected PCond2LA0QA (PrimitiveCombination2LA0 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LA0QA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LA0QA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LA0QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0QA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0QA0 : PCond2LA0QA
    {
        // (s a0 q a0)
        PCond2LA0QA0 (PrimitiveCombination2LA0 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2LA0QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0QA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0QA1 : PCond2LA0QA
    {
        // (s a0 q a1)
        PCond2LA0QA1 (PrimitiveCombination2LA0 predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2LA0QA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0QA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0QL1 : PCond2LA0QL
    {
        // (s a0 q l1)
        PCond2LA0QL1 (PrimitiveCombination2LA0 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LA0QL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0QL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0QQ : PCond2LA0Q
    {
        // (s a0 q q)
        public readonly object alternativeValue;

        PCond2LA0QQ (PrimitiveCombination2LA0 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2LA0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2LA0QQ.EvalStep";
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0SL : PCond2LA0
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LA0SL (PrimitiveCombination2LA0 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2LA0 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LA0SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LA0SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LA0SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0SL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA0SA : PCond2LA0SL
    {
        protected PCond2LA0SA (PrimitiveCombination2LA0 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LA0 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LA0SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LA0SA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LA0SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0SA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0SA0 : PCond2LA0SA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2LA0SA0 (PrimitiveCombination2LA0 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LA0 predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2LA0SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2LA0SA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0SA1 : PCond2LA0SA
    {
        PCond2LA0SA1 (PrimitiveCombination2LA0 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LA0 predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2LA0SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0SA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0SL1 : PCond2LA0SL
    {
        PCond2LA0SL1 (PrimitiveCombination2LA0 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LA0 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2LA0SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0SL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA0SQ : PCond2LA0
    {
        // (s a0 s q)
        public readonly object alternativeValue;

        PCond2LA0SQ (PrimitiveCombination2LA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA0 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2LA0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA0SQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1 : PCond2LA
    {
        // (s a1 s s)
        protected PCond2LA1 (PrimitiveCombination2LA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2LA1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2LA1Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LA1SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LA1SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2LA1.EvalStep");
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1L : PCond2LA1
    {
        // (s a1 l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2LA1L (PrimitiveCombination2LA1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2LA1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2LA1L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LA1LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LA1LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1A : PCond2LA1L
    {
        // (s a1 a s)

        protected PCond2LA1A (PrimitiveCombination2LA1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2LA1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2LA1A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LA1AL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LA1AQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1AL : PCond2LA1A
    {
        // (s a1 a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LA1AL (PrimitiveCombination2LA1 predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LA1AA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LA1AL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LA1AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1AL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1AA : PCond2LA1AL
    {
        // (s a1 a l1)

        protected PCond2LA1AA (PrimitiveCombination2LA1 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LA1AA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LA1AA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LA1AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1AA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1AA0 : PCond2LA1AA
    {
        // (s a1 a a0)

        PCond2LA1AA0 (PrimitiveCombination2LA1 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2LA1AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1AA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1AA1 : PCond2LA1AA
    {
        // (s a1 a a1)

        PCond2LA1AA1 (PrimitiveCombination2LA1 predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2LA1AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1AA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1AL1 : PCond2LA1AL
    {
        // (s a1 a l1)

        PCond2LA1AL1 (PrimitiveCombination2LA1 predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LA1AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1AL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1AQ : PCond2LA1A
    {
        // (s a1 a Q)
        public readonly object alternativeValue;
        PCond2LA1AQ (PrimitiveCombination2LA1 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2LA1AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1AQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1A0 : PCond2LA1A
    {
        // (s a1 a0 s)

        protected PCond2LA1A0 (PrimitiveCombination2LA1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LA1A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LA1A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1A0L : PCond2LA1A0
    {
        // (s a1 a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LA1A0L (PrimitiveCombination2LA1 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LA1A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LA1A0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LA1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1A0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1A0A : PCond2LA1A0L
    {
        // (s a1 a0 l1)

        protected PCond2LA1A0A (PrimitiveCombination2LA1 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LA1A0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LA1A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LA1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1A0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1A0A0 : PCond2LA1A0A
    {
        // (s a1 a0 a0)

        PCond2LA1A0A0 (PrimitiveCombination2LA1 predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2LA1A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1A0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1A0A1 : PCond2LA1A0A
    {
        // (s a1 a0 a1)

        PCond2LA1A0A1 (PrimitiveCombination2LA1 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2LA1A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1A0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1A0L1 : PCond2LA1A0L
    {
        // (s a1 a0 l1)

        PCond2LA1A0L1 (PrimitiveCombination2LA1 predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LA1A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1A0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1A0Q : PCond2LA1A0
    {
        // (s a1 a0 Q)
        public readonly object alternativeValue;
        PCond2LA1A0Q (PrimitiveCombination2LA1 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2LA1A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1A0Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1A1 : PCond2LA1A
    {
        // (s a1 a1 s)

        protected PCond2LA1A1 (PrimitiveCombination2LA1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LA1A1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LA1A1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1A1L : PCond2LA1A1
    {
        // (s a1 a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LA1A1L (PrimitiveCombination2LA1 predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LA1A1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LA1A1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LA1A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1A1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1A1A : PCond2LA1A1L
    {
        // (s a1 a1 l1)

        protected PCond2LA1A1A (PrimitiveCombination2LA1 predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LA1A1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LA1A1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LA1A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1A1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1A1A0 : PCond2LA1A1A
    {
        // (s a1 a1 a0)

        PCond2LA1A1A0 (PrimitiveCombination2LA1 predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2LA1A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1A1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1A1A1 : PCond2LA1A1A
    {
        // (s a1 a1 a1)

        PCond2LA1A1A1 (PrimitiveCombination2LA1 predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2LA1A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1A1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1A1L1 : PCond2LA1A1L
    {
        // (s a1 a1 l1)

        PCond2LA1A1L1 (PrimitiveCombination2LA1 predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LA1A1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1A1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1A1Q : PCond2LA1A1
    {
        // (s a1 a1 Q)
        public readonly object alternativeValue;
        PCond2LA1A1Q (PrimitiveCombination2LA1 predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2LA1A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1A1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1L1 : PCond2LA1L
    {
        // (s a1 l1 s)

        protected PCond2LA1L1 (PrimitiveCombination2LA1 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LA1L1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LA1L1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1L1L : PCond2LA1L1
    {
        // (s a1 l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LA1L1L (PrimitiveCombination2LA1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LA1L1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LA1L1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LA1L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1L1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1L1A : PCond2LA1L1L
    {
        // (s a1 l1 l1)

        protected PCond2LA1L1A (PrimitiveCombination2LA1 predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LA1L1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LA1L1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LA1L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1L1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1L1A0 : PCond2LA1L1A
    {
        // (s a1 l1 a0)

        PCond2LA1L1A0 (PrimitiveCombination2LA1 predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2LA1L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1L1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1L1A1 : PCond2LA1L1A
    {
        // (s a1 l1 a1)

        PCond2LA1L1A1 (PrimitiveCombination2LA1 predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2LA1L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1L1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1L1L1 : PCond2LA1L1L
    {
        // (s a1 l1 l1)

        PCond2LA1L1L1 (PrimitiveCombination2LA1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LA1L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1L1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1L1Q : PCond2LA1L1
    {
        // (s a1 l1 Q)
        public readonly object alternativeValue;
        PCond2LA1L1Q (PrimitiveCombination2LA1 predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2LA1L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1L1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1LL : PCond2LA1L
    {
        // (s a1 l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LA1LL (PrimitiveCombination2LA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LA1LA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LA1LL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LA1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1LL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1LA : PCond2LA1LL
    {
        // (s a1 l l1)

        protected PCond2LA1LA (PrimitiveCombination2LA1 predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LA1LA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LA1LA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LA1LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1LA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1LA0 : PCond2LA1LA
    {
        // (s a1 l a0)

        PCond2LA1LA0 (PrimitiveCombination2LA1 predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2LA1LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1LA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1LA1 : PCond2LA1LA
    {
        // (s a1 l a1)

        PCond2LA1LA1 (PrimitiveCombination2LA1 predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2LA1LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1LA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1LL1 : PCond2LA1LL
    {
        // (s a1 l l1)

        PCond2LA1LL1 (PrimitiveCombination2LA1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LA1LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1LL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1LQ : PCond2LA1L
    {
        // (s a1 l Q)
        public readonly object alternativeValue;
        PCond2LA1LQ (PrimitiveCombination2LA1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2LA1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1LQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1Q : PCond2LA1
    {
        // (s a1 q s)
        public readonly object consequentValue;

        protected PCond2LA1Q (PrimitiveCombination2LA1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LA1QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LA1QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1QL : PCond2LA1Q
    {
        // (s a1 q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LA1QL (PrimitiveCombination2LA1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LA1QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LA1QL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LA1QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1QL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1QA : PCond2LA1QL
    {
        // (s a1 q a)
        protected PCond2LA1QA (PrimitiveCombination2LA1 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LA1QA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LA1QA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LA1QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1QA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1QA0 : PCond2LA1QA
    {
        // (s a1 q a0)
        PCond2LA1QA0 (PrimitiveCombination2LA1 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2LA1QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1QA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1QA1 : PCond2LA1QA
    {
        // (s a1 q a1)
        PCond2LA1QA1 (PrimitiveCombination2LA1 predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2LA1QA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1QA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1QL1 : PCond2LA1QL
    {
        // (s a1 q l1)
        PCond2LA1QL1 (PrimitiveCombination2LA1 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LA1QL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1QL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1QQ : PCond2LA1Q
    {
        // (s a1 q q)
        public readonly object alternativeValue;

        PCond2LA1QQ (PrimitiveCombination2LA1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2LA1QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2LA1QQ.EvalStep";
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1SL : PCond2LA1
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LA1SL (PrimitiveCombination2LA1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2LA1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LA1SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LA1SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LA1SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1SL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LA1SA : PCond2LA1SL
    {
        protected PCond2LA1SA (PrimitiveCombination2LA1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LA1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LA1SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LA1SA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LA1SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1SA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1SA0 : PCond2LA1SA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2LA1SA0 (PrimitiveCombination2LA1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LA1 predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2LA1SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2LA1SA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1SA1 : PCond2LA1SA
    {
        PCond2LA1SA1 (PrimitiveCombination2LA1 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LA1 predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2LA1SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1SA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1SL1 : PCond2LA1SL
    {
        PCond2LA1SL1 (PrimitiveCombination2LA1 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LA1 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2LA1SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1SL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LA1SQ : PCond2LA1
    {
        // (s a1 s q)
        public readonly object alternativeValue;

        PCond2LA1SQ (PrimitiveCombination2LA1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LA1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2LA1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LA1SQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1 : PCond2LL
    {
        // (l l1 s s)
        protected PCond2LL1 (PrimitiveCombination2LL1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2LL1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2LL1Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LL1SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LL1SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2LL1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1L : PCond2LL1
    {
        // (l l1 l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2LL1L (PrimitiveCombination2LL1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2LL1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2LL1L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LL1LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LL1LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1A : PCond2LL1L
    {
        // (l l1 a s)

        protected PCond2LL1A (PrimitiveCombination2LL1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2LL1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2LL1A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LL1AL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LL1AQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1AL : PCond2LL1A
    {
        // (l l1 a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LL1AL (PrimitiveCombination2LL1 predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LL1AA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LL1AL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LL1AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1AL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1AA : PCond2LL1AL
    {
        // (l l1 a l1)

        protected PCond2LL1AA (PrimitiveCombination2LL1 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LL1AA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LL1AA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LL1AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1AA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1AA0 : PCond2LL1AA
    {
        // (l l1 a a0)

        PCond2LL1AA0 (PrimitiveCombination2LL1 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2LL1AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1AA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1AA1 : PCond2LL1AA
    {
        // (l l1 a a1)

        PCond2LL1AA1 (PrimitiveCombination2LL1 predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2LL1AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1AA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1AL1 : PCond2LL1AL
    {
        // (l l1 a l1)

        PCond2LL1AL1 (PrimitiveCombination2LL1 predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LL1AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1AL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1AQ : PCond2LL1A
    {
        // (l l1 a Q)
        public readonly object alternativeValue;
        PCond2LL1AQ (PrimitiveCombination2LL1 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2LL1AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1AQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1A0 : PCond2LL1A
    {
        // (l l1 a0 s)

        protected PCond2LL1A0 (PrimitiveCombination2LL1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LL1A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LL1A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1A0L : PCond2LL1A0
    {
        // (l l1 a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LL1A0L (PrimitiveCombination2LL1 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LL1A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LL1A0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LL1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1A0L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1A0A : PCond2LL1A0L
    {
        // (l l1 a0 l1)

        protected PCond2LL1A0A (PrimitiveCombination2LL1 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LL1A0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LL1A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LL1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1A0A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1A0A0 : PCond2LL1A0A
    {
        // (l l1 a0 a0)

        PCond2LL1A0A0 (PrimitiveCombination2LL1 predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2LL1A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1A0A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1A0A1 : PCond2LL1A0A
    {
        // (l l1 a0 a1)

        PCond2LL1A0A1 (PrimitiveCombination2LL1 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2LL1A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1A0A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1A0L1 : PCond2LL1A0L
    {
        // (l l1 a0 l1)

        PCond2LL1A0L1 (PrimitiveCombination2LL1 predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LL1A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1A0L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1A0Q : PCond2LL1A0
    {
        // (l l1 a0 Q)
        public readonly object alternativeValue;
        PCond2LL1A0Q (PrimitiveCombination2LL1 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2LL1A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1A0Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1A1 : PCond2LL1A
    {
        // (l l1 a1 s)

        protected PCond2LL1A1 (PrimitiveCombination2LL1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LL1A1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LL1A1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1A1L : PCond2LL1A1
    {
        // (l l1 a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LL1A1L (PrimitiveCombination2LL1 predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LL1A1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LL1A1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LL1A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1A1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1A1A : PCond2LL1A1L
    {
        // (l l1 a1 l1)

        protected PCond2LL1A1A (PrimitiveCombination2LL1 predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LL1A1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LL1A1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LL1A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1A1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1A1A0 : PCond2LL1A1A
    {
        // (l l1 a1 a0)

        PCond2LL1A1A0 (PrimitiveCombination2LL1 predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2LL1A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1A1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1A1A1 : PCond2LL1A1A
    {
        // (l l1 a1 a1)

        PCond2LL1A1A1 (PrimitiveCombination2LL1 predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2LL1A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1A1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1A1L1 : PCond2LL1A1L
    {
        // (l l1 a1 l1)

        PCond2LL1A1L1 (PrimitiveCombination2LL1 predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LL1A1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1A1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1A1Q : PCond2LL1A1
    {
        // (l l1 a1 Q)
        public readonly object alternativeValue;
        PCond2LL1A1Q (PrimitiveCombination2LL1 predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2LL1A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1A1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1L1 : PCond2LL1L
    {
        // (l l1 l1 s)

        protected PCond2LL1L1 (PrimitiveCombination2LL1 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LL1L1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LL1L1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1L1L : PCond2LL1L1
    {
        // (l l1 l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LL1L1L (PrimitiveCombination2LL1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LL1L1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LL1L1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LL1L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1L1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1L1A : PCond2LL1L1L
    {
        // (l l1 l1 l1)

        protected PCond2LL1L1A (PrimitiveCombination2LL1 predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LL1L1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LL1L1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LL1L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1L1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1L1A0 : PCond2LL1L1A
    {
        // (l l1 l1 a0)

        PCond2LL1L1A0 (PrimitiveCombination2LL1 predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2LL1L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1L1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1L1A1 : PCond2LL1L1A
    {
        // (l l1 l1 a1)

        PCond2LL1L1A1 (PrimitiveCombination2LL1 predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2LL1L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1L1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1L1L1 : PCond2LL1L1L
    {
        // (l l1 l1 l1)

        PCond2LL1L1L1 (PrimitiveCombination2LL1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LL1L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1L1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1L1Q : PCond2LL1L1
    {
        // (l l1 l1 Q)
        public readonly object alternativeValue;
        PCond2LL1L1Q (PrimitiveCombination2LL1 predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2LL1L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1L1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1LL : PCond2LL1L
    {
        // (l l1 l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LL1LL (PrimitiveCombination2LL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LL1LA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LL1LL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LL1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1LL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1LA : PCond2LL1LL
    {
        // (l l1 l l1)

        protected PCond2LL1LA (PrimitiveCombination2LL1 predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LL1LA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LL1LA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LL1LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1LA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1LA0 : PCond2LL1LA
    {
        // (l l1 l a0)

        PCond2LL1LA0 (PrimitiveCombination2LL1 predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2LL1LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1LA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1LA1 : PCond2LL1LA
    {
        // (l l1 l a1)

        PCond2LL1LA1 (PrimitiveCombination2LL1 predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2LL1LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1LA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1LL1 : PCond2LL1LL
    {
        // (l l1 l l1)

        PCond2LL1LL1 (PrimitiveCombination2LL1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LL1LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1LL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1LQ : PCond2LL1L
    {
        // (l l1 l Q)
        public readonly object alternativeValue;
        PCond2LL1LQ (PrimitiveCombination2LL1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2LL1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1LQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1Q : PCond2LL1
    {
        // (l l1 q s)
        public readonly object consequentValue;

        protected PCond2LL1Q (PrimitiveCombination2LL1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LL1QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LL1QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LL1Q (predicate, consequent, alternative);
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
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1QL : PCond2LL1Q
    {
        // (l l1 q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LL1QL (PrimitiveCombination2LL1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LL1QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LL1QL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LL1QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1QL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1QA : PCond2LL1QL
    {
        // (l l1 q a)
        protected PCond2LL1QA (PrimitiveCombination2LL1 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LL1QA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LL1QA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LL1QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1QA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1QA0 : PCond2LL1QA
    {
        // (l l1 q a0)
        PCond2LL1QA0 (PrimitiveCombination2LL1 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2LL1QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1QA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1QA1 : PCond2LL1QA
    {
        // (l l1 q a1)
        PCond2LL1QA1 (PrimitiveCombination2LL1 predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2LL1QA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1QA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1QL1 : PCond2LL1QL
    {
        // (l l1 q l1)
        PCond2LL1QL1 (PrimitiveCombination2LL1 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LL1QL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1QL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1QQ : PCond2LL1Q
    {
        // (l l1 q q)
        public readonly object alternativeValue;

        PCond2LL1QQ (PrimitiveCombination2LL1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2LL1QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2LL1QQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1SL : PCond2LL1
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LL1SL (PrimitiveCombination2LL1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2LL1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LL1SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LL1SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LL1SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1SL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LL1SA : PCond2LL1SL
    {
        protected PCond2LL1SA (PrimitiveCombination2LL1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LL1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LL1SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LL1SA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LL1SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1SA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1SA0 : PCond2LL1SA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2LL1SA0 (PrimitiveCombination2LL1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LL1 predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2LL1SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2LL1SA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1SA1 : PCond2LL1SA
    {
        PCond2LL1SA1 (PrimitiveCombination2LL1 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LL1 predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2LL1SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1SA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1SL1 : PCond2LL1SL
    {
        PCond2LL1SL1 (PrimitiveCombination2LL1 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LL1 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2LL1SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1SL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LL1SQ : PCond2LL1
    {
        // (l l1 s q)
        public readonly object alternativeValue;

        PCond2LL1SQ (PrimitiveCombination2LL1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LL1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2LL1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LL1SQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQ : PCond2L
    {
        // (s q s s)
        public readonly object rand1Value;

        protected PCond2LQ (PrimitiveCombination2LQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2LQL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2LQQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LQSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LQSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("PCond2LQ.EvalStep");
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQL : PCond2LQ
    {
        // (s q l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2LQL (PrimitiveCombination2LQ predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2LQA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2LQL1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LQLL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LQLQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQA : PCond2LQL
    {
        // (s q a s)

        protected PCond2LQA (PrimitiveCombination2LQ predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2LQA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2LQA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2LQAL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LQAQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQAL : PCond2LQA
    {
        // (s q a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LQAL (PrimitiveCombination2LQ predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LQAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LQAL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LQAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQAL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQAA : PCond2LQAL
    {
        // (s q a l1)

        protected PCond2LQAA (PrimitiveCombination2LQ predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LQAA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LQAA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LQAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQAA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQAA0 : PCond2LQAA
    {
        // (s q a a0)

        PCond2LQAA0 (PrimitiveCombination2LQ predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2LQAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQAA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQAA1 : PCond2LQAA
    {
        // (s q a a1)

        PCond2LQAA1 (PrimitiveCombination2LQ predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2LQAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQAA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQAL1 : PCond2LQAL
    {
        // (s q a l1)

        PCond2LQAL1 (PrimitiveCombination2LQ predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LQAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQAL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQAQ : PCond2LQA
    {
        // (s q a Q)
        public readonly object alternativeValue;
        PCond2LQAQ (PrimitiveCombination2LQ predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2LQAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQAQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQA0 : PCond2LQA
    {
        // (s q a0 s)

        protected PCond2LQA0 (PrimitiveCombination2LQ predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LQA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LQA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQA0L : PCond2LQA0
    {
        // (s q a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LQA0L (PrimitiveCombination2LQ predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LQA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LQA0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LQA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQA0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQA0A : PCond2LQA0L
    {
        // (s q a0 l1)

        protected PCond2LQA0A (PrimitiveCombination2LQ predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LQA0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LQA0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LQA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQA0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQA0A0 : PCond2LQA0A
    {
        // (s q a0 a0)

        PCond2LQA0A0 (PrimitiveCombination2LQ predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2LQA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQA0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQA0A1 : PCond2LQA0A
    {
        // (s q a0 a1)

        PCond2LQA0A1 (PrimitiveCombination2LQ predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2LQA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQA0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQA0L1 : PCond2LQA0L
    {
        // (s q a0 l1)

        PCond2LQA0L1 (PrimitiveCombination2LQ predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LQA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQA0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQA0Q : PCond2LQA0
    {
        // (s q a0 Q)
        public readonly object alternativeValue;
        PCond2LQA0Q (PrimitiveCombination2LQ predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2LQA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQA0Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQA1 : PCond2LQA
    {
        // (s q a1 s)

        protected PCond2LQA1 (PrimitiveCombination2LQ predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LQA1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LQA1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQA1L : PCond2LQA1
    {
        // (s q a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LQA1L (PrimitiveCombination2LQ predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LQA1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LQA1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LQA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQA1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQA1A : PCond2LQA1L
    {
        // (s q a1 l1)

        protected PCond2LQA1A (PrimitiveCombination2LQ predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LQA1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LQA1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LQA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQA1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQA1A0 : PCond2LQA1A
    {
        // (s q a1 a0)

        PCond2LQA1A0 (PrimitiveCombination2LQ predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2LQA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQA1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQA1A1 : PCond2LQA1A
    {
        // (s q a1 a1)

        PCond2LQA1A1 (PrimitiveCombination2LQ predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2LQA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQA1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQA1L1 : PCond2LQA1L
    {
        // (s q a1 l1)

        PCond2LQA1L1 (PrimitiveCombination2LQ predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LQA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQA1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQA1Q : PCond2LQA1
    {
        // (s q a1 Q)
        public readonly object alternativeValue;
        PCond2LQA1Q (PrimitiveCombination2LQ predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2LQA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQA1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQL1 : PCond2LQL
    {
        // (s q l1 s)

        protected PCond2LQL1 (PrimitiveCombination2LQ predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LQL1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LQL1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQL1L : PCond2LQL1
    {
        // (s q l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LQL1L (PrimitiveCombination2LQ predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LQL1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LQL1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LQL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQL1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQL1A : PCond2LQL1L
    {
        // (s q l1 l1)

        protected PCond2LQL1A (PrimitiveCombination2LQ predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LQL1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LQL1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LQL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQL1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQL1A0 : PCond2LQL1A
    {
        // (s q l1 a0)

        PCond2LQL1A0 (PrimitiveCombination2LQ predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2LQL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQL1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQL1A1 : PCond2LQL1A
    {
        // (s q l1 a1)

        PCond2LQL1A1 (PrimitiveCombination2LQ predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2LQL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQL1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQL1L1 : PCond2LQL1L
    {
        // (s q l1 l1)

        PCond2LQL1L1 (PrimitiveCombination2LQ predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LQL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQL1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQL1Q : PCond2LQL1
    {
        // (s q l1 Q)
        public readonly object alternativeValue;
        PCond2LQL1Q (PrimitiveCombination2LQ predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2LQL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQL1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQLL : PCond2LQL
    {
        // (s q l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2LQLL (PrimitiveCombination2LQ predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LQLA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LQLL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LQLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQLL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQLA : PCond2LQLL
    {
        // (s q l l1)

        protected PCond2LQLA (PrimitiveCombination2LQ predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LQLA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LQLA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LQLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQLA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQLA0 : PCond2LQLA
    {
        // (s q l a0)

        PCond2LQLA0 (PrimitiveCombination2LQ predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2LQLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQLA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQLA1 : PCond2LQLA
    {
        // (s q l a1)

        PCond2LQLA1 (PrimitiveCombination2LQ predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2LQLA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQLA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQLL1 : PCond2LQLL
    {
        // (s q l l1)

        PCond2LQLL1 (PrimitiveCombination2LQ predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LQLL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQLL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQLQ : PCond2LQL
    {
        // (s q l Q)
        public readonly object alternativeValue;
        PCond2LQLQ (PrimitiveCombination2LQ predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2LQLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQLQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQQ : PCond2LQ
    {
        // (s q q s)
        public readonly object consequentValue;

        protected PCond2LQQ (PrimitiveCombination2LQ predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2LQQL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2LQQQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2LQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQQL : PCond2LQQ
    {
        // (s q q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LQQL (PrimitiveCombination2LQ predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LQQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LQQL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LQQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQQL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQQA : PCond2LQQL
    {
        // (s q q a)
        protected PCond2LQQA (PrimitiveCombination2LQ predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LQQA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LQQA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LQQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQQA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQQA0 : PCond2LQQA
    {
        // (s q q a0)
        PCond2LQQA0 (PrimitiveCombination2LQ predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2LQQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQQA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQQA1 : PCond2LQQA
    {
        // (s q q a1)
        PCond2LQQA1 (PrimitiveCombination2LQ predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2LQQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQQA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQQL1 : PCond2LQQL
    {
        // (s q q l1)
        PCond2LQQL1 (PrimitiveCombination2LQ predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2LQQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQQL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQQQ : PCond2LQQ
    {
        // (s q q q)
        public readonly object alternativeValue;

        PCond2LQQQ (PrimitiveCombination2LQ predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2LQ predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2LQQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2LQQQ.EvalStep";
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQSL : PCond2LQ
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2LQSL (PrimitiveCombination2LQ predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2LQ predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2LQSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2LQSL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2LQSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQSL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    class PCond2LQSA : PCond2LQSL
    {
        protected PCond2LQSA (PrimitiveCombination2LQ predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LQ predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2LQSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2LQSA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2LQSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQSA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQSA0 : PCond2LQSA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2LQSA0 (PrimitiveCombination2LQ predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LQ predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2LQSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2LQSA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQSA1 : PCond2LQSA
    {
        PCond2LQSA1 (PrimitiveCombination2LQ predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LQ predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2LQSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQSA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQSL1 : PCond2LQSL
    {
        PCond2LQSL1 (PrimitiveCombination2LQ predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2LQ predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2LQSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQSL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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

    sealed class PCond2LQSQ : PCond2LQ
    {
        public readonly object alternativeValue;

        PCond2LQSQ (PrimitiveCombination2LQ predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        static public SCode Make (PrimitiveCombination2LQ predicate, SCode consequent, Quotation alternative)
        {
            return new PCond2LQSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2LQQQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString();
#endif
            object predValue;

            object ev0;
	    if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
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
