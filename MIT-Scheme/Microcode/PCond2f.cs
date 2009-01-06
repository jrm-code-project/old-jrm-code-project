using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    class PCond2A0 : PCond2L
    {
        // (a0 s s s)
        protected PCond2A0 (PrimitiveCombination2A0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveCombination2A0L) ? PCond2A0L.Make ((PrimitiveCombination2A0L) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2A0Q) ? PCond2A0Q.Make ((PrimitiveCombination2A0Q) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCond2A0SL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2A0SQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0SSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0SSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2A0.EvalStep");
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SL : PCond2A0
    {
        // (a0 s l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2A0SL (PrimitiveCombination2A0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2A0SA.Make (predicate, (Argument) consequent, alternative)
                : (consequent is LexicalVariable1) ? PCond2A0SL1.Make (predicate, (LexicalVariable1) consequent, alternative)
                : (alternative is LexicalVariable) ? PCond2A0SLL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2A0SLQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2A0SL (predicate, consequent, alternative);
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



#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SLL : PCond2A0SL
    {
        // (a0 s l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0SLL (PrimitiveCombination2A0 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0SLA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2A0SLL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2A0SLL (predicate, consequent, alternative);
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



#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SLA : PCond2A0SLL
    {
        // (a0 s l a)
        protected PCond2A0SLA (PrimitiveCombination2A0 predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0SLA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2A0SLA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2A0SLA (predicate, consequent, alternative);
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



#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SLA0 : PCond2A0SLA
    {
        // (a0 s l a0)
        PCond2A0SLA0 (PrimitiveCombination2A0 predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2A0SLA0 (predicate, consequent, alternative);
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



#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SLA1 : PCond2A0SLA
    {
        // (a0 s l a1)
        PCond2A0SLA1 (PrimitiveCombination2A0 predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2A0SLA1 (predicate, consequent, alternative);
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



#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SLL1 : PCond2A0SLL
    {
        // (a0 s l l1)
        PCond2A0SLL1 (PrimitiveCombination2A0 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0SLL1 (predicate, consequent, alternative);
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



#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SLQ : PCond2A0SL
    {
        // (a0 s l q)
        public readonly object alternativeValue;

        PCond2A0SLQ (PrimitiveCombination2A0 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2A0SLQ (predicate, consequent, alternative);
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



#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SA : PCond2A0SL
    {
        // (a0 s a s)
        protected PCond2A0SA (PrimitiveCombination2A0 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2A0SA0.Make (predicate, (Argument0) consequent, alternative)
                : (consequent is Argument1) ? PCond2A0SA1.Make (predicate, (Argument1) consequent, alternative)
                : (alternative is LexicalVariable) ? PCond2A0SAL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2A0SAQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2A0SA (predicate, consequent, alternative);
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



#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SAL : PCond2A0SA
    {
        // (a0 s a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0SAL (PrimitiveCombination2A0 predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0SAA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2A0SAL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2A0SAL (predicate, consequent, alternative);
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



#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SAA : PCond2A0SAL
    {
        // (a0 s a a)
        protected PCond2A0SAA (PrimitiveCombination2A0 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0SAA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2A0SAA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2A0SAA (predicate, consequent, alternative);
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



#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SAA0 : PCond2A0SAA
    {
        // (a0 s a a0)
        PCond2A0SAA0 (PrimitiveCombination2A0 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2A0SAA0 (predicate, consequent, alternative);
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



#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SAA1 : PCond2A0SAA
    {
        // (a0 s a a1)
        PCond2A0SAA1 (PrimitiveCombination2A0 predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2A0SAA1 (predicate, consequent, alternative);
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



#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SAL1 : PCond2A0SAL
    {
        // (a0 s a l1)
        PCond2A0SAL1 (PrimitiveCombination2A0 predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0SAL1 (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SAQ : PCond2A0SA
    {
        // (a0 s a q)
        public readonly object alternativeValue;

        PCond2A0SAQ (PrimitiveCombination2A0 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2A0SAQ (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SA0 : PCond2A0SA
    {
        // (a0 s a0 s)
        protected PCond2A0SA0 (PrimitiveCombination2A0 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0SA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0SA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2A0SA0.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SA0L : PCond2A0SA0
    {
        // (a0 s a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0SA0L (PrimitiveCombination2A0 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0SA0A.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2A0SA0L1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2A0SA0L (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SA0A : PCond2A0SA0L
    {
        // (a0 s a0 a)
        protected PCond2A0SA0A (PrimitiveCombination2A0 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0SA0A0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2A0SA0A1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2A0SA0A (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SA0A0 : PCond2A0SA0A
    {
        // (a0 s a0 a0)
        PCond2A0SA0A0 (PrimitiveCombination2A0 predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2A0SA0A0 (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SA0A1 : PCond2A0SA0A
    {
        // (a0 s a0 a1)
        PCond2A0SA0A1 (PrimitiveCombination2A0 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2A0SA0A1 (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SA0L1 : PCond2A0SA0L
    {
        // (a0 s a0 l1)
        PCond2A0SA0L1 (PrimitiveCombination2A0 predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0SA0L1 (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SA0Q : PCond2A0SA0
    {
        // (a0 s a0 q)
        public readonly object alternativeValue;

        PCond2A0SA0Q (PrimitiveCombination2A0 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2A0SA0Q (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SA1 : PCond2A0SA
    {
        // (a0 s a1 s)
        protected PCond2A0SA1 (PrimitiveCombination2A0 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0SA1L.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2A0SA1Q.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2A0SA1 (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SA1L : PCond2A0SA1
    {
        // (a0 s a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0SA1L (PrimitiveCombination2A0 predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0SA1A.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2A0SA1L1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2A0SA1L (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SA1A : PCond2A0SA1L
    {
        // (a0 s a1 a)
        protected PCond2A0SA1A (PrimitiveCombination2A0 predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0SA1A0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2A0SA1A1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2A0SA1A (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SA1A0 : PCond2A0SA1A
    {
        // (a0 s a1 a0)
        PCond2A0SA1A0 (PrimitiveCombination2A0 predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2A0SA1A0 (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SA1A1 : PCond2A0SA1A
    {
        // (a0 s a1 a1)
        PCond2A0SA1A1 (PrimitiveCombination2A0 predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2A0SA1A1 (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SA1L1 : PCond2A0SA1L
    {
        // (a0 s a1 l1)
        PCond2A0SA1L1 (PrimitiveCombination2A0 predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0SA1L1 (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SA1Q : PCond2A0SA1
    {
        // (a0 s a1 q)
        public readonly object alternativeValue;

        PCond2A0SA1Q (PrimitiveCombination2A0 predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2A0SA1Q (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SL1 : PCond2A0SL
    {
        // (a0 s l1 s)
        protected PCond2A0SL1 (PrimitiveCombination2A0 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0SL1L.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2A0SL1Q.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2A0SL1 (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SL1L : PCond2A0SL1
    {
        // (a0 s l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0SL1L (PrimitiveCombination2A0 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0SL1A.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2A0SL1L1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2A0SL1L (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SL1A : PCond2A0SL1L
    {
        // (a0 s l1 a)
        protected PCond2A0SL1A (PrimitiveCombination2A0 predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0SL1A0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2A0SL1A1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2A0SL1A (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SL1A0 : PCond2A0SL1A
    {
        // (a0 s l1 a0)
        PCond2A0SL1A0 (PrimitiveCombination2A0 predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2A0SL1A0 (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SL1A1 : PCond2A0SL1A
    {
        // (a0 s l1 a1)
        PCond2A0SL1A1 (PrimitiveCombination2A0 predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2A0SL1A1 (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SL1L1 : PCond2A0SL1L
    {
        // (a0 s l1 l1)
        PCond2A0SL1L1 (PrimitiveCombination2A0 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0SL1L1 (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SL1Q : PCond2A0SL1
    {
        // (a0 s l1 q)
        public readonly object alternativeValue;

        PCond2A0SL1Q (PrimitiveCombination2A0 predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2A0SL1Q (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SQ : PCond2A0
    {
        // (a0 s q s)
        public readonly object consequentValue;

        protected PCond2A0SQ (PrimitiveCombination2A0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0SQL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2A0SQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2A0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2A0SQ.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SQL : PCond2A0SQ
    {
        // (a0 s q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0SQL (PrimitiveCombination2A0 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0SQA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2A0SQL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2A0SQL (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SQA : PCond2A0SQL
    {
        // (a0 s q a)
        protected PCond2A0SQA (PrimitiveCombination2A0 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0SQA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2A0SQA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2A0SQA (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SQA0 : PCond2A0SQA
    {
        // (a0 s q a0)
        PCond2A0SQA0 (PrimitiveCombination2A0 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2A0SQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2A0SQA0.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            answer = ((answer is bool) && (bool) answer == false) ? ev0 : this.consequentValue;
            return false;
        }
    }

    sealed class PCond2A0SQA1 : PCond2A0SQA
    {
        // (a0 s q a1)
        PCond2A0SQA1 (PrimitiveCombination2A0 predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2A0SQA1 (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SQL1 : PCond2A0SQL
    {
        // (a0 s q l1)
        PCond2A0SQL1 (PrimitiveCombination2A0 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0SQL1 (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SQQ : PCond2A0SQ
    {
        // (a0 s q q)
        public readonly object alternativeValue;

        PCond2A0SQQ (PrimitiveCombination2A0 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2A0SQQ (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SSL : PCond2A0
    {
        // (a0 s s l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0SSL (PrimitiveCombination2A0 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0SSA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2A0SSL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2A0SSL (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0SSA : PCond2A0SSL
    {
        // (a0 s s a)
        protected PCond2A0SSA (PrimitiveCombination2A0 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0SSA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2A0SSA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2A0SSA (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SSA0 : PCond2A0SSA
    {
        // (a0 s s a0)
        PCond2A0SSA0 (PrimitiveCombination2A0 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond2A0SSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2A0SSA0.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SSA1 : PCond2A0SSA
    {
        // (a0 s s a1)
        PCond2A0SSA1 (PrimitiveCombination2A0 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCond2A0SSA1 (predicate, consequent, alternative);
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

            


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SSL1 : PCond2A0SSL
    {
        // (a0 s s l1)
        PCond2A0SSL1 (PrimitiveCombination2A0 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0SSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2A0SSL1.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0SSQ : PCond2A0
    {
        // (a0 s s q)
        public readonly object alternativeValue;

        PCond2A0SSQ (PrimitiveCombination2A0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2A0SSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2A0SSQ.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L : PCond2A0
    {
        // (a0 l s s)
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCond2A0L (PrimitiveCombination2A0L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveCombination2A0A) ? PCond2A0A.Make ((PrimitiveCombination2A0A) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2A0L1) ? PCond2A0L1.Make ((PrimitiveCombination2A0L1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCond2A0LL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2A0LQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0LSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0LSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2A0L.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0LL : PCond2A0L
    {
        // (a0 l l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2A0LL (PrimitiveCombination2A0L predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2A0LA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2A0LL1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0LLL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0LLQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0LA : PCond2A0LL
    {
        // (a0 l a s)

        protected PCond2A0LA (PrimitiveCombination2A0L predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2A0LA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2A0LA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0LAL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0LAQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0LAL : PCond2A0LA
    {
        // (a0 l a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0LAL (PrimitiveCombination2A0L predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0LAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0LAL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0LAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LAL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0LAA : PCond2A0LAL
    {
        // (a0 l a l1)

        protected PCond2A0LAA (PrimitiveCombination2A0L predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0LAA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0LAA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0LAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LAA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0LAA0 : PCond2A0LAA
    {
        // (a0 l a a0)

        PCond2A0LAA0 (PrimitiveCombination2A0L predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2A0LAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LAA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0LAA1 : PCond2A0LAA
    {
        // (a0 l a a1)

        PCond2A0LAA1 (PrimitiveCombination2A0L predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2A0LAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LAA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0LAL1 : PCond2A0LAL
    {
        // (a0 l a l1)

        PCond2A0LAL1 (PrimitiveCombination2A0L predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0LAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LAL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0LAQ : PCond2A0LA
    {
        // (a0 l a Q)
        public readonly object alternativeValue;
        PCond2A0LAQ (PrimitiveCombination2A0L predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2A0LAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LAQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    class PCond2A0LA0 : PCond2A0LA
    {
        // (a0 l a0 s)

        protected PCond2A0LA0 (PrimitiveCombination2A0L predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0LA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0LA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0LA0L : PCond2A0LA0
    {
        // (a0 l a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0LA0L (PrimitiveCombination2A0L predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0LA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0LA0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0LA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LA0L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0LA0A : PCond2A0LA0L
    {
        // (a0 l a0 l1)

        protected PCond2A0LA0A (PrimitiveCombination2A0L predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0LA0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0LA0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0LA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LA0A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0LA0A0 : PCond2A0LA0A
    {
        // (a0 l a0 a0)

        PCond2A0LA0A0 (PrimitiveCombination2A0L predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2A0LA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LA0A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0LA0A1 : PCond2A0LA0A
    {
        // (a0 l a0 a1)

        PCond2A0LA0A1 (PrimitiveCombination2A0L predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2A0LA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LA0A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0LA0L1 : PCond2A0LA0L
    {
        // (a0 l a0 l1)

        PCond2A0LA0L1 (PrimitiveCombination2A0L predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0LA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LA0L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0LA0Q : PCond2A0LA0
    {
        // (a0 l a0 Q)
        public readonly object alternativeValue;
        PCond2A0LA0Q (PrimitiveCombination2A0L predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2A0LA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LA0Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    class PCond2A0LA1 : PCond2A0LA
    {
        // (a0 l a1 s)

        protected PCond2A0LA1 (PrimitiveCombination2A0L predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0LA1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0LA1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0LA1L : PCond2A0LA1
    {
        // (a0 l a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0LA1L (PrimitiveCombination2A0L predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0LA1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0LA1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0LA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LA1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0LA1A : PCond2A0LA1L
    {
        // (a0 l a1 l1)

        protected PCond2A0LA1A (PrimitiveCombination2A0L predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0LA1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0LA1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0LA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LA1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0LA1A0 : PCond2A0LA1A
    {
        // (a0 l a1 a0)

        PCond2A0LA1A0 (PrimitiveCombination2A0L predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2A0LA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LA1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0LA1A1 : PCond2A0LA1A
    {
        // (a0 l a1 a1)

        PCond2A0LA1A1 (PrimitiveCombination2A0L predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2A0LA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LA1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0LA1L1 : PCond2A0LA1L
    {
        // (a0 l a1 l1)

        PCond2A0LA1L1 (PrimitiveCombination2A0L predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0LA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LA1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0LA1Q : PCond2A0LA1
    {
        // (a0 l a1 Q)
        public readonly object alternativeValue;
        PCond2A0LA1Q (PrimitiveCombination2A0L predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2A0LA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LA1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    class PCond2A0LL1 : PCond2A0LL
    {
        // (a0 l l1 s)

        protected PCond2A0LL1 (PrimitiveCombination2A0L predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0LL1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0LL1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0LL1L : PCond2A0LL1
    {
        // (a0 l l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0LL1L (PrimitiveCombination2A0L predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0LL1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0LL1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0LL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LL1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0LL1A : PCond2A0LL1L
    {
        // (a0 l l1 l1)

        protected PCond2A0LL1A (PrimitiveCombination2A0L predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0LL1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0LL1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0LL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LL1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0LL1A0 : PCond2A0LL1A
    {
        // (a0 l l1 a0)

        PCond2A0LL1A0 (PrimitiveCombination2A0L predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2A0LL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LL1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0LL1A1 : PCond2A0LL1A
    {
        // (a0 l l1 a1)

        PCond2A0LL1A1 (PrimitiveCombination2A0L predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2A0LL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LL1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0LL1L1 : PCond2A0LL1L
    {
        // (a0 l l1 l1)

        PCond2A0LL1L1 (PrimitiveCombination2A0L predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0LL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LL1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0LL1Q : PCond2A0LL1
    {
        // (a0 l l1 Q)
        public readonly object alternativeValue;
        PCond2A0LL1Q (PrimitiveCombination2A0L predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2A0LL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LL1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0LLL : PCond2A0LL
    {
        // (a0 l l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0LLL (PrimitiveCombination2A0L predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0LLA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0LLL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0LLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LLL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0LLA : PCond2A0LLL
    {
        // (a0 l l l1)

        protected PCond2A0LLA (PrimitiveCombination2A0L predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0LLA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0LLA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0LLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LLA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0LLA0 : PCond2A0LLA
    {
        // (a0 l l a0)

        PCond2A0LLA0 (PrimitiveCombination2A0L predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2A0LLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LLA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0LLA1 : PCond2A0LLA
    {
        // (a0 l l a1)

        PCond2A0LLA1 (PrimitiveCombination2A0L predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2A0LLA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LLA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0LLL1 : PCond2A0LLL
    {
        // (a0 l l l1)

        PCond2A0LLL1 (PrimitiveCombination2A0L predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0LLL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LLL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0LLQ : PCond2A0LL
    {
        // (a0 l l Q)
        public readonly object alternativeValue;
        PCond2A0LLQ (PrimitiveCombination2A0L predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2A0LLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LLQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0LQ : PCond2A0L
    {
        // (a0 l q s)
        public readonly object consequentValue;

        protected PCond2A0LQ (PrimitiveCombination2A0L predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0LQL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0LQQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0LQ (predicate, consequent, alternative);
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


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0LQL : PCond2A0LQ
    {
        // (a0 l q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0LQL (PrimitiveCombination2A0L predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0LQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0LQL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0LQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LQL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0LQA : PCond2A0LQL
    {
        // (a0 l q a)
        protected PCond2A0LQA (PrimitiveCombination2A0L predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0LQA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0LQA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0LQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LQA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0LQA0 : PCond2A0LQA
    {
        // (a0 l q a0)
        PCond2A0LQA0 (PrimitiveCombination2A0L predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2A0LQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LQA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0LQA1 : PCond2A0LQA
    {
        // (a0 l q a1)
        PCond2A0LQA1 (PrimitiveCombination2A0L predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2A0LQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LQA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0LQL1 : PCond2A0LQL
    {
        // (a0 l q l1)
        PCond2A0LQL1 (PrimitiveCombination2A0L predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0LQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LQL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0LQQ : PCond2A0LQ
    {
        // (a0 l q q)
        public readonly object alternativeValue;

        PCond2A0LQQ (PrimitiveCombination2A0L predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2A0LQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2A0LQQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();



#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    class PCond2A0LSL : PCond2A0L
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0LSL (PrimitiveCombination2A0L predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2A0L predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0LSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0LSL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0LSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LSL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0LSA : PCond2A0LSL
    {
        protected PCond2A0LSA (PrimitiveCombination2A0L predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0L predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0LSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0LSA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0LSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LSA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0LSA0 : PCond2A0LSA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2A0LSA0 (PrimitiveCombination2A0L predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0L predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2A0LSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0LSA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0LSA1 : PCond2A0LSA
    {
        PCond2A0LSA1 (PrimitiveCombination2A0L predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0L predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2A0LSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LSA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0LSL1 : PCond2A0LSL
    {
        PCond2A0LSL1 (PrimitiveCombination2A0L predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0L predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2A0LSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LSL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0LSQ : PCond2A0L
    {
        // (a0 l s q)
        public readonly object alternativeValue;

        PCond2A0LSQ (PrimitiveCombination2A0L predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0L predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2A0LSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0LSQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A : PCond2A0L
    {
        // (s a s s)
        protected PCond2A0A (PrimitiveCombination2A0A predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveCombination2A0A0) ? PCond2A0A0.Make ((PrimitiveCombination2A0A0) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2A0A1) ? PCond2A0A1.Make ((PrimitiveCombination2A0A1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCond2A0AL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2A0AQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0ASL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0ASQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG

            Warm ("PCond2A0A.EvalStep");
            Primitive.hotPrimitives.Note (this.procedure);
#endif

            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0AL : PCond2A0A
    {
        // (s a l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2A0AL (PrimitiveCombination2A0A predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2A0AA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2A0AL1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0ALL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0ALQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif

            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0AA : PCond2A0AL
    {
        // (s a a s)

        protected PCond2A0AA (PrimitiveCombination2A0A predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2A0AA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2A0AA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0AAL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0AAQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0AAL : PCond2A0AA
    {
        // (s a a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0AAL (PrimitiveCombination2A0A predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0AAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0AAL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0AAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AAL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0AAA : PCond2A0AAL
    {
        // (s a a l1)

        protected PCond2A0AAA (PrimitiveCombination2A0A predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0AAA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0AAA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0AAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AAA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2A0AAA0 : PCond2A0AAA
    {
        // (s a a a0)

        PCond2A0AAA0 (PrimitiveCombination2A0A predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2A0AAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AAA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2A0AAA1 : PCond2A0AAA
    {
        // (s a a a1)

        PCond2A0AAA1 (PrimitiveCombination2A0A predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2A0AAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AAA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2A0AAL1 : PCond2A0AAL
    {
        // (s a a l1)

        PCond2A0AAL1 (PrimitiveCombination2A0A predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0AAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AAL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0AAQ : PCond2A0AA
    {
        // (s a a Q)
        public readonly object alternativeValue;
        PCond2A0AAQ (PrimitiveCombination2A0A predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2A0AAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AAQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
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

    class PCond2A0AA0 : PCond2A0AA
    {
        // (s a a0 s)

        protected PCond2A0AA0 (PrimitiveCombination2A0A predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0AA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0AA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0AA0L : PCond2A0AA0
    {
        // (s a a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0AA0L (PrimitiveCombination2A0A predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0AA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0AA0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0AA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AA0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0AA0A : PCond2A0AA0L
    {
        // (s a a0 l1)

        protected PCond2A0AA0A (PrimitiveCombination2A0A predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0AA0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0AA0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0AA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AA0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2A0AA0A0 : PCond2A0AA0A
    {
        // (s a a0 a0)

        PCond2A0AA0A0 (PrimitiveCombination2A0A predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2A0AA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AA0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2A0AA0A1 : PCond2A0AA0A
    {
        // (s a a0 a1)

        PCond2A0AA0A1 (PrimitiveCombination2A0A predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2A0AA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AA0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2A0AA0L1 : PCond2A0AA0L
    {
        // (s a a0 l1)

        PCond2A0AA0L1 (PrimitiveCombination2A0A predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0AA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AA0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0AA0Q : PCond2A0AA0
    {
        // (s a a0 Q)
        public readonly object alternativeValue;
        PCond2A0AA0Q (PrimitiveCombination2A0A predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2A0AA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AA0Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
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

    class PCond2A0AA1 : PCond2A0AA
    {
        // (s a a1 s)

        protected PCond2A0AA1 (PrimitiveCombination2A0A predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0AA1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0AA1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0AA1L : PCond2A0AA1
    {
        // (s a a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0AA1L (PrimitiveCombination2A0A predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0AA1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0AA1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0AA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AA1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0AA1A : PCond2A0AA1L
    {
        // (s a a1 l1)

        protected PCond2A0AA1A (PrimitiveCombination2A0A predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0AA1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0AA1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0AA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AA1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2A0AA1A0 : PCond2A0AA1A
    {
        // (s a a1 a0)

        PCond2A0AA1A0 (PrimitiveCombination2A0A predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2A0AA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AA1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2A0AA1A1 : PCond2A0AA1A
    {
        // (s a a1 a1)

        PCond2A0AA1A1 (PrimitiveCombination2A0A predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2A0AA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AA1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2A0AA1L1 : PCond2A0AA1L
    {
        // (s a a1 l1)

        PCond2A0AA1L1 (PrimitiveCombination2A0A predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0AA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AA1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0AA1Q : PCond2A0AA1
    {
        // (s a a1 Q)
        public readonly object alternativeValue;
        PCond2A0AA1Q (PrimitiveCombination2A0A predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2A0AA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AA1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
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

    class PCond2A0AL1 : PCond2A0AL
    {
        // (s a l1 s)

        protected PCond2A0AL1 (PrimitiveCombination2A0A predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0AL1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0AL1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0AL1L : PCond2A0AL1
    {
        // (s a l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0AL1L (PrimitiveCombination2A0A predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0AL1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0AL1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0AL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AL1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0AL1A : PCond2A0AL1L
    {
        // (s a l1 l1)

        protected PCond2A0AL1A (PrimitiveCombination2A0A predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0AL1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0AL1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0AL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AL1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0AL1A0 : PCond2A0AL1A
    {
        // (s a l1 a0)

        PCond2A0AL1A0 (PrimitiveCombination2A0A predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2A0AL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AL1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0AL1A1 : PCond2A0AL1A
    {
        // (s a l1 a1)

        PCond2A0AL1A1 (PrimitiveCombination2A0A predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2A0AL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AL1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0AL1L1 : PCond2A0AL1L
    {
        // (s a l1 l1)

        PCond2A0AL1L1 (PrimitiveCombination2A0A predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0AL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AL1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0AL1Q : PCond2A0AL1
    {
        // (s a l1 Q)
        public readonly object alternativeValue;
        PCond2A0AL1Q (PrimitiveCombination2A0A predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2A0AL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AL1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0ALL : PCond2A0AL
    {
        // (s a l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0ALL (PrimitiveCombination2A0A predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0ALA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0ALL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0ALL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0ALL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0ALA : PCond2A0ALL
    {
        // (s a l l1)

        protected PCond2A0ALA (PrimitiveCombination2A0A predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0ALA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0ALA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0ALA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0ALA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0ALA0 : PCond2A0ALA
    {
        // (s a l a0)

        PCond2A0ALA0 (PrimitiveCombination2A0A predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2A0ALA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0ALA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0ALA1 : PCond2A0ALA
    {
        // (s a l a1)

        PCond2A0ALA1 (PrimitiveCombination2A0A predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2A0ALA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0ALA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0ALL1 : PCond2A0ALL
    {
        // (s a l l1)

        PCond2A0ALL1 (PrimitiveCombination2A0A predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0ALL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0ALL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0ALQ : PCond2A0AL
    {
        // (s a l Q)
        public readonly object alternativeValue;
        PCond2A0ALQ (PrimitiveCombination2A0A predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2A0ALQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0ALQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0AQ : PCond2A0A
    {
        // (s a q s)
        public readonly object consequentValue;

        protected PCond2A0AQ (PrimitiveCombination2A0A predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0AQL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0AQQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0AQL : PCond2A0AQ
    {
        // (s a q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0AQL (PrimitiveCombination2A0A predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0AQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0AQL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0AQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AQL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0AQA : PCond2A0AQL
    {
        // (s a q a)
        protected PCond2A0AQA (PrimitiveCombination2A0A predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0AQA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0AQA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0AQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AQA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2A0AQA0 : PCond2A0AQA
    {
        // (s a q a0)
        PCond2A0AQA0 (PrimitiveCombination2A0A predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2A0AQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AQA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2A0AQA1 : PCond2A0AQA
    {
        // (s a q a1)
        PCond2A0AQA1 (PrimitiveCombination2A0A predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2A0AQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AQA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2A0AQL1 : PCond2A0AQL
    {
        // (s a q l1)
        PCond2A0AQL1 (PrimitiveCombination2A0A predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0AQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0AQL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0AQQ : PCond2A0AQ
    {
        // (s a q q)
        public readonly object alternativeValue;

        PCond2A0AQQ (PrimitiveCombination2A0A predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2A0AQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2A0AQQ.EvalStep";
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
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

    class PCond2A0ASL : PCond2A0A
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0ASL (PrimitiveCombination2A0A predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2A0A predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0ASA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0ASL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0ASL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0ASL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0ASA : PCond2A0ASL
    {
        protected PCond2A0ASA (PrimitiveCombination2A0A predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0A predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0ASA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0ASA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0ASA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0ASA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0ASA0 : PCond2A0ASA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2A0ASA0 (PrimitiveCombination2A0A predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0A predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2A0ASA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0ASA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0ASA1 : PCond2A0ASA
    {
        PCond2A0ASA1 (PrimitiveCombination2A0A predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0A predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2A0ASA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0ASA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0ASL1 : PCond2A0ASL
    {
        PCond2A0ASL1 (PrimitiveCombination2A0A predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0A predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2A0ASL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0ASL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0ASQ : PCond2A0A
    {
        // (s a s q)
        public readonly object alternativeValue;

        PCond2A0ASQ (PrimitiveCombination2A0A predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2A0ASQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0ASQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A0 : PCond2A0A
    {
        // (s a0 s s)
        protected PCond2A0A0 (PrimitiveCombination2A0A0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2A0A0L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2A0A0Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0A0SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0A0SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2A0A0.EvalStep");
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A0L : PCond2A0A0
    {
        // (s a0 l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2A0A0L (PrimitiveCombination2A0A0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2A0A0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2A0A0L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0A0LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0A0LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A0A : PCond2A0A0L
    {
        // (s a0 a s)

        protected PCond2A0A0A (PrimitiveCombination2A0A0 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2A0A0A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2A0A0A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0A0AL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0A0AQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A0AL : PCond2A0A0A
    {
        // (s a0 a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0A0AL (PrimitiveCombination2A0A0 predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0A0AA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0A0AL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0A0AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0AL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A0AA : PCond2A0A0AL
    {
        // (s a0 a l1)

        protected PCond2A0A0AA (PrimitiveCombination2A0A0 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0A0AA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0A0AA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0A0AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0AA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
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

    sealed class PCond2A0A0AA0 : PCond2A0A0AA
    {
        // (s a0 a a0)

        PCond2A0A0AA0 (PrimitiveCombination2A0A0 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2A0A0AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0AA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
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

    sealed class PCond2A0A0AA1 : PCond2A0A0AA
    {
        // (s a0 a a1)

        PCond2A0A0AA1 (PrimitiveCombination2A0A0 predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2A0A0AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0AA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
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

    sealed class PCond2A0A0AL1 : PCond2A0A0AL
    {
        // (s a0 a l1)

        PCond2A0A0AL1 (PrimitiveCombination2A0A0 predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0A0AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0AL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A0AQ : PCond2A0A0A
    {
        // (s a0 a Q)
        public readonly object alternativeValue;
        PCond2A0A0AQ (PrimitiveCombination2A0A0 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2A0A0AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0AQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
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

    class PCond2A0A0A0 : PCond2A0A0A
    {
        // (s a0 a0 s)

        protected PCond2A0A0A0 (PrimitiveCombination2A0A0 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0A0A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0A0A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A0A0L : PCond2A0A0A0
    {
        // (s a0 a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0A0A0L (PrimitiveCombination2A0A0 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0A0A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0A0A0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0A0A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0A0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A0A0A : PCond2A0A0A0L
    {
        // (s a0 a0 l1)

        protected PCond2A0A0A0A (PrimitiveCombination2A0A0 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0A0A0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0A0A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0A0A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0A0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
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

    sealed class PCond2A0A0A0A0 : PCond2A0A0A0A
    {
        // (s a0 a0 a0)

        PCond2A0A0A0A0 (PrimitiveCombination2A0A0 predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2A0A0A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0A0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
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

    sealed class PCond2A0A0A0A1 : PCond2A0A0A0A
    {
        // (s a0 a0 a1)

        PCond2A0A0A0A1 (PrimitiveCombination2A0A0 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2A0A0A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0A0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
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

    sealed class PCond2A0A0A0L1 : PCond2A0A0A0L
    {
        // (s a0 a0 l1)

        PCond2A0A0A0L1 (PrimitiveCombination2A0A0 predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0A0A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0A0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A0A0Q : PCond2A0A0A0
    {
        // (s a0 a0 Q)
        public readonly object alternativeValue;
        PCond2A0A0A0Q (PrimitiveCombination2A0A0 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2A0A0A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0A0Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
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

    class PCond2A0A0A1 : PCond2A0A0A
    {
        // (s a0 a1 s)

        protected PCond2A0A0A1 (PrimitiveCombination2A0A0 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0A0A1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0A0A1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A0A1L : PCond2A0A0A1
    {
        // (s a0 a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0A0A1L (PrimitiveCombination2A0A0 predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0A0A1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0A0A1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0A0A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0A1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A0A1A : PCond2A0A0A1L
    {
        // (s a0 a1 l1)

        protected PCond2A0A0A1A (PrimitiveCombination2A0A0 predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0A0A1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0A0A1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0A0A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0A1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
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

    sealed class PCond2A0A0A1A0 : PCond2A0A0A1A
    {
        // (s a0 a1 a0)

        PCond2A0A0A1A0 (PrimitiveCombination2A0A0 predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2A0A0A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0A1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
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

    sealed class PCond2A0A0A1A1 : PCond2A0A0A1A
    {
        // (s a0 a1 a1)

        PCond2A0A0A1A1 (PrimitiveCombination2A0A0 predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2A0A0A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0A1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
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

    sealed class PCond2A0A0A1L1 : PCond2A0A0A1L
    {
        // (s a0 a1 l1)

        PCond2A0A0A1L1 (PrimitiveCombination2A0A0 predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0A0A1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0A1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A0A1Q : PCond2A0A0A1
    {
        // (s a0 a1 Q)
        public readonly object alternativeValue;
        PCond2A0A0A1Q (PrimitiveCombination2A0A0 predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2A0A0A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0A1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
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

    class PCond2A0A0L1 : PCond2A0A0L
    {
        // (s a0 l1 s)

        protected PCond2A0A0L1 (PrimitiveCombination2A0A0 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0A0L1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0A0L1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A0L1L : PCond2A0A0L1
    {
        // (s a0 l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0A0L1L (PrimitiveCombination2A0A0 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0A0L1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0A0L1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0A0L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0L1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A0L1A : PCond2A0A0L1L
    {
        // (s a0 l1 l1)

        protected PCond2A0A0L1A (PrimitiveCombination2A0A0 predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0A0L1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0A0L1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0A0L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0L1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A0L1A0 : PCond2A0A0L1A
    {
        // (s a0 l1 a0)

        PCond2A0A0L1A0 (PrimitiveCombination2A0A0 predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2A0A0L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0L1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A0L1A1 : PCond2A0A0L1A
    {
        // (s a0 l1 a1)

        PCond2A0A0L1A1 (PrimitiveCombination2A0A0 predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2A0A0L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0L1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A0L1L1 : PCond2A0A0L1L
    {
        // (s a0 l1 l1)

        PCond2A0A0L1L1 (PrimitiveCombination2A0A0 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0A0L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0L1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A0L1Q : PCond2A0A0L1
    {
        // (s a0 l1 Q)
        public readonly object alternativeValue;
        PCond2A0A0L1Q (PrimitiveCombination2A0A0 predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2A0A0L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0L1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A0LL : PCond2A0A0L
    {
        // (s a0 l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0A0LL (PrimitiveCombination2A0A0 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0A0LA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0A0LL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0A0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0LL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A0LA : PCond2A0A0LL
    {
        // (s a0 l l1)

        protected PCond2A0A0LA (PrimitiveCombination2A0A0 predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0A0LA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0A0LA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0A0LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0LA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A0LA0 : PCond2A0A0LA
    {
        // (s a0 l a0)

        PCond2A0A0LA0 (PrimitiveCombination2A0A0 predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2A0A0LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0LA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A0LA1 : PCond2A0A0LA
    {
        // (s a0 l a1)

        PCond2A0A0LA1 (PrimitiveCombination2A0A0 predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2A0A0LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0LA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A0LL1 : PCond2A0A0LL
    {
        // (s a0 l l1)

        PCond2A0A0LL1 (PrimitiveCombination2A0A0 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0A0LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0LL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A0LQ : PCond2A0A0L
    {
        // (s a0 l Q)
        public readonly object alternativeValue;
        PCond2A0A0LQ (PrimitiveCombination2A0A0 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2A0A0LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0LQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A0Q : PCond2A0A0
    {
        // (s a0 q s)
        public readonly object consequentValue;

        protected PCond2A0A0Q (PrimitiveCombination2A0A0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0A0QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0A0QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A0QL : PCond2A0A0Q
    {
        // (s a0 q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0A0QL (PrimitiveCombination2A0A0 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0A0QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0A0QL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0A0QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0QL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A0QA : PCond2A0A0QL
    {
        // (s a0 q a)
        protected PCond2A0A0QA (PrimitiveCombination2A0A0 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0A0QA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0A0QA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0A0QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0QA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
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

    sealed class PCond2A0A0QA0 : PCond2A0A0QA
    {
        // (s a0 q a0)
        PCond2A0A0QA0 (PrimitiveCombination2A0A0 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2A0A0QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0QA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
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

    sealed class PCond2A0A0QA1 : PCond2A0A0QA
    {
        // (s a0 q a1)
        PCond2A0A0QA1 (PrimitiveCombination2A0A0 predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2A0A0QA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0QA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
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

    sealed class PCond2A0A0QL1 : PCond2A0A0QL
    {
        // (s a0 q l1)
        PCond2A0A0QL1 (PrimitiveCombination2A0A0 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0A0QL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0QL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A0QQ : PCond2A0A0Q
    {
        // (s a0 q q)
        public readonly object alternativeValue;

        PCond2A0A0QQ (PrimitiveCombination2A0A0 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2A0A0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2A0A0QQ.EvalStep";
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
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

    class PCond2A0A0SL : PCond2A0A0
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0A0SL (PrimitiveCombination2A0A0 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2A0A0 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0A0SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0A0SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0A0SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0SL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A0SA : PCond2A0A0SL
    {
        protected PCond2A0A0SA (PrimitiveCombination2A0A0 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0A0 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0A0SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0A0SA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0A0SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0SA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A0SA0 : PCond2A0A0SA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2A0A0SA0 (PrimitiveCombination2A0A0 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0A0 predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2A0A0SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0A0SA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A0SA1 : PCond2A0A0SA
    {
        PCond2A0A0SA1 (PrimitiveCombination2A0A0 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0A0 predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2A0A0SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0SA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A0SL1 : PCond2A0A0SL
    {
        PCond2A0A0SL1 (PrimitiveCombination2A0A0 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0A0 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2A0A0SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0SL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A0SQ : PCond2A0A0
    {
        // (s a0 s q)
        public readonly object alternativeValue;

        PCond2A0A0SQ (PrimitiveCombination2A0A0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A0 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2A0A0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A0SQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A1 : PCond2A0A
    {
        // (s a1 s s)
        protected PCond2A0A1 (PrimitiveCombination2A0A1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2A0A1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2A0A1Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0A1SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0A1SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2A0A1.EvalStep");
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A1L : PCond2A0A1
    {
        // (s a1 l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2A0A1L (PrimitiveCombination2A0A1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2A0A1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2A0A1L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0A1LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0A1LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A1A : PCond2A0A1L
    {
        // (s a1 a s)

        protected PCond2A0A1A (PrimitiveCombination2A0A1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2A0A1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2A0A1A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0A1AL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0A1AQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A1AL : PCond2A0A1A
    {
        // (s a1 a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0A1AL (PrimitiveCombination2A0A1 predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0A1AA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0A1AL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0A1AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1AL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A1AA : PCond2A0A1AL
    {
        // (s a1 a l1)

        protected PCond2A0A1AA (PrimitiveCombination2A0A1 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0A1AA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0A1AA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0A1AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1AA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
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

    sealed class PCond2A0A1AA0 : PCond2A0A1AA
    {
        // (s a1 a a0)

        PCond2A0A1AA0 (PrimitiveCombination2A0A1 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2A0A1AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1AA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
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

    sealed class PCond2A0A1AA1 : PCond2A0A1AA
    {
        // (s a1 a a1)

        PCond2A0A1AA1 (PrimitiveCombination2A0A1 predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2A0A1AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1AA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
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

    sealed class PCond2A0A1AL1 : PCond2A0A1AL
    {
        // (s a1 a l1)

        PCond2A0A1AL1 (PrimitiveCombination2A0A1 predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0A1AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1AL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A1AQ : PCond2A0A1A
    {
        // (s a1 a Q)
        public readonly object alternativeValue;
        PCond2A0A1AQ (PrimitiveCombination2A0A1 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2A0A1AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1AQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
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

    class PCond2A0A1A0 : PCond2A0A1A
    {
        // (s a1 a0 s)

        protected PCond2A0A1A0 (PrimitiveCombination2A0A1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0A1A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0A1A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A1A0L : PCond2A0A1A0
    {
        // (s a1 a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0A1A0L (PrimitiveCombination2A0A1 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0A1A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0A1A0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0A1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1A0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A1A0A : PCond2A0A1A0L
    {
        // (s a1 a0 l1)

        protected PCond2A0A1A0A (PrimitiveCombination2A0A1 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0A1A0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0A1A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0A1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1A0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
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

    sealed class PCond2A0A1A0A0 : PCond2A0A1A0A
    {
        // (s a1 a0 a0)

        PCond2A0A1A0A0 (PrimitiveCombination2A0A1 predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2A0A1A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1A0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
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

    sealed class PCond2A0A1A0A1 : PCond2A0A1A0A
    {
        // (s a1 a0 a1)

        PCond2A0A1A0A1 (PrimitiveCombination2A0A1 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2A0A1A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1A0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
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

    sealed class PCond2A0A1A0L1 : PCond2A0A1A0L
    {
        // (s a1 a0 l1)

        PCond2A0A1A0L1 (PrimitiveCombination2A0A1 predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0A1A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1A0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A1A0Q : PCond2A0A1A0
    {
        // (s a1 a0 Q)
        public readonly object alternativeValue;
        PCond2A0A1A0Q (PrimitiveCombination2A0A1 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2A0A1A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1A0Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
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

    class PCond2A0A1A1 : PCond2A0A1A
    {
        // (s a1 a1 s)

        protected PCond2A0A1A1 (PrimitiveCombination2A0A1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0A1A1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0A1A1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A1A1L : PCond2A0A1A1
    {
        // (s a1 a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0A1A1L (PrimitiveCombination2A0A1 predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0A1A1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0A1A1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0A1A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1A1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A1A1A : PCond2A0A1A1L
    {
        // (s a1 a1 l1)

        protected PCond2A0A1A1A (PrimitiveCombination2A0A1 predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0A1A1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0A1A1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0A1A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1A1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
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

    sealed class PCond2A0A1A1A0 : PCond2A0A1A1A
    {
        // (s a1 a1 a0)

        PCond2A0A1A1A0 (PrimitiveCombination2A0A1 predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2A0A1A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1A1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
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

    sealed class PCond2A0A1A1A1 : PCond2A0A1A1A
    {
        // (s a1 a1 a1)

        PCond2A0A1A1A1 (PrimitiveCombination2A0A1 predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2A0A1A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1A1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
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

    sealed class PCond2A0A1A1L1 : PCond2A0A1A1L
    {
        // (s a1 a1 l1)

        PCond2A0A1A1L1 (PrimitiveCombination2A0A1 predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0A1A1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1A1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A1A1Q : PCond2A0A1A1
    {
        // (s a1 a1 Q)
        public readonly object alternativeValue;
        PCond2A0A1A1Q (PrimitiveCombination2A0A1 predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2A0A1A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1A1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
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

    class PCond2A0A1L1 : PCond2A0A1L
    {
        // (s a1 l1 s)

        protected PCond2A0A1L1 (PrimitiveCombination2A0A1 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0A1L1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0A1L1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0A1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A1L1L : PCond2A0A1L1
    {
        // (s a1 l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0A1L1L (PrimitiveCombination2A0A1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0A1L1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0A1L1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0A1L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1L1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A1L1A : PCond2A0A1L1L
    {
        // (s a1 l1 l1)

        protected PCond2A0A1L1A (PrimitiveCombination2A0A1 predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0A1L1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0A1L1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0A1L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1L1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A1L1A0 : PCond2A0A1L1A
    {
        // (s a1 l1 a0)

        PCond2A0A1L1A0 (PrimitiveCombination2A0A1 predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2A0A1L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1L1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A1L1A1 : PCond2A0A1L1A
    {
        // (s a1 l1 a1)

        PCond2A0A1L1A1 (PrimitiveCombination2A0A1 predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2A0A1L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1L1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A1L1L1 : PCond2A0A1L1L
    {
        // (s a1 l1 l1)

        PCond2A0A1L1L1 (PrimitiveCombination2A0A1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0A1L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1L1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A1L1Q : PCond2A0A1L1
    {
        // (s a1 l1 Q)
        public readonly object alternativeValue;
        PCond2A0A1L1Q (PrimitiveCombination2A0A1 predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2A0A1L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1L1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A1LL : PCond2A0A1L
    {
        // (s a1 l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0A1LL (PrimitiveCombination2A0A1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0A1LA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0A1LL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0A1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1LL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A1LA : PCond2A0A1LL
    {
        // (s a1 l l1)

        protected PCond2A0A1LA (PrimitiveCombination2A0A1 predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0A1LA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0A1LA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0A1LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1LA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A1LA0 : PCond2A0A1LA
    {
        // (s a1 l a0)

        PCond2A0A1LA0 (PrimitiveCombination2A0A1 predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2A0A1LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1LA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A1LA1 : PCond2A0A1LA
    {
        // (s a1 l a1)

        PCond2A0A1LA1 (PrimitiveCombination2A0A1 predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2A0A1LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1LA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A1LL1 : PCond2A0A1LL
    {
        // (s a1 l l1)

        PCond2A0A1LL1 (PrimitiveCombination2A0A1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0A1LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1LL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A1LQ : PCond2A0A1L
    {
        // (s a1 l Q)
        public readonly object alternativeValue;
        PCond2A0A1LQ (PrimitiveCombination2A0A1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2A0A1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1LQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A1Q : PCond2A0A1
    {
        // (s a1 q s)
        public readonly object consequentValue;

        protected PCond2A0A1Q (PrimitiveCombination2A0A1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0A1QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0A1QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A1QL : PCond2A0A1Q
    {
        // (s a1 q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0A1QL (PrimitiveCombination2A0A1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0A1QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0A1QL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0A1QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1QL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A1QA : PCond2A0A1QL
    {
        // (s a1 q a)
        protected PCond2A0A1QA (PrimitiveCombination2A0A1 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0A1QA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0A1QA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0A1QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1QA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
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

    sealed class PCond2A0A1QA0 : PCond2A0A1QA
    {
        // (s a1 q a0)
        PCond2A0A1QA0 (PrimitiveCombination2A0A1 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2A0A1QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1QA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
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

    sealed class PCond2A0A1QA1 : PCond2A0A1QA
    {
        // (s a1 q a1)
        PCond2A0A1QA1 (PrimitiveCombination2A0A1 predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2A0A1QA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1QA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
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

    sealed class PCond2A0A1QL1 : PCond2A0A1QL
    {
        // (s a1 q l1)
        PCond2A0A1QL1 (PrimitiveCombination2A0A1 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0A1QL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1QL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A1QQ : PCond2A0A1Q
    {
        // (s a1 q q)
        public readonly object alternativeValue;

        PCond2A0A1QQ (PrimitiveCombination2A0A1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2A0A1QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2A0A1QQ.EvalStep";
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
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

    class PCond2A0A1SL : PCond2A0A1
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0A1SL (PrimitiveCombination2A0A1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2A0A1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0A1SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0A1SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0A1SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1SL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0A1SA : PCond2A0A1SL
    {
        protected PCond2A0A1SA (PrimitiveCombination2A0A1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0A1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0A1SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0A1SA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0A1SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1SA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A1SA0 : PCond2A0A1SA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2A0A1SA0 (PrimitiveCombination2A0A1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0A1 predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2A0A1SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0A1SA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A1SA1 : PCond2A0A1SA
    {
        PCond2A0A1SA1 (PrimitiveCombination2A0A1 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0A1 predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2A0A1SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1SA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A1SL1 : PCond2A0A1SL
    {
        PCond2A0A1SL1 (PrimitiveCombination2A0A1 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0A1 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2A0A1SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1SL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0A1SQ : PCond2A0A1
    {
        // (s a1 s q)
        public readonly object alternativeValue;

        PCond2A0A1SQ (PrimitiveCombination2A0A1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0A1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2A0A1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0A1SQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L1 : PCond2A0L
    {
        // (a0 l1 s s)
        protected PCond2A0L1 (PrimitiveCombination2A0L1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2A0L1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2A0L1Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0L1SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0L1SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2A0L1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L1L : PCond2A0L1
    {
        // (a0 l1 l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2A0L1L (PrimitiveCombination2A0L1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2A0L1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2A0L1L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0L1LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0L1LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L1A : PCond2A0L1L
    {
        // (a0 l1 a s)

        protected PCond2A0L1A (PrimitiveCombination2A0L1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2A0L1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2A0L1A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0L1AL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0L1AQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L1AL : PCond2A0L1A
    {
        // (a0 l1 a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0L1AL (PrimitiveCombination2A0L1 predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0L1AA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0L1AL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0L1AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1AL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L1AA : PCond2A0L1AL
    {
        // (a0 l1 a l1)

        protected PCond2A0L1AA (PrimitiveCombination2A0L1 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0L1AA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0L1AA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0L1AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1AA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0L1AA0 : PCond2A0L1AA
    {
        // (a0 l1 a a0)

        PCond2A0L1AA0 (PrimitiveCombination2A0L1 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2A0L1AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1AA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0L1AA1 : PCond2A0L1AA
    {
        // (a0 l1 a a1)

        PCond2A0L1AA1 (PrimitiveCombination2A0L1 predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2A0L1AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1AA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0L1AL1 : PCond2A0L1AL
    {
        // (a0 l1 a l1)

        PCond2A0L1AL1 (PrimitiveCombination2A0L1 predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0L1AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1AL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0L1AQ : PCond2A0L1A
    {
        // (a0 l1 a Q)
        public readonly object alternativeValue;
        PCond2A0L1AQ (PrimitiveCombination2A0L1 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2A0L1AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1AQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    class PCond2A0L1A0 : PCond2A0L1A
    {
        // (a0 l1 a0 s)

        protected PCond2A0L1A0 (PrimitiveCombination2A0L1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0L1A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0L1A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L1A0L : PCond2A0L1A0
    {
        // (a0 l1 a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0L1A0L (PrimitiveCombination2A0L1 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0L1A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0L1A0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0L1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1A0L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L1A0A : PCond2A0L1A0L
    {
        // (a0 l1 a0 l1)

        protected PCond2A0L1A0A (PrimitiveCombination2A0L1 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0L1A0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0L1A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0L1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1A0A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0L1A0A0 : PCond2A0L1A0A
    {
        // (a0 l1 a0 a0)

        PCond2A0L1A0A0 (PrimitiveCombination2A0L1 predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2A0L1A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1A0A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0L1A0A1 : PCond2A0L1A0A
    {
        // (a0 l1 a0 a1)

        PCond2A0L1A0A1 (PrimitiveCombination2A0L1 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2A0L1A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1A0A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0L1A0L1 : PCond2A0L1A0L
    {
        // (a0 l1 a0 l1)

        PCond2A0L1A0L1 (PrimitiveCombination2A0L1 predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0L1A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1A0L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0L1A0Q : PCond2A0L1A0
    {
        // (a0 l1 a0 Q)
        public readonly object alternativeValue;
        PCond2A0L1A0Q (PrimitiveCombination2A0L1 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2A0L1A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1A0Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    class PCond2A0L1A1 : PCond2A0L1A
    {
        // (a0 l1 a1 s)

        protected PCond2A0L1A1 (PrimitiveCombination2A0L1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0L1A1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0L1A1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L1A1L : PCond2A0L1A1
    {
        // (a0 l1 a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0L1A1L (PrimitiveCombination2A0L1 predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0L1A1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0L1A1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0L1A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1A1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L1A1A : PCond2A0L1A1L
    {
        // (a0 l1 a1 l1)

        protected PCond2A0L1A1A (PrimitiveCombination2A0L1 predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0L1A1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0L1A1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0L1A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1A1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0L1A1A0 : PCond2A0L1A1A
    {
        // (a0 l1 a1 a0)

        PCond2A0L1A1A0 (PrimitiveCombination2A0L1 predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2A0L1A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1A1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0L1A1A1 : PCond2A0L1A1A
    {
        // (a0 l1 a1 a1)

        PCond2A0L1A1A1 (PrimitiveCombination2A0L1 predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2A0L1A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1A1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0L1A1L1 : PCond2A0L1A1L
    {
        // (a0 l1 a1 l1)

        PCond2A0L1A1L1 (PrimitiveCombination2A0L1 predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0L1A1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1A1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0L1A1Q : PCond2A0L1A1
    {
        // (a0 l1 a1 Q)
        public readonly object alternativeValue;
        PCond2A0L1A1Q (PrimitiveCombination2A0L1 predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2A0L1A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1A1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    class PCond2A0L1L1 : PCond2A0L1L
    {
        // (a0 l1 l1 s)

        protected PCond2A0L1L1 (PrimitiveCombination2A0L1 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0L1L1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0L1L1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L1L1L : PCond2A0L1L1
    {
        // (a0 l1 l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0L1L1L (PrimitiveCombination2A0L1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0L1L1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0L1L1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0L1L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1L1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L1L1A : PCond2A0L1L1L
    {
        // (a0 l1 l1 l1)

        protected PCond2A0L1L1A (PrimitiveCombination2A0L1 predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0L1L1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0L1L1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0L1L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1L1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0L1L1A0 : PCond2A0L1L1A
    {
        // (a0 l1 l1 a0)

        PCond2A0L1L1A0 (PrimitiveCombination2A0L1 predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2A0L1L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1L1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0L1L1A1 : PCond2A0L1L1A
    {
        // (a0 l1 l1 a1)

        PCond2A0L1L1A1 (PrimitiveCombination2A0L1 predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2A0L1L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1L1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0L1L1L1 : PCond2A0L1L1L
    {
        // (a0 l1 l1 l1)

        PCond2A0L1L1L1 (PrimitiveCombination2A0L1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0L1L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1L1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0L1L1Q : PCond2A0L1L1
    {
        // (a0 l1 l1 Q)
        public readonly object alternativeValue;
        PCond2A0L1L1Q (PrimitiveCombination2A0L1 predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2A0L1L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1L1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L1LL : PCond2A0L1L
    {
        // (a0 l1 l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0L1LL (PrimitiveCombination2A0L1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0L1LA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0L1LL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0L1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1LL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L1LA : PCond2A0L1LL
    {
        // (a0 l1 l l1)

        protected PCond2A0L1LA (PrimitiveCombination2A0L1 predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0L1LA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0L1LA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0L1LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1LA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0L1LA0 : PCond2A0L1LA
    {
        // (a0 l1 l a0)

        PCond2A0L1LA0 (PrimitiveCombination2A0L1 predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2A0L1LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1LA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0L1LA1 : PCond2A0L1LA
    {
        // (a0 l1 l a1)

        PCond2A0L1LA1 (PrimitiveCombination2A0L1 predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2A0L1LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1LA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0L1LL1 : PCond2A0L1LL
    {
        // (a0 l1 l l1)

        PCond2A0L1LL1 (PrimitiveCombination2A0L1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0L1LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1LL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0L1LQ : PCond2A0L1L
    {
        // (a0 l1 l Q)
        public readonly object alternativeValue;
        PCond2A0L1LQ (PrimitiveCombination2A0L1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2A0L1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1LQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L1Q : PCond2A0L1
    {
        // (a0 l1 q s)
        public readonly object consequentValue;

        protected PCond2A0L1Q (PrimitiveCombination2A0L1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0L1QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0L1QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0L1Q (predicate, consequent, alternative);
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
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L1QL : PCond2A0L1Q
    {
        // (a0 l1 q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0L1QL (PrimitiveCombination2A0L1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0L1QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0L1QL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0L1QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1QL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L1QA : PCond2A0L1QL
    {
        // (a0 l1 q a)
        protected PCond2A0L1QA (PrimitiveCombination2A0L1 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0L1QA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0L1QA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0L1QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1QA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0L1QA0 : PCond2A0L1QA
    {
        // (a0 l1 q a0)
        PCond2A0L1QA0 (PrimitiveCombination2A0L1 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2A0L1QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1QA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0L1QA1 : PCond2A0L1QA
    {
        // (a0 l1 q a1)
        PCond2A0L1QA1 (PrimitiveCombination2A0L1 predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2A0L1QA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1QA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    sealed class PCond2A0L1QL1 : PCond2A0L1QL
    {
        // (a0 l1 q l1)
        PCond2A0L1QL1 (PrimitiveCombination2A0L1 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0L1QL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1QL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0L1QQ : PCond2A0L1Q
    {
        // (a0 l1 q q)
        public readonly object alternativeValue;

        PCond2A0L1QQ (PrimitiveCombination2A0L1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2A0L1QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2A0L1QQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, ev1)) {
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

    class PCond2A0L1SL : PCond2A0L1
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0L1SL (PrimitiveCombination2A0L1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2A0L1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0L1SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0L1SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0L1SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1SL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0L1SA : PCond2A0L1SL
    {
        protected PCond2A0L1SA (PrimitiveCombination2A0L1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0L1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0L1SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0L1SA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0L1SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1SA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0L1SA0 : PCond2A0L1SA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2A0L1SA0 (PrimitiveCombination2A0L1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0L1 predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2A0L1SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0L1SA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0L1SA1 : PCond2A0L1SA
    {
        PCond2A0L1SA1 (PrimitiveCombination2A0L1 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0L1 predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2A0L1SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1SA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0L1SL1 : PCond2A0L1SL
    {
        PCond2A0L1SL1 (PrimitiveCombination2A0L1 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0L1 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2A0L1SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1SL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0L1SQ : PCond2A0L1
    {
        // (a0 l1 s q)
        public readonly object alternativeValue;

        PCond2A0L1SQ (PrimitiveCombination2A0L1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0L1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2A0L1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0L1SQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0Q : PCond2A0
    {
        // (s q s s)
        public readonly object rand1Value;

        protected PCond2A0Q (PrimitiveCombination2A0Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2A0QL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2A0QQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0QSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0QSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("PCond2A0Q.EvalStep");
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0QL : PCond2A0Q
    {
        // (s q l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2A0QL (PrimitiveCombination2A0Q predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2A0QA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2A0QL1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0QLL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0QLQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0QA : PCond2A0QL
    {
        // (s q a s)

        protected PCond2A0QA (PrimitiveCombination2A0Q predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2A0QA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2A0QA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2A0QAL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0QAQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0QAL : PCond2A0QA
    {
        // (s q a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0QAL (PrimitiveCombination2A0Q predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0QAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0QAL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0QAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QAL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0QAA : PCond2A0QAL
    {
        // (s q a l1)

        protected PCond2A0QAA (PrimitiveCombination2A0Q predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0QAA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0QAA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0QAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QAA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
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

    sealed class PCond2A0QAA0 : PCond2A0QAA
    {
        // (s q a a0)

        PCond2A0QAA0 (PrimitiveCombination2A0Q predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2A0QAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QAA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
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

    sealed class PCond2A0QAA1 : PCond2A0QAA
    {
        // (s q a a1)

        PCond2A0QAA1 (PrimitiveCombination2A0Q predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2A0QAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QAA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
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

    sealed class PCond2A0QAL1 : PCond2A0QAL
    {
        // (s q a l1)

        PCond2A0QAL1 (PrimitiveCombination2A0Q predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0QAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QAL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0QAQ : PCond2A0QA
    {
        // (s q a Q)
        public readonly object alternativeValue;
        PCond2A0QAQ (PrimitiveCombination2A0Q predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2A0QAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QAQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
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

    class PCond2A0QA0 : PCond2A0QA
    {
        // (s q a0 s)

        protected PCond2A0QA0 (PrimitiveCombination2A0Q predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0QA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0QA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0QA0L : PCond2A0QA0
    {
        // (s q a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0QA0L (PrimitiveCombination2A0Q predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0QA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0QA0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0QA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QA0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0QA0A : PCond2A0QA0L
    {
        // (s q a0 l1)

        protected PCond2A0QA0A (PrimitiveCombination2A0Q predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0QA0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0QA0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0QA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QA0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
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

    sealed class PCond2A0QA0A0 : PCond2A0QA0A
    {
        // (s q a0 a0)

        PCond2A0QA0A0 (PrimitiveCombination2A0Q predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2A0QA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QA0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
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

    sealed class PCond2A0QA0A1 : PCond2A0QA0A
    {
        // (s q a0 a1)

        PCond2A0QA0A1 (PrimitiveCombination2A0Q predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2A0QA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QA0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
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

    sealed class PCond2A0QA0L1 : PCond2A0QA0L
    {
        // (s q a0 l1)

        PCond2A0QA0L1 (PrimitiveCombination2A0Q predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0QA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QA0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0QA0Q : PCond2A0QA0
    {
        // (s q a0 Q)
        public readonly object alternativeValue;
        PCond2A0QA0Q (PrimitiveCombination2A0Q predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2A0QA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QA0Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
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

    class PCond2A0QA1 : PCond2A0QA
    {
        // (s q a1 s)

        protected PCond2A0QA1 (PrimitiveCombination2A0Q predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0QA1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0QA1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0QA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0QA1L : PCond2A0QA1
    {
        // (s q a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0QA1L (PrimitiveCombination2A0Q predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0QA1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0QA1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0QA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QA1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0QA1A : PCond2A0QA1L
    {
        // (s q a1 l1)

        protected PCond2A0QA1A (PrimitiveCombination2A0Q predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0QA1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0QA1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0QA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QA1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
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

    sealed class PCond2A0QA1A0 : PCond2A0QA1A
    {
        // (s q a1 a0)

        PCond2A0QA1A0 (PrimitiveCombination2A0Q predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2A0QA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QA1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
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

    sealed class PCond2A0QA1A1 : PCond2A0QA1A
    {
        // (s q a1 a1)

        PCond2A0QA1A1 (PrimitiveCombination2A0Q predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2A0QA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QA1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
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

    sealed class PCond2A0QA1L1 : PCond2A0QA1L
    {
        // (s q a1 l1)

        PCond2A0QA1L1 (PrimitiveCombination2A0Q predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0QA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QA1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0QA1Q : PCond2A0QA1
    {
        // (s q a1 Q)
        public readonly object alternativeValue;
        PCond2A0QA1Q (PrimitiveCombination2A0Q predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2A0QA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QA1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
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

    class PCond2A0QL1 : PCond2A0QL
    {
        // (s q l1 s)

        protected PCond2A0QL1 (PrimitiveCombination2A0Q predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0QL1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0QL1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0QL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0QL1L : PCond2A0QL1
    {
        // (s q l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0QL1L (PrimitiveCombination2A0Q predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0QL1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0QL1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0QL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QL1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0QL1A : PCond2A0QL1L
    {
        // (s q l1 l1)

        protected PCond2A0QL1A (PrimitiveCombination2A0Q predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0QL1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0QL1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0QL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QL1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0QL1A0 : PCond2A0QL1A
    {
        // (s q l1 a0)

        PCond2A0QL1A0 (PrimitiveCombination2A0Q predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2A0QL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QL1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0QL1A1 : PCond2A0QL1A
    {
        // (s q l1 a1)

        PCond2A0QL1A1 (PrimitiveCombination2A0Q predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2A0QL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QL1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0QL1L1 : PCond2A0QL1L
    {
        // (s q l1 l1)

        PCond2A0QL1L1 (PrimitiveCombination2A0Q predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0QL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QL1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0QL1Q : PCond2A0QL1
    {
        // (s q l1 Q)
        public readonly object alternativeValue;
        PCond2A0QL1Q (PrimitiveCombination2A0Q predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2A0QL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QL1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0QLL : PCond2A0QL
    {
        // (s q l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2A0QLL (PrimitiveCombination2A0Q predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0QLA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0QLL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0QLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QLL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0QLA : PCond2A0QLL
    {
        // (s q l l1)

        protected PCond2A0QLA (PrimitiveCombination2A0Q predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0QLA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0QLA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0QLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QLA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0QLA0 : PCond2A0QLA
    {
        // (s q l a0)

        PCond2A0QLA0 (PrimitiveCombination2A0Q predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2A0QLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QLA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0QLA1 : PCond2A0QLA
    {
        // (s q l a1)

        PCond2A0QLA1 (PrimitiveCombination2A0Q predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2A0QLA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QLA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0QLL1 : PCond2A0QLL
    {
        // (s q l l1)

        PCond2A0QLL1 (PrimitiveCombination2A0Q predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0QLL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QLL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0QLQ : PCond2A0QL
    {
        // (s q l Q)
        public readonly object alternativeValue;
        PCond2A0QLQ (PrimitiveCombination2A0Q predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2A0QLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QLQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0QQ : PCond2A0Q
    {
        // (s q q s)
        public readonly object consequentValue;

        protected PCond2A0QQ (PrimitiveCombination2A0Q predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2A0QQL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2A0QQQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0QQL : PCond2A0QQ
    {
        // (s q q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0QQL (PrimitiveCombination2A0Q predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0QQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0QQL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0QQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QQL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0QQA : PCond2A0QQL
    {
        // (s q q a)
        protected PCond2A0QQA (PrimitiveCombination2A0Q predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0QQA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0QQA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0QQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QQA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
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

    sealed class PCond2A0QQA0 : PCond2A0QQA
    {
        // (s q q a0)
        PCond2A0QQA0 (PrimitiveCombination2A0Q predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2A0QQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QQA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
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

    sealed class PCond2A0QQA1 : PCond2A0QQA
    {
        // (s q q a1)
        PCond2A0QQA1 (PrimitiveCombination2A0Q predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2A0QQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QQA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
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

    sealed class PCond2A0QQL1 : PCond2A0QQL
    {
        // (s q q l1)
        PCond2A0QQL1 (PrimitiveCombination2A0Q predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2A0QQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QQL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0QQQ : PCond2A0QQ
    {
        // (s q q q)
        public readonly object alternativeValue;

        PCond2A0QQQ (PrimitiveCombination2A0Q predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2A0QQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2A0QQQ.EvalStep";
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
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

    class PCond2A0QSL : PCond2A0Q
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2A0QSL (PrimitiveCombination2A0Q predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2A0Q predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2A0QSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2A0QSL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2A0QSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QSL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2A0QSA : PCond2A0QSL
    {
        protected PCond2A0QSA (PrimitiveCombination2A0Q predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0Q predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2A0QSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2A0QSA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2A0QSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QSA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0QSA0 : PCond2A0QSA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2A0QSA0 (PrimitiveCombination2A0Q predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0Q predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2A0QSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2A0QSA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0QSA1 : PCond2A0QSA
    {
        PCond2A0QSA1 (PrimitiveCombination2A0Q predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0Q predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2A0QSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QSA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0QSL1 : PCond2A0QSL
    {
        PCond2A0QSL1 (PrimitiveCombination2A0Q predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0Q predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2A0QSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QSL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2A0QSQ : PCond2A0Q
    {
        public readonly object alternativeValue;

        PCond2A0QSQ (PrimitiveCombination2A0Q predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        static public SCode Make (PrimitiveCombination2A0Q predicate, SCode consequent, Quotation alternative)
        {
            return new PCond2A0QSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2A0QSQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString();
#endif
            object predValue;

            if (this.method (out predValue, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = predValue as TailCallInterpreter;
                if (tci != null) {
                    predValue = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out predValue, ref cExpression, ref cEnvironment)) { };
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
