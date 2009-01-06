using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    class PCond2A : PCond2L
    {
        // (a s s s)
        protected PCond2A (PrimitiveCombination2A predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveCombination2A0) ? PCond2A0.Make ((PrimitiveCombination2A0) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2A1) ? PCond2A1.Make ((PrimitiveCombination2A1) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2AL) ? PCond2AL.Make ((PrimitiveCombination2AL) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2AQ) ? PCond2AQ.Make ((PrimitiveCombination2AQ) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCond2ASL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2ASQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2ASSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2ASSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2A.EvalStep");
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
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASL : PCond2A
    {
        // (a s l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2ASL (PrimitiveCombination2A predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2A predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2ASA.Make (predicate, (Argument) consequent, alternative)
                : (consequent is LexicalVariable1) ? PCond2ASL1.Make (predicate, (LexicalVariable1) consequent, alternative)
                : (alternative is LexicalVariable) ? PCond2ASLL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2ASLQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2ASL (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASLL : PCond2ASL
    {
        // (a s l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2ASLL (PrimitiveCombination2A predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2ASLA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2ASLL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2ASLL (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASLA : PCond2ASLL
    {
        // (a s l a)
        protected PCond2ASLA (PrimitiveCombination2A predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2ASLA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2ASLA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2ASLA (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASLA0 : PCond2ASLA
    {
        // (a s l a0)
        PCond2ASLA0 (PrimitiveCombination2A predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2ASLA0 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASLA1 : PCond2ASLA
    {
        // (a s l a1)
        PCond2ASLA1 (PrimitiveCombination2A predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2ASLA1 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASLL1 : PCond2ASLL
    {
        // (a s l l1)
        PCond2ASLL1 (PrimitiveCombination2A predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2ASLL1 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASLQ : PCond2ASL
    {
        // (a s l q)
        public readonly object alternativeValue;

        PCond2ASLQ (PrimitiveCombination2A predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2ASLQ (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASA : PCond2ASL
    {
        // (a s a s)
        protected PCond2ASA (PrimitiveCombination2A predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2ASA0.Make (predicate, (Argument0) consequent, alternative)
                : (consequent is Argument1) ? PCond2ASA1.Make (predicate, (Argument1) consequent, alternative)
                : (alternative is LexicalVariable) ? PCond2ASAL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2ASAQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2ASA (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASAL : PCond2ASA
    {
        // (a s a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2ASAL (PrimitiveCombination2A predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2ASAA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2ASAL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2ASAL (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASAA : PCond2ASAL
    {
        // (a s a a)
        protected PCond2ASAA (PrimitiveCombination2A predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2ASAA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2ASAA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2ASAA (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASAA0 : PCond2ASAA
    {
        // (a s a a0)
        PCond2ASAA0 (PrimitiveCombination2A predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2ASAA0 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASAA1 : PCond2ASAA
    {
        // (a s a a1)
        PCond2ASAA1 (PrimitiveCombination2A predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2ASAA1 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASAL1 : PCond2ASAL
    {
        // (a s a l1)
        PCond2ASAL1 (PrimitiveCombination2A predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2ASAL1 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASAQ : PCond2ASA
    {
        // (a s a q)
        public readonly object alternativeValue;

        PCond2ASAQ (PrimitiveCombination2A predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2ASAQ (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASA0 : PCond2ASA
    {
        // (a s a0 s)
        protected PCond2ASA0 (PrimitiveCombination2A predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2ASA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2ASA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2ASA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2ASA0.EvalStep";
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
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASA0L : PCond2ASA0
    {
        // (a s a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2ASA0L (PrimitiveCombination2A predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2ASA0A.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2ASA0L1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2ASA0L (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASA0A : PCond2ASA0L
    {
        // (a s a0 a)
        protected PCond2ASA0A (PrimitiveCombination2A predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2ASA0A0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2ASA0A1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2ASA0A (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASA0A0 : PCond2ASA0A
    {
        // (a s a0 a0)
        PCond2ASA0A0 (PrimitiveCombination2A predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2ASA0A0 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASA0A1 : PCond2ASA0A
    {
        // (a s a0 a1)
        PCond2ASA0A1 (PrimitiveCombination2A predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2ASA0A1 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASA0L1 : PCond2ASA0L
    {
        // (a s a0 l1)
        PCond2ASA0L1 (PrimitiveCombination2A predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2ASA0L1 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASA0Q : PCond2ASA0
    {
        // (a s a0 q)
        public readonly object alternativeValue;

        PCond2ASA0Q (PrimitiveCombination2A predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2ASA0Q (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASA1 : PCond2ASA
    {
        // (a s a1 s)
        protected PCond2ASA1 (PrimitiveCombination2A predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2ASA1L.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2ASA1Q.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2ASA1 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASA1L : PCond2ASA1
    {
        // (a s a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2ASA1L (PrimitiveCombination2A predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2ASA1A.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2ASA1L1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2ASA1L (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASA1A : PCond2ASA1L
    {
        // (a s a1 a)
        protected PCond2ASA1A (PrimitiveCombination2A predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2ASA1A0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2ASA1A1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2ASA1A (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASA1A0 : PCond2ASA1A
    {
        // (a s a1 a0)
        PCond2ASA1A0 (PrimitiveCombination2A predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2ASA1A0 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASA1A1 : PCond2ASA1A
    {
        // (a s a1 a1)
        PCond2ASA1A1 (PrimitiveCombination2A predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2ASA1A1 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASA1L1 : PCond2ASA1L
    {
        // (a s a1 l1)
        PCond2ASA1L1 (PrimitiveCombination2A predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2ASA1L1 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASA1Q : PCond2ASA1
    {
        // (a s a1 q)
        public readonly object alternativeValue;

        PCond2ASA1Q (PrimitiveCombination2A predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2ASA1Q (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASL1 : PCond2ASL
    {
        // (a s l1 s)
        protected PCond2ASL1 (PrimitiveCombination2A predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2ASL1L.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2ASL1Q.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2ASL1 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASL1L : PCond2ASL1
    {
        // (a s l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2ASL1L (PrimitiveCombination2A predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2ASL1A.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2ASL1L1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2ASL1L (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASL1A : PCond2ASL1L
    {
        // (a s l1 a)
        protected PCond2ASL1A (PrimitiveCombination2A predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2ASL1A0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2ASL1A1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2ASL1A (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASL1A0 : PCond2ASL1A
    {
        // (a s l1 a0)
        PCond2ASL1A0 (PrimitiveCombination2A predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2ASL1A0 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASL1A1 : PCond2ASL1A
    {
        // (a s l1 a1)
        PCond2ASL1A1 (PrimitiveCombination2A predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2ASL1A1 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASL1L1 : PCond2ASL1L
    {
        // (a s l1 l1)
        PCond2ASL1L1 (PrimitiveCombination2A predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2ASL1L1 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASL1Q : PCond2ASL1
    {
        // (a s l1 q)
        public readonly object alternativeValue;

        PCond2ASL1Q (PrimitiveCombination2A predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2ASL1Q (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASQ : PCond2A
    {
        // (a s q s)
        public readonly object consequentValue;

        protected PCond2ASQ (PrimitiveCombination2A predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2ASQL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2ASQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2ASQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2ASQ.EvalStep";
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
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASQL : PCond2ASQ
    {
        // (a s q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2ASQL (PrimitiveCombination2A predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2ASQA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2ASQL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2ASQL (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASQA : PCond2ASQL
    {
        // (a s q a)
        protected PCond2ASQA (PrimitiveCombination2A predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2ASQA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2ASQA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2ASQA (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASQA0 : PCond2ASQA
    {
        // (a s q a0)
        PCond2ASQA0 (PrimitiveCombination2A predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2ASQA0 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASQA1 : PCond2ASQA
    {
        // (a s q a1)
        PCond2ASQA1 (PrimitiveCombination2A predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2ASQA1 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASQL1 : PCond2ASQL
    {
        // (a s q l1)
        PCond2ASQL1 (PrimitiveCombination2A predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2ASQL1 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASQQ : PCond2ASQ
    {
        // (a s q q)
        public readonly object alternativeValue;

        PCond2ASQQ (PrimitiveCombination2A predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2ASQQ (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASSL : PCond2A
    {
        // (a s s l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2ASSL (PrimitiveCombination2A predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2A predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2ASSA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2ASSL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2ASSL (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ASSA : PCond2ASSL
    {
        // (a s s a)
        protected PCond2ASSA (PrimitiveCombination2A predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2ASSA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCond2ASSA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2ASSA (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASSA0 : PCond2ASSA
    {
        // (a s s a0)
        PCond2ASSA0 (PrimitiveCombination2A predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCond2ASSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2ASSA0.EvalStep";
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
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASSA1 : PCond2ASSA
    {
        // (a s s a1)
        PCond2ASSA1 (PrimitiveCombination2A predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCond2ASSA1 (predicate, consequent, alternative);
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
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASSL1 : PCond2ASSL
    {
        // (a s s l1)
        PCond2ASSL1 (PrimitiveCombination2A predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2ASSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2ASSL1.EvalStep";
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
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ASSQ : PCond2A
    {
        // (a s s q)
        public readonly object alternativeValue;

        PCond2ASSQ (PrimitiveCombination2A predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2ASSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCond2ASSQ.EvalStep";
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
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL : PCond2A
    {
        // (a l s s)
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCond2AL (PrimitiveCombination2AL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveCombination2AL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveCombination2AA) ? PCond2AA.Make ((PrimitiveCombination2AA) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2AL1) ? PCond2AL1.Make ((PrimitiveCombination2AL1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCond2ALL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2ALQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2ALSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2ALSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2AL.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ALL : PCond2AL
    {
        // (a l l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2ALL (PrimitiveCombination2AL predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2AL predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2ALA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2ALL1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2ALLL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2ALLQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2ALL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ALA : PCond2ALL
    {
        // (a l a s)

        protected PCond2ALA (PrimitiveCombination2AL predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2ALA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2ALA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2ALAL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2ALAQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2ALA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ALAL : PCond2ALA
    {
        // (a l a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2ALAL (PrimitiveCombination2AL predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2ALAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2ALAL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2ALAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALAL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ALAA : PCond2ALAL
    {
        // (a l a l1)

        protected PCond2ALAA (PrimitiveCombination2AL predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2ALAA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2ALAA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2ALAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALAA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2ALAA0 : PCond2ALAA
    {
        // (a l a a0)

        PCond2ALAA0 (PrimitiveCombination2AL predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2ALAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALAA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2ALAA1 : PCond2ALAA
    {
        // (a l a a1)

        PCond2ALAA1 (PrimitiveCombination2AL predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2ALAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALAA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2ALAL1 : PCond2ALAL
    {
        // (a l a l1)

        PCond2ALAL1 (PrimitiveCombination2AL predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2ALAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALAL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ALAQ : PCond2ALA
    {
        // (a l a Q)
        public readonly object alternativeValue;
        PCond2ALAQ (PrimitiveCombination2AL predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2ALAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALAQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    class PCond2ALA0 : PCond2ALA
    {
        // (a l a0 s)

        protected PCond2ALA0 (PrimitiveCombination2AL predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2ALA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2ALA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2ALA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ALA0L : PCond2ALA0
    {
        // (a l a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2ALA0L (PrimitiveCombination2AL predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2ALA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2ALA0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2ALA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALA0L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ALA0A : PCond2ALA0L
    {
        // (a l a0 l1)

        protected PCond2ALA0A (PrimitiveCombination2AL predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2ALA0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2ALA0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2ALA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALA0A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2ALA0A0 : PCond2ALA0A
    {
        // (a l a0 a0)

        PCond2ALA0A0 (PrimitiveCombination2AL predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2ALA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALA0A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2ALA0A1 : PCond2ALA0A
    {
        // (a l a0 a1)

        PCond2ALA0A1 (PrimitiveCombination2AL predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2ALA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALA0A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2ALA0L1 : PCond2ALA0L
    {
        // (a l a0 l1)

        PCond2ALA0L1 (PrimitiveCombination2AL predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2ALA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALA0L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ALA0Q : PCond2ALA0
    {
        // (a l a0 Q)
        public readonly object alternativeValue;
        PCond2ALA0Q (PrimitiveCombination2AL predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2ALA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALA0Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    class PCond2ALA1 : PCond2ALA
    {
        // (a l a1 s)

        protected PCond2ALA1 (PrimitiveCombination2AL predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2ALA1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2ALA1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2ALA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ALA1L : PCond2ALA1
    {
        // (a l a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2ALA1L (PrimitiveCombination2AL predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2ALA1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2ALA1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2ALA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALA1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ALA1A : PCond2ALA1L
    {
        // (a l a1 l1)

        protected PCond2ALA1A (PrimitiveCombination2AL predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2ALA1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2ALA1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2ALA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALA1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2ALA1A0 : PCond2ALA1A
    {
        // (a l a1 a0)

        PCond2ALA1A0 (PrimitiveCombination2AL predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2ALA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALA1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2ALA1A1 : PCond2ALA1A
    {
        // (a l a1 a1)

        PCond2ALA1A1 (PrimitiveCombination2AL predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2ALA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALA1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2ALA1L1 : PCond2ALA1L
    {
        // (a l a1 l1)

        PCond2ALA1L1 (PrimitiveCombination2AL predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2ALA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALA1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ALA1Q : PCond2ALA1
    {
        // (a l a1 Q)
        public readonly object alternativeValue;
        PCond2ALA1Q (PrimitiveCombination2AL predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2ALA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALA1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    class PCond2ALL1 : PCond2ALL
    {
        // (a l l1 s)

        protected PCond2ALL1 (PrimitiveCombination2AL predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2ALL1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2ALL1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2ALL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ALL1L : PCond2ALL1
    {
        // (a l l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2ALL1L (PrimitiveCombination2AL predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AL predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2ALL1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2ALL1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2ALL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALL1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ALL1A : PCond2ALL1L
    {
        // (a l l1 l1)

        protected PCond2ALL1A (PrimitiveCombination2AL predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2ALL1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2ALL1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2ALL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALL1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ALL1A0 : PCond2ALL1A
    {
        // (a l l1 a0)

        PCond2ALL1A0 (PrimitiveCombination2AL predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2ALL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALL1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ALL1A1 : PCond2ALL1A
    {
        // (a l l1 a1)

        PCond2ALL1A1 (PrimitiveCombination2AL predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2ALL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALL1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ALL1L1 : PCond2ALL1L
    {
        // (a l l1 l1)

        PCond2ALL1L1 (PrimitiveCombination2AL predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2ALL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALL1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ALL1Q : PCond2ALL1
    {
        // (a l l1 Q)
        public readonly object alternativeValue;
        PCond2ALL1Q (PrimitiveCombination2AL predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AL predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2ALL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALL1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ALLL : PCond2ALL
    {
        // (a l l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2ALLL (PrimitiveCombination2AL predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AL predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2ALLA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2ALLL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2ALLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALLL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ALLA : PCond2ALLL
    {
        // (a l l l1)

        protected PCond2ALLA (PrimitiveCombination2AL predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2ALLA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2ALLA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2ALLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALLA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ALLA0 : PCond2ALLA
    {
        // (a l l a0)

        PCond2ALLA0 (PrimitiveCombination2AL predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2ALLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALLA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ALLA1 : PCond2ALLA
    {
        // (a l l a1)

        PCond2ALLA1 (PrimitiveCombination2AL predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2ALLA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALLA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ALLL1 : PCond2ALLL
    {
        // (a l l l1)

        PCond2ALLL1 (PrimitiveCombination2AL predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2ALLL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALLL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ALLQ : PCond2ALL
    {
        // (a l l Q)
        public readonly object alternativeValue;
        PCond2ALLQ (PrimitiveCombination2AL predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AL predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2ALLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALLQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ALQ : PCond2AL
    {
        // (a l q s)
        public readonly object consequentValue;

        protected PCond2ALQ (PrimitiveCombination2AL predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2ALQL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2ALQQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2ALQ (predicate, consequent, alternative);
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
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ALQL : PCond2ALQ
    {
        // (a l q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2ALQL (PrimitiveCombination2AL predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2ALQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2ALQL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2ALQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALQL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ALQA : PCond2ALQL
    {
        // (a l q a)
        protected PCond2ALQA (PrimitiveCombination2AL predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2ALQA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2ALQA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2ALQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALQA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2ALQA0 : PCond2ALQA
    {
        // (a l q a0)
        PCond2ALQA0 (PrimitiveCombination2AL predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2ALQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALQA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2ALQA1 : PCond2ALQA
    {
        // (a l q a1)
        PCond2ALQA1 (PrimitiveCombination2AL predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2ALQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALQA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2ALQL1 : PCond2ALQL
    {
        // (a l q l1)
        PCond2ALQL1 (PrimitiveCombination2AL predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2ALQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALQL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ALQQ : PCond2ALQ
    {
        // (a l q q)
        public readonly object alternativeValue;

        PCond2ALQQ (PrimitiveCombination2AL predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AL predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2ALQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2ALQQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();



#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    class PCond2ALSL : PCond2AL
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2ALSL (PrimitiveCombination2AL predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2AL predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2ALSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2ALSL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2ALSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALSL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2ALSA : PCond2ALSL
    {
        protected PCond2ALSA (PrimitiveCombination2AL predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AL predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2ALSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2ALSA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2ALSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALSA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ALSA0 : PCond2ALSA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2ALSA0 (PrimitiveCombination2AL predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AL predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2ALSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2ALSA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ALSA1 : PCond2ALSA
    {
        PCond2ALSA1 (PrimitiveCombination2AL predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AL predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2ALSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALSA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ALSL1 : PCond2ALSL
    {
        PCond2ALSL1 (PrimitiveCombination2AL predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AL predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2ALSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALSL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2ALSQ : PCond2AL
    {
        // (a l s q)
        public readonly object alternativeValue;

        PCond2ALSQ (PrimitiveCombination2AL predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AL predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2ALSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2ALSQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA : PCond2AL
    {
        // (s a s s)
        protected PCond2AA (PrimitiveCombination2AA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveCombination2AA0) ? PCond2AA0.Make ((PrimitiveCombination2AA0) predicate, consequent, alternative) :
                (predicate is PrimitiveCombination2AA1) ? PCond2AA1.Make ((PrimitiveCombination2AA1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCond2AAL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2AAQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2AASL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AASQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG

            Warm ("PCond2AA.EvalStep");
            Primitive.hotPrimitives.Note (this.procedure);
#endif

            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AAL : PCond2AA
    {
        // (s a l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2AAL (PrimitiveCombination2AA predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2AAA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2AAL1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2AALL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AALQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif

            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AAA : PCond2AAL
    {
        // (s a a s)

        protected PCond2AAA (PrimitiveCombination2AA predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2AAA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2AAA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2AAAL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AAAQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AAAL : PCond2AAA
    {
        // (s a a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AAAL (PrimitiveCombination2AA predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AAAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AAAL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AAAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAAL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AAAA : PCond2AAAL
    {
        // (s a a l1)

        protected PCond2AAAA (PrimitiveCombination2AA predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AAAA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AAAA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AAAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAAA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2AAAA0 : PCond2AAAA
    {
        // (s a a a0)

        PCond2AAAA0 (PrimitiveCombination2AA predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2AAAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAAA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2AAAA1 : PCond2AAAA
    {
        // (s a a a1)

        PCond2AAAA1 (PrimitiveCombination2AA predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2AAAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAAA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2AAAL1 : PCond2AAAL
    {
        // (s a a l1)

        PCond2AAAL1 (PrimitiveCombination2AA predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AAAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAAL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AAAQ : PCond2AAA
    {
        // (s a a Q)
        public readonly object alternativeValue;
        PCond2AAAQ (PrimitiveCombination2AA predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2AAAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAAQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
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

    class PCond2AAA0 : PCond2AAA
    {
        // (s a a0 s)

        protected PCond2AAA0 (PrimitiveCombination2AA predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AAA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AAA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AAA0L : PCond2AAA0
    {
        // (s a a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AAA0L (PrimitiveCombination2AA predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AAA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AAA0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AAA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAA0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AAA0A : PCond2AAA0L
    {
        // (s a a0 l1)

        protected PCond2AAA0A (PrimitiveCombination2AA predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AAA0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AAA0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AAA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAA0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2AAA0A0 : PCond2AAA0A
    {
        // (s a a0 a0)

        PCond2AAA0A0 (PrimitiveCombination2AA predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2AAA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAA0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2AAA0A1 : PCond2AAA0A
    {
        // (s a a0 a1)

        PCond2AAA0A1 (PrimitiveCombination2AA predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2AAA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAA0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2AAA0L1 : PCond2AAA0L
    {
        // (s a a0 l1)

        PCond2AAA0L1 (PrimitiveCombination2AA predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AAA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAA0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AAA0Q : PCond2AAA0
    {
        // (s a a0 Q)
        public readonly object alternativeValue;
        PCond2AAA0Q (PrimitiveCombination2AA predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2AAA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAA0Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
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

    class PCond2AAA1 : PCond2AAA
    {
        // (s a a1 s)

        protected PCond2AAA1 (PrimitiveCombination2AA predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AAA1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AAA1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AAA1L : PCond2AAA1
    {
        // (s a a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AAA1L (PrimitiveCombination2AA predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AAA1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AAA1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AAA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAA1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AAA1A : PCond2AAA1L
    {
        // (s a a1 l1)

        protected PCond2AAA1A (PrimitiveCombination2AA predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AAA1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AAA1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AAA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAA1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2AAA1A0 : PCond2AAA1A
    {
        // (s a a1 a0)

        PCond2AAA1A0 (PrimitiveCombination2AA predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2AAA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAA1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2AAA1A1 : PCond2AAA1A
    {
        // (s a a1 a1)

        PCond2AAA1A1 (PrimitiveCombination2AA predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2AAA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAA1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2AAA1L1 : PCond2AAA1L
    {
        // (s a a1 l1)

        PCond2AAA1L1 (PrimitiveCombination2AA predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AAA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAA1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AAA1Q : PCond2AAA1
    {
        // (s a a1 Q)
        public readonly object alternativeValue;
        PCond2AAA1Q (PrimitiveCombination2AA predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2AAA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAA1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
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

    class PCond2AAL1 : PCond2AAL
    {
        // (s a l1 s)

        protected PCond2AAL1 (PrimitiveCombination2AA predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AAL1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AAL1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AAL1L : PCond2AAL1
    {
        // (s a l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AAL1L (PrimitiveCombination2AA predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AAL1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AAL1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AAL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAL1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AAL1A : PCond2AAL1L
    {
        // (s a l1 l1)

        protected PCond2AAL1A (PrimitiveCombination2AA predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AAL1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AAL1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AAL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAL1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AAL1A0 : PCond2AAL1A
    {
        // (s a l1 a0)

        PCond2AAL1A0 (PrimitiveCombination2AA predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2AAL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAL1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AAL1A1 : PCond2AAL1A
    {
        // (s a l1 a1)

        PCond2AAL1A1 (PrimitiveCombination2AA predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2AAL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAL1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AAL1L1 : PCond2AAL1L
    {
        // (s a l1 l1)

        PCond2AAL1L1 (PrimitiveCombination2AA predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AAL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAL1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AAL1Q : PCond2AAL1
    {
        // (s a l1 Q)
        public readonly object alternativeValue;
        PCond2AAL1Q (PrimitiveCombination2AA predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2AAL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAL1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AALL : PCond2AAL
    {
        // (s a l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AALL (PrimitiveCombination2AA predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AALA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AALL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AALL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AALL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AALA : PCond2AALL
    {
        // (s a l l1)

        protected PCond2AALA (PrimitiveCombination2AA predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AALA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AALA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AALA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AALA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AALA0 : PCond2AALA
    {
        // (s a l a0)

        PCond2AALA0 (PrimitiveCombination2AA predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2AALA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AALA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AALA1 : PCond2AALA
    {
        // (s a l a1)

        PCond2AALA1 (PrimitiveCombination2AA predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2AALA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AALA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AALL1 : PCond2AALL
    {
        // (s a l l1)

        PCond2AALL1 (PrimitiveCombination2AA predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AALL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AALL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AALQ : PCond2AAL
    {
        // (s a l Q)
        public readonly object alternativeValue;
        PCond2AALQ (PrimitiveCombination2AA predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2AALQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AALQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AAQ : PCond2AA
    {
        // (s a q s)
        public readonly object consequentValue;

        protected PCond2AAQ (PrimitiveCombination2AA predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AAQL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AAQQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AAQL : PCond2AAQ
    {
        // (s a q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2AAQL (PrimitiveCombination2AA predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AAQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AAQL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AAQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAQL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AAQA : PCond2AAQL
    {
        // (s a q a)
        protected PCond2AAQA (PrimitiveCombination2AA predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AAQA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AAQA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AAQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAQA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2AAQA0 : PCond2AAQA
    {
        // (s a q a0)
        PCond2AAQA0 (PrimitiveCombination2AA predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2AAQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAQA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2AAQA1 : PCond2AAQA
    {
        // (s a q a1)
        PCond2AAQA1 (PrimitiveCombination2AA predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2AAQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAQA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
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

    sealed class PCond2AAQL1 : PCond2AAQL
    {
        // (s a q l1)
        PCond2AAQL1 (PrimitiveCombination2AA predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AAQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AAQL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AAQQ : PCond2AAQ
    {
        // (s a q q)
        public readonly object alternativeValue;

        PCond2AAQQ (PrimitiveCombination2AA predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2AAQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2AAQQ.EvalStep";
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
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

    class PCond2AASL : PCond2AA
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2AASL (PrimitiveCombination2AA predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2AA predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AASA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AASL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AASL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AASL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AASA : PCond2AASL
    {
        protected PCond2AASA (PrimitiveCombination2AA predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AA predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AASA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AASA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AASA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AASA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AASA0 : PCond2AASA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2AASA0 (PrimitiveCombination2AA predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AA predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2AASA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AASA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AASA1 : PCond2AASA
    {
        PCond2AASA1 (PrimitiveCombination2AA predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AA predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2AASA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AASA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AASL1 : PCond2AASL
    {
        PCond2AASL1 (PrimitiveCombination2AA predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AA predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2AASL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AASL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AASQ : PCond2AA
    {
        // (s a s q)
        public readonly object alternativeValue;

        PCond2AASQ (PrimitiveCombination2AA predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2AASQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AASQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA0 : PCond2AA
    {
        // (s a0 s s)
        protected PCond2AA0 (PrimitiveCombination2AA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2AA0L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2AA0Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2AA0SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AA0SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2AA0.EvalStep");
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA0L : PCond2AA0
    {
        // (s a0 l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2AA0L (PrimitiveCombination2AA0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2AA0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2AA0L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2AA0LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AA0LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA0A : PCond2AA0L
    {
        // (s a0 a s)

        protected PCond2AA0A (PrimitiveCombination2AA0 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2AA0A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2AA0A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2AA0AL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AA0AQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA0AL : PCond2AA0A
    {
        // (s a0 a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AA0AL (PrimitiveCombination2AA0 predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AA0AA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AA0AL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AA0AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0AL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA0AA : PCond2AA0AL
    {
        // (s a0 a l1)

        protected PCond2AA0AA (PrimitiveCombination2AA0 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AA0AA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AA0AA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AA0AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0AA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
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

    sealed class PCond2AA0AA0 : PCond2AA0AA
    {
        // (s a0 a a0)

        PCond2AA0AA0 (PrimitiveCombination2AA0 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2AA0AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0AA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
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

    sealed class PCond2AA0AA1 : PCond2AA0AA
    {
        // (s a0 a a1)

        PCond2AA0AA1 (PrimitiveCombination2AA0 predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2AA0AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0AA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
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

    sealed class PCond2AA0AL1 : PCond2AA0AL
    {
        // (s a0 a l1)

        PCond2AA0AL1 (PrimitiveCombination2AA0 predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AA0AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0AL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA0AQ : PCond2AA0A
    {
        // (s a0 a Q)
        public readonly object alternativeValue;
        PCond2AA0AQ (PrimitiveCombination2AA0 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2AA0AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0AQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
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

    class PCond2AA0A0 : PCond2AA0A
    {
        // (s a0 a0 s)

        protected PCond2AA0A0 (PrimitiveCombination2AA0 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AA0A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AA0A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA0A0L : PCond2AA0A0
    {
        // (s a0 a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AA0A0L (PrimitiveCombination2AA0 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AA0A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AA0A0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AA0A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0A0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA0A0A : PCond2AA0A0L
    {
        // (s a0 a0 l1)

        protected PCond2AA0A0A (PrimitiveCombination2AA0 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AA0A0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AA0A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AA0A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0A0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
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

    sealed class PCond2AA0A0A0 : PCond2AA0A0A
    {
        // (s a0 a0 a0)

        PCond2AA0A0A0 (PrimitiveCombination2AA0 predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2AA0A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0A0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
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

    sealed class PCond2AA0A0A1 : PCond2AA0A0A
    {
        // (s a0 a0 a1)

        PCond2AA0A0A1 (PrimitiveCombination2AA0 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2AA0A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0A0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
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

    sealed class PCond2AA0A0L1 : PCond2AA0A0L
    {
        // (s a0 a0 l1)

        PCond2AA0A0L1 (PrimitiveCombination2AA0 predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AA0A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0A0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA0A0Q : PCond2AA0A0
    {
        // (s a0 a0 Q)
        public readonly object alternativeValue;
        PCond2AA0A0Q (PrimitiveCombination2AA0 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2AA0A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0A0Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
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

    class PCond2AA0A1 : PCond2AA0A
    {
        // (s a0 a1 s)

        protected PCond2AA0A1 (PrimitiveCombination2AA0 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AA0A1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AA0A1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA0A1L : PCond2AA0A1
    {
        // (s a0 a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AA0A1L (PrimitiveCombination2AA0 predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AA0A1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AA0A1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AA0A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0A1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA0A1A : PCond2AA0A1L
    {
        // (s a0 a1 l1)

        protected PCond2AA0A1A (PrimitiveCombination2AA0 predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AA0A1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AA0A1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AA0A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0A1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
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

    sealed class PCond2AA0A1A0 : PCond2AA0A1A
    {
        // (s a0 a1 a0)

        PCond2AA0A1A0 (PrimitiveCombination2AA0 predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2AA0A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0A1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
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

    sealed class PCond2AA0A1A1 : PCond2AA0A1A
    {
        // (s a0 a1 a1)

        PCond2AA0A1A1 (PrimitiveCombination2AA0 predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2AA0A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0A1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
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

    sealed class PCond2AA0A1L1 : PCond2AA0A1L
    {
        // (s a0 a1 l1)

        PCond2AA0A1L1 (PrimitiveCombination2AA0 predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AA0A1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0A1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA0A1Q : PCond2AA0A1
    {
        // (s a0 a1 Q)
        public readonly object alternativeValue;
        PCond2AA0A1Q (PrimitiveCombination2AA0 predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2AA0A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0A1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
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

    class PCond2AA0L1 : PCond2AA0L
    {
        // (s a0 l1 s)

        protected PCond2AA0L1 (PrimitiveCombination2AA0 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AA0L1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AA0L1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA0L1L : PCond2AA0L1
    {
        // (s a0 l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AA0L1L (PrimitiveCombination2AA0 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AA0L1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AA0L1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AA0L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0L1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA0L1A : PCond2AA0L1L
    {
        // (s a0 l1 l1)

        protected PCond2AA0L1A (PrimitiveCombination2AA0 predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AA0L1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AA0L1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AA0L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0L1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA0L1A0 : PCond2AA0L1A
    {
        // (s a0 l1 a0)

        PCond2AA0L1A0 (PrimitiveCombination2AA0 predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2AA0L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0L1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA0L1A1 : PCond2AA0L1A
    {
        // (s a0 l1 a1)

        PCond2AA0L1A1 (PrimitiveCombination2AA0 predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2AA0L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0L1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA0L1L1 : PCond2AA0L1L
    {
        // (s a0 l1 l1)

        PCond2AA0L1L1 (PrimitiveCombination2AA0 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AA0L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0L1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA0L1Q : PCond2AA0L1
    {
        // (s a0 l1 Q)
        public readonly object alternativeValue;
        PCond2AA0L1Q (PrimitiveCombination2AA0 predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2AA0L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0L1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA0LL : PCond2AA0L
    {
        // (s a0 l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AA0LL (PrimitiveCombination2AA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AA0LA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AA0LL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AA0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0LL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA0LA : PCond2AA0LL
    {
        // (s a0 l l1)

        protected PCond2AA0LA (PrimitiveCombination2AA0 predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AA0LA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AA0LA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AA0LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0LA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA0LA0 : PCond2AA0LA
    {
        // (s a0 l a0)

        PCond2AA0LA0 (PrimitiveCombination2AA0 predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2AA0LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0LA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA0LA1 : PCond2AA0LA
    {
        // (s a0 l a1)

        PCond2AA0LA1 (PrimitiveCombination2AA0 predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2AA0LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0LA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA0LL1 : PCond2AA0LL
    {
        // (s a0 l l1)

        PCond2AA0LL1 (PrimitiveCombination2AA0 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AA0LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0LL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA0LQ : PCond2AA0L
    {
        // (s a0 l Q)
        public readonly object alternativeValue;
        PCond2AA0LQ (PrimitiveCombination2AA0 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2AA0LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0LQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA0Q : PCond2AA0
    {
        // (s a0 q s)
        public readonly object consequentValue;

        protected PCond2AA0Q (PrimitiveCombination2AA0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AA0QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AA0QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA0QL : PCond2AA0Q
    {
        // (s a0 q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2AA0QL (PrimitiveCombination2AA0 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AA0QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AA0QL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AA0QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0QL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA0QA : PCond2AA0QL
    {
        // (s a0 q a)
        protected PCond2AA0QA (PrimitiveCombination2AA0 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AA0QA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AA0QA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AA0QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0QA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
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

    sealed class PCond2AA0QA0 : PCond2AA0QA
    {
        // (s a0 q a0)
        PCond2AA0QA0 (PrimitiveCombination2AA0 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2AA0QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0QA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
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

    sealed class PCond2AA0QA1 : PCond2AA0QA
    {
        // (s a0 q a1)
        PCond2AA0QA1 (PrimitiveCombination2AA0 predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2AA0QA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0QA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
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

    sealed class PCond2AA0QL1 : PCond2AA0QL
    {
        // (s a0 q l1)
        PCond2AA0QL1 (PrimitiveCombination2AA0 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AA0QL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0QL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA0QQ : PCond2AA0Q
    {
        // (s a0 q q)
        public readonly object alternativeValue;

        PCond2AA0QQ (PrimitiveCombination2AA0 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2AA0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2AA0QQ.EvalStep";
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
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

    class PCond2AA0SL : PCond2AA0
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2AA0SL (PrimitiveCombination2AA0 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2AA0 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AA0SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AA0SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AA0SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0SL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA0SA : PCond2AA0SL
    {
        protected PCond2AA0SA (PrimitiveCombination2AA0 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AA0 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AA0SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AA0SA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AA0SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0SA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA0SA0 : PCond2AA0SA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2AA0SA0 (PrimitiveCombination2AA0 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AA0 predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2AA0SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AA0SA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA0SA1 : PCond2AA0SA
    {
        PCond2AA0SA1 (PrimitiveCombination2AA0 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AA0 predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2AA0SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0SA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA0SL1 : PCond2AA0SL
    {
        PCond2AA0SL1 (PrimitiveCombination2AA0 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AA0 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2AA0SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0SL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA0SQ : PCond2AA0
    {
        // (s a0 s q)
        public readonly object alternativeValue;

        PCond2AA0SQ (PrimitiveCombination2AA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA0 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2AA0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA0SQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA1 : PCond2AA
    {
        // (s a1 s s)
        protected PCond2AA1 (PrimitiveCombination2AA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2AA1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2AA1Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2AA1SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AA1SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2AA1.EvalStep");
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA1L : PCond2AA1
    {
        // (s a1 l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2AA1L (PrimitiveCombination2AA1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2AA1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2AA1L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2AA1LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AA1LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA1A : PCond2AA1L
    {
        // (s a1 a s)

        protected PCond2AA1A (PrimitiveCombination2AA1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2AA1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2AA1A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2AA1AL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AA1AQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA1AL : PCond2AA1A
    {
        // (s a1 a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AA1AL (PrimitiveCombination2AA1 predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AA1AA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AA1AL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AA1AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1AL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA1AA : PCond2AA1AL
    {
        // (s a1 a l1)

        protected PCond2AA1AA (PrimitiveCombination2AA1 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AA1AA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AA1AA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AA1AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1AA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
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

    sealed class PCond2AA1AA0 : PCond2AA1AA
    {
        // (s a1 a a0)

        PCond2AA1AA0 (PrimitiveCombination2AA1 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2AA1AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1AA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
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

    sealed class PCond2AA1AA1 : PCond2AA1AA
    {
        // (s a1 a a1)

        PCond2AA1AA1 (PrimitiveCombination2AA1 predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2AA1AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1AA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
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

    sealed class PCond2AA1AL1 : PCond2AA1AL
    {
        // (s a1 a l1)

        PCond2AA1AL1 (PrimitiveCombination2AA1 predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AA1AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1AL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA1AQ : PCond2AA1A
    {
        // (s a1 a Q)
        public readonly object alternativeValue;
        PCond2AA1AQ (PrimitiveCombination2AA1 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2AA1AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1AQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
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

    class PCond2AA1A0 : PCond2AA1A
    {
        // (s a1 a0 s)

        protected PCond2AA1A0 (PrimitiveCombination2AA1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AA1A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AA1A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA1A0L : PCond2AA1A0
    {
        // (s a1 a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AA1A0L (PrimitiveCombination2AA1 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AA1A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AA1A0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AA1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1A0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA1A0A : PCond2AA1A0L
    {
        // (s a1 a0 l1)

        protected PCond2AA1A0A (PrimitiveCombination2AA1 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AA1A0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AA1A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AA1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1A0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
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

    sealed class PCond2AA1A0A0 : PCond2AA1A0A
    {
        // (s a1 a0 a0)

        PCond2AA1A0A0 (PrimitiveCombination2AA1 predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2AA1A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1A0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
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

    sealed class PCond2AA1A0A1 : PCond2AA1A0A
    {
        // (s a1 a0 a1)

        PCond2AA1A0A1 (PrimitiveCombination2AA1 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2AA1A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1A0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
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

    sealed class PCond2AA1A0L1 : PCond2AA1A0L
    {
        // (s a1 a0 l1)

        PCond2AA1A0L1 (PrimitiveCombination2AA1 predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AA1A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1A0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA1A0Q : PCond2AA1A0
    {
        // (s a1 a0 Q)
        public readonly object alternativeValue;
        PCond2AA1A0Q (PrimitiveCombination2AA1 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2AA1A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1A0Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
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

    class PCond2AA1A1 : PCond2AA1A
    {
        // (s a1 a1 s)

        protected PCond2AA1A1 (PrimitiveCombination2AA1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AA1A1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AA1A1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA1A1L : PCond2AA1A1
    {
        // (s a1 a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AA1A1L (PrimitiveCombination2AA1 predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AA1A1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AA1A1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AA1A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1A1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA1A1A : PCond2AA1A1L
    {
        // (s a1 a1 l1)

        protected PCond2AA1A1A (PrimitiveCombination2AA1 predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AA1A1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AA1A1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AA1A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1A1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
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

    sealed class PCond2AA1A1A0 : PCond2AA1A1A
    {
        // (s a1 a1 a0)

        PCond2AA1A1A0 (PrimitiveCombination2AA1 predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2AA1A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1A1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
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

    sealed class PCond2AA1A1A1 : PCond2AA1A1A
    {
        // (s a1 a1 a1)

        PCond2AA1A1A1 (PrimitiveCombination2AA1 predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2AA1A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1A1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
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

    sealed class PCond2AA1A1L1 : PCond2AA1A1L
    {
        // (s a1 a1 l1)

        PCond2AA1A1L1 (PrimitiveCombination2AA1 predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AA1A1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1A1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA1A1Q : PCond2AA1A1
    {
        // (s a1 a1 Q)
        public readonly object alternativeValue;
        PCond2AA1A1Q (PrimitiveCombination2AA1 predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2AA1A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1A1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
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

    class PCond2AA1L1 : PCond2AA1L
    {
        // (s a1 l1 s)

        protected PCond2AA1L1 (PrimitiveCombination2AA1 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AA1L1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AA1L1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA1L1L : PCond2AA1L1
    {
        // (s a1 l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AA1L1L (PrimitiveCombination2AA1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AA1L1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AA1L1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AA1L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1L1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA1L1A : PCond2AA1L1L
    {
        // (s a1 l1 l1)

        protected PCond2AA1L1A (PrimitiveCombination2AA1 predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AA1L1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AA1L1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AA1L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1L1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA1L1A0 : PCond2AA1L1A
    {
        // (s a1 l1 a0)

        PCond2AA1L1A0 (PrimitiveCombination2AA1 predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2AA1L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1L1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA1L1A1 : PCond2AA1L1A
    {
        // (s a1 l1 a1)

        PCond2AA1L1A1 (PrimitiveCombination2AA1 predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2AA1L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1L1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA1L1L1 : PCond2AA1L1L
    {
        // (s a1 l1 l1)

        PCond2AA1L1L1 (PrimitiveCombination2AA1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AA1L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1L1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA1L1Q : PCond2AA1L1
    {
        // (s a1 l1 Q)
        public readonly object alternativeValue;
        PCond2AA1L1Q (PrimitiveCombination2AA1 predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2AA1L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1L1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA1LL : PCond2AA1L
    {
        // (s a1 l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AA1LL (PrimitiveCombination2AA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AA1LA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AA1LL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AA1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1LL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA1LA : PCond2AA1LL
    {
        // (s a1 l l1)

        protected PCond2AA1LA (PrimitiveCombination2AA1 predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AA1LA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AA1LA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AA1LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1LA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA1LA0 : PCond2AA1LA
    {
        // (s a1 l a0)

        PCond2AA1LA0 (PrimitiveCombination2AA1 predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2AA1LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1LA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA1LA1 : PCond2AA1LA
    {
        // (s a1 l a1)

        PCond2AA1LA1 (PrimitiveCombination2AA1 predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2AA1LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1LA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA1LL1 : PCond2AA1LL
    {
        // (s a1 l l1)

        PCond2AA1LL1 (PrimitiveCombination2AA1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AA1LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1LL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA1LQ : PCond2AA1L
    {
        // (s a1 l Q)
        public readonly object alternativeValue;
        PCond2AA1LQ (PrimitiveCombination2AA1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2AA1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1LQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA1Q : PCond2AA1
    {
        // (s a1 q s)
        public readonly object consequentValue;

        protected PCond2AA1Q (PrimitiveCombination2AA1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AA1QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AA1QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA1QL : PCond2AA1Q
    {
        // (s a1 q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2AA1QL (PrimitiveCombination2AA1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AA1QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AA1QL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AA1QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1QL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA1QA : PCond2AA1QL
    {
        // (s a1 q a)
        protected PCond2AA1QA (PrimitiveCombination2AA1 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AA1QA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AA1QA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AA1QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1QA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
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

    sealed class PCond2AA1QA0 : PCond2AA1QA
    {
        // (s a1 q a0)
        PCond2AA1QA0 (PrimitiveCombination2AA1 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2AA1QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1QA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
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

    sealed class PCond2AA1QA1 : PCond2AA1QA
    {
        // (s a1 q a1)
        PCond2AA1QA1 (PrimitiveCombination2AA1 predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2AA1QA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1QA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
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

    sealed class PCond2AA1QL1 : PCond2AA1QL
    {
        // (s a1 q l1)
        PCond2AA1QL1 (PrimitiveCombination2AA1 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AA1QL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1QL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA1QQ : PCond2AA1Q
    {
        // (s a1 q q)
        public readonly object alternativeValue;

        PCond2AA1QQ (PrimitiveCombination2AA1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2AA1QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2AA1QQ.EvalStep";
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
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

    class PCond2AA1SL : PCond2AA1
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2AA1SL (PrimitiveCombination2AA1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2AA1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AA1SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AA1SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AA1SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1SL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AA1SA : PCond2AA1SL
    {
        protected PCond2AA1SA (PrimitiveCombination2AA1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AA1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AA1SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AA1SA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AA1SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1SA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA1SA0 : PCond2AA1SA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2AA1SA0 (PrimitiveCombination2AA1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AA1 predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2AA1SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AA1SA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA1SA1 : PCond2AA1SA
    {
        PCond2AA1SA1 (PrimitiveCombination2AA1 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AA1 predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2AA1SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1SA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA1SL1 : PCond2AA1SL
    {
        PCond2AA1SL1 (PrimitiveCombination2AA1 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AA1 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2AA1SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1SL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AA1SQ : PCond2AA1
    {
        // (s a1 s q)
        public readonly object alternativeValue;

        PCond2AA1SQ (PrimitiveCombination2AA1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AA1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2AA1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AA1SQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL1 : PCond2AL
    {
        // (a l1 s s)
        protected PCond2AL1 (PrimitiveCombination2AL1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2AL1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2AL1Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2AL1SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AL1SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2AL1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL1L : PCond2AL1
    {
        // (a l1 l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2AL1L (PrimitiveCombination2AL1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2AL1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2AL1L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2AL1LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AL1LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL1A : PCond2AL1L
    {
        // (a l1 a s)

        protected PCond2AL1A (PrimitiveCombination2AL1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2AL1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2AL1A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2AL1AL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AL1AQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL1AL : PCond2AL1A
    {
        // (a l1 a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AL1AL (PrimitiveCombination2AL1 predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AL1AA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AL1AL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AL1AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1AL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL1AA : PCond2AL1AL
    {
        // (a l1 a l1)

        protected PCond2AL1AA (PrimitiveCombination2AL1 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AL1AA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AL1AA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AL1AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1AA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2AL1AA0 : PCond2AL1AA
    {
        // (a l1 a a0)

        PCond2AL1AA0 (PrimitiveCombination2AL1 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2AL1AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1AA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2AL1AA1 : PCond2AL1AA
    {
        // (a l1 a a1)

        PCond2AL1AA1 (PrimitiveCombination2AL1 predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2AL1AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1AA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2AL1AL1 : PCond2AL1AL
    {
        // (a l1 a l1)

        PCond2AL1AL1 (PrimitiveCombination2AL1 predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AL1AL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1AL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AL1AQ : PCond2AL1A
    {
        // (a l1 a Q)
        public readonly object alternativeValue;
        PCond2AL1AQ (PrimitiveCombination2AL1 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2AL1AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1AQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    class PCond2AL1A0 : PCond2AL1A
    {
        // (a l1 a0 s)

        protected PCond2AL1A0 (PrimitiveCombination2AL1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AL1A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AL1A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL1A0L : PCond2AL1A0
    {
        // (a l1 a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AL1A0L (PrimitiveCombination2AL1 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AL1A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AL1A0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AL1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1A0L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL1A0A : PCond2AL1A0L
    {
        // (a l1 a0 l1)

        protected PCond2AL1A0A (PrimitiveCombination2AL1 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AL1A0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AL1A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AL1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1A0A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2AL1A0A0 : PCond2AL1A0A
    {
        // (a l1 a0 a0)

        PCond2AL1A0A0 (PrimitiveCombination2AL1 predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2AL1A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1A0A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2AL1A0A1 : PCond2AL1A0A
    {
        // (a l1 a0 a1)

        PCond2AL1A0A1 (PrimitiveCombination2AL1 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2AL1A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1A0A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2AL1A0L1 : PCond2AL1A0L
    {
        // (a l1 a0 l1)

        PCond2AL1A0L1 (PrimitiveCombination2AL1 predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AL1A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1A0L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AL1A0Q : PCond2AL1A0
    {
        // (a l1 a0 Q)
        public readonly object alternativeValue;
        PCond2AL1A0Q (PrimitiveCombination2AL1 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2AL1A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1A0Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    class PCond2AL1A1 : PCond2AL1A
    {
        // (a l1 a1 s)

        protected PCond2AL1A1 (PrimitiveCombination2AL1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AL1A1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AL1A1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL1A1L : PCond2AL1A1
    {
        // (a l1 a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AL1A1L (PrimitiveCombination2AL1 predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AL1A1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AL1A1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AL1A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1A1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL1A1A : PCond2AL1A1L
    {
        // (a l1 a1 l1)

        protected PCond2AL1A1A (PrimitiveCombination2AL1 predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AL1A1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AL1A1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AL1A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1A1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2AL1A1A0 : PCond2AL1A1A
    {
        // (a l1 a1 a0)

        PCond2AL1A1A0 (PrimitiveCombination2AL1 predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2AL1A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1A1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2AL1A1A1 : PCond2AL1A1A
    {
        // (a l1 a1 a1)

        PCond2AL1A1A1 (PrimitiveCombination2AL1 predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2AL1A1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1A1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2AL1A1L1 : PCond2AL1A1L
    {
        // (a l1 a1 l1)

        PCond2AL1A1L1 (PrimitiveCombination2AL1 predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AL1A1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1A1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AL1A1Q : PCond2AL1A1
    {
        // (a l1 a1 Q)
        public readonly object alternativeValue;
        PCond2AL1A1Q (PrimitiveCombination2AL1 predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2AL1A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1A1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    class PCond2AL1L1 : PCond2AL1L
    {
        // (a l1 l1 s)

        protected PCond2AL1L1 (PrimitiveCombination2AL1 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AL1L1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AL1L1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL1L1L : PCond2AL1L1
    {
        // (a l1 l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AL1L1L (PrimitiveCombination2AL1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AL1L1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AL1L1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AL1L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1L1L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL1L1A : PCond2AL1L1L
    {
        // (a l1 l1 l1)

        protected PCond2AL1L1A (PrimitiveCombination2AL1 predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AL1L1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AL1L1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AL1L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1L1A.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AL1L1A0 : PCond2AL1L1A
    {
        // (a l1 l1 a0)

        PCond2AL1L1A0 (PrimitiveCombination2AL1 predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2AL1L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1L1A0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AL1L1A1 : PCond2AL1L1A
    {
        // (a l1 l1 a1)

        PCond2AL1L1A1 (PrimitiveCombination2AL1 predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2AL1L1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1L1A1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AL1L1L1 : PCond2AL1L1L
    {
        // (a l1 l1 l1)

        PCond2AL1L1L1 (PrimitiveCombination2AL1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AL1L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1L1L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AL1L1Q : PCond2AL1L1
    {
        // (a l1 l1 Q)
        public readonly object alternativeValue;
        PCond2AL1L1Q (PrimitiveCombination2AL1 predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2AL1L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1L1Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL1LL : PCond2AL1L
    {
        // (a l1 l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AL1LL (PrimitiveCombination2AL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AL1LA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AL1LL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AL1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1LL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL1LA : PCond2AL1LL
    {
        // (a l1 l l1)

        protected PCond2AL1LA (PrimitiveCombination2AL1 predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AL1LA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AL1LA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AL1LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1LA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AL1LA0 : PCond2AL1LA
    {
        // (a l1 l a0)

        PCond2AL1LA0 (PrimitiveCombination2AL1 predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2AL1LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1LA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AL1LA1 : PCond2AL1LA
    {
        // (a l1 l a1)

        PCond2AL1LA1 (PrimitiveCombination2AL1 predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2AL1LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1LA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AL1LL1 : PCond2AL1LL
    {
        // (a l1 l l1)

        PCond2AL1LL1 (PrimitiveCombination2AL1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AL1LL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1LL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AL1LQ : PCond2AL1L
    {
        // (a l1 l Q)
        public readonly object alternativeValue;
        PCond2AL1LQ (PrimitiveCombination2AL1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2AL1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1LQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL1Q : PCond2AL1
    {
        // (a l1 q s)
        public readonly object consequentValue;

        protected PCond2AL1Q (PrimitiveCombination2AL1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AL1QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AL1QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AL1Q (predicate, consequent, alternative);
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
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL1QL : PCond2AL1Q
    {
        // (a l1 q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2AL1QL (PrimitiveCombination2AL1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AL1QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AL1QL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AL1QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1QL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL1QA : PCond2AL1QL
    {
        // (a l1 q a)
        protected PCond2AL1QA (PrimitiveCombination2AL1 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AL1QA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AL1QA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AL1QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1QA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2AL1QA0 : PCond2AL1QA
    {
        // (a l1 q a0)
        PCond2AL1QA0 (PrimitiveCombination2AL1 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2AL1QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1QA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2AL1QA1 : PCond2AL1QA
    {
        // (a l1 q a1)
        PCond2AL1QA1 (PrimitiveCombination2AL1 predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2AL1QA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1QA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    sealed class PCond2AL1QL1 : PCond2AL1QL
    {
        // (a l1 q l1)
        PCond2AL1QL1 (PrimitiveCombination2AL1 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AL1QL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1QL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AL1QQ : PCond2AL1Q
    {
        // (a l1 q q)
        public readonly object alternativeValue;

        PCond2AL1QQ (PrimitiveCombination2AL1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2AL1QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2AL1QQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();


#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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

    class PCond2AL1SL : PCond2AL1
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2AL1SL (PrimitiveCombination2AL1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2AL1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AL1SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AL1SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AL1SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1SL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AL1SA : PCond2AL1SL
    {
        protected PCond2AL1SA (PrimitiveCombination2AL1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AL1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AL1SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AL1SA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AL1SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1SA.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AL1SA0 : PCond2AL1SA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2AL1SA0 (PrimitiveCombination2AL1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AL1 predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2AL1SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AL1SA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AL1SA1 : PCond2AL1SA
    {
        PCond2AL1SA1 (PrimitiveCombination2AL1 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AL1 predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2AL1SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1SA1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AL1SL1 : PCond2AL1SL
    {
        PCond2AL1SL1 (PrimitiveCombination2AL1 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AL1 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2AL1SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1SL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AL1SQ : PCond2AL1
    {
        // (a l1 s q)
        public readonly object alternativeValue;

        PCond2AL1SQ (PrimitiveCombination2AL1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AL1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCond2AL1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AL1SQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AQ : PCond2A
    {
        // (s q s s)
        public readonly object rand1Value;

        protected PCond2AQ (PrimitiveCombination2AQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCond2AQL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCond2AQQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2AQSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AQSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("PCond2AQ.EvalStep");
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AQL : PCond2AQ
    {
        // (s q l s)
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2AQL (PrimitiveCombination2AQ predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2AQA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCond2AQL1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2AQLL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AQLQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AQA : PCond2AQL
    {
        // (s q a s)

        protected PCond2AQA (PrimitiveCombination2AQ predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCond2AQA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCond2AQA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCond2AQAL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AQAQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AQAL : PCond2AQA
    {
        // (s q a l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AQAL (PrimitiveCombination2AQ predicate, Argument consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AQAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AQAL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AQAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQAL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AQAA : PCond2AQAL
    {
        // (s q a l1)

        protected PCond2AQAA (PrimitiveCombination2AQ predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AQAA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AQAA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AQAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQAA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
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

    sealed class PCond2AQAA0 : PCond2AQAA
    {
        // (s q a a0)

        PCond2AQAA0 (PrimitiveCombination2AQ predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCond2AQAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQAA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
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

    sealed class PCond2AQAA1 : PCond2AQAA
    {
        // (s q a a1)

        PCond2AQAA1 (PrimitiveCombination2AQ predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument consequent, Argument1 alternative)
        {
            return
                new PCond2AQAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQAA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
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

    sealed class PCond2AQAL1 : PCond2AQAL
    {
        // (s q a l1)

        PCond2AQAL1 (PrimitiveCombination2AQ predicate, Argument consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AQAL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQAL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AQAQ : PCond2AQA
    {
        // (s q a Q)
        public readonly object alternativeValue;
        PCond2AQAQ (PrimitiveCombination2AQ predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCond2AQAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQAQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
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

    class PCond2AQA0 : PCond2AQA
    {
        // (s q a0 s)

        protected PCond2AQA0 (PrimitiveCombination2AQ predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AQA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AQA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AQA0L : PCond2AQA0
    {
        // (s q a0 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AQA0L (PrimitiveCombination2AQ predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AQA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AQA0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AQA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQA0L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AQA0A : PCond2AQA0L
    {
        // (s q a0 l1)

        protected PCond2AQA0A (PrimitiveCombination2AQ predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AQA0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AQA0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AQA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQA0A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
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

    sealed class PCond2AQA0A0 : PCond2AQA0A
    {
        // (s q a0 a0)

        PCond2AQA0A0 (PrimitiveCombination2AQ predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new PCond2AQA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQA0A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
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

    sealed class PCond2AQA0A1 : PCond2AQA0A
    {
        // (s q a0 a1)

        PCond2AQA0A1 (PrimitiveCombination2AQ predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCond2AQA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQA0A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
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

    sealed class PCond2AQA0L1 : PCond2AQA0L
    {
        // (s q a0 l1)

        PCond2AQA0L1 (PrimitiveCombination2AQ predicate, Argument0 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument0 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AQA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQA0L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AQA0Q : PCond2AQA0
    {
        // (s q a0 Q)
        public readonly object alternativeValue;
        PCond2AQA0Q (PrimitiveCombination2AQ predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCond2AQA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQA0Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
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

    class PCond2AQA1 : PCond2AQA
    {
        // (s q a1 s)

        protected PCond2AQA1 (PrimitiveCombination2AQ predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AQA1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AQA1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AQA1L : PCond2AQA1
    {
        // (s q a1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AQA1L (PrimitiveCombination2AQ predicate, Argument1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AQA1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AQA1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AQA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQA1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AQA1A : PCond2AQA1L
    {
        // (s q a1 l1)

        protected PCond2AQA1A (PrimitiveCombination2AQ predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AQA1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AQA1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AQA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQA1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
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

    sealed class PCond2AQA1A0 : PCond2AQA1A
    {
        // (s q a1 a0)

        PCond2AQA1A0 (PrimitiveCombination2AQ predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new PCond2AQA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQA1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
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

    sealed class PCond2AQA1A1 : PCond2AQA1A
    {
        // (s q a1 a1)

        PCond2AQA1A1 (PrimitiveCombination2AQ predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new PCond2AQA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQA1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
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

    sealed class PCond2AQA1L1 : PCond2AQA1L
    {
        // (s q a1 l1)

        PCond2AQA1L1 (PrimitiveCombination2AQ predicate, Argument1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AQA1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQA1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AQA1Q : PCond2AQA1
    {
        // (s q a1 Q)
        public readonly object alternativeValue;
        PCond2AQA1Q (PrimitiveCombination2AQ predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new PCond2AQA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQA1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
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

    class PCond2AQL1 : PCond2AQL
    {
        // (s q l1 s)

        protected PCond2AQL1 (PrimitiveCombination2AQ predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AQL1L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AQL1Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AQL1L : PCond2AQL1
    {
        // (s q l1 l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AQL1L (PrimitiveCombination2AQ predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AQL1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AQL1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AQL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQL1L.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AQL1A : PCond2AQL1L
    {
        // (s q l1 l1)

        protected PCond2AQL1A (PrimitiveCombination2AQ predicate, LexicalVariable1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, LexicalVariable1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AQL1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AQL1A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AQL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQL1A.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AQL1A0 : PCond2AQL1A
    {
        // (s q l1 a0)

        PCond2AQL1A0 (PrimitiveCombination2AQ predicate, LexicalVariable1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, LexicalVariable1 consequent, Argument0 alternative)
        {
            return
                new PCond2AQL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQL1A0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AQL1A1 : PCond2AQL1A
    {
        // (s q l1 a1)

        PCond2AQL1A1 (PrimitiveCombination2AQ predicate, LexicalVariable1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, LexicalVariable1 consequent, Argument1 alternative)
        {
            return
                new PCond2AQL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQL1A1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AQL1L1 : PCond2AQL1L
    {
        // (s q l1 l1)

        PCond2AQL1L1 (PrimitiveCombination2AQ predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AQL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQL1L1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AQL1Q : PCond2AQL1
    {
        // (s q l1 Q)
        public readonly object alternativeValue;
        PCond2AQL1Q (PrimitiveCombination2AQ predicate, LexicalVariable1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, LexicalVariable1 consequent, Quotation alternative)
        {
            return
                new PCond2AQL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQL1Q.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AQLL : PCond2AQL
    {
        // (s q l l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCond2AQLL (PrimitiveCombination2AQ predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AQLA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AQLL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AQLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQLL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AQLA : PCond2AQLL
    {
        // (s q l l1)

        protected PCond2AQLA (PrimitiveCombination2AQ predicate, LexicalVariable consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, LexicalVariable consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AQLA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AQLA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AQLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQLA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AQLA0 : PCond2AQLA
    {
        // (s q l a0)

        PCond2AQLA0 (PrimitiveCombination2AQ predicate, LexicalVariable consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, LexicalVariable consequent, Argument0 alternative)
        {
            return
                new PCond2AQLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQLA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AQLA1 : PCond2AQLA
    {
        // (s q l a1)

        PCond2AQLA1 (PrimitiveCombination2AQ predicate, LexicalVariable consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, LexicalVariable consequent, Argument1 alternative)
        {
            return
                new PCond2AQLA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQLA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AQLL1 : PCond2AQLL
    {
        // (s q l l1)

        PCond2AQLL1 (PrimitiveCombination2AQ predicate, LexicalVariable consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, LexicalVariable consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AQLL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQLL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AQLQ : PCond2AQL
    {
        // (s q l Q)
        public readonly object alternativeValue;
        PCond2AQLQ (PrimitiveCombination2AQ predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCond2AQLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQLQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AQQ : PCond2AQ
    {
        // (s q q s)
        public readonly object consequentValue;

        protected PCond2AQQ (PrimitiveCombination2AQ predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCond2AQQL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCond2AQQQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCond2AQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Pcond2SSQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AQQL : PCond2AQQ
    {
        // (s q q l)
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2AQQL (PrimitiveCombination2AQ predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AQQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AQQL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AQQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQQL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AQQA : PCond2AQQL
    {
        // (s q q a)
        protected PCond2AQQA (PrimitiveCombination2AQ predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AQQA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AQQA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AQQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQQA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
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

    sealed class PCond2AQQA0 : PCond2AQQA
    {
        // (s q q a0)
        PCond2AQQA0 (PrimitiveCombination2AQ predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCond2AQQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQQA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
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

    sealed class PCond2AQQA1 : PCond2AQQA
    {
        // (s q q a1)
        PCond2AQQA1 (PrimitiveCombination2AQ predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new PCond2AQQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQQA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
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

    sealed class PCond2AQQL1 : PCond2AQQL
    {
        // (s q q l1)
        PCond2AQQL1 (PrimitiveCombination2AQ predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                new PCond2AQQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQQL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AQQQ : PCond2AQQ
    {
        // (s q q q)
        public readonly object alternativeValue;

        PCond2AQQQ (PrimitiveCombination2AQ predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2AQ predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCond2AQQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            SCode.location = "PCond2AQQQ.EvalStep";
#endif

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
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

    class PCond2AQSL : PCond2AQ
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCond2AQSL (PrimitiveCombination2AQ predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        static public SCode Make (PrimitiveCombination2AQ predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2AQSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCond2AQSL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCond2AQSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQSL.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2AQSA : PCond2AQSL
    {
        protected PCond2AQSA (PrimitiveCombination2AQ predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AQ predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCond2AQSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCond2AQSA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCond2AQSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQSA.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AQSA0 : PCond2AQSA
    {
#if DEBUG
                static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCond2AQSA0 (PrimitiveCombination2AQ predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AQ predicate, SCode consequent, Argument0 alternative)
        {
            return new PCond2AQSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "PCond2AQSA0.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AQSA1 : PCond2AQSA
    {
        PCond2AQSA1 (PrimitiveCombination2AQ predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AQ predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2AQSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQSA1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AQSL1 : PCond2AQSL
    {
        PCond2AQSL1 (PrimitiveCombination2AQ predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2AQ predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2AQSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQSL1.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2AQSQ : PCond2AQ
    {
        public readonly object alternativeValue;

        PCond2AQSQ (PrimitiveCombination2AQ predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        static public SCode Make (PrimitiveCombination2AQ predicate, SCode consequent, Quotation alternative)
        {
            return new PCond2AQSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCond2AQQQ.EvalStep";
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString();
#endif
            object predValue;

            if (this.method (out predValue, environment.ArgumentValue (this.rand0Offset), this.rand1Value)) {
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
