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
                this.rand0 = predicate.Rand0;
                this.rand1 = predicate.Rand1;
#if DEBUG
                rand0Type = rand0.GetType ();
                rand1Type = rand1.GetType ();
#endif
            }

            public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            {
                return
                    (predicate is PrimitiveIsCharEq) ? PCondIsCharEq.Make ((PrimitiveIsCharEq) predicate, consequent, alternative) :
                    (predicate is PrimitiveIsIntEq) ? PCondIsIntEq.Make ((PrimitiveIsIntEq) predicate, consequent, alternative) :
                    (predicate is PrimitiveIsEq) ? PCondIsEq.Make ((PrimitiveIsEq) predicate, consequent, alternative) :
                    (predicate is PrimitiveIsObjectEq) ? PCondIsObjectEq.Make ((PrimitiveIsObjectEq) predicate, consequent, alternative) :
                    (predicate is PrimitiveIsObjectType) ? PCondIsObjectType.Make ((PrimitiveIsObjectType) predicate, consequent, alternative) :
                    (predicate is PrimitiveLessThanFixnum) ? PCondLessThanFixnum.Make ((PrimitiveLessThanFixnum) predicate, consequent, alternative) :
                //(predicate is PrimitiveCombination2L) ? PCond2L.Make ((PrimitiveCombination2L) predicate, consequent, alternative)
                //: (predicate is PrimitiveCombination2Q) ? Unimplemented () // PCond2Q.Make ((PrimitiveCombination2Q) predicate, consequent, alternative)
                //: (predicate is PrimitiveCombination2SL) ? PCond2SL.Make ((PrimitiveCombination2SL) predicate, consequent, alternative)
                //: (predicate is PrimitiveCombination2SQ) ? PCond2SQ.Make ((PrimitiveCombination2SQ) predicate, consequent, alternative)
                //: (consequent is LexicalVariable) ? PCond2SSL.Make (predicate, (LexicalVariable) consequent, alternative)
                //: (consequent is Quotation) ? PCond2SSQ.Make (predicate, (Quotation) consequent, alternative)
                //: (alternative is LexicalVariable) ? PCond2SSSL.Make (predicate, consequent, (LexicalVariable) alternative)
                //: (alternative is Quotation) ? PCond2SSSQ.Make (predicate, consequent, (Quotation) alternative)
                //: 
                new PCond2 (predicate, consequent, alternative);
            }

            public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
            {
#if DEBUG
                Warm ("-");
                noteCalls (this.rand0);
                noteCalls (this.rand1);
                procedureHistogram.Note (this.procedure);
                rand0TypeHistogram.Note (this.rand0Type);
                rand1TypeHistogram.Note (this.rand1Type);
                SCode.location = "PCond2.EvalStep";
#endif
                Control unev = this.rand1;
                Environment env = environment;
                object ev1;
                while (unev.EvalStep (out ev1, ref unev, ref env)) { };
                if (ev1 == Interpreter.UnwindStack) {
                    throw new NotImplementedException ();
                }

                unev = this.rand0;
                env = environment;
                object ev0;
                while (unev.EvalStep (out ev0, ref unev, ref env)) { };
                if (ev0 == Interpreter.UnwindStack) {
                    throw new NotImplementedException ();
                    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                    //answer = Interpreter.UnwindStack;
                    //environment = env;
                    //return false;
                }

                // It is expensive to bounce down to invoke the procedure
                // we invoke it directly and pass along the ref args.
#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
#endif
                if (this.method (out answer, ev0, ev1)) {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
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
                    alternativeTypeHistogram.Note (this.alternativeType);
#endif
                    expression = this.alternative;
                    return true;
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

    class PCond2L : PCond2
    {
        protected PCond2L (PrimitiveCombination2L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2L predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveCombination2A) ? PCond2A.Make ((PrimitiveCombination2A) predicate, consequent, alternative)
                : (predicate is PrimitiveCombination2L1) ? Unimplemented ()
                : (predicate is PrimitiveCombination2LL) ? Unimplemented ()
                : (predicate is PrimitiveCombination2LQ) ? Unimplemented ()
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCond2L (predicate, consequent, alternative);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
#endif
            throw new NotImplementedException ();
        }

    }

    class PCond2A : PCond2L
    {
        protected PCond2A (PrimitiveCombination2A predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A predicate, SCode consequent, SCode alternative)
        {
            return
                //predicate is PrimitiveCombination2A0) ? PCond2A0.Make ((PrimitiveCombination2A0) predicate, consequent, alternative)
                //: (predicate is PrimitiveCombination2A1) ? Unimplemented ()
                //: (predicate is PrimitiveCombination2AL) ? Unimplemented ()
                //: (predicate is PrimitiveCombination2AQ) ? Unimplemented ()
                //: (consequent is LexicalVariable) ? Unimplemented ()
                //: (consequent is Quotation) ? Unimplemented ()
                //: (alternative is LexicalVariable) ? Unimplemented ()
                //: (alternative is Quotation) ? Unimplemented ()
                new PCond2A (predicate, consequent, alternative);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
#endif
            throw new NotImplementedException ();
        }

    }

    class PCond2A0 : PCond2A
    {
        protected PCond2A0 (PrimitiveCombination2A0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0 predicate, SCode consequent, SCode alternative)
        {
            return
                //(predicate is PrimitiveCombination2A0L) ? PCond2A0L.Make ((PrimitiveCombination2A0L) predicate, consequent, alternative) :
                //(predicate is PrimitiveCombination2A0Q) ? PCond2A0Q.Make ((PrimitiveCombination2A0Q) predicate, consequent, alternative):
                //: (consequent is LexicalVariable) ? Unimplemented ()
                //: (consequent is Quotation) ? Unimplemented ()
                //: (alternative is LexicalVariable) ? Unimplemented ()
                //: (alternative is Quotation) ? Unimplemented ()
                new PCond2A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
#endif
            throw new NotImplementedException ();
        }

    }

    class PCond2A0L : PCond2A0
    {
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

        static public SCode Make (PrimitiveCombination2A0L predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveCombination2A0A) ? PCond2A0A.Make ((PrimitiveCombination2A0A) predicate, consequent, alternative)
                : (predicate is PrimitiveCombination2A0L1) ? PCond2A0L1.Make ((PrimitiveCombination2A0L1) predicate, consequent, alternative)
                //: (consequent is LexicalVariable) ? Unimplemented ()
                //: (consequent is Quotation) ? Unimplemented ()
                //: (alternative is LexicalVariable) ? Unimplemented ()
                //: (alternative is Quotation) ? Unimplemented ()
                : new PCond2A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
#endif
            throw new NotImplementedException ();
        }

    }

    class PCond2A0A : PCond2A0L
    {
         protected PCond2A0A (PrimitiveCombination2A0A predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0A predicate, SCode consequent, SCode alternative)
        {
            return
                //(predicate is PrimitiveCombination2A0A0) ? Unimplemented()
                //: (predicate is PrimitiveCombination2A0A0) ? Unimplemented()
                //: (consequent is LexicalVariable) ? Unimplemented ()
                //: (consequent is Quotation) ? Unimplemented ()
                //: (alternative is LexicalVariable) ? Unimplemented ()
                //: 
                (alternative is Quotation) ? PCond2A0ASQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
#endif
            throw new NotImplementedException ();
        }
    }

    sealed class PCond2A0ASQ : PCond2A0A
    {
        PCond2A0ASQ (PrimitiveCombination2A0A predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0A predicate, SCode consequent, Quotation alternative)
        {
            return
                 new PCond2A0ASQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
#endif
            throw new NotImplementedException ();
        }
    }



    class PCond2A0L1 : PCond2A0L
    {
        protected PCond2A0L1 (PrimitiveCombination2A0L1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2A0L1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? PCond2A0L1SQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2A0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
#endif
            throw new NotImplementedException ();
        }

    }

    sealed class PCond2A0L1SQ : PCond2A0L1
    {
        public readonly object alternativeValue;

        PCond2A0L1SQ (PrimitiveCombination2A0L1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        static public SCode Make (PrimitiveCombination2A0L1 predicate, SCode consequent, Quotation alternative)
        {
            return new PCond2A0L1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2A0L1SQ.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
              throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            Debug.WriteLineIf (Primitive.Noisy, this.procedure.ToString ());
            SCode.location = this.procedure.Name;
#endif
            object predValue;

            if (this.method (out predValue, ev0, ev1)) {
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

            if ((predValue is bool) && (bool) predValue == false) {

                answer = this.alternativeValue;
                return false;
            }
            else {
                expression = this.consequent;
                answer = null;
                return true;
            }
        } 

    }

    class PCond2A0Q : PCond2A0
    {
        public readonly object rand1Value;

        protected PCond2A0Q (PrimitiveCombination2A0Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? PCond2A0QQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? PCond2A0QSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCond2A0QQ : PCond2A0Q
    {
        public readonly object consequentValue;

        protected PCond2A0QQ (PrimitiveCombination2A0Q predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCond2A0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            Primitive.hotPrimitives.Note (this.procedure);
            Debug.WriteLineIf (Primitive.Noisy, this.procedure.ToString ());
            SCode.location = this.procedure.Name;
#endif
            object predValue;
            if (this.method (out predValue, environment.Argument0Value, this.rand1Value)) {
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
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }

        }
    }

    sealed class PCond2A0QSQ : PCond2A0Q
    {
        PCond2A0QSQ (PrimitiveCombination2A0Q predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2A0Q predicate, SCode consequent, Quotation alternative)
        {
            return
                 new PCond2A0QSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCond2SL : PCond2
    {
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
                //(consequent is LexicalVariable) ? Unimplemented ()
                //: (consequent is Quotation) ? Unimplemented() //PCond2SQQ.Make (predicate, (Quotation) consequent, alternative)
                //: 
                (alternative is LexicalVariable) ? PCond2SLSL.Make (predicate, consequent, (LexicalVariable) alternative)
                //: (alternative is Quotation) ? Unimplemented() //PCond2SQSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL.EvalStep";
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

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

        public static SCode Make (PrimitiveCombination2SL predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                 (alternative is Argument) ? PCond2SLSA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? Unimplemented () //PCond2SQSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2SLSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL.EvalStep";
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

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

        public static SCode Make (PrimitiveCombination2SL predicate, SCode consequent, Argument alternative)
        {
            return
                 (alternative is Argument0) ? PCond2SLSA0.Make (predicate, consequent, (Argument0) alternative)
                 : (alternative is Argument1) ? PCond2SLSA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2SLSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL.EvalStep";
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

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
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

        PCond2SLSA0 (PrimitiveCombination2SL predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, SCode consequent, Argument0 alternative)
        {
            return
                  new PCond2SLSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SL.EvalStep";
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

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    sealed class PCond2SLSA1 : PCond2SLSA
    {

        PCond2SLSA1 (PrimitiveCombination2SL predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SL predicate, SCode consequent, Argument1 alternative)
        {
            return
                  new PCond2SLSA1 (predicate, consequent, alternative);
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

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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



    class PCond2SQ : PCond2
    {
        public readonly object rand1Value;
        protected PCond2SQ (PrimitiveCombination2SQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? PCond2SQQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? PCond2SQSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PCond2SQ.EvalStep";
#endif
            object ev1 = this.rand1Value;

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

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
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

    class PCond2SQQ : PCond2SQ
    {
        protected PCond2SQQ (PrimitiveCombination2SQ predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCond2SQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    sealed class PCond2SQSQ : PCond2SQ
    {
        PCond2SQSQ (PrimitiveCombination2SQ predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2SQ predicate, SCode consequent, Quotation alternative)
        {
            return
                 new PCond2SQSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCond2SSL : PCond2
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCond2SSL (PrimitiveCombination2 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
        }

        static public SCode Make (PrimitiveCombination2 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCond2SSA.Make (predicate, (Argument) consequent, alternative) :
                //: (consequent is LexicalVariable1) ? Unimplemented()
                //: (alternative is LexicalVariable) ? Unimplemented ()
                //: (alternative is Quotation) ? Unimplemented()
                new PCond2SSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
#endif
            throw new NotImplementedException ();
        }

    }

    class PCond2SSA : PCond2SSL
    {
        protected PCond2SSA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                 (alternative is Argument0) ? PCond2SSA0.Make (predicate, (Argument0) consequent, alternative)
                //: (alternative is Argument1) ? Unimplemented ()
                : new PCond2SSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            noteCalls (this.rand1);
            SCode.location = "PCond2SSA.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, environment, ev1));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            Debug.WriteLineIf (Primitive.Noisy, this.procedure.ToString ());
            SCode.location = this.procedure.Name;
#endif
            object predValue;
            if (this.method (out predValue, ev0, ev1)) {
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

            if ((predValue is bool) && (bool) predValue == false) {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }

    }



    class PCond2SSA0 : PCond2SSA
    {
        protected PCond2SSA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                 (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCond2SSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
#endif
            throw new NotImplementedException ();
        }

    }



    class PCond2SSQ : PCond2
    {
        public readonly object consequentValue;

        protected PCond2SSQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        static public SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return 
                (alternative is LexicalVariable) ? PCond2SSQL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCond2SSQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCond2SSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            noteCalls (this.rand1);
            SCode.location = "PCond2SSQ.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, environment, ev1));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            Debug.WriteLineIf (Primitive.Noisy, this.procedure.ToString ());
            SCode.location = this.procedure.Name;
#endif
            object predValue;
            if (this.method (out predValue, ev0, ev1)) {
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

            if ((predValue is bool) && (bool) predValue == false) {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }

    }

    class PCond2SSQL : PCond2SSQ
    {

        protected PCond2SSQL (PrimitiveCombination2 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                //(alternative is Argument) ? Unimplemented()
                //: 
                (alternative is LexicalVariable1) ? PCond2SSQL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2SSQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
#endif
            throw new NotImplementedException ();
        }
    }


    sealed class PCond2SSQL1 : PCond2SSQL
    {

        PCond2SSQL1 (PrimitiveCombination2 predicate, Quotation consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2 predicate, Quotation consequent, LexicalVariable1 alternative)
        {
            return
                 new PCond2SSQL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
#endif
            throw new NotImplementedException ();
        }
    }


    sealed class PCond2SSQQ : PCond2SSQ
    {
        public readonly object alternativeValue;

        PCond2SSQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        static public SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return new PCond2SSQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            noteCalls (this.rand1);
            SCode.location = "PCond2SSQQ.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, environment, ev1));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            Debug.WriteLineIf (Primitive.Noisy, this.procedure.ToString ());
            SCode.location = this.procedure.Name;
#endif
            object predValue;

            if (this.method (out predValue, ev0, ev1)) {
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

            answer = (predValue is bool && (bool) predValue == false) ? this.alternativeValue : this.consequentValue;
            return false;
        }

    }

    class PCond2SSSL : PCond2
    {
        protected PCond2SSSL (PrimitiveCombination2 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCond2SSSA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? PCond2SSSL1.Make (predicate, consequent, (LexicalVariable1) alternative)
                : new PCond2SSSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
#endif
            throw new NotImplementedException ();
        }

    }

    class PCond2SSSA : PCond2SSSL
    {
        protected PCond2SSSA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return 
                //(alternative is Argument0) ? Unimplemented()
                //: 
                (alternative is Argument1) ? PCond2SSSA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCond2SSSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
#endif
            throw new NotImplementedException ();
        }

    }



    sealed class PCond2SSSA1 : PCond2SSSA
    {
        PCond2SSSA1 (PrimitiveCombination2 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument1 alternative)
        {
            return new PCond2SSSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
#endif
            throw new NotImplementedException ();
        }

    }

    sealed class PCond2SSSL1 : PCond2SSSL
    {
        PCond2SSSL1 (PrimitiveCombination2 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveCombination2 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return new PCond2SSSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
#endif
            throw new NotImplementedException ();
        }

    }



    sealed class PCond2SSSQ : PCond2
    {
        public readonly object alternativeValue;

        PCond2SSSQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        static public SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return new PCond2SSSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            {
#if DEBUG
                Warm ("-");
                noteCalls (this.rand0);
                noteCalls (this.rand1);
                SCode.location = "PCond2SSQQ.EvalStep";
#endif
                Control unev = this.rand1;
                Environment env = environment;
                object ev1;
                while (unev.EvalStep (out ev1, ref unev, ref env)) { };
                if (ev1 == Interpreter.UnwindStack) {
                    throw new NotImplementedException ();
                    //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                    //answer = Interpreter.UnwindStack;
                    //environment = env;
                    //return false;
                }

                unev = this.rand0;
                env = environment;
                object ev0;
                while (unev.EvalStep (out ev0, ref unev, ref env)) { };
                if (ev0 == Interpreter.UnwindStack) {
                    //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, environment, ev1));
                    //answer = Interpreter.UnwindStack;
                    //environment = env;
                    //return false;
                }

#if DEBUG
                Primitive.hotPrimitives.Note (this.procedure);
                Debug.WriteLineIf (Primitive.Noisy, this.procedure.ToString ());
                SCode.location = this.procedure.Name;
#endif
                object predValue;

                if (this.method (out predValue, ev0, ev1)) {
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
}
