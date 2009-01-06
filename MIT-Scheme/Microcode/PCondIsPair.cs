using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;


namespace Microcode
{
    [Serializable]
    class PCondIsPair : PCond1
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsPair (PrimitiveIsPair predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsPair predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsPairL) ? PCondIsPairL.Make ((PrimitiveIsPairL) predicate, consequent, alternative) :
                 (consequent is LexicalVariable) ? PCondIsPairSL.Make (predicate, (LexicalVariable) consequent, alternative) :
                 (consequent is Quotation) ? PCondIsPairSQ.Make (predicate, (Quotation) consequent, alternative) :
                 (alternative is LexicalVariable) ? PCondIsPairSSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                 (alternative is Quotation) ? PCondIsPairSSQ.Make (predicate, consequent, (Quotation) alternative) :
                 new PCondIsPair (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPair.EvalStep");
            noteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsPairFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is Cons)) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    class PCondIsPairL : PCondIsPair
    {
        public readonly object predicateName;
        public readonly int predicateDepth;
        public readonly int predicateOffset;

        protected PCondIsPairL (PrimitiveIsPairL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateName = ((LexicalVariable) predicate.Operand).Name;
            this.predicateDepth = ((LexicalVariable) predicate.Operand).Depth;
            this.predicateOffset = ((LexicalVariable) predicate.Operand).Offset;
        }

        public static SCode Make (PrimitiveIsPairL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsPairA) ? PCondIsPairA.Make ((PrimitiveIsPairA) predicate, consequent, alternative)
                : (predicate is PrimitiveIsPairL1) ? PCondIsPairL1.Make ((PrimitiveIsPairL1) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? PCondIsPairLL.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondIsPairLQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondIsPairLSL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsPairLSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsPairL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();


            if (!(ev0 is Cons)) {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
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

    class PCondIsPairA : PCondIsPairL
    {
        protected PCondIsPairA (PrimitiveIsPairA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsPairA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsPairA0) ? PCondIsPairA0.Make ((PrimitiveIsPairA0) predicate, consequent, alternative)
                : (predicate is PrimitiveIsPairA1) ? PCondIsPairA1.Make ((PrimitiveIsPairA1) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? PCondIsPairAL.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondIsPairAQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondIsPairASL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsPairASQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsPairA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA.EvalStep");
#endif
            expression = environment.ArgumentValue (this.predicateOffset) is Cons
                ? this.consequent
                : this.alternative;
            answer = null;
#if DEBUG
            noteCalls ((SCode) expression);
#endif
            return true;
        }
    }

    class PCondIsPairA0 : PCondIsPairA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
#endif
        protected PCondIsPairA0 (PrimitiveIsPairA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsPairA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsPairA0L.Make (predicate, (LexicalVariable) consequent, alternative) :
                //(consequent is SimpleLet1CarA0) ? PCondIsPairA0SimpleLet1CarA0.Make (predicate, (SimpleLet1CarA0) consequent, alternative) :
                //(consequent is PCondIsEqCarA0LA0) ? PCondIsPairA0Fragment6.Make (predicate, (PCondIsEqCarA0LA0) consequent, alternative) :
                //: (consequent is SComb1Fragment3) ? PCondIsPairFragment4.Make (predicate, (SComb1Fragment3) consequent, alternative) :
                (consequent is Quotation) ? PCondIsPairA0Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCondIsPairA0SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondIsPairA0SQ.Make (predicate, consequent, (Quotation) alternative) :
               new PCondIsPairA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            if (environment.Argument0Value is Cons)
                consequentTypeHistogram.Note (this.consequentType);
            else
                alternativeTypeHistogram.Note (this.alternativeType);
            SCode.location = "PCondIsPairA0.EvalStep";
#endif
            expression = (environment.Argument0Value is Cons)
                ? this.consequent
                : this.alternative;
            answer = null;
#if DEBUG
            SCode.location = "-";
            noteCalls ((SCode) expression);
#endif
            return true;
        }
    }

    class PCondIsPairA0L : PCondIsPairA0
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsPairA0L (PrimitiveIsPairA0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        internal static SCode Make (PrimitiveIsPairA0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsPairA0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCondIsPairA0L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCondIsPairA0LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondIsPairA0LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsPairA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 is Cons)) {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;

                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCondIsPairA0A : PCondIsPairA0L
    {
        protected PCondIsPairA0A (PrimitiveIsPairA0 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsPairA0 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsPairA0A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented() :
                (alternative is LexicalVariable) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented() :
                new PCondIsPairA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 is Cons)) {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;

                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCondIsPairA0A0 : PCondIsPairA0A
    {
        protected PCondIsPairA0A0 (PrimitiveIsPairA0 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsPairA0 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsPairA0A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsPairA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA0A0.EvalStep");
#endif
            object ev0 = environment.Argument0Value;
            if (ev0 is Cons) {
                answer = ev0;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    sealed class PCondIsPairA0A0Q : PCondIsPairA0A0
    {
        readonly object alternativeValue;

        PCondIsPairA0A0Q (PrimitiveIsPairA0 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsPairA0 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCondIsPairA0A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA0A0Q.EvalStep");
#endif
            object ev0 = environment.Argument0Value;
            answer = (ev0 is Cons) ? ev0 : this.alternativeValue;
            return false;
        }
    }

    class PCondIsPairA0L1 : PCondIsPairA0L
    {
        protected PCondIsPairA0L1 (PrimitiveIsPairA0 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsPairA0 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented() :
                new PCondIsPairA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 is Cons)) {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;

                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }



    class PCondIsPairA0LL : PCondIsPairA0L
    {
        protected PCondIsPairA0LL (PrimitiveIsPairA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsPairA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsPairA0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsPairA0LQ : PCondIsPairA0L
    {
        public readonly object alternativeValue;

        protected PCondIsPairA0LQ (PrimitiveIsPairA0 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsPairA0 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsPairA0LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA0LQ.EvalStep");
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 is Cons)) {

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

    class PCondIsPairA0SimpleLet1CarA0 : PCondIsPairA0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();

        public readonly Type bodyType;
#endif
        public readonly SimpleLambda rator;
        public readonly SCode body;

        protected PCondIsPairA0SimpleLet1CarA0 (PrimitiveIsPairA0 predicate, SimpleLet1CarA0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rator = (SimpleLambda) consequent.rator;
            this.body = this.rator.Body;
#if DEBUG
            this.bodyType = this.body.GetType ();
#endif
        }

        internal static SCode Make (PrimitiveIsPairA0 predicate, SimpleLet1CarA0 consequent, SCode alternative)
        {
            return
                 new PCondIsPairA0SimpleLet1CarA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA0SimpleLet1CarA0.EvalStep");
#endif
            Cons evarg = environment.Argument0Value as Cons;
            if (evarg == null) {
#if DEBUG
                alternativeTypeHistogram.Note(this.alternativeType);
#endif
                // tail call the alternative
                expression = this.alternative;
            }
            else {
                // tail call the lambdaBody after setting up the environment.
                SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);
                expression = this.body;
                environment = new SmallEnvironment1 (cl, evarg.Car);
#if DEBUG
                bodyTypeHistogram.Note (this.bodyType);
#endif
            }
            answer = null;
#if DEBUG
            noteCalls ((SCode) expression);
#endif
            return true;
        }
    }

    class PCondIsPairFragment4 : PCondIsPairA0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternative0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequent1TypeHistogram = new Histogram<Type> ();
#endif

        public readonly SCode rator;
        public readonly SCode alternative0;

        public readonly object pred1Rand1Name;
        public readonly int pred1Rand1Depth;
        public readonly int pred1Rand1Offset;
        public readonly object altRatorName;
        public readonly int altRatorDepth;
        public readonly int altRatorOffset;
        public readonly object altRandName;
        public readonly int altRandDepth;
        public readonly int altRandOffset;
        public readonly SCode consequent1;
#if DEBUG
        public readonly Type alternative0Type;
        public readonly Type consequent1Type;
#endif

        protected PCondIsPairFragment4 (PrimitiveIsPairA0 predicate, SComb1Fragment3 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rator = consequent.rator;
            this.alternative0 = consequent.alternative;

            this.pred1Rand1Name = consequent.pred1Rand1Name;
            this.pred1Rand1Depth = consequent.pred1Rand1Depth;
            this.pred1Rand1Offset = consequent.pred1Rand1Offset;
            this.altRatorName = consequent.altRatorName;
            this.altRatorDepth = consequent.altRatorDepth;
            this.altRatorOffset = consequent.altRatorOffset;
            this.altRandName = consequent.altRandName;
            this.altRandDepth = consequent.altRandDepth;
            this.altRandOffset = consequent.altRandOffset;
            this.consequent1 = consequent.consequent1;
#if DEBUG
            this.consequent1Type = consequent.consequent1Type;
            this.alternative0Type = consequent.alternative.GetType ();
#endif
        }

        public static SCode Make (PrimitiveIsPairA0 predicate, SComb1Fragment3 consequent, SCode alternative)
        {
            return
                (consequent.altRandDepth == 1
                && consequent.altRandOffset == 0) ? PCondIsPairFragment5.Make (predicate, consequent, alternative)
                : new PCondIsPairFragment4 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            Cons evarg = environment.Argument0Value as Cons;

            if (evarg == null) {
#if DEBUG
                alternativeTypeHistogram.Note (this.alternativeType);
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                object evargCar = evarg.Car;

                SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);
                environment = new SmallEnvironment1 (cl, evargCar);

                Cons evargCarCons = evargCar as Cons;

                if (evargCarCons == null) {
#if DEBUG
                    noteCalls (this.alternative0);
                    alternative0TypeHistogram.Note (this.alternative0Type);
#endif
                    expression = this.alternative0;
                    answer = null;
                    return true;
                }
                else {
                    object ev;
                    object pred1Arg1;
                    if (environment.FastLexicalRef (out pred1Arg1, this.pred1Rand1Name, this.pred1Rand1Depth, this.pred1Rand1Offset))
                        throw new NotImplementedException ();
                    object pred1Arg0 = evargCarCons.Car;

                    ObjectModel.Eq (out ev, pred1Arg0, pred1Arg1);

                    if ((ev is bool) && (bool) ev == false) {

                        object alt1Evrandtemp;
                        if (environment.FastLexicalRef (out alt1Evrandtemp, this.altRandName, this.altRandDepth, this.altRandOffset))
                            throw new NotImplementedException ();

                        object alt1Evrand = ((Cons) alt1Evrandtemp).Cdr;

                        object evop;
                        if (environment.FastLexicalRef (out evop, this.altRatorName, this.altRatorDepth, this.altRatorOffset))
                            throw new NotImplementedException ();
                        return Interpreter.Call (out answer, ref expression, ref environment, evop, alt1Evrand);
                    }

                    else {
#if DEBUG
                        noteCalls (this.consequent1);
                        consequent1TypeHistogram.Note (this.consequent1Type);
#endif
                        expression = this.consequent1;
                        answer = null;
                        return true;
                    }
                }
            }
        }
    }

    class PCondIsPairFragment5 : PCondIsPairFragment4
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternative0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequent1TypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsPairFragment5 (PrimitiveIsPairA0 predicate, SComb1Fragment3 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveIsPairA0 predicate, SComb1Fragment3 consequent, SCode alternative)
        {
            return
                 new PCondIsPairFragment5 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairFragment5.EvalStep");
#endif
            Cons evarg = environment.Argument0Value as Cons;

            if (evarg == null) {
#if DEBUG
                alternativeTypeHistogram.Note (this.alternativeType);
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                object evargCar = evarg.Car;

                // defer construction
                //SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);
                //environment = new SmallEnvironment1 (cl, evargCar);

                Cons evargCarCons = evargCar as Cons;

                if (evargCarCons == null) {
#if DEBUG
                    noteCalls (this.alternative0);
                    alternative0TypeHistogram.Note (this.alternative0Type);
#endif
                    SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);
                    environment = new SmallEnvironment1 (cl, evargCar);
                    expression = this.alternative0;
                    answer = null;
                    return true;
                }
                else {
                    //object ev;
                    object pred1Arg1;
                    if (environment.FastLexicalRef (out pred1Arg1, this.pred1Rand1Name, this.pred1Rand1Depth - 1, this.pred1Rand1Offset))
                        throw new NotImplementedException ();
                    object pred1Arg0 = evargCarCons.Car;

                    //ObjectModel.Eq (out ev, pred1Arg0, pred1Arg1);

                    if (
                        //(ev is bool) && (bool) ev == false
                        ((pred1Arg0 == null) && (pred1Arg1 == null))
                || ((pred1Arg1 != null) &&
                    ((pred1Arg0 == pred1Arg1)
                     || ((pred1Arg0 is Int32 && pred1Arg1 is Int32) && ((int) pred1Arg0 == (int) pred1Arg1))
                     || ((pred1Arg0 is char && pred1Arg1 is char) && ((char) pred1Arg0 == (char) pred1Arg1))
                     || ((pred1Arg0 is bool && pred1Arg1 is bool) && ((bool) pred1Arg0 == (bool) pred1Arg1))))
                        ) {
#if DEBUG
                        noteCalls (this.consequent1);
                        consequent1TypeHistogram.Note (this.consequent1Type);
#endif
                        SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);
                        environment = new SmallEnvironment1 (cl, evargCar);
                        expression = this.consequent1;
                        answer = null;
                        return true;
                    }
                    else {

                        //object alt1Evrandtemp;
                        //if (environment.FastLexicalRef (out alt1Evrandtemp, this.altRandName, this.altRandDepth, this.altRandOffset))
                        //    throw new NotImplementedException ();

                        //object alt1Evrand = ((Cons) alt1Evrandtemp).Cdr;

                        object evop;

                        if (environment.FastLexicalRef (out evop, this.altRatorName, this.altRatorDepth - 1, this.altRatorOffset))
                            throw new NotImplementedException ();
                        return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg.Cdr);
                    }


                }
            }
        }
    }

    class PCondIsPairA0Fragment6 : PCondIsPairA0
    {
#if DEBUG
        static Histogram<Type> consequentAlternativeTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
        public readonly Type consequentAlternativeType;
#endif
        public readonly object consequentRand1Name;
        public readonly int consequentRand1Depth;
        public readonly int consequentRand1Offset;

        public readonly SCode consequentAlternative;

        protected PCondIsPairA0Fragment6 (PrimitiveIsPairA0 predicate,  PCondIsEqCarA0LA0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentRand1Name = consequent.rand1Name;
            this.consequentRand1Depth = consequent.rand1Depth;
            this.consequentRand1Offset = consequent.rand1Offset;

            this.consequentAlternative = consequent.Alternative;
#if DEBUG
            this.consequentAlternativeType = this.consequentAlternative.GetType ();
#endif
        }

        internal static SCode Make (PrimitiveIsPairA0 predicate, PCondIsEqCarA0LA0 consequent, SCode alternative)
        {
            return
               new PCondIsPairA0Fragment6 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsPairA0.EvalStep";
#endif
            Cons ev0 = environment.Argument0Value as Cons;
            if (ev0 == null) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                object arg1;
                if (environment.FastLexicalRef (out arg1, this.consequentRand1Name, this.consequentRand1Depth, this.consequentRand1Offset))
                    throw new NotImplementedException ();

                object arg0 = ev0.Car;
                if (((arg0 == null) && (arg1 == null))
                || ((arg1 != null) &&
                    ((arg0 == arg1)
                     || ((arg0 is Int32 && arg1 is Int32) && ((int) arg0 == (int) arg1))
                     || ((arg0 is char && arg1 is char) && ((char) arg0 == (char) arg1))
                     || ((arg0 is bool && arg1 is bool) && ((bool) arg0 == (bool) arg1))))) {
                    answer = ev0;
                    return false;
                }
                else {
#if DEBUG
                    noteCalls (this.consequentAlternative);
                    consequentAlternativeTypeHistogram.Note (this.consequentAlternativeType);
#endif
                    expression = this.consequentAlternative;
                    answer = null;
                    return true;
                }
            }
        }
    }

    class PCondIsPairA0Q : PCondIsPairA0
    {
        public readonly object consequentValue;

        protected PCondIsPairA0Q (PrimitiveIsPairA0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        internal static SCode Make (PrimitiveIsPairA0 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsPairA0QL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsPairA0QQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsPairA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA0Q.EvalStep");
#endif
            if (environment.Argument0Value is Cons) {
                answer = this.consequentValue;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    class PCondIsPairA0QL : PCondIsPairA0Q
    {
        protected PCondIsPairA0QL (PrimitiveIsPairA0 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsPairA0 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsPairA0QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsPairA0QQ : PCondIsPairA0Q
    {
        protected PCondIsPairA0QQ (PrimitiveIsPairA0 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsPairA0 predicate, Quotation consequent, Quotation alternative)
        {
            if (consequent.Quoted == alternative.Quoted) {
                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, consequent);
            }
            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, alternative);
            }
            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, consequent);
            }
            return new PCondIsPairA0QQ (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsPairA0SL : PCondIsPairA0
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsPairA0SL (PrimitiveIsPairA0 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        internal static SCode Make (PrimitiveIsPairA0 predicate, SCode consequent, LexicalVariable alternative)
        {
            return 
                (alternative is Argument) ? PCondIsPairA0SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCondIsPairA0SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCondIsPairA0SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA0SL.EvalStep");
#endif
            object ev0 = environment.Argument0Value;

            if (!(ev0 is Cons)) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
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

    class PCondIsPairA0SA : PCondIsPairA0SL
    {
        protected PCondIsPairA0SA (PrimitiveIsPairA0 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsPairA0 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsPairA0SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCondIsPairA0SA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCondIsPairA0SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA0SA.EvalStep");
#endif
            throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if (!(ev0 is Cons)) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
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

    sealed class PCondIsPairA0SA0 : PCondIsPairA0SA
    {
        PCondIsPairA0SA0 (PrimitiveIsPairA0 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsPairA0 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsPairA0SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA0SA1.EvalStep");
#endif
            object temp = environment.Argument0Value;
            if (temp is Cons) {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                answer = temp;
                return false;
            }
        }
    }

    sealed class PCondIsPairA0SA1 : PCondIsPairA0SA
    {
        PCondIsPairA0SA1 (PrimitiveIsPairA0 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsPairA0 predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCondIsPairA0SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA0SA1.EvalStep");
#endif
            if (environment.Argument0Value is Cons) {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    sealed class PCondIsPairA0SL1 : PCondIsPairA0SL
    {
        PCondIsPairA0SL1 (PrimitiveIsPairA0 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsPairA0 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return
                new PCondIsPairA0SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA0SL1.EvalStep");
#endif
            if (environment.Argument0Value is Cons) {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    sealed class PCondIsPairA0SQ : PCondIsPairA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
#endif
        public readonly object alternativeValue;

        PCondIsPairA0SQ (PrimitiveIsPairA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsPairA0 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsPairA0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA0SQ.EvalStep");
#endif
            if (environment.Argument0Value is Cons) {
#if DEBUG
                SCode.location = "-";
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsPairA0SQ.EvalStep";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                answer = this.alternativeValue;
                return false;
            }
        }
    }

    class PCondIsPairA1 : PCondIsPairA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram <Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsPairA1 (PrimitiveIsPairA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA1.EvalStep");
#endif
            if (environment.Argument1Value is Cons) {
#if DEBUG
                SCode.location = "-";
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsPairA1.EvalStep.1";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            } else {
#if DEBUG
                SCode.location = "-";
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsPairA1.EvalStep.2";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }

        internal static SCode Make (PrimitiveIsPairA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsPairA1L.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondIsPairA1Q.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondIsPairA1SL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsPairA1SQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsPairA1 (predicate, consequent, alternative);
        }
    }

    class PCondIsPairA1L : PCondIsPairA1
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsPairA1L (PrimitiveIsPairA1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;

        }

        internal static SCode Make (PrimitiveIsPairA1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsPairA1LL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsPairA1LQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsPairA1L (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();

#endif
            object ev0 = environment.Argument1Value;

            if (!(ev0 is Cons)) {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
            #endregion

        }


    }

    class PCondIsPairA1LL : PCondIsPairA1L
    {
        protected PCondIsPairA1LL (PrimitiveIsPairA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsPairA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsPairA1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsPairA1LQ : PCondIsPairA1L
    {
        public readonly object alternativeValue;

        protected PCondIsPairA1LQ (PrimitiveIsPairA1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsPairA1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsPairA1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 is Cons)) {

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

    class PCondIsPairA1Q : PCondIsPairA1
    {
        public readonly object consequentValue;
        protected PCondIsPairA1Q (PrimitiveIsPairA1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }
        internal static SCode Make (PrimitiveIsPairA1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsPairA1QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondIsPairA1QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsPairA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA1Q.EvalStep");
#endif
            object ev0 = environment.Argument1Value;

            if (ev0 is Cons) {
                answer = this.consequentValue;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }  
        }
    }

    class PCondIsPairA1QL : PCondIsPairA1Q
    {
        protected PCondIsPairA1QL (PrimitiveIsPairA1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsPairA1 predicate, Quotation quotation, LexicalVariable alternative)
        {

            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsPairA1QQ : PCondIsPairA1Q
    {
        protected PCondIsPairA1QQ (PrimitiveIsPairA1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsPairA1 predicate, Quotation consequent, Quotation alternative)
        {
            if (consequent.Quoted == alternative.Quoted) {
                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, consequent);
            }
            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, alternative);
            }
            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, consequent);
            }
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsPairA1SL : PCondIsPairA1
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsPairA1SL (PrimitiveIsPairA1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }
        internal static SCode Make (PrimitiveIsPairA1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return 
                (alternative is Argument) ? PCondIsPairA1SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? Unimplemented():
                new PCondIsPairA1SL (predicate, consequent, alternative);

        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA1SL.EvalStep");
#endif
            object ev0 = environment.Argument1Value;

            if (!(ev0 is Cons)) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
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

    class PCondIsPairA1SA : PCondIsPairA1SL
    {

        protected PCondIsPairA1SA (PrimitiveIsPairA1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }
        internal static SCode Make (PrimitiveIsPairA1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsPairA1SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? Unimplemented () :
                new PCondIsPairA1SA (predicate, consequent, alternative);

        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsPairA1SL.EvalStep");
#endif
            object ev0 = environment.Argument1Value;

            if (!(ev0 is Cons)) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
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

    sealed class PCondIsPairA1SA0 : PCondIsPairA1SA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
#endif

        PCondIsPairA1SA0 (PrimitiveIsPairA1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsPairA1 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsPairA1SA0 (predicate, consequent, alternative);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA1SA0.EvalStep");
#endif
            object ev0 = environment.Argument1Value;

            if (ev0 is Cons) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    class PCondIsPairA1SQ : PCondIsPairA1
    {
        public readonly object alternativeValue;

        protected PCondIsPairA1SQ (PrimitiveIsPairA1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsPairA1 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsPairA1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA1SQ.EvalStep");
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 is Cons)) {

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

    class PCondIsPairAL : PCondIsPairA
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsPairAL (PrimitiveIsPairA predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();

#endif
            object ev0 = environment.ArgumentValue (this.predicateOffset);

            if (!(ev0 is Cons)) {
#if DEBUG
                noteCalls (this.alternative);

#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
            #endregion
        }

        internal static SCode Make (PrimitiveIsPairA predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsPairALL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsPairALQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsPairAL (predicate, consequent, alternative);
        }
    }

    class PCondIsPairAA : PCondIsPairAL
    {
        protected PCondIsPairAA (PrimitiveIsPairA predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        { }

        static public SCode Make (PrimitiveIsPairA predicate, Argument consequent, SCode alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

    }

    class PCondIsPairAL1 : PCondIsPairAL
    {
        protected PCondIsPairAL1 (PrimitiveIsPairA predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsPairA predicate, LexicalVariable1 consequent, SCode alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsPairALL : PCondIsPairAL
    {
        protected PCondIsPairALL (PrimitiveIsPairA predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsPairA predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsPairALQ : PCondIsPairAL
    {
        protected PCondIsPairALQ (PrimitiveIsPairA predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsPairA predicate, LexicalVariable consequent, Quotation alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsPairAQ : PCondIsPairA
    {
        public readonly object consequentValue;

        protected PCondIsPairAQ (PrimitiveIsPairA predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();
            noteCalls (this.arg0);
#endif
            object ev0 = environment.ArgumentValue (this.predicateOffset);


            if (!(ev0 is Cons)) {
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
            #endregion

        }

        internal static SCode Make (PrimitiveIsPairA predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsPairAQL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsPairAQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsPairAQ (predicate, consequent, alternative);
        }
    }

    class PCondIsPairAQL : PCondIsPairAQ
    {
        protected PCondIsPairAQL (PrimitiveIsPairA predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        internal static SCode Make (PrimitiveIsPairA predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsPairAQL (predicate, consequent, alternative);
        }
    }

    class PCondIsPairAQQ : PCondIsPairAQ
    {
        protected PCondIsPairAQQ (PrimitiveIsPairA predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {

            throw new NotImplementedException ();
        }

        internal static SCode Make (PrimitiveIsPairA predicate, Quotation consequent, Quotation alternative)
        {
            if (consequent.Quoted == alternative.Quoted) {
                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, consequent);
            }
            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, alternative);
            }
            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, consequent);
            }
            return new PCondIsPairAQQ (predicate, consequent, alternative);
        }
    }

    class PCondIsPairASL : PCondIsPairA
    {
        protected PCondIsPairASL (PrimitiveIsPairA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        internal static PCondIsPairA Make (PrimitiveIsPairA predicate, SCode consequent, LexicalVariable lexicalVariable)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsPairASQ : PCondIsPairA
    {
        public readonly object alternativeValue;

        protected PCondIsPairASQ (PrimitiveIsPairA predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairASQ.EvalStep");
#endif
            object ev0 = environment.ArgumentValue (predicateOffset);


            if (!(ev0 is Cons)) {

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

        internal static PCondIsPairA Make (PrimitiveIsPairA predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsPairASQ (predicate, consequent, alternative);
        }
    }

    class PCondIsPairL1 : PCondIsPairL
    {
        protected PCondIsPairL1 (PrimitiveIsPairL1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsPairL1 predicate, SCode consequent, SCode alternative)
        {
            return
       (consequent is LexicalVariable) ? PCondIsPairL1L.Make (predicate, (LexicalVariable) consequent, alternative)
       : (consequent is Quotation) ? PCondIsPairL1Q.Make (predicate, (Quotation) consequent, alternative)
       : (alternative is LexicalVariable) ? PCondIsPairL1SL.Make (predicate, consequent, (LexicalVariable) alternative)
       : (alternative is Quotation) ? PCondIsPairL1SQ.Make (predicate, consequent, (Quotation) alternative)
       : new PCondIsPairL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();


            if (!(ev0 is Cons)) {
#if DEBUG
                noteCalls (this.alternative);

#endif
                expression = this.alternative;
                answer = null;
                return true;
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

    class PCondIsPairL1L : PCondIsPairL1
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsPairL1L (PrimitiveIsPairL1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        internal static SCode Make (PrimitiveIsPairL1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? Unimplemented():
                (consequent is LexicalVariable1) ? PCondIsPairL1L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCondIsPairL1LL.Make (predicate, consequent, (LexicalVariable) alternative):
                (alternative is Quotation) ? PCondIsPairL1LQ.Make (predicate, consequent, (Quotation) alternative):
                new PCondIsPairL1L (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairL1L.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (ev0 is Cons) {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.alternative);

#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }


    }

    class PCondIsPairL1L1 : PCondIsPairL1L
    {
        protected PCondIsPairL1L1 (PrimitiveIsPairL1 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsPairL1 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented() :
                new PCondIsPairL1L1 (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairL1L1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (ev0 is Cons) {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.alternative);

#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }


    }



    class PCondIsPairL1LL : PCondIsPairL1L
    {
        protected PCondIsPairL1LL (PrimitiveIsPairL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsPairL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsPairL1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsPairL1LQ : PCondIsPairL1L
    {
        public readonly object alternativeValue;

        protected PCondIsPairL1LQ (PrimitiveIsPairL1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsPairL1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsPairL1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 is Cons)) {

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

    class PCondIsPairL1Q : PCondIsPairL1
    {
        public readonly object consequentValue;

        protected PCondIsPairL1Q (PrimitiveIsPairL1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }
        internal static SCode Make (PrimitiveIsPairL1 predicate, Quotation consequent, SCode alternative)
        {
            return
    (alternative is LexicalVariable) ? PCondIsPairL1QL.Make (predicate, consequent, (LexicalVariable) alternative)
    : (alternative is Quotation) ? PCondIsPairL1QQ.Make (predicate, consequent, (Quotation) alternative)
    : new PCondIsPairL1Q (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();
            noteCalls (this.arg0);
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();


            if (!(ev0 is Cons)) {
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
            #endregion

        }
    }

    class PCondIsPairL1QL : PCondIsPairL1Q
    {
        protected PCondIsPairL1QL (PrimitiveIsPairL1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsPairL1 predicate, Quotation quotation, LexicalVariable alternative)
        {

            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsPairL1QQ : PCondIsPairL1Q
    {
        protected PCondIsPairL1QQ (PrimitiveIsPairL1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsPairL1 predicate, Quotation consequent, Quotation alternative)
        {
            if (consequent.Quoted == alternative.Quoted) {
                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, consequent);
            }
            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, alternative);
            }
            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, consequent);
            }
            return new PCondIsPairL1QQ (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsPairL1SL : PCondIsPairL1
    {
        protected PCondIsPairL1SL (PrimitiveIsPairL1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsPairL1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return 
                (alternative is Argument) ? PCondIsPairL1SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCondIsPairL1SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCondIsPairL1SL (predicate, consequent, alternative);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsPairL1SA : PCondIsPairL1SL
    {
        protected PCondIsPairL1SA (PrimitiveIsPairL1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsPairL1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsPairL1SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? Unimplemented () :
                new PCondIsPairL1SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    sealed class PCondIsPairL1SA0 : PCondIsPairL1SA
    {
        PCondIsPairL1SA0 (PrimitiveIsPairL1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsPairL1 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsPairL1SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairL1SA0.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException();

            if (ev0 is Cons) {
#if DEBUG
                noteCalls(this.consequent);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    sealed class PCondIsPairL1SL1 : PCondIsPairL1SL
    {
        PCondIsPairL1SL1 (PrimitiveIsPairL1 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsPairL1 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return
                new PCondIsPairL1SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    sealed class PCondIsPairL1SQ : PCondIsPairL1
    {
        public readonly object alternativeValue;

        PCondIsPairL1SQ (PrimitiveIsPairL1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsPairL1 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsPairL1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairL1SQ.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (!(ev0 is Cons)) {
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

    class PCondIsPairLL : PCondIsPairL
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsPairLL (PrimitiveIsPairL predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsPairL predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsPairLA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented():
                (alternative is LexicalVariable) ? PCondIsPairLLL.Make (predicate, consequent, (LexicalVariable)  alternative) :
                (alternative is Quotation) ? PCondIsPairLLQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsPairLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairLL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

            if (ev0 is Cons) {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException();
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    class PCondIsPairLA : PCondIsPairLL
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsPairLA (PrimitiveIsPairL predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsPairL predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsPairLA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented() :
                new PCondIsPairLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsPairLL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

            if (ev0 is Cons) {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    class PCondIsPairLA0 : PCondIsPairLA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsPairLA0 (PrimitiveIsPairL predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsPairL predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented() :
                new PCondIsPairLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairLA0.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

            if (ev0 is Cons) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }



    class PCondIsPairLLL : PCondIsPairLL
    {
        protected PCondIsPairLLL (PrimitiveIsPairL predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsPairL predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsPairLLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsPairLLQ : PCondIsPairLL
    {
        protected PCondIsPairLLQ (PrimitiveIsPairL predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsPairL predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsPairLLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsPairLQ : PCondIsPairL
    {
        protected PCondIsPairLQ (PrimitiveIsPairL predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsPairL predicate, Quotation consequent, SCode alternative)
        {
            return
              (alternative is LexicalVariable) ? PCondIsPairLQL.Make (predicate, consequent, (LexicalVariable) alternative)
              : (alternative is Quotation) ? PCondIsPairLQQ.Make (predicate, consequent, (Quotation) alternative)
              : new PCondIsPairLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsPairLQL : PCondIsPairLQ
    {
        protected PCondIsPairLQL (PrimitiveIsPairL predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsPairL predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsPairLQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsPairLQQ : PCondIsPairLQ
    {
        protected PCondIsPairLQQ (PrimitiveIsPairL predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsPairL predicate, Quotation consequent, Quotation alternative)
        {
            if (consequent.Quoted == alternative.Quoted) {
                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, consequent);
            }
            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, alternative);
            }
            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, consequent);
            }
            return new PCondIsPairLQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsPairLSL : PCondIsPairL
    {
        protected PCondIsPairLSL (PrimitiveIsPairL predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsPairL predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsPairLSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsPairLSQ : PCondIsPairL
    {
        public readonly object alternativeValue;

        protected PCondIsPairLSQ (PrimitiveIsPairL predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsPairL predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsPairLSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairLSQ.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

            if (!(ev0 is Cons)) {
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

    class PCondIsPairSL : PCondIsPair
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsPairSL (PrimitiveIsPair predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsPair predicate, LexicalVariable consequent, SCode alternative)
        {
            return
     (alternative is LexicalVariable) ? PCondIsPairSLL.Make (predicate, consequent, (LexicalVariable) alternative)
     : (alternative is Quotation) ? PCondIsPairSLQ.Make (predicate, consequent, (Quotation) alternative)
     : new PCondIsPairSL (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairSL.EvalStep");
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsPairFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is Cons)) {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCondIsPairSLL : PCondIsPairSL
    {

        protected PCondIsPairSLL (PrimitiveIsPair predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsPair predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsPairSLQ : PCondIsPairSL
    {
        public readonly object alternativeValue;

        protected PCondIsPairSLQ (PrimitiveIsPair predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }


        public static SCode Make (PrimitiveIsPair predicate, LexicalVariable consequent, Quotation alternative)
        {

            return new PCondIsPairSLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsPairFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is Cons)) {
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

    class PCondIsPairSQ : PCondIsPair
    {
        public readonly object consequentValue;

        protected PCondIsPairSQ (PrimitiveIsPair predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsPair predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsPairSQL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsPairSQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsPairSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.arg0);
            SCode.location = "PCondIsPairSQ.EvalStep";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
                        SCode.location = "PCondIsPairSQ.EvalStep.1";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsPairFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is Cons)) {
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

    class PCondIsPairSQL : PCondIsPairSQ
    {
        protected PCondIsPairSQL (PrimitiveIsPair predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsPair predicate, Quotation consequent, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsPairSQQ : PCondIsPairSQ
    {
        public readonly object alternativeValue;

        protected PCondIsPairSQQ (PrimitiveIsPair predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }


        public static SCode Make (PrimitiveIsPair predicate, Quotation consequent, Quotation alternative)
        {
            if (consequent.Quoted == alternative.Quoted) {
                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, consequent);
            }
            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, alternative);
            }
            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, consequent);
            }

            return new PCondIsPairSQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsPairFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is Cons)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
            #endregion
        }
    }

    class PCondIsPairSSL : PCondIsPair
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsPairSSL (PrimitiveIsPair predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveIsPair predicate, SCode consequent, LexicalVariable alternative)
        {
            return 
                (alternative is Argument) ? PCondIsPairSSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCondIsPairSSL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCondIsPairSSL (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsPairFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ev0 is Cons) {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                answer = null;
                return true; 
            }
            else {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCondIsPairSSA : PCondIsPairSSL
    {

        protected PCondIsPairSSA (PrimitiveIsPair predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsPair predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsPairSSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? Unimplemented () :
                new PCondIsPairSSA (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsPairSSL.EvalStep");
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsPairFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }



            if (!(ev0 is Cons)) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
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

    sealed class PCondIsPairSSA0 : PCondIsPairSSA
    {

        PCondIsPairSSA0 (PrimitiveIsPair predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsPair predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsPairSSA0 (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.arg0);
            SCode.location = "PCondIsPairSSA0.EvalStep";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsPairFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ev0 is Cons) {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    sealed class PCondIsPairSSL1 : PCondIsPairSSL
    {
        PCondIsPairSSL1 (PrimitiveIsPair predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsPair predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return
                new PCondIsPairSSL1 (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairSSL1.EvalStep");
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsPairFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ev0 is Cons) {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    sealed class PCondIsPairSSQ : PCondIsPair
    {
        public readonly object alternativeValue;

        PCondIsPairSSQ (PrimitiveIsPair predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsPair predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsPairSSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairSSQ.EvalStep");
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsPairFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ev0 is Cons) {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                answer = this.alternativeValue;
                return false;
            }
        }
    }

}
