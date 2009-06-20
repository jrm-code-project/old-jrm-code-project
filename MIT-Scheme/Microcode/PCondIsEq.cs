using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PCondIsEq : PCond2
    {
#if DEBUG
            static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
            static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
            static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
            static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEq (PrimitiveIsEq predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public static SCode Make (PrimitiveIsEq predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqL) ? PCondIsEqL.Make ((PrimitiveIsEqL) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqCar) ? PCondIsEqCar.Make ((PrimitiveIsEqCar) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqCaar) ? PCondIsEqCaar.Make ((PrimitiveIsEqCaar) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqQ) ? PCondIsEqQ.Make ((PrimitiveIsEqQ) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqSL) ? PCondIsEqSL.Make ((PrimitiveIsEqSL) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqSQ) ? PCondIsEqSQ.Make ((PrimitiveIsEqSQ) predicate, consequent, alternative) :
                new PCondIsEq (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEq.EvalStep");
            noteCalls (this.rand0);
            noteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
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

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqL : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PCondIsEqL (PrimitiveIsEqL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Name = predicate.rand0Name;
            this.rand0Depth = predicate.rand0Depth;
            this.rand0Offset = predicate.rand0Offset;
        }

        public static SCode Make (PrimitiveIsEqL predicate, SCode consequent, SCode alternative)
        {
            return 
                (predicate is PrimitiveIsEqA) ? PCondIsEqA.Make ((PrimitiveIsEqA) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqL1) ? PCondIsEqL1.Make ((PrimitiveIsEqL1) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqLL) ? PCondIsEqLL.Make ((PrimitiveIsEqLL) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqLQ) ? PCondIsEqLQ.Make ((PrimitiveIsEqLQ) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCondIsEqLSL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqLSQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCondIsEqLSSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondIsEqLSSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqL.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqA : PCondIsEqL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqA (PrimitiveIsEqA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveIsEqA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqA0) ? PCondIsEqA0.Make ((PrimitiveIsEqA0) predicate, consequent, alternative)
                : (predicate is PrimitiveIsEqA1) ? PCondIsEqA1.Make ((PrimitiveIsEqA1) predicate, consequent, alternative)
                : new PCondIsEqA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, environment.ArgumentValue (this.rand0Offset), ev1);

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

    class PCondIsEqA0 : PCondIsEqA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqA0 (PrimitiveIsEqA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveIsEqA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqA0Car) ? PCondIsEqA0Car.Make ((PrimitiveIsEqA0Car) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqA0L) ? PCondIsEqA0L.Make ((PrimitiveIsEqA0L) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqA0Q) ? PCondIsEqA0Q.Make ((PrimitiveIsEqA0Q) predicate, consequent, alternative) :
                new PCondIsEqA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA0.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, environment.Argument0Value, ev1);

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

    class PCondIsEqA0Car : PCondIsEqA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqA0Car (PrimitiveIsEqA0Car predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveIsEqA0Car predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqA0CarL) ? PCondIsEqA0CarL.Make ((PrimitiveIsEqA0CarL) predicate, consequent, alternative) :
                new PCondIsEqA0Car (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

    }

    class PCondIsEqA0CarL : PCondIsEqA0Car
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsEqA0CarL (PrimitiveIsEqA0CarL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;

        }

        public static SCode Make (PrimitiveIsEqA0CarL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqA0CarA) ? PCondIsEqA0CarA.Make ((PrimitiveIsEqA0CarA) predicate, consequent, alternative) :
                new PCondIsEqA0CarL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA0CarL.EvalStep");
#endif
            object ev1; 
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException();
            ObjectModel.Eq (out answer, environment.Argument0Value, ((Cons)ev1).Car);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqA0CarA.EvalStep.1";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqA0CarA.EvalStep.2";
#endif
                expression = this.consequent;
                return true;
            }
        }

    }

    class PCondIsEqA0CarA : PCondIsEqA0CarL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqA0CarA (PrimitiveIsEqA0CarA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveIsEqA0CarA predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondIsEqA0CarA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA0CarA.EvalStep");
#endif
            Cons ev1 = environment.ArgumentValue (this.rand1Offset) as Cons;
            ObjectModel.Eq (out answer, environment.Argument0Value, ev1.Car);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqA0CarA.EvalStep.1";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqA0CarA.EvalStep.2";
#endif
                expression = this.consequent;
                return true;
            }
        }

    }

    class PCondIsEqA0L : PCondIsEqA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsEqA0L (PrimitiveIsEqA0L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsEqA0L predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqA0A) ? PCondIsEqA0A.Make ((PrimitiveIsEqA0A) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqA0L1) ? PCondIsEqA0L1.Make ((PrimitiveIsEqA0L1) predicate, consequent, alternative) :
                new PCondIsEqA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA0L.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, environment.Argument0Value, ev1);

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

    class PCondIsEqA0A : PCondIsEqA0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqA0A (PrimitiveIsEqA0A predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqA0A predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqA0A1) ? PCondIsEqA0A1.Make ((PrimitiveIsEqA0A1) predicate, consequent, alternative) :
                new PCondIsEqA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA0A.EvalStep");
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            ObjectModel.Eq (out answer, environment.Argument0Value, ev1);

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

    class PCondIsEqA0A1 : PCondIsEqA0A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqA0A1 (PrimitiveIsEqA0A1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqA0A1 predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondIsEqA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA0A1.EvalStep");
#endif
            ObjectModel.Eq (out answer, environment.Argument0Value, environment.Argument1Value);

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

    class PCondIsEqA0L1 : PCondIsEqA0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqA0L1 (PrimitiveIsEqA0L1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqA0L1 predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondIsEqA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA0L1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, environment.Argument0Value, ev1);

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

    class PCondIsEqA0Q : PCondIsEqA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PCondIsEqA0Q (PrimitiveIsEqA0Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;

        }

        public static SCode Make (PrimitiveIsEqA0Q predicate, SCode consequent, SCode alternative)
        {
            return
                 new PCondIsEqA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA0Q.EvalStep");
#endif
            ObjectModel.Eq (out answer, environment.Argument0Value, this.rand1Value);

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

    class PCondIsEqA1 : PCondIsEqA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqA1 (PrimitiveIsEqA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveIsEqA1 predicate, SCode consequent, SCode alternative)
        {
            return
                 new PCondIsEqA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA1.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, environment.Argument1Value, ev1);

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

    class PCondIsEqL1 : PCondIsEqL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqL1 (PrimitiveIsEqL1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveIsEqL1 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqL1L) ? PCondIsEqL1L.Make ((PrimitiveIsEqL1L) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqL1Q) ? Unimplemented() :
                (consequent is LexicalVariable) ? PCondIsEqL1SL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqL1SQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCondIsEqL1SSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondIsEqL1SSQ.Make (predicate, consequent, (Quotation) alternative) :
                 new PCondIsEqL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqA1.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, environment.Argument1Value, ev1);

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

    class PCondIsEqL1L : PCondIsEqL1
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsEqL1L (PrimitiveIsEqL1L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsEqL1L predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqL1A) ? PCondIsEqL1A.Make ((PrimitiveIsEqL1A) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqL1L1) ? PCondIsEqL1L1.Make ((PrimitiveIsEqL1L1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? Unimplemented() :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                 new PCondIsEqL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqA1.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, environment.Argument1Value, ev1);

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

    class PCondIsEqL1A : PCondIsEqL1L
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqL1A (PrimitiveIsEqL1A predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqL1A predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqL1A0) ? PCondIsEqL1A0.Make ((PrimitiveIsEqL1A0) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqL1A1) ? PCondIsEqL1A1.Make ((PrimitiveIsEqL1A1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                 new PCondIsEqL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqA1.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, environment.Argument1Value, ev1);

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

    class PCondIsEqL1A0 : PCondIsEqL1A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqL1A0 (PrimitiveIsEqL1A0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqL1A0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsEqL1A0L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqL1A0SQ.Make (predicate, consequent, (Quotation) alternative) :
                 new PCondIsEqL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqA1.EvalStep");
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, environment.Argument1Value, ev1);

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

    class PCondIsEqL1A0L : PCondIsEqL1A0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqL1A0L (PrimitiveIsEqL1A0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqL1A0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? Unimplemented() :
                (consequent is LexicalVariable1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                 new PCondIsEqL1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqA1.EvalStep");
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, environment.Argument1Value, ev1);

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
               
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCondIsEqL1A0SQ : PCondIsEqL1A0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        PCondIsEqL1A0SQ (PrimitiveIsEqL1A0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqL1A0 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqL1A0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqA1.EvalStep");
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, environment.Argument1Value, ev1);

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



    class PCondIsEqL1A1 : PCondIsEqL1A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqL1A1 (PrimitiveIsEqL1A1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqL1A1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                 new PCondIsEqL1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqA1.EvalStep");
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, environment.Argument1Value, ev1);

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

    class PCondIsEqL1L1 : PCondIsEqL1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqL1L1 (PrimitiveIsEqL1L1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveIsEqL1L1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsEqL1L1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqL1L1Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqL1L1SQ.Make (predicate, consequent, (Quotation) alternative) :
                 new PCondIsEqL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqL1L1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqL1L1L : PCondIsEqL1L1
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsEqL1L1L (PrimitiveIsEqL1L1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsEqL1L1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqL1L1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented() :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented() :
                 new PCondIsEqL1L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqL1L1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqL1L1A : PCondIsEqL1L1L
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqL1L1A (PrimitiveIsEqL1L1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqL1L1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqL1L1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                 new PCondIsEqL1L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqL1L1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqL1L1A0 : PCondIsEqL1L1A
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqL1L1A0 (PrimitiveIsEqL1L1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqL1L1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                 new PCondIsEqL1L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqL1L1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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
#endif
                expression = this.consequent;
                return true;
            }
        }
    }



    class PCondIsEqL1L1Q : PCondIsEqL1L1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqL1L1Q (PrimitiveIsEqL1L1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsEqL1L1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                 new PCondIsEqL1L1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqL1L1Q.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
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

    sealed class PCondIsEqL1L1SQ : PCondIsEqL1L1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        PCondIsEqL1L1SQ (PrimitiveIsEqL1L1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsEqL1L1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqL1L1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqL1L1SQ.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternative;
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



    class PCondIsEqL1SL : PCondIsEqL1
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqL1SL (PrimitiveIsEqL1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveIsEqL1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqL1SA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                 new PCondIsEqL1SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqA1.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, environment.Argument1Value, ev1);

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

    class PCondIsEqL1SA : PCondIsEqL1SL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqL1SA (PrimitiveIsEqL1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveIsEqL1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqL1SA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                 new PCondIsEqL1SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqA1.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, environment.Argument1Value, ev1);

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

    class PCondIsEqL1SA0 : PCondIsEqL1SA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqL1SA0 (PrimitiveIsEqL1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveIsEqL1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqL1SA0Q.Make (predicate, consequent, (Quotation) alternative) :
                 new PCondIsEqL1SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqA1.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, environment.Argument1Value, ev1);

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

    class PCondIsEqL1SA0Q : PCondIsEqL1SA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object alternativeValue;

        protected PCondIsEqL1SA0Q (PrimitiveIsEqL1 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsEqL1 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCondIsEqL1SA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqL1SA0Q.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqL1SQ : PCondIsEqL1
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqL1SQ (PrimitiveIsEqL1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveIsEqL1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqL1SSQ.Make (predicate, consequent, (Quotation) alternative) :
                 new PCondIsEqL1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqA1.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, environment.Argument1Value, ev1);

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
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqLSSL : PCondIsEqL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCondIsEqLSSL (PrimitiveIsEqL predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveIsEqL predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondIsEqLSSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? Unimplemented() :
                new PCondIsEqLSSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqLSSL.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
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

    class PCondIsEqLSSA : PCondIsEqLSSL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqLSSA (PrimitiveIsEqL predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqL predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqLSSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCondIsEqLSSA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCondIsEqLSSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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

    sealed class PCondIsEqLSSA0 : PCondIsEqLSSA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        PCondIsEqLSSA0 (PrimitiveIsEqL predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqL predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsEqLSSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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

    sealed class PCondIsEqLSSA1 : PCondIsEqLSSA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        PCondIsEqLSSA1 (PrimitiveIsEqL predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqL predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCondIsEqLSSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqL1SSL : PCondIsEqL1
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsEqL1SSL (PrimitiveIsEqL1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        internal static SCode Make (PrimitiveIsEqL1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondIsEqL1SSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? Unimplemented():
                 new PCondIsEqL1SSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
            answer = false;
            return false;
        }
    }

    class PCondIsEqL1SSA : PCondIsEqL1SSL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqL1SSA (PrimitiveIsEqL1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsEqL1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqL1SSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? Unimplemented () :
                 new PCondIsEqL1SSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
            answer = false;
            return false;
        }
    }

    sealed class PCondIsEqL1SSA0 : PCondIsEqL1SSA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCondIsEqL1SSA0 (PrimitiveIsEqL1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsEqL1 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsEqL1SSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCondIsEqL1SSA0.EvalStep";
#endif
            object ev1;
            Control exp = this.rand1;
            Environment env = environment;
            while (exp.EvalStep (out ev1, ref exp, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqL1SSA0.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack)
                throw new NotImplementedException ();

            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();
            object tmp;
            ObjectModel.Eq (out tmp, ev0, ev1);

            if (tmp is bool && (bool) tmp == false) {
                answer = environment.Argument0Value;
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



    sealed class PCondIsEqL1SSQ : PCondIsEqL1
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        readonly object alternativeValue;
        PCondIsEqL1SSQ (PrimitiveIsEqL1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsEqL1 predicate, SCode consequent, Quotation alternative)
        {
            return
                 new PCondIsEqL1SSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqL1SSQ.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
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

    class PCondIsEqLL : PCondIsEqL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsEqLL (PrimitiveIsEqLL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsEqLL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqLA) ? PCondIsEqLA.Make ((PrimitiveIsEqLA) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqLL1) ? PCondIsEqLL1.Make ((PrimitiveIsEqLL1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCondIsEqLLL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqLA : PCondIsEqLL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqLA (PrimitiveIsEqLA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqLA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqLA0) ? PCondIsEqLA0.Make ((PrimitiveIsEqLA0) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqLA1) ? Unimplemented () :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqLA0 : PCondIsEqLA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqLA0 (PrimitiveIsEqLA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqLA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsEqLA0L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqLA0Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqLA0L : PCondIsEqLA0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsEqLA0L (PrimitiveIsEqLA0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsEqLA0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqLA0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCondIsEqLA0L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqLA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqLA0L.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, environment.Argument0Value);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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

    class PCondIsEqLA0A : PCondIsEqLA0L
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqLA0A (PrimitiveIsEqLA0 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqLA0 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? Unimplemented() :
                (consequent is Argument1) ? PCondIsEqLA0A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqLA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqLA0A1 : PCondIsEqLA0A
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqLA0A1 (PrimitiveIsEqLA0 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqLA0 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqLA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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
#endif
                expression = this.consequent;
                return true;
            }
        }
    }



    class PCondIsEqLA0L1 : PCondIsEqLA0L
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqLA0L1 (PrimitiveIsEqLA0 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqLA0 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqLA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqLA0L1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, environment.Argument0Value);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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

    class PCondIsEqLA0Q : PCondIsEqLA0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondIsEqLA0Q (PrimitiveIsEqLA0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsEqLA0 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqLA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqLA0Q.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, environment.Argument0Value);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
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

    class PCondIsEqLL1 : PCondIsEqL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqLL1 (PrimitiveIsEqLL1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqLL1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqLL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqLLL : PCondIsEqLL
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsEqLLL (PrimitiveIsEqLL predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsEqLL predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqLLA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCondIsEqLLL1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqLLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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

#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqLLA : PCondIsEqLLL
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqLLA (PrimitiveIsEqLL predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqLL predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqLLA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqLLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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

#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqLLA0 : PCondIsEqLLA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqLLA0 (PrimitiveIsEqLL predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqLL predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqLLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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

#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqLLL1 : PCondIsEqLLL
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqLLL1 (PrimitiveIsEqLL predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqLL predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqLLL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqLLL1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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



    class PCondIsEqLQ : PCondIsEqL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PCondIsEqLQ (PrimitiveIsEqLQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveIsEqLQ predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqLQ.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, this.rand1Value);

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

    class PCondIsEqLSL : PCondIsEqL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqLSL (PrimitiveIsEqL predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqL predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqLSA.Make(predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented():
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqLSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqLSA : PCondIsEqLSL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqLSA (PrimitiveIsEqL predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqL predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqLSA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqLSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqLSA0 : PCondIsEqLSA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqLSA0 (PrimitiveIsEqL predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqL predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqLSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqLSA0.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
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



    class PCondIsEqLSQ : PCondIsEqL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqLSQ (PrimitiveIsEqL predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
   this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsEqL predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsEqLSQL.Make (predicate, consequent, (LexicalVariable) alternative):
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqLSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqLSQL : PCondIsEqLSQ
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqLSQL (PrimitiveIsEqL predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqL predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? Unimplemented() :
                (alternative is LexicalVariable1) ? Unimplemented () :
                new PCondIsEqLSQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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



    sealed class PCondIsEqLSSQ : PCondIsEqL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        PCondIsEqLSSQ (PrimitiveIsEqL predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsEqL predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqLSSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

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

    [Serializable]
    class PCondIsEqCar : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly SCode rand0Arg;

        protected PCondIsEqCar (PrimitiveIsEqCar predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Arg = predicate.rand0Arg;
        }

        public static SCode Make (PrimitiveIsEqCar predicate, SCode consequent, SCode alternative)
        {
            return 
                (predicate is PrimitiveIsEqCarL) ? PCondIsEqCarL.Make ((PrimitiveIsEqCarL) predicate, consequent, alternative)
                //(predicate is PrimitiveIsEqCarQ) ? Unimplemented()
                //: (predicate is PrimitiveIsEqCarSL) ? 
                : (predicate is PrimitiveIsEqCarSQ) ? PCondIsEqCarSQ.Make ((PrimitiveIsEqCarSQ) predicate, consequent, alternative)
                //: (consequent is LexicalVariable) ? Unimplemented()
                //: (consequent is Quotation) ? Unimplemented()
                //: (alternative is LexicalVariable) ? Unimplemented()
                //: (alternative is Quotation) ? Unimplemented()
                : new PCondIsEqCar (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCar.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0Arg;
            env = environment;
            object ev0temp;
            while (unev.EvalStep (out ev0temp, ref unev, ref env)) { };
            if (ev0temp == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            Cons ev0 = ev0temp as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0.Car, ev1);

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

    class PCondIsEqCarL : PCondIsEqCar
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PCondIsEqCarL (PrimitiveIsEqCarL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Name = predicate.rand0Name;
            this.rand0Depth = predicate.rand0Depth;
            this.rand0Offset = predicate.rand0Offset;
        }


        public static SCode Make (PrimitiveIsEqCarL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCarA) ? PCondIsEqCarA.Make ((PrimitiveIsEqCarA) predicate, consequent, alternative)
                : new PCondIsEqCarL (predicate, consequent, alternative);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqCarL.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqCarL.EvalStep.1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }


            object ev0a;
            if (environment.FastLexicalRef (out ev0a, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            Cons ev0 = ev0a as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0.Car, ev1);

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

    class PCondIsEqCarA : PCondIsEqCarL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqCarA (PrimitiveIsEqCarA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsEqCarA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCarA0) ? PCondIsEqCarA0.Make ((PrimitiveIsEqCarA0) predicate, consequent, alternative)
                : new PCondIsEqCarA (predicate, consequent, alternative);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCondIsEqCarA.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ((Cons) environment.ArgumentValue(this.rand0Offset)).Car, ev1);

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

    class PCondIsEqCarA0 : PCondIsEqCarA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqCarA0 (PrimitiveIsEqCarA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCarA0 predicate, SCode consequent, SCode alternative)
        {
            return 
                (predicate is PrimitiveIsEqCarA0Car) ? PCondIsEqCarA0Car.Make ((PrimitiveIsEqCarA0Car) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqCarA0L) ? PCondIsEqCarA0L.Make ((PrimitiveIsEqCarA0L) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqCarA0Q) ? PCondIsEqCarA0Q.Make ((PrimitiveIsEqCarA0Q) predicate, consequent, alternative) :
                //(consequent is LexicalVariable) ? Unimplemented() :
                //(consequent is Quotation) ? Unimplemented() :
                //(alternative is LexicalVariable) ? Unimplemented():
                (alternative is Quotation) ? PCondIsEqCarA0SSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqCarA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCondIsEqCarA0.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ((Cons)environment.Argument0Value).Car, ev1);

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

    class PCondIsEqCarA0Car : PCondIsEqCarA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqCarA0Car (PrimitiveIsEqCarA0Car predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCarA0Car predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCarA0CarL) ? PCondIsEqCarA0CarL.Make ((PrimitiveIsEqCarA0CarL) predicate, consequent, alternative) :
                new PCondIsEqCarA0Car (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();

        }
    }

    class PCondIsEqCarA0CarL : PCondIsEqCarA0Car
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqCarA0CarL (PrimitiveIsEqCarA0CarL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCarA0CarL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCarA0CarA) ? PCondIsEqCarA0CarA.Make ((PrimitiveIsEqCarA0CarA) predicate, consequent, alternative) :
                new PCondIsEqCarA0CarL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();

        }
    }

    class PCondIsEqCarA0CarA : PCondIsEqCarA0CarL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqCarA0CarA (PrimitiveIsEqCarA0CarA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCarA0CarA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCarA0CarA1) ? PCondIsEqCarA0CarA1.Make ((PrimitiveIsEqCarA0CarA1) predicate, consequent, alternative) :
                new PCondIsEqCarA0CarA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();

        }
    }

    class PCondIsEqCarA0CarA1 : PCondIsEqCarA0CarA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqCarA0CarA1 (PrimitiveIsEqCarA0CarA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCarA0CarA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (alternative is Quotation) ? PCondIsEqCarA0CarA1SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqCarA0CarA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("PCondIsEqCarA0CarA1");
#endif
            Cons ev1 = environment.Argument1Value as Cons;
            Cons ev0 = environment.Argument0Value as Cons;
            ObjectModel.Eq (out answer, ev0.Car, ev1.Car);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqCarA0CarA1.EvalStep.1";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqCarA0CarA1.EvalStep.1";
#endif
                expression = this.consequent;
                return true;
            }
            

        }
    }

    class PCondIsEqCarA0CarA1SQ : PCondIsEqCarA0CarA1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondIsEqCarA0CarA1SQ (PrimitiveIsEqCarA0CarA1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsEqCarA0CarA1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqCarA0CarA1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCarA0CarA1SQ");
#endif
            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ((Cons) environment.Argument1Value).Car);

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqCarA0CarA1.EvalStep.1";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }



    sealed class PCondIsEqCarA0SSQ : PCondIsEqCarA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        PCondIsEqCarA0SSQ (PrimitiveIsEqCarA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsEqCarA0 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqCarA0SSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqCarA0SSQ.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
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

    class PCondIsEqCarA0L : PCondIsEqCarA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsEqCarA0L (PrimitiveIsEqCarA0L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }


        public static SCode Make (PrimitiveIsEqCarA0L predicate, SCode consequent, SCode alternative)
        {
            return 
                //(predicate is PrimitiveIsEqCarA0A) ? Unimplemented():
                (predicate is PrimitiveIsEqCarA0L1) ? PCondIsEqCarA0L1.Make ((PrimitiveIsEqCarA0L1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCondIsEqCarA0LL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented():
                (alternative is LexicalVariable) ? Unimplemented():
                (alternative is Quotation) ? Unimplemented():
                new PCondIsEqCarA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCarA0L.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ev1);

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

    class PCondIsEqCarA0L1 : PCondIsEqCarA0L
    {


        protected PCondIsEqCarA0L1 (PrimitiveIsEqCarA0L1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsEqCarA0L1 predicate, SCode consequent, SCode alternative)
        {
            return
                 new PCondIsEqCarA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();

        }
    }

    class PCondIsEqCarA0LL : PCondIsEqCarA0L
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsEqCarA0LL (PrimitiveIsEqCarA0L predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }


        public static SCode Make (PrimitiveIsEqCarA0L predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqCarA0LA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqCarA0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCarA0LL.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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

    class PCondIsEqCarA0LA : PCondIsEqCarA0LL
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqCarA0LA (PrimitiveIsEqCarA0L predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsEqCarA0L predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqCarA0LA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqCarA0LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqCarA0LA.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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

    class PCondIsEqCarA0LA0 : PCondIsEqCarA0LA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqCarA0LA0 (PrimitiveIsEqCarA0L predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCarA0L predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Combination1LCdrL1) ? PCondIsEqCarA0LA0Fragment0.Make (predicate, consequent, (Combination1LCdrL1) alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqCarA0LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCarA0LA0.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;
            ObjectModel.Eq (out answer, ((Cons) ev0).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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

    sealed class PCondIsEqCarA0LA0Fragment0 : PCondIsEqCarA0LA0
    {
        public readonly object altRand0Name;
        public readonly int altRand0Offset;
        public readonly object altRatorName;
        public readonly int altRatorDepth;
        public readonly int altRatorOffset;
        PCondIsEqCarA0LA0Fragment0 (PrimitiveIsEqCarA0L predicate, Argument0 consequent, Combination1LCdrL1 alternative)
            : base (predicate, consequent, alternative)
        {
            this.altRand0Name = alternative.rand0Name;
            this.altRand0Offset = alternative.rand0Offset;
            this.altRatorName = alternative.ratorName;
            this.altRatorDepth = alternative.ratorDepth;
            this.altRatorOffset = alternative.ratorOffset;
        }

        public static SCode Make (PrimitiveIsEqCarA0L predicate, Argument0 consequent, Combination1LCdrL1 alternative)
        {
            return
                new PCondIsEqCarA0LA0Fragment0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCarA0LA0Fragment0.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;
            object ev0Car = ((Cons) ev0).Car;
            //             ObjectModel.Eq (out answer, ((Cons) ev0).Car, ev1);
            if (((ev0Car == null) && (ev1 == null))
                || ((ev1 != null) &&
                    ((ev0Car == ev1)
                     || ((ev0Car is Int32 && ev1 is Int32) && ((int) ev0Car == (int) ev1))
                     || ((ev0Car is char && ev1 is char) && ((char) ev0Car == (char) ev1))
                     || ((ev0Car is bool && ev1 is bool) && ((bool) ev0Car == (bool) ev1))))) {
                answer = ev0;
                return false;
            }
            else {
                object evarg;
                if (environment.FastLexicalRef1 (out evarg, this.altRand0Name, this.altRand0Offset))
                    throw new NotImplementedException ();

                object evop;
                if (environment.FastLexicalRef (out evop, this.altRatorName, this.altRatorDepth, this.altRatorOffset))
                    throw new NotImplementedException ();

                return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) evarg).Cdr);
            }
        }
    }

    class PCondIsEqCarA0Q : PCondIsEqCarA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PCondIsEqCarA0Q (PrimitiveIsEqCarA0Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }


        public static SCode Make (PrimitiveIsEqCarA0Q predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqCarA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCarA1 : PCondIsEqCarA
    {
        protected PCondIsEqCarA1 (PrimitiveIsEqCarA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsEqCarA1 predicate, SCode consequent, SCode alternative)
        {
            return Unimplemented();
                 //(rand1 is LexicalVariable) ? PCondIsEqCarA1L.Make (rator, rand0, (LexicalVariable) rand1)
                 //: (rand1 is Quotation) ? PCondIsEqCarA1Q.Make (rator, rand0, (Quotation) rand1)
                 //: new PCondIsEqCarA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCarA1L : PCondIsEqCarA1
    {
        //public readonly object randName;
        //public readonly int randDepth;
        //public readonly int randOffset;

        protected PCondIsEqCarA1L (PrimitiveIsEqCarA1L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            //this.randName = rand1.Name;
            //this.randDepth = rand1.Depth;
            //this.randOffset = rand1.Offset;
        }


        public static SCode Make (PrimitiveIsEqCarA1L predicate, SCode consequent, SCode alternative)
        {
            return Unimplemented();
                //(rand1 is Argument) ? Unimplemented ()
                //: (rand1 is LexicalVariable1) ? Unimplemented ()
                //: new PCondIsEqCarA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCarA1Q : PCondIsEqCarA1
    {
        //public readonly object rand1Value;

        protected PCondIsEqCarA1Q (PrimitiveIsEqCarA1Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }


        public static SCode Make (PrimitiveIsEqCarA1Q predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqCarA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();

        }
    }

    class PCondIsEqCarLL : PCondIsEqCarL
    {
        //public readonly object randName;
        //public readonly int randDepth;
        //public readonly int randOffset;

        protected PCondIsEqCarLL (PrimitiveIsEqCarLL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }


        public static SCode Make (PrimitiveIsEqCarLL predicate, SCode consequent, SCode alternative)
        {
            return Unimplemented();

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCarLQ : PCondIsEqCarL
    {
       // public readonly object rand1Value;

        protected PCondIsEqCarLQ (PrimitiveIsEqCarLQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {


        }


        public static SCode Make (PrimitiveIsEqCarLQ predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqCarLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCarSL : PCondIsEqCar
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsEqCarSL (PrimitiveIsEqCarSL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsEqCarSL predicate, SCode consequent, SCode alternative)
        {
            return Unimplemented ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0Arg;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            Cons ev0c = ev0 as Cons;
            ObjectModel.Eq (out answer, ev0c.Car, ev1);
            return false;
        }
    }

    class PCondIsEqCarSQ : PCondIsEqCar
    {
        public readonly object rand1Value;

        protected PCondIsEqCarSQ (PrimitiveIsEqCarSQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveIsEqCarSQ predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqCarSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsEqCaar : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly SCode rand0Arg;

        protected PCondIsEqCaar (PrimitiveIsEqCaar predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Arg = predicate.rand0Arg;
        }

        public static SCode Make (PrimitiveIsEqCaar predicate, SCode consequent, SCode alternative)
        {
            return 
                (predicate is PrimitiveIsEqCaarL) ? PCondIsEqCaarL.Make ((PrimitiveIsEqCaarL) predicate, consequent, alternative)
                //(predicate is PrimitiveIsEqCaarQ) ? Unimplemented()
                //: (predicate is PrimitiveIsEqCaarSL) ? 
                : (predicate is PrimitiveIsEqCaarSQ) ? PCondIsEqCaarSQ.Make ((PrimitiveIsEqCaarSQ) predicate, consequent, alternative)
                //: (consequent is LexicalVariable) ? Unimplemented()
                //: (consequent is Quotation) ? Unimplemented()
                //: (alternative is LexicalVariable) ? Unimplemented()
                //: (alternative is Quotation) ? Unimplemented()
                : new PCondIsEqCaar (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ("PCondIsEqCaar.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0Arg;
            env = environment;
            object ev0temp;
            while (unev.EvalStep (out ev0temp, ref unev, ref env)) { };
            if (ev0temp == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            Cons ev0 = ev0temp as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0.Car, ev1);

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

    class PCondIsEqCaarL : PCondIsEqCaar
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PCondIsEqCaarL (PrimitiveIsEqCaarL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Name = predicate.rand0Name;
            this.rand0Depth = predicate.rand0Depth;
            this.rand0Offset = predicate.rand0Offset;
        }


        public static SCode Make (PrimitiveIsEqCaarL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCaarA) ? PCondIsEqCaarA.Make ((PrimitiveIsEqCaarA) predicate, consequent, alternative)
                : new PCondIsEqCaarL (predicate, consequent, alternative);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqCaarL.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqCaarL.EvalStep.1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }


            object ev0a;
            if (environment.FastLexicalRef (out ev0a, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            Cons ev0 = ev0a as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0.Car, ev1);

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

    class PCondIsEqCaarA : PCondIsEqCaarL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqCaarA (PrimitiveIsEqCaarA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsEqCaarA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCaarA0) ? PCondIsEqCaarA0.Make ((PrimitiveIsEqCaarA0) predicate, consequent, alternative)
                : new PCondIsEqCaarA (predicate, consequent, alternative);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCondIsEqCaarA.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ((Cons) environment.ArgumentValue(this.rand0Offset)).Car, ev1);

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

    class PCondIsEqCaarA0 : PCondIsEqCaarA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqCaarA0 (PrimitiveIsEqCaarA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCaarA0 predicate, SCode consequent, SCode alternative)
        {
            return 
                (predicate is PrimitiveIsEqCaarA0L) ? PCondIsEqCaarA0L.Make ((PrimitiveIsEqCaarA0L) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqCaarA0Q) ? PCondIsEqCaarA0Q.Make ((PrimitiveIsEqCaarA0Q) predicate, consequent, alternative) :
                //(consequent is LexicalVariable) ? Unimplemented() :
                //(consequent is Quotation) ? Unimplemented() :
                //(alternative is LexicalVariable) ? Unimplemented():
                (alternative is Quotation) ? PCondIsEqCaarA0SSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqCaarA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCondIsEqCaarA0.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ((Cons)environment.Argument0Value).Car, ev1);

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



    sealed class PCondIsEqCaarA0SSQ : PCondIsEqCaarA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        PCondIsEqCaarA0SSQ (PrimitiveIsEqCaarA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsEqCaarA0 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqCaarA0SSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqCaarA0SSQ.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
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

    class PCondIsEqCaarA0L : PCondIsEqCaarA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsEqCaarA0L (PrimitiveIsEqCaarA0L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }


        public static SCode Make (PrimitiveIsEqCaarA0L predicate, SCode consequent, SCode alternative)
        {
            return 
                //(predicate is PrimitiveIsEqCaarA0A) ? Unimplemented():
                (predicate is PrimitiveIsEqCaarA0L1) ? PCondIsEqCaarA0L1.Make ((PrimitiveIsEqCaarA0L1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCondIsEqCaarA0LL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented():
                (alternative is LexicalVariable) ? Unimplemented():
                (alternative is Quotation) ? Unimplemented():
                new PCondIsEqCaarA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ("PCondIsEqCaarA0L.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ev1);

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

    class PCondIsEqCaarA0L1 : PCondIsEqCaarA0L
    {


        protected PCondIsEqCaarA0L1 (PrimitiveIsEqCaarA0L1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsEqCaarA0L1 predicate, SCode consequent, SCode alternative)
        {
            return
                 new PCondIsEqCaarA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();

        }
    }

    class PCondIsEqCaarA0LL : PCondIsEqCaarA0L
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsEqCaarA0LL (PrimitiveIsEqCaarA0L predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }


        public static SCode Make (PrimitiveIsEqCaarA0L predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqCaarA0LA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqCaarA0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ("PCondIsEqCaarA0LL.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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

    class PCondIsEqCaarA0LA : PCondIsEqCaarA0LL
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqCaarA0LA (PrimitiveIsEqCaarA0L predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsEqCaarA0L predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqCaarA0LA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqCaarA0LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqCaarA0LA.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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

    class PCondIsEqCaarA0LA0 : PCondIsEqCaarA0LA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqCaarA0LA0 (PrimitiveIsEqCaarA0L predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCaarA0L predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Combination1LCdrL1) ? PCondIsEqCaarA0LA0Fragment0.Make (predicate, consequent, (Combination1LCdrL1) alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqCaarA0LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ("PCondIsEqCaarA0LA0.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;
            ObjectModel.Eq (out answer, ((Cons) ev0).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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

    sealed class PCondIsEqCaarA0LA0Fragment0 : PCondIsEqCaarA0LA0
    {
        public readonly object altRand0Name;
        public readonly int altRand0Offset;
        public readonly object altRatorName;
        public readonly int altRatorDepth;
        public readonly int altRatorOffset;
        PCondIsEqCaarA0LA0Fragment0 (PrimitiveIsEqCaarA0L predicate, Argument0 consequent, Combination1LCdrL1 alternative)
            : base (predicate, consequent, alternative)
        {
            this.altRand0Name = alternative.rand0Name;
            this.altRand0Offset = alternative.rand0Offset;
            this.altRatorName = alternative.ratorName;
            this.altRatorDepth = alternative.ratorDepth;
            this.altRatorOffset = alternative.ratorOffset;
        }

        public static SCode Make (PrimitiveIsEqCaarA0L predicate, Argument0 consequent, Combination1LCdrL1 alternative)
        {
            return
                new PCondIsEqCaarA0LA0Fragment0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ("PCondIsEqCaarA0LA0Fragment0.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;
            object ev0Car = ((Cons) ev0).Car;
            //             ObjectModel.Eq (out answer, ((Cons) ev0).Car, ev1);
            if (((ev0Car == null) && (ev1 == null))
                || ((ev1 != null) &&
                    ((ev0Car == ev1)
                     || ((ev0Car is Int32 && ev1 is Int32) && ((int) ev0Car == (int) ev1))
                     || ((ev0Car is char && ev1 is char) && ((char) ev0Car == (char) ev1))
                     || ((ev0Car is bool && ev1 is bool) && ((bool) ev0Car == (bool) ev1))))) {
                answer = ev0;
                return false;
            }
            else {
                object evarg;
                if (environment.FastLexicalRef1 (out evarg, this.altRand0Name, this.altRand0Offset))
                    throw new NotImplementedException ();

                object evop;
                if (environment.FastLexicalRef (out evop, this.altRatorName, this.altRatorDepth, this.altRatorOffset))
                    throw new NotImplementedException ();

                return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) evarg).Cdr);
            }
        }
    }

    class PCondIsEqCaarA0Q : PCondIsEqCaarA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PCondIsEqCaarA0Q (PrimitiveIsEqCaarA0Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }


        public static SCode Make (PrimitiveIsEqCaarA0Q predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqCaarA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCaarA1 : PCondIsEqCaarA
    {
        protected PCondIsEqCaarA1 (PrimitiveIsEqCaarA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsEqCaarA1 predicate, SCode consequent, SCode alternative)
        {
            return Unimplemented();
                 //(rand1 is LexicalVariable) ? PCondIsEqCaarA1L.Make (rator, rand0, (LexicalVariable) rand1)
                 //: (rand1 is Quotation) ? PCondIsEqCaarA1Q.Make (rator, rand0, (Quotation) rand1)
                 //: new PCondIsEqCaarA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCaarA1L : PCondIsEqCaarA1
    {
        //public readonly object randName;
        //public readonly int randDepth;
        //public readonly int randOffset;

        protected PCondIsEqCaarA1L (PrimitiveIsEqCaarA1L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            //this.randName = rand1.Name;
            //this.randDepth = rand1.Depth;
            //this.randOffset = rand1.Offset;
        }


        public static SCode Make (PrimitiveIsEqCaarA1L predicate, SCode consequent, SCode alternative)
        {
            return Unimplemented();
                //(rand1 is Argument) ? Unimplemented ()
                //: (rand1 is LexicalVariable1) ? Unimplemented ()
                //: new PCondIsEqCaarA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCaarA1Q : PCondIsEqCaarA1
    {
        //public readonly object rand1Value;

        protected PCondIsEqCaarA1Q (PrimitiveIsEqCaarA1Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }


        public static SCode Make (PrimitiveIsEqCaarA1Q predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqCaarA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();

        }
    }

    class PCondIsEqCaarLL : PCondIsEqCaarL
    {
        //public readonly object randName;
        //public readonly int randDepth;
        //public readonly int randOffset;

        protected PCondIsEqCaarLL (PrimitiveIsEqCaarLL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }


        public static SCode Make (PrimitiveIsEqCaarLL predicate, SCode consequent, SCode alternative)
        {
            return Unimplemented();

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCaarLQ : PCondIsEqCaarL
    {
       // public readonly object rand1Value;

        protected PCondIsEqCaarLQ (PrimitiveIsEqCaarLQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {


        }


        public static SCode Make (PrimitiveIsEqCaarLQ predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqCaarLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCaarSL : PCondIsEqCaar
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsEqCaarSL (PrimitiveIsEqCaarSL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsEqCaarSL predicate, SCode consequent, SCode alternative)
        {
            return Unimplemented ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0Arg;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            Cons ev0c = ev0 as Cons;
            ObjectModel.Eq (out answer, ev0c.Car, ev1);
            return false;
        }
    }

    class PCondIsEqCaarSQ : PCondIsEqCaar
    {
        public readonly object rand1Value;

        protected PCondIsEqCaarSQ (PrimitiveIsEqCaarSQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveIsEqCaarSQ predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqCaarSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }


    class PCondIsEqQ : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;

        protected PCondIsEqQ (PrimitiveIsEqQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Value = predicate.rand0Value;
        }

        public static SCode Make (PrimitiveIsEqQ predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqQ.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSL : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsEqSL (PrimitiveIsEqSL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsEqSL predicate, SCode consequent, SCode alternative)
        {
            return 
                (predicate is PrimitiveIsEqSA) ? PCondIsEqSA.Make ((PrimitiveIsEqSA) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqSL1) ? PCondIsEqSL1.Make ((PrimitiveIsEqSL1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCondIsEqSLL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqSLQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCondIsEqSLSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? Unimplemented():
                new PCondIsEqSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSA : PCondIsEqSL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA (PrimitiveIsEqSA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqSA0) ? PCondIsEqSA0.Make ((PrimitiveIsEqSA0) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqSA1) ? PCondIsEqSA1.Make ((PrimitiveIsEqSA1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented() :
                (alternative is LexicalVariable) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSA0 : PCondIsEqSA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA0 (PrimitiveIsEqSA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsEqSA0L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqSA0Q.Make (predicate, (Quotation) consequent, alternative) :
                //(alternative is LexicalVariable) ? Unimplemented () :
                //(alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSA0L : PCondIsEqSA0
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA0L (PrimitiveIsEqSA0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqSA0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSA0A : PCondIsEqSA0L
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA0A (PrimitiveIsEqSA0 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA0 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqSA0A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSA0A0 : PCondIsEqSA0A
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA0A0 (PrimitiveIsEqSA0 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA0 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqSA0A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqSA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    sealed class PCondIsEqSA0A0Q : PCondIsEqSA0A0
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif

        PCondIsEqSA0A0Q (PrimitiveIsEqSA0 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA0 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCondIsEqSA0A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSA0Q : PCondIsEqSA0
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA0Q (PrimitiveIsEqSA0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA0 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSA1 : PCondIsEqSA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA1 (PrimitiveIsEqSA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsEqSA1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqSA1Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqSA1SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSA1L : PCondIsEqSA1
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA1L (PrimitiveIsEqSA1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqSA1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented() :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSA1A : PCondIsEqSA1L
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA1A (PrimitiveIsEqSA1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqSA1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSA1A0 : PCondIsEqSA1A
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA1A0 (PrimitiveIsEqSA1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqSA1A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqSA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    sealed class PCondIsEqSA1A0Q : PCondIsEqSA1A0
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif

        PCondIsEqSA1A0Q (PrimitiveIsEqSA1 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA1 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCondIsEqSA1A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSA1Q : PCondIsEqSA1
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA1Q (PrimitiveIsEqSA1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented():
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    sealed class PCondIsEqSA1SQ : PCondIsEqSA1
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        PCondIsEqSA1SQ (PrimitiveIsEqSA1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqSA1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG

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



    class PCondIsEqSL1 : PCondIsEqSL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSL1 (PrimitiveIsEqSL1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsEqSL1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented() :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSL1L : PCondIsEqSL1
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsEqSL1L (PrimitiveIsEqSL1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsEqSL1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqSL1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqSL1LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqSL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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
 
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSL1A : PCondIsEqSL1L
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSL1A (PrimitiveIsEqSL1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqSL1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented() :
                new PCondIsEqSL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSL1A0 : PCondIsEqSL1A
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSL1A0 (PrimitiveIsEqSL1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqSL1A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqSL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    sealed class PCondIsEqSL1A0Q : PCondIsEqSL1A0
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        PCondIsEqSL1A0Q (PrimitiveIsEqSL1 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsEqSL1 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCondIsEqSL1A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqSL1A0Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object temp;
            ObjectModel.Eq (out temp, ev0, ev1);

            answer = ((temp is bool) && (bool) temp == false) ? this.alternativeValue : environment.Argument0Value;
            return false;
        }
    }

    sealed class PCondIsEqSL1LQ : PCondIsEqSL1L
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        PCondIsEqSL1LQ (PrimitiveIsEqSL1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsEqSL1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCondIsEqSL1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqSL1LQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
            }
            return false;
        }
    }

    class PCondIsEqSLL : PCondIsEqSL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif


        protected PCondIsEqSLL (PrimitiveIsEqSL predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqSLA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented() :
                (alternative is LexicalVariable) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSLA : PCondIsEqSLL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif


        protected PCondIsEqSLA (PrimitiveIsEqSL predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqSLA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSLA0 : PCondIsEqSLA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif


        protected PCondIsEqSLA0 (PrimitiveIsEqSL predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqSLA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object temp;
            ObjectModel.Eq (out temp, ev0, ev1);

            if ((temp is bool) && (bool) temp == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }



    class PCondIsEqSLQ : PCondIsEqSL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSLQ (PrimitiveIsEqSL predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL predicate, Quotation consequent, SCode alternative)
        {
            return 
                (alternative is LexicalVariable) ? Unimplemented():
                (alternative is Quotation) ? Unimplemented():
                new PCondIsEqSLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSLSL : PCondIsEqSL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsEqSLSL (PrimitiveIsEqSL predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name ;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveIsEqSL predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondIsEqSLSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? Unimplemented () :
                new PCondIsEqSLSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    class PCondIsEqSLSA : PCondIsEqSLSL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSLSA (PrimitiveIsEqSL predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqSLSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? Unimplemented () :
                new PCondIsEqSLSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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

    sealed class PCondIsEqSLSA0 : PCondIsEqSLSA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        PCondIsEqSLSA0 (PrimitiveIsEqSL predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsEqSLSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

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



    class PCondIsEqSQ : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PCondIsEqSQ (PrimitiveIsEqSQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveIsEqSQ predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqSQ.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }


            ObjectModel.Eq (out answer, ev0, ev1);

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


}
