using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PCondGreaterThanFixnum : PCond2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnum (PrimitiveGreaterThanFixnum predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnum predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumA) ? PCondGreaterThanFixnumA.Make ((PrimitiveGreaterThanFixnumA) predicate, consequent, alternative) :
                (predicate is PrimitiveGreaterThanFixnumS) ? PCondGreaterThanFixnumS.Make ((PrimitiveGreaterThanFixnumS) predicate, consequent, alternative) :
                (predicate is PrimitiveGreaterThanFixnumQ) ? PCondGreaterThanFixnumQ.Make ((PrimitiveGreaterThanFixnumQ) predicate, consequent, alternative) :
                (predicate is PrimitiveGreaterThanFixnumXQ) ? PCondGreaterThanFixnumXQ.Make ((PrimitiveGreaterThanFixnumXQ) predicate, consequent, alternative) :
                new PCondGreaterThanFixnum (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondGreaterThanFixnum";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondGreaterThanFixnum";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondGreaterThanFixnum";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if ((int) ev0 > (int) ev1) {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondGreaterThanFixnumA : PCondGreaterThanFixnum
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;

        protected PCondGreaterThanFixnumA (PrimitiveGreaterThanFixnumA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Offset = predicate.rand0Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumA0) ? PCondGreaterThanFixnumA0.Make ((PrimitiveGreaterThanFixnumA0) predicate, consequent, alternative) :
                 new PCondGreaterThanFixnumA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondGreaterThanFixnumA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondGreaterThanFixnumA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            if ((int) ev0 > (int) ev1) {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondGreaterThanFixnumA0 : PCondGreaterThanFixnumA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA0 (PrimitiveGreaterThanFixnumA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumA0Q) ? PCondGreaterThanFixnumA0Q.Make ((PrimitiveGreaterThanFixnumA0Q) predicate, consequent, alternative) :
                new PCondGreaterThanFixnumA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondGreaterThanFixnumA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondGreaterThanFixnumA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

            if ((int) ev0 > (int) ev1) {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondGreaterThanFixnumA0Q : PCondGreaterThanFixnumA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PCondGreaterThanFixnumA0Q (PrimitiveGreaterThanFixnumA0Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
             this.rand1Value = (int) predicate.rand1Value;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0Q predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondGreaterThanFixnumA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondGreaterThanFixnumA0Q";
#endif
            object ev0 = environment.Argument0Value;

            if ((int) ev0 > this.rand1Value) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondGreaterThanFixnumA0Q";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondGreaterThanFixnumA0Q";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }


    [Serializable]
    class PCondGreaterThanFixnumS : PCondGreaterThanFixnum
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected PCondGreaterThanFixnumS (PrimitiveGreaterThanFixnumS predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Name = predicate.rand0Name;
            this.rand0Offset = predicate.rand0Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumS predicate, SCode consequent, SCode alternative)
        {
            return
               new PCondGreaterThanFixnumS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondGreaterThanFixnumS";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondGreaterThanFixnumS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 > (int) ev1) {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondGreaterThanFixnumQ : PCondGreaterThanFixnum
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Value;

        protected PCondGreaterThanFixnumQ (PrimitiveGreaterThanFixnumQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Value = predicate.rand0Value;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumQ predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumQA) ? PCondGreaterThanFixnumQA.Make ((PrimitiveGreaterThanFixnumQA) predicate, consequent, alternative) :
                new PCondGreaterThanFixnumQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondGreaterThanFixnumQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondGreaterThanFixnumQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }


            if (this.rand0Value > (int) ev1) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondGreaterThanFixnumQ";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondGreaterThanFixnumQ";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondGreaterThanFixnumQA : PCondGreaterThanFixnumQ
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;

        protected PCondGreaterThanFixnumQA (PrimitiveGreaterThanFixnumQA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumQA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumQA0) ? PCondGreaterThanFixnumQA0.Make ((PrimitiveGreaterThanFixnumQA0) predicate, consequent, alternative) :
               new PCondGreaterThanFixnumQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondGreaterThanFixnumQA";
#endif

            if (this.rand0Value > (int) environment.ArgumentValue(this.rand1Offset)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondGreaterThanFixnumQA";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondGreaterThanFixnumQA";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    class PCondGreaterThanFixnumQA0 : PCondGreaterThanFixnumQA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumQA0 (PrimitiveGreaterThanFixnumQA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumQA0 predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondGreaterThanFixnumQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondGreaterThanFixnumQA0";
#endif

            if (this.rand0Value > (int) environment.Argument0Value) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondGreaterThanFixnumQA0";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondGreaterThanFixnumQA0";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondGreaterThanFixnumXQ : PCondGreaterThanFixnum
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PCondGreaterThanFixnumXQ (PrimitiveGreaterThanFixnumXQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumXQ predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondGreaterThanFixnumXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondGreaterThanFixnumXQ";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondGreaterThanFixnumXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }


            if ((int) ev0 > this.rand1Value) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondGreaterThanFixnumXQ";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondGreaterThanFixnumXQ";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

}
