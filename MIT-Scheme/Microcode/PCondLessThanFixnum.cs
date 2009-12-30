using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PCondLessThanFixnum : PCond2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnum (PrimitiveLessThanFixnum predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnum predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumA) ? PCondLessThanFixnumA.Make ((PrimitiveLessThanFixnumA) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumS) ? PCondLessThanFixnumS.Make ((PrimitiveLessThanFixnumS) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumXQ) ? PCondLessThanFixnumXQ.Make ((PrimitiveLessThanFixnumXQ) predicate, consequent, alternative) :
                new PCondLessThanFixnum (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnum";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnum";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnum";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if ((int) ev0 < (int) ev1) {
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
    class PCondLessThanFixnumA : PCondLessThanFixnum
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;

        protected PCondLessThanFixnumA (PrimitiveLessThanFixnumA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Offset = predicate.rand0Offset;
        }

        public static SCode Make (PrimitiveLessThanFixnumA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumA0) ? PCondLessThanFixnumA0.Make ((PrimitiveLessThanFixnumA0) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumA1) ? PCondLessThanFixnumA1.Make ((PrimitiveLessThanFixnumA1) predicate, consequent, alternative) :
                new PCondLessThanFixnumA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            if ((int) ev0 < (int) ev1) {
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
    class PCondLessThanFixnumA0 : PCondLessThanFixnumA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA0 (PrimitiveLessThanFixnumA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveLessThanFixnumA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumA0Q) ? PCondLessThanFixnumA0Q.Make ((PrimitiveLessThanFixnumA0Q) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumA0S) ? PCondLessThanFixnumA0S.Make ((PrimitiveLessThanFixnumA0S) predicate, consequent, alternative) :
                new PCondLessThanFixnumA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
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
    class PCondLessThanFixnumA0Q : PCondLessThanFixnumA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PCondLessThanFixnumA0Q (PrimitiveLessThanFixnumA0Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveLessThanFixnumA0Q predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Quotation) ? PCondLessThanFixnumA0QQ.Make (predicate, (Quotation) consequent, alternative) :
                new PCondLessThanFixnumA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0Q");
#endif
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < this.rand1Value) {
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
    class PCondLessThanFixnumA0QQ : PCondLessThanFixnumA0Q
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondLessThanFixnumA0QQ (PrimitiveLessThanFixnumA0Q predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumA0Q predicate, Quotation consequent, SCode alternative)
        {
            return
                new PCondLessThanFixnumA0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0QQ");
#endif
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < this.rand1Value) {
                answer = this.consequentValue;
                return false;
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
    class PCondLessThanFixnumA0S : PCondLessThanFixnumA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PCondLessThanFixnumA0S (PrimitiveLessThanFixnumA0S predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveLessThanFixnumA0S predicate, SCode consequent, SCode alternative)
        {
            return
                (alternative is Quotation) ? new PCondLessThanFixnumA0SXQ (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumA0S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0S");
#endif
            object ev0 = environment.Argument0Value;
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondLessThanFixnumA0S";
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
                SCode.location = "PCondLessThanFixnumA0S";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondLessThanFixnumA0SXQ : PCondLessThanFixnumA0S
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        internal PCondLessThanFixnumA0SXQ (PrimitiveLessThanFixnumA0S predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0SXQ");
#endif
            object ev0 = environment.Argument0Value;
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondLessThanFixnumA0SXQ";
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

    [Serializable]
    class PCondLessThanFixnumA1 : PCondLessThanFixnumA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA1 (PrimitiveLessThanFixnumA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveLessThanFixnumA1 predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondLessThanFixnumA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumA1";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumA1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument1Value;

            if ((int) ev0 < (int) ev1) {
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
    class PCondLessThanFixnumS : PCondLessThanFixnum
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected PCondLessThanFixnumS (PrimitiveLessThanFixnumS predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Name = predicate.rand0Name;
            this.rand0Offset = predicate.rand0Offset;
        }

        public static SCode Make (PrimitiveLessThanFixnumS predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumSQ) ? PCondLessThanFixnumSQ.Make ((PrimitiveLessThanFixnumSQ) predicate, consequent, alternative) :
                (consequent is Quotation) ? PCondLessThanFixnumSXQ.Make (predicate, (Quotation) consequent, alternative) :
                new PCondLessThanFixnumS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumS";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
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
    class PCondLessThanFixnumSQ : PCondLessThanFixnumS
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PCondLessThanFixnumSQ (PrimitiveLessThanFixnumSQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = (int) predicate.rand1Value;
        }

        public static SCode Make (PrimitiveLessThanFixnumSQ predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondLessThanFixnumSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondLessThanFixnumSQ";
#endif


            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < this.rand1Value) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondLessThanFixnumSQ";
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
                SCode.location = "PCondLessThanFixnumSQ";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondLessThanFixnumSXQ : PCondLessThanFixnumS
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondLessThanFixnumSXQ (PrimitiveLessThanFixnumS predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumS predicate, Quotation consequent, SCode alternative)
        {
            return
                new PCondLessThanFixnumSXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondLessThanFixnumSXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumSXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
                answer = this.consequentValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondLessThanFixnumSXQ";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondLessThanFixnumXQ : PCondLessThanFixnum
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PCondLessThanFixnumXQ (PrimitiveLessThanFixnumXQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveLessThanFixnumXQ predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Quotation) ? PCondLessThanFixnumXQQ.Make (predicate, (Quotation) consequent, alternative) :
                new PCondLessThanFixnumXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondLessThanFixnumXQ";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if ((int) ev0 < this.rand1Value) {
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
    class PCondLessThanFixnumXQQ : PCondLessThanFixnumXQ
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondLessThanFixnumXQQ (PrimitiveLessThanFixnumXQ predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumXQ predicate, Quotation consequent, SCode alternative)
        {
            return
                new PCondLessThanFixnumXQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondLessThanFixnumXQQ";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if ((int) ev0 < this.rand1Value) {
                answer = this.consequentValue;
                return false;
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

#if NIL
    class PCondLessThanFixnumL : PCondLessThanFixnum
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PCondLessThanFixnumL (PrimitiveLessThanFixnumL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Name = predicate.rand0Name;
            this.rand0Depth = predicate.rand0Depth;
            this.rand0Offset = predicate.rand0Offset;
        }

        public static SCode Make (PrimitiveLessThanFixnumL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumA) ? PCondLessThanFixnumA.Make ((PrimitiveLessThanFixnumA) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumL1) ? PCondLessThanFixnumL1.Make ((PrimitiveLessThanFixnumL1) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumLL) ? PCondLessThanFixnumLL.Make ((PrimitiveLessThanFixnumLL) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumLQ) ? PCondLessThanFixnumLQ.Make ((PrimitiveLessThanFixnumLQ) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? Unimplemented():
                (consequent is Quotation) ? PCondLessThanFixnumLSQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCondLessThanFixnumLSSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? Unimplemented() :
                new PCondLessThanFixnumL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA : PCondLessThanFixnumL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA (PrimitiveLessThanFixnumA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumA0) ? PCondLessThanFixnumA0.Make ((PrimitiveLessThanFixnumA0) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumA1) ? PCondLessThanFixnumA1.Make ((PrimitiveLessThanFixnumA1) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumAL) ? PCondLessThanFixnumAL.Make ((PrimitiveLessThanFixnumAL) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumAQ) ? Unimplemented () :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0 : PCondLessThanFixnumA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA0 (PrimitiveLessThanFixnumA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumA0L) ? PCondLessThanFixnumA0L.Make ((PrimitiveLessThanFixnumA0L) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumA0Q) ? PCondLessThanFixnumA0Q.Make ((PrimitiveLessThanFixnumA0Q) predicate, consequent, alternative) :
                //(consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? PCondLessThanFixnumA0SQ.Make (predicate, (Quotation) consequent, alternative) :
                //(alternative is LexicalVariable) ? Unimplemented () :
                //(alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumA0.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumA0.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            if ((int) environment.Argument0Value < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0L : PCondLessThanFixnumA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondLessThanFixnumA0L (PrimitiveLessThanFixnumA0L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveLessThanFixnumA0L predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumA0A) ? PCondLessThanFixnumA0A.Make ((PrimitiveLessThanFixnumA0A) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumA0L1) ? PCondLessThanFixnumA0L1.Make ((PrimitiveLessThanFixnumA0L1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCondLessThanFixnumA0LL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondLessThanFixnumA0LQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCondLessThanFixnumA0LSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondLessThanFixnumA0LSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0A : PCondLessThanFixnumA0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA0A (PrimitiveLessThanFixnumA0A predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0A predicate, SCode consequent, SCode alternative)
        {
            return
                //(predicate is PrimitiveLessThanFixnumA0A0) ? Unimplemented() :
                (predicate is PrimitiveLessThanFixnumA0A1) ? PCondLessThanFixnumA0A1.Make ((PrimitiveLessThanFixnumA0A1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented():
                (alternative is Quotation) ? Unimplemented():
                new PCondLessThanFixnumA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0A1 : PCondLessThanFixnumA0A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA0A1 (PrimitiveLessThanFixnumA0A1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0A1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondLessThanFixnumA0A1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondLessThanFixnumA0A1Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondLessThanFixnumA0A1SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA0L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0A1L : PCondLessThanFixnumA0A1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA0A1L (PrimitiveLessThanFixnumA0A1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0A1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondLessThanFixnumA0A1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA0A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA0L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0A1A : PCondLessThanFixnumA0A1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA0A1A (PrimitiveLessThanFixnumA0A1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0A1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondLessThanFixnumA0A1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA0A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA0L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0A1A0 : PCondLessThanFixnumA0A1A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA0A1A0 (PrimitiveLessThanFixnumA0A1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0A1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondLessThanFixnumA0A1A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA0A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA0L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0A1A0L : PCondLessThanFixnumA0A1A0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA0A1A0L (PrimitiveLessThanFixnumA0A1 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0A1 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondLessThanFixnumA0A1A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? Unimplemented () :
                new PCondLessThanFixnumA0A1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA0L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0A1A0A : PCondLessThanFixnumA0A1A0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA0A1A0A (PrimitiveLessThanFixnumA0A1 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0A1 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented() :
                (alternative is Argument1) ? PCondLessThanFixnumA0A1A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCondLessThanFixnumA0A1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA0L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    sealed class PCondLessThanFixnumA0A1A0A1 : PCondLessThanFixnumA0A1A0A
    {
        PCondLessThanFixnumA0A1A0A1 (PrimitiveLessThanFixnumA0A1 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0A1 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCondLessThanFixnumA0A1A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0A1A0A1");
#endif
            object ev1 = environment.Argument1Value;
            object ev0 = environment.Argument0Value;

            answer = ((int) ev0 < (int) ev1) ? ev0 : ev1;
            return false;
        }
    }

    class PCondLessThanFixnumA0A1Q : PCondLessThanFixnumA0A1
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondLessThanFixnumA0A1Q (PrimitiveLessThanFixnumA0A1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumA0A1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA0A1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0A1Q");
#endif
            if ((int) environment.Argument0Value < (int) environment.Argument1Value) {
                answer = this.consequentValue;
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

    sealed class PCondLessThanFixnumA0A1SQ : PCondLessThanFixnumA0A1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        PCondLessThanFixnumA0A1SQ (PrimitiveLessThanFixnumA0A1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumA0A1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondLessThanFixnumA0A1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0A1SQ");
#endif
            if ((int) environment.Argument0Value < (int) environment.Argument1Value) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
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

    class PCondLessThanFixnumA0L1 : PCondLessThanFixnumA0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA0L1 (PrimitiveLessThanFixnumA0L1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0L1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondLessThanFixnumA0L1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented() :
                (alternative is LexicalVariable) ? Unimplemented() :
                (alternative is Quotation) ? PCondLessThanFixnumA0L1SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0L1L : PCondLessThanFixnumA0L1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA0L1L (PrimitiveLessThanFixnumA0L1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0L1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondLessThanFixnumA0L1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented() :
                new PCondLessThanFixnumA0L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA0L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0L1A : PCondLessThanFixnumA0L1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA0L1A (PrimitiveLessThanFixnumA0L1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0L1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondLessThanFixnumA0L1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented() :
                new PCondLessThanFixnumA0L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA0L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0L1A0 : PCondLessThanFixnumA0L1A
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA0L1A0 (PrimitiveLessThanFixnumA0L1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0L1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA0L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0L1A0");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
                answer = ev0;
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



    sealed class PCondLessThanFixnumA0L1SQ : PCondLessThanFixnumA0L1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        PCondLessThanFixnumA0L1SQ (PrimitiveLessThanFixnumA0L1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0L1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondLessThanFixnumA0L1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondLessThanFixnumA0L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0LL : PCondLessThanFixnumA0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif


        protected PCondLessThanFixnumA0LL (PrimitiveLessThanFixnumA0L predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0L predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondLessThanFixnumA0LA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented() :
                (alternative is LexicalVariable) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented() :
                new PCondLessThanFixnumA0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA0L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0LA : PCondLessThanFixnumA0LL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif


        protected PCondLessThanFixnumA0LA (PrimitiveLessThanFixnumA0L predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0L predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondLessThanFixnumA0LA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA0LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA0L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0LA0 : PCondLessThanFixnumA0LA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondLessThanFixnumA0LA0 (PrimitiveLessThanFixnumA0L predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0L predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondLessThanFixnumA0LA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumA0LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0LA0");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
                answer = ev0;
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

    sealed class PCondLessThanFixnumA0LA0Q : PCondLessThanFixnumA0LA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif


        PCondLessThanFixnumA0LA0Q (PrimitiveLessThanFixnumA0L predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0L predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCondLessThanFixnumA0LA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA0L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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



    class PCondLessThanFixnumA0LQ : PCondLessThanFixnumA0L
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondLessThanFixnumA0LQ (PrimitiveLessThanFixnumA0L predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumA0L predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondLessThanFixnumA0LSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondLessThanFixnumA0LSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumA0LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0LQ");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            if ((int) environment.Argument0Value < (int) ev1) {
answer = this.consequentValue;
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



    class PCondLessThanFixnumA0LSL : PCondLessThanFixnumA0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondLessThanFixnumA0LSL (PrimitiveLessThanFixnumA0L predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveLessThanFixnumA0L predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondLessThanFixnumA0LSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? PCondLessThanFixnumA0LSL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCondLessThanFixnumA0LSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0LSL.EvalStep");

#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            if ((int) environment.Argument0Value < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false; ;
            }
        }
    }

    class PCondLessThanFixnumA0LSA : PCondLessThanFixnumA0LSL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA0LSA (PrimitiveLessThanFixnumA0L predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0L predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondLessThanFixnumA0LSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCondLessThanFixnumA0LSA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCondLessThanFixnumA0LSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    sealed class PCondLessThanFixnumA0LSA0 : PCondLessThanFixnumA0LSA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        PCondLessThanFixnumA0LSA0 (PrimitiveLessThanFixnumA0L predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0L predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondLessThanFixnumA0LSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0LSA0");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;
            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                answer = ev0;
                return false;
            }
        }
    }

    sealed class PCondLessThanFixnumA0LSA1 : PCondLessThanFixnumA0LSA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        PCondLessThanFixnumA0LSA1 (PrimitiveLessThanFixnumA0L predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0L predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCondLessThanFixnumA0LSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0LSA1");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            if ((int) environment.Argument0Value < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
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

    sealed class PCondLessThanFixnumA0LSL1 : PCondLessThanFixnumA0LSL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        PCondLessThanFixnumA0LSL1 (PrimitiveLessThanFixnumA0L predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0L predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return
                new PCondLessThanFixnumA0LSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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



    sealed class PCondLessThanFixnumA0LSQ : PCondLessThanFixnumA0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        PCondLessThanFixnumA0LSQ (PrimitiveLessThanFixnumA0L predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumA0L predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondLessThanFixnumA0LSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0LSQ.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException();

            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
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

    class PCondLessThanFixnumA0Q : PCondLessThanFixnumA0
    {
        public readonly int rand1Value;

        protected PCondLessThanFixnumA0Q (PrimitiveLessThanFixnumA0Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveLessThanFixnumA0Q predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondLessThanFixnumA0QL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondLessThanFixnumA0QQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondLessThanFixnumA0QSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0Q");
#endif
            expression = ((int) environment.Argument0Value < this.rand1Value) ? this.consequent : this.alternative;
#if DEBUG
            noteCalls((SCode)expression);
#endif
            answer = null;
            return true;
        }
    }

    class PCondLessThanFixnumA0QL : PCondLessThanFixnumA0Q
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA0QL (PrimitiveLessThanFixnumA0Q predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0Q predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondLessThanFixnumA0QA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented() :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA0QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0QA : PCondLessThanFixnumA0QL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA0QA (PrimitiveLessThanFixnumA0Q predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0Q predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondLessThanFixnumA0QA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA0QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0QA0 : PCondLessThanFixnumA0QA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA0QA0 (PrimitiveLessThanFixnumA0Q predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0Q predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondLessThanFixnumA0QA0Q.Make (predicate, consequent, (Quotation) alternative):
                new PCondLessThanFixnumA0QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA0QA0.EvalStep");
#endif
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < this.rand1Value) { 
                answer = ev0;
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

    sealed class PCondLessThanFixnumA0QA0Q : PCondLessThanFixnumA0QA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        PCondLessThanFixnumA0QA0Q (PrimitiveLessThanFixnumA0Q predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0Q predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCondLessThanFixnumA0QA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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



    class PCondLessThanFixnumA0QQ : PCondLessThanFixnumA0Q
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondLessThanFixnumA0QQ (PrimitiveLessThanFixnumA0Q predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumA0Q predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondLessThanFixnumA0QQ";
#endif
            if ((int) environment.Argument0Value < this.rand1Value) {
                answer = this.consequentValue;
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
    sealed class PCondLessThanFixnumA0QSQ : PCondLessThanFixnumA0Q
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        PCondLessThanFixnumA0QSQ (PrimitiveLessThanFixnumA0Q predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA0Q predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondLessThanFixnumA0QSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA0SQ : PCondLessThanFixnumA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondLessThanFixnumA0SQ (PrimitiveLessThanFixnumA0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumA0 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumA0SQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumA0SQ.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            if ((int) environment.Argument0Value < (int) ev1) {
                answer = this.consequentValue;
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



    class PCondLessThanFixnumA1 : PCondLessThanFixnumA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA1 (PrimitiveLessThanFixnumA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumA1L) ? PCondLessThanFixnumA1L.Make ((PrimitiveLessThanFixnumA1L) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumA1Q) ? PCondLessThanFixnumA1Q.Make ((PrimitiveLessThanFixnumA1Q) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? PCondLessThanFixnumA1SQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondLessThanFixnumA1SSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumA1.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumA1.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            if ((int) environment.Argument1Value < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1L : PCondLessThanFixnumA1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondLessThanFixnumA1L (PrimitiveLessThanFixnumA1L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveLessThanFixnumA1L predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumA1A) ? PCondLessThanFixnumA1A.Make ((PrimitiveLessThanFixnumA1A) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumA1L1) ? Unimplemented () :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? PCondLessThanFixnumA1LSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondLessThanFixnumA1LSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument1Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1A : PCondLessThanFixnumA1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA1A (PrimitiveLessThanFixnumA1A predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumA1A0) ? PCondLessThanFixnumA1A0.Make ((PrimitiveLessThanFixnumA1A0) predicate, consequent, alternative) :
                //(predicate is PrimitiveLessThanFixnumA1A0) ? Unimplemented() :
                (consequent is LexicalVariable) ? PCondLessThanFixnumA1AL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? PCondLessThanFixnumA1ASL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondLessThanFixnumA1ASQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA1A");
#endif
            if ((int) environment.Argument1Value < (int) environment.ArgumentValue(this.rand1Offset)) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1A0 : PCondLessThanFixnumA1A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA1A0 (PrimitiveLessThanFixnumA1A0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondLessThanFixnumA1A0L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? PCondLessThanFixnumA1A0SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondLessThanFixnumA1A0SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1A0L : PCondLessThanFixnumA1A0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA1A0L (PrimitiveLessThanFixnumA1A0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondLessThanFixnumA1A0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1A0A : PCondLessThanFixnumA1A0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA1A0A (PrimitiveLessThanFixnumA1A0 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A0 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondLessThanFixnumA1A0A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1A0A0 : PCondLessThanFixnumA1A0A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA1A0A0 (PrimitiveLessThanFixnumA1A0 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A0 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondLessThanFixnumA1A0A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA1A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1A0A0L : PCondLessThanFixnumA1A0A0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA1A0A0L (PrimitiveLessThanFixnumA1A0 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A0 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondLessThanFixnumA1A0A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? Unimplemented () :
                new PCondLessThanFixnumA1A0A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1A0A0A : PCondLessThanFixnumA1A0A0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA1A0A0A (PrimitiveLessThanFixnumA1A0 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A0 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented () :
                (alternative is Argument1) ? PCondLessThanFixnumA1A0A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCondLessThanFixnumA1A0A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    sealed class PCondLessThanFixnumA1A0A0A1 : PCondLessThanFixnumA1A0A0A
    {

        PCondLessThanFixnumA1A0A0A1 (PrimitiveLessThanFixnumA1A0 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A0 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCondLessThanFixnumA1A0A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);

#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1A0SL : PCondLessThanFixnumA1A0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA1A0SL (PrimitiveLessThanFixnumA1A0 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A0 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondLessThanFixnumA1A0SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? Unimplemented():
                new PCondLessThanFixnumA1A0SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1A0SA : PCondLessThanFixnumA1A0SL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondLessThanFixnumA1A0SA (PrimitiveLessThanFixnumA1A0 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A0 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondLessThanFixnumA1A0SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCondLessThanFixnumA1A0SA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCondLessThanFixnumA1A0SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1A0SA0 : PCondLessThanFixnumA1A0SA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        PCondLessThanFixnumA1A0SA0 (PrimitiveLessThanFixnumA1A0 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A0 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondLessThanFixnumA1A0SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    sealed class PCondLessThanFixnumA1A0SA1 : PCondLessThanFixnumA1A0SA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        PCondLessThanFixnumA1A0SA1 (PrimitiveLessThanFixnumA1A0 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A0 predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCondLessThanFixnumA1A0SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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



    sealed class PCondLessThanFixnumA1A0SQ : PCondLessThanFixnumA1A0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        PCondLessThanFixnumA1A0SQ (PrimitiveLessThanFixnumA1A0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A0 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondLessThanFixnumA1A0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1AL : PCondLessThanFixnumA1A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA1AL (PrimitiveLessThanFixnumA1A predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondLessThanFixnumA1AA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondLessThanFixnumA1ASQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumA1AL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1AA : PCondLessThanFixnumA1AL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA1AA (PrimitiveLessThanFixnumA1A predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondLessThanFixnumA1AA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCondLessThanFixnumA1AA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ?Unimplemented() :
                new PCondLessThanFixnumA1AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1AA0 : PCondLessThanFixnumA1AA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA1AA0 (PrimitiveLessThanFixnumA1A predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA1AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1AA1 : PCondLessThanFixnumA1AA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA1AA1 (PrimitiveLessThanFixnumA1A predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA1AA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1ASL : PCondLessThanFixnumA1A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;
        protected PCondLessThanFixnumA1ASL (PrimitiveLessThanFixnumA1A predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondLessThanFixnumA1ASA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? Unimplemented():
                new PCondLessThanFixnumA1ASL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1ASA : PCondLessThanFixnumA1ASL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA1ASA (PrimitiveLessThanFixnumA1A predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented () :
                (alternative is Argument1) ? Unimplemented () :
                new PCondLessThanFixnumA1ASA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA1ASA");
#endif
            if ((int) environment.Argument1Value < (int) environment.ArgumentValue (this.rand1Offset)) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
        }
    }



    sealed class PCondLessThanFixnumA1ASQ : PCondLessThanFixnumA1A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        PCondLessThanFixnumA1ASQ (PrimitiveLessThanFixnumA1A predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumA1A predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondLessThanFixnumA1ASQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA1ASQ");
#endif
            if ((int) environment.Argument1Value < (int) environment.ArgumentValue (this.rand1Offset)) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
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

    class PCondLessThanFixnumA1LSL : PCondLessThanFixnumA1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondLessThanFixnumA1LSL (PrimitiveLessThanFixnumA1L predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveLessThanFixnumA1L predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondLessThanFixnumA1LSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? Unimplemented () :
                new PCondLessThanFixnumA1LSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1LSA : PCondLessThanFixnumA1LSL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA1LSA (PrimitiveLessThanFixnumA1L predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1L predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented () :
                (alternative is Argument1) ? PCondLessThanFixnumA1LSA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCondLessThanFixnumA1LSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA1LSA");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            if ((int) environment.Argument1Value < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
        }
    }

    sealed class PCondLessThanFixnumA1LSA1 : PCondLessThanFixnumA1LSA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        PCondLessThanFixnumA1LSA1 (PrimitiveLessThanFixnumA1L predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1L predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCondLessThanFixnumA1LSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    sealed class PCondLessThanFixnumA1LSQ : PCondLessThanFixnumA1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        PCondLessThanFixnumA1LSQ (PrimitiveLessThanFixnumA1L predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumA1L predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondLessThanFixnumA1LSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA1LSQ");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            if ((int) environment.Argument1Value < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
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

    class PCondLessThanFixnumAL : PCondLessThanFixnumA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondLessThanFixnumAL (PrimitiveLessThanFixnumAL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveLessThanFixnumAL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumAA) ? PCondLessThanFixnumAA.Make ((PrimitiveLessThanFixnumAA) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumAL1) ? Unimplemented () :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumAA : PCondLessThanFixnumAL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumAA (PrimitiveLessThanFixnumAA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumAA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumAA0) ? Unimplemented() :
                (predicate is PrimitiveLessThanFixnumAA1) ? Unimplemented () :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondLessThanFixnumAASQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    sealed class PCondLessThanFixnumAASQ : PCondLessThanFixnumAA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        PCondLessThanFixnumAASQ (PrimitiveLessThanFixnumAA predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumAA predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondLessThanFixnumAASQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumAASQ");
#endif
            if ((int) environment.ArgumentValue (this.rand0Offset) < (int) environment.ArgumentValue (this.rand1Offset)) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
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




    class PCondLessThanFixnumA1Q : PCondLessThanFixnumA1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;
        protected PCondLessThanFixnumA1Q (PrimitiveLessThanFixnumA1Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveLessThanFixnumA1Q predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? PCondLessThanFixnumA1QQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA1Q.EvalStep");
#endif

            if ((int) environment.Argument1Value < this.rand1Value) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumA1QQ : PCondLessThanFixnumA1Q
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondLessThanFixnumA1QQ (PrimitiveLessThanFixnumA1Q predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumA1Q predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumA1QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumA1QQ");
#endif

            if ((int) environment.Argument1Value < this.rand1Value) {
                answer = this.consequentValue;
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

    class PCondLessThanFixnumA1SQ : PCondLessThanFixnumA1
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumA1SQ (PrimitiveLessThanFixnumA1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumA1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented() :
                new PCondLessThanFixnumA1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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



    sealed class PCondLessThanFixnumA1SSQ : PCondLessThanFixnumA1
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        PCondLessThanFixnumA1SSQ (PrimitiveLessThanFixnumA1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumA1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondLessThanFixnumA1SSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumA1SSQ.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumA1SSQ.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            if ((int) environment.Argument1Value < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
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

    class PCondLessThanFixnumL1 : PCondLessThanFixnumL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1 (PrimitiveLessThanFixnumL1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumL1L) ? PCondLessThanFixnumL1L.Make ((PrimitiveLessThanFixnumL1L) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumL1Q) ? PCondLessThanFixnumL1Q.Make ((PrimitiveLessThanFixnumL1Q) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondLessThanFixnumL1SSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1L : PCondLessThanFixnumL1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondLessThanFixnumL1L (PrimitiveLessThanFixnumL1L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveLessThanFixnumL1L predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumL1A) ? PCondLessThanFixnumL1A.Make ((PrimitiveLessThanFixnumL1A) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumL1L1) ? PCondLessThanFixnumL1L1.Make ((PrimitiveLessThanFixnumL1L1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? PCondLessThanFixnumL1LSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondLessThanFixnumL1LSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1A : PCondLessThanFixnumL1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1A (PrimitiveLessThanFixnumL1A predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1A predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumL1A0) ? PCondLessThanFixnumL1A0.Make ((PrimitiveLessThanFixnumL1A0) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumL1A0) ? Unimplemented () :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1A0 : PCondLessThanFixnumL1A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1A0 (PrimitiveLessThanFixnumL1A0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1A0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondLessThanFixnumL1A0L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondLessThanFixnumL1A0Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondLessThanFixnumL1A0SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1A0L : PCondLessThanFixnumL1A0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1A0L (PrimitiveLessThanFixnumL1A0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1A0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondLessThanFixnumL1A0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumL1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1A0A : PCondLessThanFixnumL1A0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1A0A (PrimitiveLessThanFixnumL1A0 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1A0 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondLessThanFixnumL1A0A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumL1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1A0A0 : PCondLessThanFixnumL1A0A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1A0A0 (PrimitiveLessThanFixnumL1A0 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1A0 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondLessThanFixnumL1A0A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumL1A0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1A0A0L : PCondLessThanFixnumL1A0A0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1A0A0L (PrimitiveLessThanFixnumL1A0 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1A0 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondLessThanFixnumL1A0A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? Unimplemented () :
                new PCondLessThanFixnumL1A0A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1A0A0A : PCondLessThanFixnumL1A0A0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1A0A0A (PrimitiveLessThanFixnumL1A0 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1A0 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented () :
                (alternative is Argument1) ? PCondLessThanFixnumL1A0A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCondLessThanFixnumL1A0A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    sealed class PCondLessThanFixnumL1A0A0A1 : PCondLessThanFixnumL1A0A0A
    {

        PCondLessThanFixnumL1A0A0A1 (PrimitiveLessThanFixnumL1A0 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1A0 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCondLessThanFixnumL1A0A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);

#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1A0Q : PCondLessThanFixnumL1A0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondLessThanFixnumL1A0Q (PrimitiveLessThanFixnumL1A0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumL1A0 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondLessThanFixnumL1A0QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? Unimplemented() :
                new PCondLessThanFixnumL1A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);

#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1A0QL : PCondLessThanFixnumL1A0Q
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1A0QL (PrimitiveLessThanFixnumL1A0 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1A0 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondLessThanFixnumL1A0QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? Unimplemented () :
                new PCondLessThanFixnumL1A0QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1A0QA : PCondLessThanFixnumL1A0QL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1A0QA(PrimitiveLessThanFixnumL1A0 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1A0 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondLessThanFixnumL1A0QA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? Unimplemented () :
                new PCondLessThanFixnumL1A0QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    sealed class PCondLessThanFixnumL1A0QA0 : PCondLessThanFixnumL1A0QA
    {
        PCondLessThanFixnumL1A0QA0 (PrimitiveLessThanFixnumL1A0 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1A0 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCondLessThanFixnumL1A0QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumL1A0QA0");
#endif
            object ev1 = environment.Argument0Value;  
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((int) ev0 < (int) ev1) ? this.consequentValue : ev1;
            return false;
        }
    }

    sealed class PCondLessThanFixnumL1A0SQ : PCondLessThanFixnumL1A0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        PCondLessThanFixnumL1A0SQ (PrimitiveLessThanFixnumL1A0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1A0 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondLessThanFixnumL1A0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1L1 : PCondLessThanFixnumL1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1L1 (PrimitiveLessThanFixnumL1L1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1L1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondLessThanFixnumL1L1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? PCondLessThanFixnumL1L1SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondLessThanFixnumL1L1SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumL1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1L1L : PCondLessThanFixnumL1L1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1L1L (PrimitiveLessThanFixnumL1L1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1L1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondLessThanFixnumL1L1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumL1L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1L1A : PCondLessThanFixnumL1L1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1L1A (PrimitiveLessThanFixnumL1L1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1L1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondLessThanFixnumL1L1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumL1L1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1L1A0 : PCondLessThanFixnumL1L1A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1L1A0 (PrimitiveLessThanFixnumL1L1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1L1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondLessThanFixnumL1L1A0L.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumL1L1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1L1A0L : PCondLessThanFixnumL1L1A0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1L1A0L (PrimitiveLessThanFixnumL1L1 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1L1 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondLessThanFixnumL1L1A0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? Unimplemented () :
                new PCondLessThanFixnumL1L1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1L1A0A : PCondLessThanFixnumL1L1A0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1L1A0A (PrimitiveLessThanFixnumL1L1 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1L1 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented () :
                (alternative is Argument1) ? PCondLessThanFixnumL1L1A0A1.Make (predicate, consequent, (Argument1) alternative) :
                new PCondLessThanFixnumL1L1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    sealed class PCondLessThanFixnumL1L1A0A1 : PCondLessThanFixnumL1L1A0A
    {

        PCondLessThanFixnumL1L1A0A1 (PrimitiveLessThanFixnumL1L1 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1L1 predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new PCondLessThanFixnumL1L1A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);

#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1L1SL : PCondLessThanFixnumL1L1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondLessThanFixnumL1L1SL (PrimitiveLessThanFixnumL1L1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveLessThanFixnumL1L1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? Unimplemented() :
                (alternative is LexicalVariable1) ? PCondLessThanFixnumL1L1SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
                new PCondLessThanFixnumL1L1SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    sealed class PCondLessThanFixnumL1L1SL1 : PCondLessThanFixnumL1L1SL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCondLessThanFixnumL1L1SL1 (PrimitiveLessThanFixnumL1L1 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1L1 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return
                new PCondLessThanFixnumL1L1SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumL1L1SL1");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
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


    sealed class PCondLessThanFixnumL1L1SQ : PCondLessThanFixnumL1L1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        PCondLessThanFixnumL1L1SQ (PrimitiveLessThanFixnumL1L1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1L1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondLessThanFixnumL1L1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondLessThanFixnumL1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1LSL : PCondLessThanFixnumL1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1LSL (PrimitiveLessThanFixnumL1L predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1L predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondLessThanFixnumL1LSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? Unimplemented () :
                new PCondLessThanFixnumL1LSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1LSA : PCondLessThanFixnumL1LSL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1LSA (PrimitiveLessThanFixnumL1L predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1L predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented () :
                (alternative is Argument1) ? PCondLessThanFixnumL1LSA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCondLessThanFixnumL1LSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    sealed class PCondLessThanFixnumL1LSA1 : PCondLessThanFixnumL1LSA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        PCondLessThanFixnumL1LSA1 (PrimitiveLessThanFixnumL1L predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1L predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCondLessThanFixnumL1LSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    sealed class PCondLessThanFixnumL1LSQ : PCondLessThanFixnumL1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        PCondLessThanFixnumL1LSQ (PrimitiveLessThanFixnumL1L predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1L predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondLessThanFixnumL1LSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1Q : PCondLessThanFixnumL1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;
        protected PCondLessThanFixnumL1Q (PrimitiveLessThanFixnumL1Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveLessThanFixnumL1Q predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? PCondLessThanFixnumL1QQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumL1Q");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < this.rand1Value) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumL1QQ : PCondLessThanFixnumL1Q
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumL1QQ (PrimitiveLessThanFixnumL1Q predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL1Q predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumL1QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    sealed class PCondLessThanFixnumL1SSQ : PCondLessThanFixnumL1
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        PCondLessThanFixnumL1SSQ (PrimitiveLessThanFixnumL1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumL1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondLessThanFixnumL1SSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumL1SSQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL1SSQ.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
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

    class PCondLessThanFixnumLL : PCondLessThanFixnumL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondLessThanFixnumLL (PrimitiveLessThanFixnumLL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveLessThanFixnumLL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumLA) ? PCondLessThanFixnumLA.Make ((PrimitiveLessThanFixnumLA) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumLL1) ? Unimplemented () :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondLessThanFixnumLLSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondLessThanFixnumLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumLA : PCondLessThanFixnumLL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumLA (PrimitiveLessThanFixnumLA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumLA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveLessThanFixnumLA0) ? PCondLessThanFixnumLA0.Make ((PrimitiveLessThanFixnumLA0) predicate, consequent, alternative) :
                (predicate is PrimitiveLessThanFixnumLA1) ? Unimplemented () :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumLA0 : PCondLessThanFixnumLA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumLA0 (PrimitiveLessThanFixnumLA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumLA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    sealed class PCondLessThanFixnumLLSQ : PCondLessThanFixnumLL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        PCondLessThanFixnumLLSQ (PrimitiveLessThanFixnumLL predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumLL predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondLessThanFixnumLLSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumLLSQ.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
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

    class PCondLessThanFixnumLQ : PCondLessThanFixnumL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;
        protected PCondLessThanFixnumLQ (PrimitiveLessThanFixnumLQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveLessThanFixnumLQ predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? PCondLessThanFixnumLQQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondLessThanFixnumLQ.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < this.rand1Value) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumLQQ : PCondLessThanFixnumLQ
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumLQQ (PrimitiveLessThanFixnumLQ predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumLQ predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumLQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumLSQ : PCondLessThanFixnumL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondLessThanFixnumLSQ (PrimitiveLessThanFixnumL predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveLessThanFixnumL predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondLessThanFixnumLSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumLSQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumLSQ.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
                answer = this.consequentValue;
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

    class PCondLessThanFixnumLSSL : PCondLessThanFixnumL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumLSSL (PrimitiveLessThanFixnumL predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondLessThanFixnumLSSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? Unimplemented () :
                new PCondLessThanFixnumLSSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumLSSA : PCondLessThanFixnumLSSL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumLSSA (PrimitiveLessThanFixnumL predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondLessThanFixnumLSSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? Unimplemented () :
                new PCondLessThanFixnumLSSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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

    class PCondLessThanFixnumLSSA0 : PCondLessThanFixnumLSSA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondLessThanFixnumLSSA0 (PrimitiveLessThanFixnumL predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveLessThanFixnumL predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondLessThanFixnumLSSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondLessThanFixnumL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 < (int) ev1) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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




    class PCondLessThanFixnumSQ : PCondLessThanFixnum
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PCondLessThanFixnumSQ (PrimitiveLessThanFixnumSQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveLessThanFixnumSQ predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondLessThanFixnumSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondLessThanFixnumSQ.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondLessThanFixnumSQ.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            if ((int) ev0 < this.rand1Value) {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
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
#endif

}
