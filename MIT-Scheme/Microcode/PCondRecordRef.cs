using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PCondRecordRef : PCond2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondRecordRef (PrimitiveRecordRef predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveRecordRef predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveRecordRefA) ? PCondRecordRefA.Make ((PrimitiveRecordRefA) predicate, consequent, alternative) :
                (predicate is PrimitiveRecordRefS) ? PCondRecordRefS.Make ((PrimitiveRecordRefS) predicate, consequent, alternative) :
                (predicate is PrimitiveRecordRefXQ) ? PCondRecordRefXQ.Make ((PrimitiveRecordRefXQ) predicate, consequent, alternative) :
                new PCondRecordRef (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondRecordRef";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondRecordRef";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondRecordRef";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object result = ((Record) ev0).Ref ((int) ev1);

            if (result is Boolean && (((bool) result) == false)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondRecordRef";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            } else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondRecordRef";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondRecordRefA : PCondRecordRef
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;

        protected PCondRecordRefA (PrimitiveRecordRefA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Offset = predicate.rand0Offset;
        }

        public static SCode Make (PrimitiveRecordRefA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveRecordRefA0) ? PCondRecordRefA0.Make ((PrimitiveRecordRefA0) predicate, consequent, alternative) :
                new PCondRecordRefA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondRecordRefA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondRecordRefA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            object result = ((Record) ev0).Ref ((int) ev1);

            if (result is Boolean && (((bool) result) == false)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondRecordRefA";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondRecordRefA";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondRecordRefA0 : PCondRecordRefA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondRecordRefA0 (PrimitiveRecordRefA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveRecordRefA0 predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondRecordRefA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondRecordRefA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondRecordRefA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

            object result = ((Record) ev0).Ref ((int) ev1);

            if (result is Boolean && (((bool) result) == false)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondRecordRefA0";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondRecordRefA0";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondRecordRefS : PCondRecordRef
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected PCondRecordRefS (PrimitiveRecordRefS predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Name = predicate.rand0Name;
            this.rand0Offset = predicate.rand0Offset;
        }

        public static SCode Make (PrimitiveRecordRefS predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveRecordRefSQ) ? PCondRecordRefSQ.Make ((PrimitiveRecordRefSQ) predicate, consequent, alternative) :
                new PCondRecordRefS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondRecordRefS";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondRecordRefS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object result = ((Record) ev0).Ref ((int) ev1);

            if (result is Boolean && (((bool) result) == false)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondRecordRefS";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondRecordRefS";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondRecordRefSQ : PCondRecordRefS
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PCondRecordRefSQ (PrimitiveRecordRefSQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.offset;
        }

        public static SCode Make (PrimitiveRecordRefSQ predicate, SCode consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCondRecordRefSQXA.Make (predicate, consequent, (Argument) alternative) :
                new PCondRecordRefSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondRecordRefSQ");
#endif

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object result = ((Record) ev0).Ref (this.rand1Value);

            if (result is Boolean && (((bool) result) == false)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondRecordRefSQ";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondRecordRefSQ";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondRecordRefSQXA : PCondRecordRefSQ
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;

        protected PCondRecordRefSQXA (PrimitiveRecordRefSQ predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveRecordRefSQ predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? new PCondRecordRefSQXA0 (predicate, consequent, (Argument0) alternative) :
                new PCondRecordRefSQXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondRecordRefSQXA");
#endif

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object result = ((Record) ev0).Ref (this.rand1Value);

            if (result is Boolean && (((bool) result) == false)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondRecordRefSQXA";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    sealed class PCondRecordRefSQXA0 : PCondRecordRefSQXA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        internal PCondRecordRefSQXA0 (PrimitiveRecordRefSQ predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondRecordRefSQXA0");
#endif

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object result = ((Record) ev0).Ref (this.rand1Value);

            if (result is Boolean && (((bool) result) == false)) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondRecordRefSQXA0";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondRecordRefXQ : PCondRecordRef
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;
        protected PCondRecordRefXQ (PrimitiveRecordRefXQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.offset;
        }

        public static SCode Make (PrimitiveRecordRefXQ predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Quotation) ? PCondRecordRefXQQ.Make (predicate, (Quotation) consequent, alternative) :
                new PCondRecordRefXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondRecordRefXQ";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondRecordRefXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object result = ((Record) ev0).Ref (this.rand1Value);

            if (result is Boolean && (((bool) result) == false)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondRecordRefXQ";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondRecordRefXQ";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondRecordRefXQQ : PCondRecordRefXQ
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondRecordRefXQQ (PrimitiveRecordRefXQ predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveRecordRefXQ predicate, Quotation consequent, SCode alternative)
        {
            return
                new PCondRecordRefXQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondRecordRefXQQ";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondRecordRefXQQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object result = ((Record) ev0).Ref (this.rand1Value);

            if (result is Boolean && (((bool) result) == false)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondRecordRefXQQ";
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

}
