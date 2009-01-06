using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;


namespace Microcode
{
    [Serializable]
    class PCondIsFixnumEqual : PCond2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqual (PrimitiveIsFixnumEqual predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqual predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsFixnumEqualL) ? PCondIsFixnumEqualL.Make ((PrimitiveIsFixnumEqualL) predicate, consequent, alternative) :
                //(predicate is PrimitiveIsFixnumEqualQ) ? Unimplemented() :
                //(predicate is PrimitiveIsFixnumEqualSL) ? Unimplemented() :
                (predicate is PrimitiveIsFixnumEqualSQ) ? PCondIsFixnumEqualSQ.Make ((PrimitiveIsFixnumEqualSQ) predicate, consequent, alternative) :
                new PCondIsFixnumEqual (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            noteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsFixnumEqual";
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

            if ((int) ev0==(int) ev1) {
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

    class PCondIsFixnumEqualL : PCondIsFixnumEqual
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PCondIsFixnumEqualL (PrimitiveIsFixnumEqualL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Name = predicate.rand0Name;
            this.rand0Depth = predicate.rand0Depth;
            this.rand0Offset = predicate.rand0Offset;
        }

        public static SCode Make (PrimitiveIsFixnumEqualL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsFixnumEqualA) ? PCondIsFixnumEqualA.Make ((PrimitiveIsFixnumEqualA) predicate, consequent, alternative) :
                new PCondIsFixnumEqualL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0==(int) ev1) {
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

    class PCondIsFixnumEqualA : PCondIsFixnumEqualL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualA (PrimitiveIsFixnumEqualA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsFixnumEqualA0) ? PCondIsFixnumEqualA0.Make ((PrimitiveIsFixnumEqualA0) predicate, consequent, alternative) :
                (predicate is PrimitiveIsFixnumEqualA1) ? PCondIsFixnumEqualA1.Make ((PrimitiveIsFixnumEqualA1) predicate, consequent, alternative) :
                (predicate is PrimitiveIsFixnumEqualAL) ? PCondIsFixnumEqualAL.Make ((PrimitiveIsFixnumEqualAL) predicate, consequent, alternative) :
                (predicate is PrimitiveIsFixnumEqualAQ) ? Unimplemented () :
                (consequent is LexicalVariable) ? Unimplemented():
                (consequent is Quotation) ? Unimplemented():
                (alternative is LexicalVariable) ? Unimplemented():
                (alternative is Quotation) ? Unimplemented ():
                new PCondIsFixnumEqualA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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

    class PCondIsFixnumEqualA0 : PCondIsFixnumEqualA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualA0 (PrimitiveIsFixnumEqualA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsFixnumEqualA0L) ? PCondIsFixnumEqualA0L.Make ((PrimitiveIsFixnumEqualA0L) predicate, consequent, alternative) :
                (predicate is PrimitiveIsFixnumEqualA0Q) ? PCondIsFixnumEqualA0Q.Make ((PrimitiveIsFixnumEqualA0Q) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsFixnumEqualA0SSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsFixnumEqualA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsFixnumEqualA0.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualA0.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            if ((int) environment.Argument0Value == (int) ev1) {
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

    class PCondIsFixnumEqualA0L : PCondIsFixnumEqualA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsFixnumEqualA0L (PrimitiveIsFixnumEqualA0L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0L predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsFixnumEqualA0A) ? PCondIsFixnumEqualA0A.Make ((PrimitiveIsFixnumEqualA0A) predicate, consequent, alternative) :
                (predicate is PrimitiveIsFixnumEqualA0L1) ? PCondIsFixnumEqualA0L1.Make ((PrimitiveIsFixnumEqualA0L1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCondIsFixnumEqualA0LL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondIsFixnumEqualA0LQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsFixnumEqualA0LSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsFixnumEqualA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsFixnumEqualA0L";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            if ((int) environment.Argument0Value == (int) ev1) {
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

    class PCondIsFixnumEqualA0A : PCondIsFixnumEqualA0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualA0A (PrimitiveIsFixnumEqualA0A predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0A predicate, SCode consequent, SCode alternative)
        {
            return
                //(predicate is PrimitiveIsFixnumEqualA0A0) ? Unimplemented() :
                (predicate is PrimitiveIsFixnumEqualA0A1) ? PCondIsFixnumEqualA0A1.Make ((PrimitiveIsFixnumEqualA0A1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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

    class PCondIsFixnumEqualA0A1 : PCondIsFixnumEqualA0A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualA0A1 (PrimitiveIsFixnumEqualA0A1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0A1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsFixnumEqualA0A1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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

    class PCondIsFixnumEqualA0A1L : PCondIsFixnumEqualA0A1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualA0A1L (PrimitiveIsFixnumEqualA0A1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0A1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsFixnumEqualA0A1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualA0A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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

    class PCondIsFixnumEqualA0A1A : PCondIsFixnumEqualA0A1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualA0A1A (PrimitiveIsFixnumEqualA0A1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0A1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsFixnumEqualA0A1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualA0A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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

    class PCondIsFixnumEqualA0A1A0 : PCondIsFixnumEqualA0A1A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualA0A1A0 (PrimitiveIsFixnumEqualA0A1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0A1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualA0A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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



    class PCondIsFixnumEqualA0L1 : PCondIsFixnumEqualA0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualA0L1 (PrimitiveIsFixnumEqualA0L1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0L1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsFixnumEqualA0L1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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

    class PCondIsFixnumEqualA0L1L : PCondIsFixnumEqualA0L1
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsFixnumEqualA0L1L (PrimitiveIsFixnumEqualA0L1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0L1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? Unimplemented() :
                (consequent is LexicalVariable1) ? PCondIsFixnumEqualA0L1L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualA0L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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

    class PCondIsFixnumEqualA0L1L1 : PCondIsFixnumEqualA0L1L
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualA0L1L1 (PrimitiveIsFixnumEqualA0L1 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0L1 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualA0L1L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumEqualA0L1L1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            if ((int) environment.Argument0Value == (int) ev1) {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
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

    class PCondIsFixnumEqualA0LL : PCondIsFixnumEqualA0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualA0LL (PrimitiveIsFixnumEqualA0L predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0L predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsFixnumEqualA0LA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented() :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualA0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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

    class PCondIsFixnumEqualA0LA : PCondIsFixnumEqualA0LL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualA0LA (PrimitiveIsFixnumEqualA0L predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0L predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? Unimplemented():
                (consequent is Argument1) ? PCondIsFixnumEqualA0LA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualA0LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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

    class PCondIsFixnumEqualA0LA1 : PCondIsFixnumEqualA0LA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualA0LA1 (PrimitiveIsFixnumEqualA0L predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0L predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualA0LA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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


    class PCondIsFixnumEqualA0LQ : PCondIsFixnumEqualA0L
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsFixnumEqualA0LQ (PrimitiveIsFixnumEqualA0L predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0L predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualA0LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumEqualA0LQ");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            if ((int) environment.Argument0Value == (int) ev1) {
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

    sealed class PCondIsFixnumEqualA0LSQ : PCondIsFixnumEqualA0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        PCondIsFixnumEqualA0LSQ (PrimitiveIsFixnumEqualA0L predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0L predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsFixnumEqualA0LSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsFixnumEqualA0L";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            if ((int) environment.Argument0Value == (int) ev1) {
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



    class PCondIsFixnumEqualA0Q : PCondIsFixnumEqualA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PCondIsFixnumEqualA0Q (PrimitiveIsFixnumEqualA0Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0Q predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? PCondIsFixnumEqualA0QQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCondIsFixnumEqualA0QSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondIsFixnumEqualA0QSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsFixnumEqualA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumEqualA0Q.EvalStep");
#endif

            if ((int) environment.Argument0Value == this.rand1Value) {
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

    class PCondIsFixnumEqualA0QQ : PCondIsFixnumEqualA0Q
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsFixnumEqualA0QQ (PrimitiveIsFixnumEqualA0Q predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0Q predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualA0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsFixnumEqualA0Q.EvalStep");
#endif

            if ((int) environment.Argument0Value == this.rand1Value) {
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

    class PCondIsFixnumEqualA0QSL : PCondIsFixnumEqualA0Q
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsFixnumEqualA0QSL (PrimitiveIsFixnumEqualA0Q predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0Q predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondIsFixnumEqualA0QSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? Unimplemented():
                new PCondIsFixnumEqualA0QSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsFixnumEqualA0Q.EvalStep");
#endif

            if ((int) environment.Argument0Value == this.rand1Value) {
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

    class PCondIsFixnumEqualA0QSA : PCondIsFixnumEqualA0QSL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualA0QSA (PrimitiveIsFixnumEqualA0Q predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0Q predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsFixnumEqualA0QSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? Unimplemented () :
                new PCondIsFixnumEqualA0QSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsFixnumEqualA0Q.EvalStep");
#endif

            if ((int) environment.Argument0Value == this.rand1Value) {
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

    sealed class PCondIsFixnumEqualA0QSA0 : PCondIsFixnumEqualA0QSA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCondIsFixnumEqualA0QSA0 (PrimitiveIsFixnumEqualA0Q predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0Q predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsFixnumEqualA0QSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("PCondIsFixnumEqualA0Q.EvalStep");
#endif

            if ((int) environment.Argument0Value == this.rand1Value) {
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



    sealed class PCondIsFixnumEqualA0QSQ : PCondIsFixnumEqualA0Q
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();

#endif
        public readonly object alternativeValue;
        PCondIsFixnumEqualA0QSQ (PrimitiveIsFixnumEqualA0Q predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0Q predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsFixnumEqualA0QSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumEqualA0QSQ.EvalStep");
#endif
            if ((int) environment.Argument0Value == this.rand1Value) {
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

    sealed class PCondIsFixnumEqualA0SSQ : PCondIsFixnumEqualA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        PCondIsFixnumEqualA0SSQ (PrimitiveIsFixnumEqualA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsFixnumEqualA0 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsFixnumEqualA0SSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsFixnumEqualA0SSQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualA0SSQ.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            if ((int) environment.Argument0Value == (int) ev1) {
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

    class PCondIsFixnumEqualA1 : PCondIsFixnumEqualA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualA1 (PrimitiveIsFixnumEqualA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsFixnumEqualA1L) ? PCondIsFixnumEqualA1L.Make ((PrimitiveIsFixnumEqualA1L) predicate, consequent, alternative) :
                (predicate is PrimitiveIsFixnumEqualA1Q) ? PCondIsFixnumEqualA1Q.Make ((PrimitiveIsFixnumEqualA1Q) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsFixnumEqualA1SSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsFixnumEqualA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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

    class PCondIsFixnumEqualA1L : PCondIsFixnumEqualA1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsFixnumEqualA1L (PrimitiveIsFixnumEqualA1L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsFixnumEqualA1L predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsFixnumEqualA1A) ? PCondIsFixnumEqualA1A.Make ((PrimitiveIsFixnumEqualA1A) predicate, consequent, alternative) :
                (predicate is PrimitiveIsFixnumEqualA1L1) ? Unimplemented () :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsFixnumEqualA1LSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsFixnumEqualA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumEqualA1L");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            if ((int) environment.Argument1Value == (int) ev1) {
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

    class PCondIsFixnumEqualA1A : PCondIsFixnumEqualA1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualA1A (PrimitiveIsFixnumEqualA1A predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA1A predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsFixnumEqualA1A0) ? Unimplemented():
                (predicate is PrimitiveIsFixnumEqualA1A1) ? Unimplemented () :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? PCondIsFixnumEqualA1AQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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

    class PCondIsFixnumEqualA1AQ : PCondIsFixnumEqualA1A
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsFixnumEqualA1AQ (PrimitiveIsFixnumEqualA1A predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsFixnumEqualA1A predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualA1AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumA1AQ.EvalStep");
#endif
            if ((int) environment.Argument1Value == (int) environment.ArgumentValue (this.rand1Offset)) {
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

    sealed class PCondIsFixnumEqualA1LSQ : PCondIsFixnumEqualA1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        PCondIsFixnumEqualA1LSQ (PrimitiveIsFixnumEqualA1L predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA1L predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsFixnumEqualA1LSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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



    sealed class PCondIsFixnumEqualA1SSQ : PCondIsFixnumEqualA1
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        PCondIsFixnumEqualA1SSQ (PrimitiveIsFixnumEqualA1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualA1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsFixnumEqualA1SSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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

    class PCondIsFixnumEqualA1Q : PCondIsFixnumEqualA1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PCondIsFixnumEqualA1Q (PrimitiveIsFixnumEqualA1Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveIsFixnumEqualA1Q predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented():
                new PCondIsFixnumEqualA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumEqualA1Q.EvalStep");
#endif
            if ((int) environment.Argument1Value == this.rand1Value) {
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

    class PCondIsFixnumEqualAL : PCondIsFixnumEqualA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsFixnumEqualAL (PrimitiveIsFixnumEqualAL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsFixnumEqualAL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsFixnumEqualAA) ? PCondIsFixnumEqualAA.Make ((PrimitiveIsFixnumEqualAA) predicate, consequent, alternative) :
                (predicate is PrimitiveIsFixnumEqualAL1) ? Unimplemented():
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumEqualAL");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            if ((int) ev0 == (int) ev1) {
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

    class PCondIsFixnumEqualAA : PCondIsFixnumEqualAL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualAA (PrimitiveIsFixnumEqualAA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualAA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsFixnumEqualAA0) ? Unimplemented() :
                (predicate is PrimitiveIsFixnumEqualAA1) ? PCondIsFixnumEqualAA1.Make ((PrimitiveIsFixnumEqualAA1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? PCondIsFixnumEqualAAQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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

    class PCondIsFixnumEqualAA1 : PCondIsFixnumEqualAA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualAA1 (PrimitiveIsFixnumEqualAA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualAA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented():
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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


    class PCondIsFixnumEqualAAQ : PCondIsFixnumEqualAA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsFixnumEqualAAQ (PrimitiveIsFixnumEqualAA predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumEqualAA predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumEqualAAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsFixnumEqualL";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if ((int) ev0 == (int) ev1) {
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

    [Serializable]
    class PCondIsFixnumEqualSQ : PCondIsFixnumEqual
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PCondIsFixnumEqualSQ (PrimitiveIsFixnumEqualSQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveIsFixnumEqualSQ predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondIsFixnumEqualSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsFixnumEqualSQ";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsFixnumEqualL.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            if ((int) ev0==this.rand1Value) {
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

}

