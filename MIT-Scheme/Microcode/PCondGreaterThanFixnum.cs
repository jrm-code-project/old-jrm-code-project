using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    class PCondGreaterThanFixnum : PCond2
    {
        protected SCode rand0;
        protected SCode rand1;

        protected PCondGreaterThanFixnum (PrimitiveGreaterThanFixnum predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0 = predicate.Rand0;
            this.rand1 = predicate.Rand1;
        }

        static public SCode Make (PrimitiveGreaterThanFixnum predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumL) ? PCondGreaterThanFixnumL.Make ((PrimitiveGreaterThanFixnumL) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumQ) ? PCondGreaterThanFixnumQ.Make ((PrimitiveGreaterThanFixnumQ) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumSL) ? Unimplemented ()
                : (predicate is PrimitiveGreaterThanFixnumSQ) ? PCondGreaterThanFixnumSQ.Make ((PrimitiveGreaterThanFixnumSQ) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? PCondGreaterThanFixnumSSL.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondGreaterThanFixnumSSQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondGreaterThanFixnumSSSL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondGreaterThanFixnumSSSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnum (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
            noteCalls (this.rand1);
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            object ev0;

            unev = this.rand0;
            env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;

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

    class PCondGreaterThanFixnumL : PCondGreaterThanFixnum
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PCondGreaterThanFixnumL (PrimitiveGreaterThanFixnumL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Name = predicate.rand0Name;
            this.rand0Depth = predicate.rand0Depth;
            this.rand0Offset = predicate.rand0Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumA) ? PCondGreaterThanFixnumA.Make ((PrimitiveGreaterThanFixnumA) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumL1) ? PCondGreaterThanFixnumL1.Make ((PrimitiveGreaterThanFixnumL1) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumLL) ? PCondGreaterThanFixnumLL.Make ((PrimitiveGreaterThanFixnumLL) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumLQ) ? Unimplemented ()
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented()
                : new PCondGreaterThanFixnumL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA : PCondGreaterThanFixnumL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA (PrimitiveGreaterThanFixnumA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumA0) ? PCondGreaterThanFixnumA0.Make ((PrimitiveGreaterThanFixnumA0) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumA1) ? PCondGreaterThanFixnumA1.Make ((PrimitiveGreaterThanFixnumA1) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumAL) ? PCondGreaterThanFixnumAL.Make (
                (PrimitiveGreaterThanFixnumAL) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumAQ) ? Unimplemented ()
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? PCondGreaterThanFixnumASSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

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
                (predicate is PrimitiveGreaterThanFixnumA0L) ? PCondGreaterThanFixnumA0L.Make ((PrimitiveGreaterThanFixnumA0L) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumA0Q) ? PCondGreaterThanFixnumA0Q.Make ((PrimitiveGreaterThanFixnumA0Q) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? PCondGreaterThanFixnumA0SQ.Make (predicate, (Quotation) consequent,  alternative)
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? PCondGreaterThanFixnumA0SSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA0L : PCondGreaterThanFixnumA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA0L (PrimitiveGreaterThanFixnumA0L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0L predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumA0A) ? PCondGreaterThanFixnumA0A.Make ((PrimitiveGreaterThanFixnumA0A) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumA0L1) ? Unimplemented ()
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? PCondGreaterThanFixnumA0LQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? PCondGreaterThanFixnumA0LSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA0A : PCondGreaterThanFixnumA0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA0A (PrimitiveGreaterThanFixnumA0A predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0A predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumA0A1) ? PCondGreaterThanFixnumA0A1.Make ((PrimitiveGreaterThanFixnumA0A1) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA0A1 : PCondGreaterThanFixnumA0A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA0A1 (PrimitiveGreaterThanFixnumA0A1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0A1 predicate, SCode consequent, SCode alternative)
        {
            return
                 (consequent is LexicalVariable) ? PCondGreaterThanFixnumA0A1L.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? PCondGreaterThanFixnumA0A1SQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA0A1L : PCondGreaterThanFixnumA0A1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondGreaterThanFixnumA0A1L (PrimitiveGreaterThanFixnumA0A1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0A1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                 (consequent is Argument) ? PCondGreaterThanFixnumA0A1A.Make (predicate, (Argument) consequent, alternative)
                : (consequent is LexicalVariable1) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumA0A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA0A1A : PCondGreaterThanFixnumA0A1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA0A1A (PrimitiveGreaterThanFixnumA0A1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0A1 predicate, Argument consequent, SCode alternative)
        {
            return
                 (consequent is Argument0) ? PCondGreaterThanFixnumA0A1A0.Make (predicate, (Argument0) consequent, alternative)
                : (consequent is Argument1) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumA0A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA0A1A0 : PCondGreaterThanFixnumA0A1A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA0A1A0 (PrimitiveGreaterThanFixnumA0A1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0A1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondGreaterThanFixnumA0A1A0L.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumA0A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA0A1A0L : PCondGreaterThanFixnumA0A1A0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondGreaterThanFixnumA0A1A0L (PrimitiveGreaterThanFixnumA0A1 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0A1 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondGreaterThanFixnumA0A1A0A.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? Unimplemented()
                : new PCondGreaterThanFixnumA0A1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA0A1A0A : PCondGreaterThanFixnumA0A1A0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA0A1A0A (PrimitiveGreaterThanFixnumA0A1 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0A1 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented ()
                : (alternative is Argument1) ? PCondGreaterThanFixnumA0A1A0A1.Make (predicate, consequent, (Argument1) alternative)
                : new PCondGreaterThanFixnumA0A1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA0A1A0A1 : PCondGreaterThanFixnumA0A1A0A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA0A1A0A1 (PrimitiveGreaterThanFixnumA0A1 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0A1 predicate, Argument0 consequent, Argument1 alternative)
        {
            return new PCondGreaterThanFixnumA0A1A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA0A1SQ : PCondGreaterThanFixnumA0A1
    {
        public readonly object alternativeValue;

        protected PCondGreaterThanFixnumA0A1SQ (PrimitiveGreaterThanFixnumA0A1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0A1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondGreaterThanFixnumA0A1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA0LSQ : PCondGreaterThanFixnumA0L
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object alternativeValue;

        protected PCondGreaterThanFixnumA0LSQ (PrimitiveGreaterThanFixnumA0L predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0L predicate, SCode consequent, Quotation alternative)
        {
            return new PCondGreaterThanFixnumA0LSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA0LQ : PCondGreaterThanFixnumA0L
    {
        public readonly object consequentValue;

        protected PCondGreaterThanFixnumA0LQ (PrimitiveGreaterThanFixnumA0L predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0L predicate, Quotation consequent, SCode alternative)
        {
            return
                 (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented()
                : new PCondGreaterThanFixnumA0LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA0SQ : PCondGreaterThanFixnumA0
    {
        protected PCondGreaterThanFixnumA0SQ (PrimitiveGreaterThanFixnumA0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented()
                : new PCondGreaterThanFixnumA0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA0Q : PCondGreaterThanFixnumA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PCondGreaterThanFixnumA0Q (PrimitiveGreaterThanFixnumA0Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0Q predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondGreaterThanFixnumA0QL.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondGreaterThanFixnumA0QQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondGreaterThanFixnumA0QSL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondGreaterThanFixnumA0QSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument1
            object ev1 = this.rand1Value;

            // Eval argument0
            object ev0 = environment.Argument0Value;

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;

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

    class PCondGreaterThanFixnumA0QL : PCondGreaterThanFixnumA0Q
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondGreaterThanFixnumA0QL (PrimitiveGreaterThanFixnumA0Q predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0Q predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondGreaterThanFixnumA0QA.Make (predicate, (Argument) consequent, alternative)
                : (consequent is LexicalVariable1) ? Unimplemented()
                : (alternative is LexicalVariable) ? Unimplemented () //PCondGreaterThanFixnumA0QLL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? Unimplemented() //PCondGreaterThanFixnumA0QLQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumA0QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

    }

    class PCondGreaterThanFixnumA0QA : PCondGreaterThanFixnumA0QL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA0QA (PrimitiveGreaterThanFixnumA0Q predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0Q predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondGreaterThanFixnumA0QA0.Make (predicate, (Argument0) consequent, alternative)
                : (consequent is Argument1) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented () //PCondGreaterThanFixnumA0QLL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? Unimplemented () //PCondGreaterThanFixnumA0QLQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumA0QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

    }

    class PCondGreaterThanFixnumA0QA0 : PCondGreaterThanFixnumA0QA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA0QA0 (PrimitiveGreaterThanFixnumA0Q predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0Q predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () //PCondGreaterThanFixnumA0QLL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? Unimplemented () //PCondGreaterThanFixnumA0QLQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumA0QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

    }



    class PCondGreaterThanFixnumA0QQ : PCondGreaterThanFixnumA0Q
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondGreaterThanFixnumA0QQ (PrimitiveGreaterThanFixnumA0Q predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0Q predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented()
                : (alternative is Quotation) ? PCondGreaterThanFixnumA0QQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumA0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

    }

    class PCondGreaterThanFixnumA0QQQ : PCondGreaterThanFixnumA0QQ
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondGreaterThanFixnumA0QQQ (PrimitiveGreaterThanFixnumA0Q predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0Q predicate, Quotation consequent, Quotation alternative)
        {
            return
                 new PCondGreaterThanFixnumA0QQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

    }



    class PCondGreaterThanFixnumA0QSL : PCondGreaterThanFixnumA0Q
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondGreaterThanFixnumA0QSL (PrimitiveGreaterThanFixnumA0Q predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0Q predicate, SCode consequent, LexicalVariable alternative)
        {
            return 
                (alternative is Argument) ? PCondGreaterThanFixnumA0QSA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? Unimplemented()
                : new PCondGreaterThanFixnumA0QSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA0QSA : PCondGreaterThanFixnumA0QSL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA0QSA (PrimitiveGreaterThanFixnumA0Q predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0Q predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondGreaterThanFixnumA0QSA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCondGreaterThanFixnumA0QSA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCondGreaterThanFixnumA0QSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA0QSA0 : PCondGreaterThanFixnumA0QSA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA0QSA0 (PrimitiveGreaterThanFixnumA0Q predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0Q predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondGreaterThanFixnumA0QSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA0QSA1 : PCondGreaterThanFixnumA0QSA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA0QSA1 (PrimitiveGreaterThanFixnumA0Q predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0Q predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCondGreaterThanFixnumA0QSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }



    class PCondGreaterThanFixnumA0QSQ : PCondGreaterThanFixnumA0Q
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object alternativeValue;

        protected PCondGreaterThanFixnumA0QSQ (PrimitiveGreaterThanFixnumA0Q predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0Q predicate, SCode consequent, Quotation alternative)
        {
            return new PCondGreaterThanFixnumA0QSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
            if ((int) environment.Argument0Value > this.rand1Value) {
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

    class PCondGreaterThanFixnumA0SSQ : PCondGreaterThanFixnumA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object alternativeValue;

        protected PCondGreaterThanFixnumA0SSQ (PrimitiveGreaterThanFixnumA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA0 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondGreaterThanFixnumA0SSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA1 : PCondGreaterThanFixnumA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA1 (PrimitiveGreaterThanFixnumA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumA1L) ? PCondGreaterThanFixnumA1L.Make ((PrimitiveGreaterThanFixnumA1L) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumA1Q) ? PCondGreaterThanFixnumA1Q.Make ((PrimitiveGreaterThanFixnumA1Q) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? PCondGreaterThanFixnumA1SSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA1L : PCondGreaterThanFixnumA1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondGreaterThanFixnumA1L (PrimitiveGreaterThanFixnumA1L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1L predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumA1A) ? PCondGreaterThanFixnumA1A.Make ((PrimitiveGreaterThanFixnumA1A) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumA1L1) ? Unimplemented ()
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? PCondGreaterThanFixnumA1LSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA1A : PCondGreaterThanFixnumA1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA1A (PrimitiveGreaterThanFixnumA1A predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1A predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumA1A1) ? PCondGreaterThanFixnumA1A1.Make ((PrimitiveGreaterThanFixnumA1A1) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? PCondGreaterThanFixnumA1AQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? PCondGreaterThanFixnumA1ASQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA1A1 : PCondGreaterThanFixnumA1A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA1A1 (PrimitiveGreaterThanFixnumA1A1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1A1 predicate, SCode consequent, SCode alternative)
        {
            return
                 (consequent is LexicalVariable) ? PCondGreaterThanFixnumA1A1L.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA1A1L : PCondGreaterThanFixnumA1A1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondGreaterThanFixnumA1A1L (PrimitiveGreaterThanFixnumA1A1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1A1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                 (consequent is Argument) ? PCondGreaterThanFixnumA1A1A.Make (predicate, (Argument) consequent, alternative)
                : (consequent is LexicalVariable1) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumA1A1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA1A1A : PCondGreaterThanFixnumA1A1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA1A1A (PrimitiveGreaterThanFixnumA1A1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1A1 predicate, Argument consequent, SCode alternative)
        {
            return
                 (consequent is Argument0) ? PCondGreaterThanFixnumA1A1A0.Make (predicate, (Argument0) consequent, alternative)
                : (consequent is Argument1) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumA1A1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA1A1A0 : PCondGreaterThanFixnumA1A1A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA1A1A0 (PrimitiveGreaterThanFixnumA1A1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1A1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondGreaterThanFixnumA1A1A0L.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumA1A1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA1A1A0L : PCondGreaterThanFixnumA1A1A0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondGreaterThanFixnumA1A1A0L (PrimitiveGreaterThanFixnumA1A1 predicate, Argument0 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1A1 predicate, Argument0 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondGreaterThanFixnumA1A1A0A.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? Unimplemented ()
                : new PCondGreaterThanFixnumA1A1A0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA1A1A0A : PCondGreaterThanFixnumA1A1A0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA1A1A0A (PrimitiveGreaterThanFixnumA1A1 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1A1 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented ()
                : (alternative is Argument1) ? PCondGreaterThanFixnumA1A1A0A1.Make (predicate, consequent, (Argument1) alternative)
                : new PCondGreaterThanFixnumA1A1A0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA1A1A0A1 : PCondGreaterThanFixnumA1A1A0A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumA1A1A0A1 (PrimitiveGreaterThanFixnumA1A1 predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1A1 predicate, Argument0 consequent, Argument1 alternative)
        {
            return new PCondGreaterThanFixnumA1A1A0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA1ASQ : PCondGreaterThanFixnumA1A
    {
        public readonly object alternativeValue;

        protected PCondGreaterThanFixnumA1ASQ (PrimitiveGreaterThanFixnumA1A predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1A predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondGreaterThanFixnumA1ASQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA1AQ : PCondGreaterThanFixnumA1A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondGreaterThanFixnumA1AQ (PrimitiveGreaterThanFixnumA1A predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1A predicate, Quotation consequent, SCode alternative)
        {
            return
                 (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumA1AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument1
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            // Eval argument0
            object ev0 = environment.Argument1Value;

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;

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

    class PCondGreaterThanFixnumA1LSQ : PCondGreaterThanFixnumA1L
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object alternativeValue;

        protected PCondGreaterThanFixnumA1LSQ (PrimitiveGreaterThanFixnumA1L predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1L predicate, SCode consequent, Quotation alternative)
        {
            return new PCondGreaterThanFixnumA1LSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA1Q : PCondGreaterThanFixnumA1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PCondGreaterThanFixnumA1Q (PrimitiveGreaterThanFixnumA1Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1Q predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? PCondGreaterThanFixnumA1QSL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondGreaterThanFixnumA1QSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA1QSL : PCondGreaterThanFixnumA1Q
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondGreaterThanFixnumA1QSL (PrimitiveGreaterThanFixnumA1Q predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;

        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1Q predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondGreaterThanFixnumA1QSA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? Unimplemented()
                : new PCondGreaterThanFixnumA1QSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA1QSA : PCondGreaterThanFixnumA1QSL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondGreaterThanFixnumA1QSA (PrimitiveGreaterThanFixnumA1Q predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1Q predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondGreaterThanFixnumA1QSA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? Unimplemented ()
                : new PCondGreaterThanFixnumA1QSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumA1QSA0 : PCondGreaterThanFixnumA1QSA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondGreaterThanFixnumA1QSA0 (PrimitiveGreaterThanFixnumA1Q predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1Q predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondGreaterThanFixnumA1QSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }



    class PCondGreaterThanFixnumA1QSQ : PCondGreaterThanFixnumA1Q
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object alternativeValue;

        protected PCondGreaterThanFixnumA1QSQ (PrimitiveGreaterThanFixnumA1Q predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1Q predicate, SCode consequent, Quotation alternative)
        {
            return new PCondGreaterThanFixnumA1QSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            if ((int) environment.Argument0Value > this.rand1Value) {
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

    class PCondGreaterThanFixnumA1SSQ : PCondGreaterThanFixnumA1
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object alternativeValue;

        protected PCondGreaterThanFixnumA1SSQ (PrimitiveGreaterThanFixnumA1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA1 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondGreaterThanFixnumA1SSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumAL : PCondGreaterThanFixnumA
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondGreaterThanFixnumAL (PrimitiveGreaterThanFixnumAL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumAL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumAA) ? PCondGreaterThanFixnumAA.Make ((PrimitiveGreaterThanFixnumAA) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumAL1) ? Unimplemented()
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumAA : PCondGreaterThanFixnumA
    {

        protected PCondGreaterThanFixnumAA (PrimitiveGreaterThanFixnumAA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumAA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumAA0) ? Unimplemented ()
                : (predicate is PrimitiveGreaterThanFixnumAA1) ? Unimplemented ()
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? PCondGreaterThanFixnumAAQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumAAQ : PCondGreaterThanFixnumAA
    {
        public readonly object consequentValue;
        protected PCondGreaterThanFixnumAAQ (PrimitiveGreaterThanFixnumAA predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumAA predicate, Quotation consequent, SCode alternative)
        {
            return
                 (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumAAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumASSQ : PCondGreaterThanFixnumA
    {
        public readonly object alternativeValue;

        protected PCondGreaterThanFixnumASSQ (PrimitiveGreaterThanFixnumA predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumA predicate, SCode consequent, Quotation alternative)
        {
            return new PCondGreaterThanFixnumASSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumL1 : PCondGreaterThanFixnumL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumL1 (PrimitiveGreaterThanFixnumL1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumL1 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumL1L) ? PCondGreaterThanFixnumL1L.Make ((PrimitiveGreaterThanFixnumL1L) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumL1Q) ? PCondGreaterThanFixnumL1Q.Make ((PrimitiveGreaterThanFixnumL1Q) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? PCondGreaterThanFixnumL1SQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;

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

    class PCondGreaterThanFixnumL1L : PCondGreaterThanFixnumL1
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondGreaterThanFixnumL1L (PrimitiveGreaterThanFixnumL1L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumL1L predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumL1A) ? PCondGreaterThanFixnumL1A.Make ((PrimitiveGreaterThanFixnumL1A) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumL1L1) ? Unimplemented ()
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? PCondGreaterThanFixnumL1LSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumL1A : PCondGreaterThanFixnumL1L
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumL1A (PrimitiveGreaterThanFixnumL1A predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumL1A predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumL1A0) ? PCondGreaterThanFixnumL1A0.Make ((PrimitiveGreaterThanFixnumL1A0) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumL1A1) ? Unimplemented ()
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumL1A0 : PCondGreaterThanFixnumL1A
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumL1A0 (PrimitiveGreaterThanFixnumL1A0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumL1A0 predicate, SCode consequent, SCode alternative)
        {
            return
                 (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm();
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;

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

    class PCondGreaterThanFixnumL1LSQ : PCondGreaterThanFixnumL1L
    {
        public readonly object alternativeValue;

        protected PCondGreaterThanFixnumL1LSQ (PrimitiveGreaterThanFixnumL1L predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumL1L predicate, SCode consequent, Quotation alternative)
        {
            return new PCondGreaterThanFixnumL1LSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumL1SQ : PCondGreaterThanFixnumL1
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondGreaterThanFixnumL1SQ (PrimitiveGreaterThanFixnumL1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumL1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented()
                : new PCondGreaterThanFixnumL1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumL1Q : PCondGreaterThanFixnumL1
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;


        protected PCondGreaterThanFixnumL1Q (PrimitiveGreaterThanFixnumL1Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumL1Q predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? PCondGreaterThanFixnumL1QSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }


    class PCondGreaterThanFixnumL1QSQ : PCondGreaterThanFixnumL1Q
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;


        protected PCondGreaterThanFixnumL1QSQ (PrimitiveGreaterThanFixnumL1Q predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumL1Q predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondGreaterThanFixnumL1QSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }


    class PCondGreaterThanFixnumLL : PCondGreaterThanFixnumL
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondGreaterThanFixnumLL (PrimitiveGreaterThanFixnumLL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumLL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumLA) ? PCondGreaterThanFixnumLA.Make ((PrimitiveGreaterThanFixnumLA) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumLL1) ? Unimplemented ()
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumLA : PCondGreaterThanFixnumLL
    {

        protected PCondGreaterThanFixnumLA (PrimitiveGreaterThanFixnumLA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumLA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumLA0) ? PCondGreaterThanFixnumLA0.Make ((PrimitiveGreaterThanFixnumLA0) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumLA1) ? Unimplemented ()
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumLA0 : PCondGreaterThanFixnumLA
    {

        protected PCondGreaterThanFixnumLA0 (PrimitiveGreaterThanFixnumLA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumLA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? PCondGreaterThanFixnumLA0Q.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondGreaterThanFixnumLA0SL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumLA0SL : PCondGreaterThanFixnumLA0
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondGreaterThanFixnumLA0SL (PrimitiveGreaterThanFixnumLA0 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumLA0 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondGreaterThanFixnumLA0SA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? Unimplemented()
                : new PCondGreaterThanFixnumLA0SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumLA0SA : PCondGreaterThanFixnumLA0SL
    {
        protected PCondGreaterThanFixnumLA0SA (PrimitiveGreaterThanFixnumLA0 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumLA0 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondGreaterThanFixnumLA0SA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? PCondGreaterThanFixnumLA0SA1.Make (predicate, consequent, (Argument1) alternative)
                : new PCondGreaterThanFixnumLA0SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumLA0SA0 : PCondGreaterThanFixnumLA0SA
    {
        protected PCondGreaterThanFixnumLA0SA0 (PrimitiveGreaterThanFixnumLA0 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumLA0 predicate, SCode consequent, Argument0 alternative)
        {
            return
                 new PCondGreaterThanFixnumLA0SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumLA0SA1 : PCondGreaterThanFixnumLA0SA
    {
        protected PCondGreaterThanFixnumLA0SA1 (PrimitiveGreaterThanFixnumLA0 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumLA0 predicate, SCode consequent, Argument1 alternative)
        {
            return new PCondGreaterThanFixnumLA0SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumLA0SQ : PCondGreaterThanFixnumLA0
    {
        public readonly object alternativeValue;

        protected PCondGreaterThanFixnumLA0SQ (PrimitiveGreaterThanFixnumLA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumLA0 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondGreaterThanFixnumLA0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }



    class PCondGreaterThanFixnumLA0Q : PCondGreaterThanFixnumLA0
    {
        public readonly object consequentValue;

        protected PCondGreaterThanFixnumLA0Q (PrimitiveGreaterThanFixnumLA0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumLA0 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumLA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }


    class PCondGreaterThanFixnumLQ : PCondGreaterThanFixnumL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PCondGreaterThanFixnumLQ (PrimitiveGreaterThanFixnumLQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumLQ predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? PCondGreaterThanFixnumLQSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumLQSQ : PCondGreaterThanFixnumLQ
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondGreaterThanFixnumLQSQ (PrimitiveGreaterThanFixnumLQ predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumLQ predicate, SCode consequent, Quotation alternative)
        {
            return new PCondGreaterThanFixnumLQSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumQ : PCondGreaterThanFixnum
    {
        public readonly int rand0Value;

        protected PCondGreaterThanFixnumQ (PrimitiveGreaterThanFixnumQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Value = predicate.rand0Value;
        }

        static public SCode Make (PrimitiveGreaterThanFixnumQ predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumQL) ? PCondGreaterThanFixnumQL.Make ((PrimitiveGreaterThanFixnumQL) predicate, consequent, alternative)
                //: (predicate is PrimitiveGreaterThanFixnumQQ) ? Unimplemented()
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? PCondGreaterThanFixnumQSQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumQL : PCondGreaterThanFixnumQ
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondGreaterThanFixnumQL (PrimitiveGreaterThanFixnumQL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        static public SCode Make (PrimitiveGreaterThanFixnumQL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumQA) ? PCondGreaterThanFixnumQA.Make ((PrimitiveGreaterThanFixnumQA) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumQL1) ? Unimplemented()
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumQA : PCondGreaterThanFixnumQL
    {

        protected PCondGreaterThanFixnumQA (PrimitiveGreaterThanFixnumQA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveGreaterThanFixnumQA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveGreaterThanFixnumQA0) ? PCondGreaterThanFixnumQA0.Make ((PrimitiveGreaterThanFixnumQA0) predicate, consequent, alternative)
                : (predicate is PrimitiveGreaterThanFixnumQA1) ? PCondGreaterThanFixnumQA1.Make ((PrimitiveGreaterThanFixnumQA1) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? Unimplemented ()
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumQA0 : PCondGreaterThanFixnumQA
    {
        protected PCondGreaterThanFixnumQA0 (PrimitiveGreaterThanFixnumQA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveGreaterThanFixnumQA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? PCondGreaterThanFixnumQA0Q.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumQA0Q : PCondGreaterThanFixnumQA0
    {
        public readonly object consequentValue;

        protected PCondGreaterThanFixnumQA0Q (PrimitiveGreaterThanFixnumQA predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        static public SCode Make (PrimitiveGreaterThanFixnumQA0 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumQA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumQA1 : PCondGreaterThanFixnumQA
    {
        protected PCondGreaterThanFixnumQA1 (PrimitiveGreaterThanFixnumQA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        static public SCode Make (PrimitiveGreaterThanFixnumQA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? PCondGreaterThanFixnumQA1Q.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumQA1Q : PCondGreaterThanFixnumQA1
    {
        public readonly object consequentValue;

        protected PCondGreaterThanFixnumQA1Q (PrimitiveGreaterThanFixnumQA1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        static public SCode Make (PrimitiveGreaterThanFixnumQA1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumQA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }


    class PCondGreaterThanFixnumQSQ : PCondGreaterThanFixnumQ
    {
        public readonly object consequentValue;

        protected PCondGreaterThanFixnumQSQ (PrimitiveGreaterThanFixnumQ predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        static public SCode Make (PrimitiveGreaterThanFixnumQ predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumQSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumSQ : PCondGreaterThanFixnum
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PCondGreaterThanFixnumSQ (PrimitiveGreaterThanFixnumSQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumSQ predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? Unimplemented ()
                : (consequent is Quotation) ? PCondGreaterThanFixnumSQQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondGreaterThanFixnumSQSL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondGreaterThanFixnumSQSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumSQQ : PCondGreaterThanFixnumSQ
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondGreaterThanFixnumSQQ (PrimitiveGreaterThanFixnumSQ predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumSQ predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? PCondGreaterThanFixnumSQQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondGreaterThanFixnumSQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumSQQQ : PCondGreaterThanFixnumSQQ
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondGreaterThanFixnumSQQQ (PrimitiveGreaterThanFixnumSQ predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumSQ predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCondGreaterThanFixnumSQQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumSQSL : PCondGreaterThanFixnumSQ
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondGreaterThanFixnumSQSL (PrimitiveGreaterThanFixnumSQ predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumSQ predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondGreaterThanFixnumSQSA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? Unimplemented ()
                : new PCondGreaterThanFixnumSQSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumSQSA : PCondGreaterThanFixnumSQSL
    {

        protected PCondGreaterThanFixnumSQSA (PrimitiveGreaterThanFixnumSQ predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumSQ predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondGreaterThanFixnumSQSA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? Unimplemented ()
                : new PCondGreaterThanFixnumSQSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondGreaterThanFixnumSQSA0 : PCondGreaterThanFixnumSQSA
    {

        protected PCondGreaterThanFixnumSQSA0 (PrimitiveGreaterThanFixnumSQ predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveGreaterThanFixnumSQ predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondGreaterThanFixnumSQSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }


    class PCondGreaterThanFixnumSQSQ : PCondGreaterThanFixnumSQ
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondGreaterThanFixnumSQSQ (PrimitiveGreaterThanFixnumSQ predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnumSQ predicate, SCode consequent, Quotation alternative)
        {
            return new PCondGreaterThanFixnumSQSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);

#endif
            // Eval argument1
            object ev1 = this.rand1Value;

            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;

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

    class PCondGreaterThanFixnumSSL : PCondGreaterThanFixnum
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondGreaterThanFixnumSSL (PrimitiveGreaterThanFixnum predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnum predicate, LexicalVariable consequent, SCode alternative)
        {
            throw new NotImplementedException ();
            //return new PCondGreaterThanFixnumSSLS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.arg0);
            noteCalls (this.arg1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.arg1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.arg0;
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
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCondGreaterThanFixnumSSQ : PCondGreaterThanFixnum
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondGreaterThanFixnumSSQ (PrimitiveGreaterThanFixnum predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnum predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new PCondGreaterThanFixnumSSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.arg0);
            noteCalls (this.arg1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.arg1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.arg0;
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
                answer = this.consequentValue;
                return false;
            }
        }
    }

    class PCondGreaterThanFixnumSSSL : PCondGreaterThanFixnum
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        public object alternativeName;
        public int alternativeDepth;
        public int alternativeOffset;

        protected PCondGreaterThanFixnumSSSL (PrimitiveGreaterThanFixnum predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveGreaterThanFixnum predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondGreaterThanFixnumSSSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.arg0);
            noteCalls (this.arg1);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.arg1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.arg0;
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

    class PCondGreaterThanFixnumSSSQ : PCondGreaterThanFixnum
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();

        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public object alternativeValue;

        protected PCondGreaterThanFixnumSSSQ (PrimitiveGreaterThanFixnum predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveGreaterThanFixnum predicate, SCode consequent, Quotation alternative)
        {
            return new PCondGreaterThanFixnumSSSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.arg0);
            noteCalls (this.arg1);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.arg1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.arg0;
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

}
