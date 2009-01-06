using System;
using System.Diagnostics;

namespace Microcode
{
    [Serializable]
    class PCondIsBigFixnum : PCond1
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsBigFixnum (PrimitiveIsBigFixnum predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsBigFixnum predicate, SCode consequent, SCode alternative)
        {
            return
                 (predicate is PrimitiveIsBigFixnumL) ? PCondIsBigFixnumL.Make ((PrimitiveIsBigFixnumL) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? PCondIsBigFixnumSL.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondIsBigFixnumSQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondIsBigFixnumSSL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsBigFixnumSSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsBigFixnum (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();
            noteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsBigFixnumFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is Int64)) {
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
            #endregion
        }
    }

    class PCondIsBigFixnumL : PCondIsBigFixnum
    {
        public readonly object predicateName;
        public readonly int predicateDepth;
        public readonly int predicateOffset;

        protected PCondIsBigFixnumL (PrimitiveIsBigFixnumL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateName = ((LexicalVariable) predicate.Operand).Name;
            this.predicateDepth = ((LexicalVariable) predicate.Operand).Depth;
            this.predicateOffset = ((LexicalVariable) predicate.Operand).Offset;
        }

        public static SCode Make (PrimitiveIsBigFixnumL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsBigFixnumA) ? PCondIsBigFixnumA.Make ((PrimitiveIsBigFixnumA) predicate, consequent, alternative)
                : (predicate is PrimitiveIsBigFixnumL1) ? PCondIsBigFixnumL1.Make ((PrimitiveIsBigFixnumL1) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? PCondIsBigFixnumLL.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondIsBigFixnumLQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondIsBigFixnumLSL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsBigFixnumLSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsBigFixnumL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ("PCondIsBigFixnumL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();


            if (!(ev0 is Int64)) {
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
            #endregion
        }
    }

    class PCondIsBigFixnumA : PCondIsBigFixnumL
    {
        protected PCondIsBigFixnumA (PrimitiveIsBigFixnumA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsBigFixnumA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsBigFixnumA0) ? PCondIsBigFixnumA0.Make ((PrimitiveIsBigFixnumA0) predicate, consequent, alternative)
                : (predicate is PrimitiveIsBigFixnumA1) ? PCondIsBigFixnumA1.Make ((PrimitiveIsBigFixnumA1) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? PCondIsBigFixnumAL.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondIsBigFixnumAQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondIsBigFixnumASL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsBigFixnumASQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsBigFixnumA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();

#endif
            object ev0 = environment.ArgumentValue (this.predicateOffset);


            if (!(ev0 is Int64)) {
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
            #endregion

        }
    }

    class PCondIsBigFixnumA0 : PCondIsBigFixnumA
    {
        protected PCondIsBigFixnumA0 (PrimitiveIsBigFixnumA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsBigFixnum.EvalStep");
#endif

            object ev0 = environment.Argument0Value;



            if (!(ev0 is Int64)) {
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

        internal static SCode Make (PrimitiveIsBigFixnumA0 predicate, SCode consequent, SCode alternative)
        {
            return
       (consequent is LexicalVariable) ? PCondIsBigFixnumA0L.Make (predicate, (LexicalVariable) consequent, alternative)
       : (consequent is Quotation) ? PCondIsBigFixnumA0Q.Make (predicate, (Quotation) consequent, alternative)
       : (alternative is LexicalVariable) ? PCondIsBigFixnumA0SL.Make (predicate, consequent, (LexicalVariable) alternative)
       : (alternative is Quotation) ? PCondIsBigFixnumA0SQ.Make (predicate, consequent, (Quotation) alternative)
       : new PCondIsBigFixnumA0 (predicate, consequent, alternative);
        }
    }

    class PCondIsBigFixnumA0L : PCondIsBigFixnumA0
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsBigFixnumA0L (PrimitiveIsBigFixnumA0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        internal static SCode Make (PrimitiveIsBigFixnumA0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsBigFixnumA0LL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsBigFixnumA0LQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsBigFixnumA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsBigFixnumA0L.EvalStep");
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 is Int64)) {
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

    class PCondIsBigFixnumA0LL : PCondIsBigFixnumA0L
    {
        protected PCondIsBigFixnumA0LL (PrimitiveIsBigFixnumA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsBigFixnumA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsBigFixnumA0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsBigFixnumA0LQ : PCondIsBigFixnumA0L
    {
        public readonly object alternativeValue;

        protected PCondIsBigFixnumA0LQ (PrimitiveIsBigFixnumA0 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsBigFixnumA0 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsBigFixnumA0LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 is Int64)) {

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

    class PCondIsBigFixnumA0Q : PCondIsBigFixnumA0
    {
        public readonly object consequentValue;

        protected PCondIsBigFixnumA0Q (PrimitiveIsBigFixnumA0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }
        internal static SCode Make (PrimitiveIsBigFixnumA0 predicate, Quotation consequent, SCode alternative)
        {
            return
    (alternative is LexicalVariable) ? PCondIsBigFixnumA0QL.Make (predicate, consequent, (LexicalVariable) alternative)
    : (alternative is Quotation) ? PCondIsBigFixnumA0QQ.Make (predicate, consequent, (Quotation) alternative)
    : new PCondIsBigFixnumA0Q (predicate, consequent, alternative);

        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsBigFixnumA0Q.EvalStep");
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 is Int64)) {
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

    class PCondIsBigFixnumA0QL : PCondIsBigFixnumA0Q
    {
        protected PCondIsBigFixnumA0QL (PrimitiveIsBigFixnumA0 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsBigFixnumA0 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsBigFixnumA0QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsBigFixnumA0QQ : PCondIsBigFixnumA0Q
    {
        protected PCondIsBigFixnumA0QQ (PrimitiveIsBigFixnumA0 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsBigFixnumA0 predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsBigFixnumA0QQ (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsBigFixnumA0SL : PCondIsBigFixnumA0
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsBigFixnumA0SL (PrimitiveIsBigFixnumA0 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }
        internal static SCode Make (PrimitiveIsBigFixnumA0 predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsBigFixnumA0SL (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument0Value;

            if (!(ev0 is Int64)) {
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
            #endregion


        }


    }

    class PCondIsBigFixnumA0SQ : PCondIsBigFixnumA0
    {
        public readonly object alternativeValue;

        protected PCondIsBigFixnumA0SQ (PrimitiveIsBigFixnumA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsBigFixnumA0 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsBigFixnumA0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsBigFixnumA0SQ.EvalStep");
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 is Int64)) {

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

    class PCondIsBigFixnumA1 : PCondIsBigFixnumA
    {
        protected PCondIsBigFixnumA1 (PrimitiveIsBigFixnumA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsBigFixnum");

#endif

            if (environment.Argument1Value is Int64) {
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

        internal static SCode Make (PrimitiveIsBigFixnumA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsBigFixnumA1L.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondIsBigFixnumA1Q.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondIsBigFixnumA1SL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsBigFixnumA1SQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsBigFixnumA1 (predicate, consequent, alternative);
        }
    }

    class PCondIsBigFixnumA1L : PCondIsBigFixnumA1
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsBigFixnumA1L (PrimitiveIsBigFixnumA1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;

        }

        internal static SCode Make (PrimitiveIsBigFixnumA1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsBigFixnumA1LL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsBigFixnumA1LQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsBigFixnumA1L (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();

#endif
            object ev0 = environment.Argument1Value;

            if (!(ev0 is Int64)) {
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

    class PCondIsBigFixnumA1LL : PCondIsBigFixnumA1L
    {
        protected PCondIsBigFixnumA1LL (PrimitiveIsBigFixnumA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsBigFixnumA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsBigFixnumA1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsBigFixnumA1LQ : PCondIsBigFixnumA1L
    {
        public readonly object alternativeValue;

        protected PCondIsBigFixnumA1LQ (PrimitiveIsBigFixnumA1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsBigFixnumA1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsBigFixnumA1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 is Int64)) {

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

    class PCondIsBigFixnumA1Q : PCondIsBigFixnumA1
    {
        public readonly object consequentValue;
        protected PCondIsBigFixnumA1Q (PrimitiveIsBigFixnumA1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }
        internal static SCode Make (PrimitiveIsBigFixnumA1 predicate, Quotation consequent, SCode alternative)
        {
            return
    (alternative is LexicalVariable) ? PCondIsBigFixnumA1QL.Make (predicate, consequent, (LexicalVariable) alternative)
    : (alternative is Quotation) ? PCondIsBigFixnumA1QQ.Make (predicate, consequent, (Quotation) alternative)
    : new PCondIsBigFixnumA1Q (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsBigFixnumA1Q");
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 is Int64)) {
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

    class PCondIsBigFixnumA1QL : PCondIsBigFixnumA1Q
    {
        protected PCondIsBigFixnumA1QL (PrimitiveIsBigFixnumA1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsBigFixnumA1 predicate, Quotation quotation, LexicalVariable alternative)
        {

            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsBigFixnumA1QQ : PCondIsBigFixnumA1Q
    {
        protected PCondIsBigFixnumA1QQ (PrimitiveIsBigFixnumA1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsBigFixnumA1 predicate, Quotation consequent, Quotation alternative)
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

    class PCondIsBigFixnumA1SL : PCondIsBigFixnumA1
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsBigFixnumA1SL (PrimitiveIsBigFixnumA1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }
        internal static SCode Make (PrimitiveIsBigFixnumA1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsBigFixnumA1SL (predicate, consequent, alternative);

        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;

            if (!(ev0 is Int64)) {
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
            #endregion

        }


    }

    class PCondIsBigFixnumA1SQ : PCondIsBigFixnumA1
    {
        public readonly object alternativeValue;

        protected PCondIsBigFixnumA1SQ (PrimitiveIsBigFixnumA1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsBigFixnumA1 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsBigFixnumA1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 is Int64)) {

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

    class PCondIsBigFixnumAL : PCondIsBigFixnumA
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsBigFixnumAL (PrimitiveIsBigFixnumA predicate, LexicalVariable consequent, SCode alternative)
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

            if (!(ev0 is Int64)) {
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

        internal static SCode Make (PrimitiveIsBigFixnumA predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsBigFixnumALL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsBigFixnumALQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsBigFixnumAL (predicate, consequent, alternative);
        }
    }

    class PCondIsBigFixnumAA : PCondIsBigFixnumAL
    {
        protected PCondIsBigFixnumAA (PrimitiveIsBigFixnumA predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        { }

        static public SCode Make (PrimitiveIsBigFixnumA predicate, Argument consequent, SCode alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

    }

    class PCondIsBigFixnumAL1 : PCondIsBigFixnumAL
    {
        protected PCondIsBigFixnumAL1 (PrimitiveIsBigFixnumA predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsBigFixnumA predicate, LexicalVariable1 consequent, SCode alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsBigFixnumALL : PCondIsBigFixnumAL
    {
        protected PCondIsBigFixnumALL (PrimitiveIsBigFixnumA predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsBigFixnumA predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsBigFixnumALQ : PCondIsBigFixnumAL
    {
        protected PCondIsBigFixnumALQ (PrimitiveIsBigFixnumA predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsBigFixnumA predicate, LexicalVariable consequent, Quotation alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsBigFixnumAQ : PCondIsBigFixnumA
    {
        public readonly object consequentValue;

        protected PCondIsBigFixnumAQ (PrimitiveIsBigFixnumA predicate, Quotation consequent, SCode alternative)
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


            if (!(ev0 is Int64)) {
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

        internal static SCode Make (PrimitiveIsBigFixnumA predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsBigFixnumAQL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsBigFixnumAQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsBigFixnumAQ (predicate, consequent, alternative);
        }
    }

    class PCondIsBigFixnumAQL : PCondIsBigFixnumAQ
    {
        protected PCondIsBigFixnumAQL (PrimitiveIsBigFixnumA predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        internal static SCode Make (PrimitiveIsBigFixnumA predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsBigFixnumAQL (predicate, consequent, alternative);
        }
    }

    class PCondIsBigFixnumAQQ : PCondIsBigFixnumAQ
    {
        protected PCondIsBigFixnumAQQ (PrimitiveIsBigFixnumA predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {

            throw new NotImplementedException ();
        }

        internal static SCode Make (PrimitiveIsBigFixnumA predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsBigFixnumAQQ (predicate, consequent, alternative);
        }
    }

    class PCondIsBigFixnumASL : PCondIsBigFixnumA
    {
        protected PCondIsBigFixnumASL (PrimitiveIsBigFixnumA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        internal static PCondIsBigFixnumA Make (PrimitiveIsBigFixnumA predicate, SCode consequent, LexicalVariable lexicalVariable)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsBigFixnumASQ : PCondIsBigFixnumA
    {
        public readonly object alternativeValue;

        protected PCondIsBigFixnumASQ (PrimitiveIsBigFixnumA predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.ArgumentValue (predicateOffset);


            if (!(ev0 is Int64)) {

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

        internal static PCondIsBigFixnumA Make (PrimitiveIsBigFixnumA predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsBigFixnumASQ (predicate, consequent, alternative);
        }
    }

    class PCondIsBigFixnumL1 : PCondIsBigFixnumL
    {
        protected PCondIsBigFixnumL1 (PrimitiveIsBigFixnumL1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsBigFixnumL1 predicate, SCode consequent, SCode alternative)
        {
            return
       (consequent is LexicalVariable) ? PCondIsBigFixnumL1L.Make (predicate, (LexicalVariable) consequent, alternative)
       : (consequent is Quotation) ? PCondIsBigFixnumL1Q.Make (predicate, (Quotation) consequent, alternative)
       : (alternative is LexicalVariable) ? PCondIsBigFixnumL1SL.Make (predicate, consequent, (LexicalVariable) alternative)
       : (alternative is Quotation) ? PCondIsBigFixnumL1SQ.Make (predicate, consequent, (Quotation) alternative)
       : new PCondIsBigFixnumL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ("PCondIsBigFixnumL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();


            if (!(ev0 is Int64)) {
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
            #endregion

        }
    }

    class PCondIsBigFixnumL1L : PCondIsBigFixnumL1
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsBigFixnumL1L (PrimitiveIsBigFixnumL1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;

        }

        internal static SCode Make (PrimitiveIsBigFixnumL1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsBigFixnumL1LL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsBigFixnumL1LQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsBigFixnumL1L (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (!(ev0 is Int64)) {
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

    class PCondIsBigFixnumL1LL : PCondIsBigFixnumL1L
    {
        protected PCondIsBigFixnumL1LL (PrimitiveIsBigFixnumL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsBigFixnumL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsBigFixnumL1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsBigFixnumL1LQ : PCondIsBigFixnumL1L
    {
        public readonly object alternativeValue;

        protected PCondIsBigFixnumL1LQ (PrimitiveIsBigFixnumL1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsBigFixnumL1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsBigFixnumL1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 is Int64)) {

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

    class PCondIsBigFixnumL1Q : PCondIsBigFixnumL1
    {
        public readonly object consequentValue;

        protected PCondIsBigFixnumL1Q (PrimitiveIsBigFixnumL1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }
        internal static SCode Make (PrimitiveIsBigFixnumL1 predicate, Quotation consequent, SCode alternative)
        {
            return
    (alternative is LexicalVariable) ? PCondIsBigFixnumL1QL.Make (predicate, consequent, (LexicalVariable) alternative)
    : (alternative is Quotation) ? PCondIsBigFixnumL1QQ.Make (predicate, consequent, (Quotation) alternative)
    : new PCondIsBigFixnumL1Q (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsBigFixnumL1Q.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (!(ev0 is Int64)) {
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

    class PCondIsBigFixnumL1QL : PCondIsBigFixnumL1Q
    {
        protected PCondIsBigFixnumL1QL (PrimitiveIsBigFixnumL1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsBigFixnumL1 predicate, Quotation quotation, LexicalVariable alternative)
        {

            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsBigFixnumL1QQ : PCondIsBigFixnumL1Q
    {
        protected PCondIsBigFixnumL1QQ (PrimitiveIsBigFixnumL1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsBigFixnumL1 predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsBigFixnumL1QQ (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsBigFixnumL1SL : PCondIsBigFixnumL1
    {
        protected PCondIsBigFixnumL1SL (PrimitiveIsBigFixnumL1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsBigFixnumL1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsBigFixnumL1SL (predicate, consequent, alternative);

        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsBigFixnumL1SQ : PCondIsBigFixnumL1
    {
        public readonly object alternativeValue;

        protected PCondIsBigFixnumL1SQ (PrimitiveIsBigFixnumL1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsBigFixnumL1 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsBigFixnumL1SQ (predicate, consequent, alternative);
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


            if (!(ev0 is Int64)) {
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
            #endregion

            throw new NotImplementedException ();
        }
    }

    class PCondIsBigFixnumLL : PCondIsBigFixnumL
    {
        protected PCondIsBigFixnumLL (PrimitiveIsBigFixnumL predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsBigFixnumL predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsBigFixnumLLL.Make (predicate, consequent, alternative)
                : (alternative is Quotation) ? PCondIsBigFixnumLLQ.Make (predicate, consequent, alternative)
                : new PCondIsBigFixnumLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsBigFixnumLLL : PCondIsBigFixnumLL
    {
        protected PCondIsBigFixnumLLL (PrimitiveIsBigFixnumL predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsBigFixnumL predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsBigFixnumLLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsBigFixnumLLQ : PCondIsBigFixnumLL
    {
        protected PCondIsBigFixnumLLQ (PrimitiveIsBigFixnumL predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsBigFixnumL predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsBigFixnumLLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsBigFixnumLQ : PCondIsBigFixnumL
    {
        protected readonly object consequentValue;

        protected PCondIsBigFixnumLQ (PrimitiveIsBigFixnumL predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsBigFixnumL predicate, Quotation consequent, SCode alternative)
        {
            return
              (alternative is LexicalVariable) ? PCondIsBigFixnumLQL.Make (predicate, consequent, (LexicalVariable) alternative)
              : (alternative is Quotation) ? PCondIsBigFixnumLQQ.Make (predicate, consequent, (Quotation) alternative)
              : new PCondIsBigFixnumLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsBigFixnumLQ.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();


            if (!(ev0 is Int64)) {
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

    class PCondIsBigFixnumLQL : PCondIsBigFixnumLQ
    {
        protected PCondIsBigFixnumLQL (PrimitiveIsBigFixnumL predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsBigFixnumL predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsBigFixnumLQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsBigFixnumLQQ : PCondIsBigFixnumLQ
    {
        protected PCondIsBigFixnumLQQ (PrimitiveIsBigFixnumL predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsBigFixnumL predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsBigFixnumLQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsBigFixnumLSL : PCondIsBigFixnumL
    {
        protected PCondIsBigFixnumLSL (PrimitiveIsBigFixnumL predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsBigFixnumL predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsBigFixnumLSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsBigFixnumLSQ : PCondIsBigFixnumL
    {
        public readonly object alternativeValue;

        protected PCondIsBigFixnumLSQ (PrimitiveIsBigFixnumL predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsBigFixnumL predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsBigFixnumLSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();

#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

            if (!(ev0 is Int64)) {
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
            #endregion

        }
    }

    class PCondIsBigFixnumSL : PCondIsBigFixnum
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsBigFixnumSL (PrimitiveIsBigFixnum predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsBigFixnum predicate, LexicalVariable consequent, SCode alternative)
        {
            return
     (alternative is LexicalVariable) ? PCondIsBigFixnumSLL.Make (predicate, consequent, (LexicalVariable) alternative)
     : (alternative is Quotation) ? PCondIsBigFixnumSLQ.Make (predicate, consequent, (Quotation) alternative)
     : new PCondIsBigFixnumSL (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsBigFixnumFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is Int64)) {
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

    class PCondIsBigFixnumSLL : PCondIsBigFixnumSL
    {

        protected PCondIsBigFixnumSLL (PrimitiveIsBigFixnum predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsBigFixnum predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsBigFixnumSLQ : PCondIsBigFixnumSL
    {
        public readonly object alternativeValue;

        protected PCondIsBigFixnumSLQ (PrimitiveIsBigFixnum predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }


        public static SCode Make (PrimitiveIsBigFixnum predicate, LexicalVariable consequent, Quotation alternative)
        {

            return new PCondIsBigFixnumSLQ (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsBigFixnumFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is Int64)) {
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

    class PCondIsBigFixnumSQ : PCondIsBigFixnum
    {
        public readonly object consequentValue;

        protected PCondIsBigFixnumSQ (PrimitiveIsBigFixnum predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsBigFixnum predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsBigFixnumSQL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsBigFixnumSQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsBigFixnumSQ (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsBigFixnumFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is Int64)) {
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

    class PCondIsBigFixnumSQL : PCondIsBigFixnumSQ
    {
        protected PCondIsBigFixnumSQL (PrimitiveIsBigFixnum predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsBigFixnum predicate, Quotation consequent, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsBigFixnumSQQ : PCondIsBigFixnumSQ
    {
        public readonly object alternativeValue;

        protected PCondIsBigFixnumSQQ (PrimitiveIsBigFixnum predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }


        public static SCode Make (PrimitiveIsBigFixnum predicate, Quotation consequent, Quotation alternative)
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

            return new PCondIsBigFixnumSQQ (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsBigFixnumFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is Int64)) {
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

    class PCondIsBigFixnumSSL : PCondIsBigFixnum
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsBigFixnumSSL (PrimitiveIsBigFixnum predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveIsBigFixnum predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsBigFixnumSSL (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsBigFixnumFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }



            if (!(ev0 is Int64)) {
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

    class PCondIsBigFixnumSSQ : PCondIsBigFixnum
    {
        public readonly object alternativeValue;

        protected PCondIsBigFixnumSSQ (PrimitiveIsBigFixnum predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsBigFixnum predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsBigFixnumSSQ (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsBigFixnumFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (!(ev0 is Int64)) {
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
