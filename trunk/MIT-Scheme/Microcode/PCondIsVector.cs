using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PCondIsVector : PCond1
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsVector (PrimitiveIsVector predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsVector predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsVectorL) ? PCondIsVectorL.Make ((PrimitiveIsVectorL) predicate, consequent, alternative):
                (consequent is LexicalVariable) ? PCondIsVectorSL.Make (predicate, (LexicalVariable) consequent, alternative):
                (consequent is Quotation) ? PCondIsVectorSQ.Make (predicate, (Quotation) consequent, alternative):
                (alternative is LexicalVariable) ? PCondIsVectorSSL.Make (predicate, consequent, (LexicalVariable) alternative):
                (alternative is Quotation) ? PCondIsVectorSSQ.Make (predicate, consequent, (Quotation) alternative):
                new PCondIsVector (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCondIsVector.EvalStep";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsVectorFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorL : PCondIsVector
    {
        public readonly object predicateName;
        public readonly int predicateDepth;
        public readonly int predicateOffset;

        protected PCondIsVectorL (PrimitiveIsVectorL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateName = ((LexicalVariable) predicate.Operand).Name;
            this.predicateDepth = ((LexicalVariable) predicate.Operand).Depth;
            this.predicateOffset = ((LexicalVariable) predicate.Operand).Offset;
        }

        public static SCode Make (PrimitiveIsVectorL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsVectorA) ? PCondIsVectorA.Make ((PrimitiveIsVectorA) predicate, consequent, alternative):
                (predicate is PrimitiveIsVectorL1) ? PCondIsVectorL1.Make ((PrimitiveIsVectorL1) predicate, consequent, alternative):
                (consequent is LexicalVariable) ? PCondIsVectorLL.Make (predicate, (LexicalVariable) consequent, alternative):
                (consequent is Quotation) ? PCondIsVectorLQ.Make (predicate, (Quotation) consequent, alternative):
                (alternative is LexicalVariable) ? PCondIsVectorLSL.Make (predicate, consequent, (LexicalVariable) alternative):
                (alternative is Quotation) ? PCondIsVectorLSQ.Make (predicate, consequent, (Quotation) alternative):
                new PCondIsVectorL (predicate, consequent, alternative);
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


            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorA : PCondIsVectorL
    {
        protected PCondIsVectorA (PrimitiveIsVectorA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsVectorA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsVectorA0) ? PCondIsVectorA0.Make ((PrimitiveIsVectorA0) predicate, consequent, alternative):
                (predicate is PrimitiveIsVectorA1) ? PCondIsVectorA1.Make ((PrimitiveIsVectorA1) predicate, consequent, alternative):
                (consequent is LexicalVariable) ? PCondIsVectorAL.Make (predicate, (LexicalVariable) consequent, alternative):
                (consequent is Quotation) ? PCondIsVectorAQ.Make (predicate, (Quotation) consequent, alternative):
                (alternative is LexicalVariable) ? PCondIsVectorASL.Make (predicate, consequent, (LexicalVariable) alternative):
                (alternative is Quotation) ? PCondIsVectorASQ.Make (predicate, consequent, (Quotation) alternative):
                new PCondIsVectorA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();

#endif
            object ev0 = environment.ArgumentValue (this.predicateOffset);


            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorA0 : PCondIsVectorA
    {
        protected PCondIsVectorA0 (PrimitiveIsVectorA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsVectorA0.EvalStep");
#endif

            object ev0 = environment.Argument0Value;



            if (!(ev0 is object [])) {
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

        internal static SCode Make (PrimitiveIsVectorA0 predicate, SCode consequent, SCode alternative)
        {
            return
       (consequent is LexicalVariable) ? PCondIsVectorA0L.Make (predicate, (LexicalVariable) consequent, alternative)
       : (consequent is Quotation) ? PCondIsVectorA0Q.Make (predicate, (Quotation) consequent, alternative)
       : (alternative is LexicalVariable) ? PCondIsVectorA0SL.Make (predicate, consequent, (LexicalVariable) alternative)
       : (alternative is Quotation) ? PCondIsVectorA0SQ.Make (predicate, consequent, (Quotation) alternative)
       : new PCondIsVectorA0 (predicate, consequent, alternative);
        }
    }

    [Serializable]
    class PCondIsVectorA0L : PCondIsVectorA0
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsVectorA0L (PrimitiveIsVectorA0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        internal static SCode Make (PrimitiveIsVectorA0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsVectorA0LL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsVectorA0LQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsVectorA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorA0LL : PCondIsVectorA0L
    {
        protected PCondIsVectorA0LL (PrimitiveIsVectorA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsVectorA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsVectorA0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsVectorA0LQ : PCondIsVectorA0L
    {
        public readonly object alternativeValue;

        protected PCondIsVectorA0LQ (PrimitiveIsVectorA0 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsVectorA0 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsVectorA0LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 is object [])) {

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

    [Serializable]
    class PCondIsVectorA0Q : PCondIsVectorA0
    {
        public readonly object consequentValue;

        protected PCondIsVectorA0Q (PrimitiveIsVectorA0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }
        internal static SCode Make (PrimitiveIsVectorA0 predicate, Quotation consequent, SCode alternative)
        {
            return
    (alternative is LexicalVariable) ? PCondIsVectorA0QL.Make (predicate, consequent, (LexicalVariable) alternative)
    : (alternative is Quotation) ? PCondIsVectorA0QQ.Make (predicate, consequent, (Quotation) alternative)
    : new PCondIsVectorA0Q (predicate, consequent, alternative);

        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsVectorA0Q.EvalStep");
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorA0QL : PCondIsVectorA0Q
    {
        protected PCondIsVectorA0QL (PrimitiveIsVectorA0 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsVectorA0 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsVectorA0QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    [Serializable]
    class PCondIsVectorA0QQ : PCondIsVectorA0Q
    {
        protected PCondIsVectorA0QQ (PrimitiveIsVectorA0 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsVectorA0 predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsVectorA0QQ (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    [Serializable]
    class PCondIsVectorA0SL : PCondIsVectorA0
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsVectorA0SL (PrimitiveIsVectorA0 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }
        internal static SCode Make (PrimitiveIsVectorA0 predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsVectorA0SL (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument0Value;

            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorA0SQ : PCondIsVectorA0
    {
        public readonly object alternativeValue;

        protected PCondIsVectorA0SQ (PrimitiveIsVectorA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsVectorA0 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsVectorA0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsVectorA0SQ.EvalStep");
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 is object [])) {

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

    [Serializable]
    class PCondIsVectorA1 : PCondIsVectorA
    {
        protected PCondIsVectorA1 (PrimitiveIsVectorA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsVectorA1.EvalStep");
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 is object [])) {
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

        internal static SCode Make (PrimitiveIsVectorA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsVectorA1L.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondIsVectorA1Q.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondIsVectorA1SL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsVectorA1SQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsVectorA1 (predicate, consequent, alternative);
        }
    }

    [Serializable]
    class PCondIsVectorA1L : PCondIsVectorA1
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsVectorA1L (PrimitiveIsVectorA1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;

        }

        internal static SCode Make (PrimitiveIsVectorA1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsVectorA1LL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsVectorA1LQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsVectorA1L (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();

#endif
            object ev0 = environment.Argument1Value;

            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorA1LL : PCondIsVectorA1L
    {
        protected PCondIsVectorA1LL (PrimitiveIsVectorA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsVectorA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsVectorA1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsVectorA1LQ : PCondIsVectorA1L
    {
        public readonly object alternativeValue;

        protected PCondIsVectorA1LQ (PrimitiveIsVectorA1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsVectorA1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsVectorA1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 is object [])) {

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

    [Serializable]
    class PCondIsVectorA1Q : PCondIsVectorA1
    {
        public readonly object consequentValue;
        protected PCondIsVectorA1Q (PrimitiveIsVectorA1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }
        internal static SCode Make (PrimitiveIsVectorA1 predicate, Quotation consequent, SCode alternative)
        {
            return
    (alternative is LexicalVariable) ? PCondIsVectorA1QL.Make (predicate, consequent, (LexicalVariable) alternative)
    : (alternative is Quotation) ? PCondIsVectorA1QQ.Make (predicate, consequent, (Quotation) alternative)
    : new PCondIsVectorA1Q (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsVectorA1Q.EvalStep");
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorA1QL : PCondIsVectorA1Q
    {
        protected PCondIsVectorA1QL (PrimitiveIsVectorA1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsVectorA1 predicate, Quotation quotation, LexicalVariable alternative)
        {

            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    [Serializable]
    class PCondIsVectorA1QQ : PCondIsVectorA1Q
    {
        protected PCondIsVectorA1QQ (PrimitiveIsVectorA1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsVectorA1 predicate, Quotation consequent, Quotation alternative)
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

    [Serializable]
    class PCondIsVectorA1SL : PCondIsVectorA1
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsVectorA1SL (PrimitiveIsVectorA1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }
        internal static SCode Make (PrimitiveIsVectorA1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsVectorA1SL (predicate, consequent, alternative);

        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;

            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorA1SQ : PCondIsVectorA1
    {
        public readonly object alternativeValue;

        protected PCondIsVectorA1SQ (PrimitiveIsVectorA1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsVectorA1 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsVectorA1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsVectorA1SQ.EvalStep");
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 is object [])) {

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

    [Serializable]
    class PCondIsVectorAL : PCondIsVectorA
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsVectorAL (PrimitiveIsVectorA predicate, LexicalVariable consequent, SCode alternative)
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

            if (!(ev0 is object [])) {
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

        internal static SCode Make (PrimitiveIsVectorA predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? Unimplemented():
                (consequent is LexicalVariable1) ? Unimplemented():
                (alternative is LexicalVariable) ? PCondIsVectorALL.Make (predicate, consequent, (LexicalVariable) alternative):
                (alternative is Quotation) ? PCondIsVectorALQ.Make (predicate, consequent, (Quotation) alternative):
                new PCondIsVectorAL (predicate, consequent, alternative);
        }
    }

    [Serializable]
    class PCondIsVectorAA : PCondIsVectorAL
    {
        protected PCondIsVectorAA (PrimitiveIsVectorA predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        { }

        static public SCode Make (PrimitiveIsVectorA predicate, Argument consequent, SCode alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

    }

    [Serializable]
    class PCondIsVectorAL1 : PCondIsVectorAL
    {
        protected PCondIsVectorAL1 (PrimitiveIsVectorA predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsVectorA predicate, LexicalVariable1 consequent, SCode alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsVectorALL : PCondIsVectorAL
    {
        protected PCondIsVectorALL (PrimitiveIsVectorA predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsVectorA predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsVectorALQ : PCondIsVectorAL
    {
        protected PCondIsVectorALQ (PrimitiveIsVectorA predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsVectorA predicate, LexicalVariable consequent, Quotation alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsVectorAQ : PCondIsVectorA
    {
        public readonly object consequentValue;

        protected PCondIsVectorAQ (PrimitiveIsVectorA predicate, Quotation consequent, SCode alternative)
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


            if (!(ev0 is object [])) {
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

        internal static SCode Make (PrimitiveIsVectorA predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsVectorAQL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsVectorAQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsVectorAQ (predicate, consequent, alternative);
        }
    }

    [Serializable]
    class PCondIsVectorAQL : PCondIsVectorAQ
    {
        protected PCondIsVectorAQL (PrimitiveIsVectorA predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        internal static SCode Make (PrimitiveIsVectorA predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsVectorAQL (predicate, consequent, alternative);
        }
    }

    [Serializable]
    class PCondIsVectorAQQ : PCondIsVectorAQ
    {
        protected PCondIsVectorAQQ (PrimitiveIsVectorA predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {

            throw new NotImplementedException ();
        }

        internal static SCode Make (PrimitiveIsVectorA predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsVectorAQQ (predicate, consequent, alternative);
        }
    }

    [Serializable]
    class PCondIsVectorASL : PCondIsVectorA
    {
        protected PCondIsVectorASL (PrimitiveIsVectorA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        internal static PCondIsVectorA Make (PrimitiveIsVectorA predicate, SCode consequent, LexicalVariable lexicalVariable)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsVectorASQ : PCondIsVectorA
    {
        public readonly object alternativeValue;

        protected PCondIsVectorASQ (PrimitiveIsVectorA predicate, SCode consequent, Quotation alternative)
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


            if (!(ev0 is object [])) {

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

        internal static PCondIsVectorA Make (PrimitiveIsVectorA predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsVectorASQ (predicate, consequent, alternative);
        }
    }

    [Serializable]
    class PCondIsVectorL1 : PCondIsVectorL
    {
        protected PCondIsVectorL1 (PrimitiveIsVectorL1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsVectorL1 predicate, SCode consequent, SCode alternative)
        {
            return
       (consequent is LexicalVariable) ? PCondIsVectorL1L.Make (predicate, (LexicalVariable) consequent, alternative)
       : (consequent is Quotation) ? PCondIsVectorL1Q.Make (predicate, (Quotation) consequent, alternative)
       : (alternative is LexicalVariable) ? PCondIsVectorL1SL.Make (predicate, consequent, (LexicalVariable) alternative)
       : (alternative is Quotation) ? PCondIsVectorL1SQ.Make (predicate, consequent, (Quotation) alternative)
       : new PCondIsVectorL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ("PCondIsVectorL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();


            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorL1L : PCondIsVectorL1
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsVectorL1L (PrimitiveIsVectorL1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;

        }

        internal static SCode Make (PrimitiveIsVectorL1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsVectorL1LL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsVectorL1LQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsVectorL1L (predicate, consequent, alternative);
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

            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorL1LL : PCondIsVectorL1L
    {
        protected PCondIsVectorL1LL (PrimitiveIsVectorL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsVectorL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsVectorL1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsVectorL1LQ : PCondIsVectorL1L
    {
        public readonly object alternativeValue;

        protected PCondIsVectorL1LQ (PrimitiveIsVectorL1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsVectorL1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsVectorL1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 is object [])) {

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

    [Serializable]
    class PCondIsVectorL1Q : PCondIsVectorL1
    {
        public readonly object consequentValue;

        protected PCondIsVectorL1Q (PrimitiveIsVectorL1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }
        internal static SCode Make (PrimitiveIsVectorL1 predicate, Quotation consequent, SCode alternative)
        {
            return
    (alternative is LexicalVariable) ? PCondIsVectorL1QL.Make (predicate, consequent, (LexicalVariable) alternative)
    : (alternative is Quotation) ? PCondIsVectorL1QQ.Make (predicate, consequent, (Quotation) alternative)
    : new PCondIsVectorL1Q (predicate, consequent, alternative);
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


            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorL1QL : PCondIsVectorL1Q
    {
        protected PCondIsVectorL1QL (PrimitiveIsVectorL1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsVectorL1 predicate, Quotation quotation, LexicalVariable alternative)
        {

            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    [Serializable]
    class PCondIsVectorL1QQ : PCondIsVectorL1Q
    {
        protected PCondIsVectorL1QQ (PrimitiveIsVectorL1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsVectorL1 predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsVectorL1QQ (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    [Serializable]
    class PCondIsVectorL1SL : PCondIsVectorL1
    {
        protected PCondIsVectorL1SL (PrimitiveIsVectorL1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsVectorL1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsVectorL1SL (predicate, consequent, alternative);

        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    [Serializable]
    class PCondIsVectorL1SQ : PCondIsVectorL1
    {
        public readonly object alternativeValue;

        protected PCondIsVectorL1SQ (PrimitiveIsVectorL1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsVectorL1 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsVectorL1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ("PCondIsVectorL1SQ.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();


            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorLL : PCondIsVectorL
    {
        protected PCondIsVectorLL (PrimitiveIsVectorL predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsVectorL predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsVectorLLL.Make (predicate, consequent, alternative)
                : (alternative is Quotation) ? PCondIsVectorLLQ.Make (predicate, consequent, alternative)
                : new PCondIsVectorLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsVectorLLL : PCondIsVectorLL
    {
        protected PCondIsVectorLLL (PrimitiveIsVectorL predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsVectorL predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsVectorLLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsVectorLLQ : PCondIsVectorLL
    {
        protected PCondIsVectorLLQ (PrimitiveIsVectorL predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsVectorL predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsVectorLLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsVectorLQ : PCondIsVectorL
    {
        protected PCondIsVectorLQ (PrimitiveIsVectorL predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsVectorL predicate, Quotation consequent, SCode alternative)
        {
            return
              (alternative is LexicalVariable) ? PCondIsVectorLQL.Make (predicate, consequent, (LexicalVariable) alternative)
              : (alternative is Quotation) ? PCondIsVectorLQQ.Make (predicate, consequent, (Quotation) alternative)
              : new PCondIsVectorLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsVectorLQL : PCondIsVectorLQ
    {
        protected PCondIsVectorLQL (PrimitiveIsVectorL predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsVectorL predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsVectorLQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsVectorLQQ : PCondIsVectorLQ
    {
        protected PCondIsVectorLQQ (PrimitiveIsVectorL predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsVectorL predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsVectorLQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsVectorLSL : PCondIsVectorL
    {
        protected PCondIsVectorLSL (PrimitiveIsVectorL predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsVectorL predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsVectorLSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsVectorLSQ : PCondIsVectorL
    {
        public readonly object alternativeValue;

        protected PCondIsVectorLSQ (PrimitiveIsVectorL predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsVectorL predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsVectorLSQ (predicate, consequent, alternative);
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

            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorSL : PCondIsVector
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsVectorSL (PrimitiveIsVector predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsVector predicate, LexicalVariable consequent, SCode alternative)
        {
            return
     (alternative is LexicalVariable) ? PCondIsVectorSLL.Make (predicate, consequent, (LexicalVariable) alternative)
     : (alternative is Quotation) ? PCondIsVectorSLQ.Make (predicate, consequent, (Quotation) alternative)
     : new PCondIsVectorSL (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsVectorFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorSLL : PCondIsVectorSL
    {

        protected PCondIsVectorSLL (PrimitiveIsVector predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsVector predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsVectorSLQ : PCondIsVectorSL
    {
        public readonly object alternativeValue;

        protected PCondIsVectorSLQ (PrimitiveIsVector predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }


        public static SCode Make (PrimitiveIsVector predicate, LexicalVariable consequent, Quotation alternative)
        {

            return new PCondIsVectorSLQ (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsVectorFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorSQ : PCondIsVector
    {
        public readonly object consequentValue;

        protected PCondIsVectorSQ (PrimitiveIsVector predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsVector predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsVectorSQL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondIsVectorSQQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsVectorSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsVectorSQ.EvalStep");
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsVectorFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ev0 is object []) {
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

    [Serializable]
    class PCondIsVectorSQL : PCondIsVectorSQ
    {
        protected PCondIsVectorSQL (PrimitiveIsVector predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsVector predicate, Quotation consequent, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsVectorSQQ : PCondIsVectorSQ
    {
        public readonly object alternativeValue;

        protected PCondIsVectorSQQ (PrimitiveIsVector predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }


        public static SCode Make (PrimitiveIsVector predicate, Quotation consequent, Quotation alternative)
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

            return new PCondIsVectorSQQ (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsVectorFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorSSL : PCondIsVector
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsVectorSSL (PrimitiveIsVector predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveIsVector predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsVectorSSL (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsVectorFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }



            if (!(ev0 is object [])) {
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

    [Serializable]
    class PCondIsVectorSSQ : PCondIsVector
    {
        public readonly object alternativeValue;

        protected PCondIsVectorSSQ (PrimitiveIsVector predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsVector predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsVectorSSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.arg0);
            SCode.location = "PCondIsVectorSSQ.EvalStep";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
                        SCode.location = "PCondIsVectorSSQ.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsVectorFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (!(ev0 is object [])) {
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
