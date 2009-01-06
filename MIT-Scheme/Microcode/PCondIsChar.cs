using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PCondIsChar : PCond1
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsChar (PrimitiveIsChar predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsChar predicate, SCode consequent, SCode alternative)
        {
            return
                 (predicate is PrimitiveIsCharL) ? PCondIsCharL.Make ((PrimitiveIsCharL) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? PCondIsCharSL.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondIsCharSQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondIsCharSSL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsCharSSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsChar (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsCharFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is char)) {
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

    class PCondIsCharL : PCondIsChar
    {
        public readonly object predicateName;
        public readonly int predicateDepth;
        public readonly int predicateOffset;

        protected PCondIsCharL (PrimitiveIsCharL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateName = ((LexicalVariable) predicate.Operand).Name;
            this.predicateDepth = ((LexicalVariable) predicate.Operand).Depth;
            this.predicateOffset = ((LexicalVariable) predicate.Operand).Offset;
        }

        public static SCode Make (PrimitiveIsCharL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsCharA) ? PCondIsCharA.Make ((PrimitiveIsCharA) predicate, consequent, alternative)
                : (predicate is PrimitiveIsCharL1) ? PCondIsCharL1.Make ((PrimitiveIsCharL1) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? PCondIsCharLL.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondIsCharLQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondIsCharLSL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsCharLSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsCharL (predicate, consequent, alternative);
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


            if (!(ev0 is char)) {
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

    class PCondIsCharA : PCondIsCharL
    {
        protected PCondIsCharA (PrimitiveIsCharA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsCharA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsCharA0) ? PCondIsCharA0.Make ((PrimitiveIsCharA0) predicate, consequent, alternative)
                : (predicate is PrimitiveIsCharA1) ? PCondIsCharA1.Make ((PrimitiveIsCharA1) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? PCondIsCharAL.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondIsCharAQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondIsCharASL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsCharASQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsCharA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();

#endif
            object ev0 = environment.ArgumentValue (this.predicateOffset);


            if (!(ev0 is char)) {
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

    class PCondIsCharA0 : PCondIsCharA
    {
        protected PCondIsCharA0 (PrimitiveIsCharA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsCharA0.EvalStep");
#endif

            object ev0 = environment.Argument0Value;



            if (!(ev0 is char)) {
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

        internal static SCode Make (PrimitiveIsCharA0 predicate, SCode consequent, SCode alternative)
        {
            return
       (consequent is LexicalVariable) ? PCondIsCharA0L.Make (predicate, (LexicalVariable) consequent, alternative)
       : (consequent is Quotation) ? PCondIsCharA0Q.Make (predicate, (Quotation) consequent, alternative)
       : (alternative is LexicalVariable) ? PCondIsCharA0SL.Make (predicate, consequent, (LexicalVariable) alternative)
       : (alternative is Quotation) ? PCondIsCharA0SQ.Make (predicate, consequent, (Quotation) alternative)
       : new PCondIsCharA0 (predicate, consequent, alternative);
        }
    }

    class PCondIsCharA0L : PCondIsCharA0
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsCharA0L (PrimitiveIsCharA0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        internal static SCode Make (PrimitiveIsCharA0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsCharA0LL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsCharA0LQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsCharA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsCharA0L.EvalStep");
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 is char)) {
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

    class PCondIsCharA0LL : PCondIsCharA0L
    {
        protected PCondIsCharA0LL (PrimitiveIsCharA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsCharA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsCharA0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsCharA0LQ : PCondIsCharA0L
    {
        public readonly object alternativeValue;

        protected PCondIsCharA0LQ (PrimitiveIsCharA0 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsCharA0 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsCharA0LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 is char)) {

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

    class PCondIsCharA0Q : PCondIsCharA0
    {
        public readonly object consequentValue;

        protected PCondIsCharA0Q (PrimitiveIsCharA0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }
        internal static SCode Make (PrimitiveIsCharA0 predicate, Quotation consequent, SCode alternative)
        {
            return
    (alternative is LexicalVariable) ? PCondIsCharA0QL.Make (predicate, consequent, (LexicalVariable) alternative)
    : (alternative is Quotation) ? PCondIsCharA0QQ.Make (predicate, consequent, (Quotation) alternative)
    : new PCondIsCharA0Q (predicate, consequent, alternative);

        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsCharA0Q.EvalStep");
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 is char)) {
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

    class PCondIsCharA0QL : PCondIsCharA0Q
    {
        protected PCondIsCharA0QL (PrimitiveIsCharA0 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsCharA0 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsCharA0QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsCharA0QQ : PCondIsCharA0Q
    {
        protected PCondIsCharA0QQ (PrimitiveIsCharA0 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsCharA0 predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsCharA0QQ (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsCharA0SL : PCondIsCharA0
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsCharA0SL (PrimitiveIsCharA0 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }
        internal static SCode Make (PrimitiveIsCharA0 predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsCharA0SL (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsCharA0SL.EvalStep");
#endif
            object ev0 = environment.Argument0Value;

            if (!(ev0 is char)) {
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

    class PCondIsCharA0SQ : PCondIsCharA0
    {
        public readonly object alternativeValue;

        protected PCondIsCharA0SQ (PrimitiveIsCharA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsCharA0 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsCharA0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsChar.EvalStepA0SQ");
#endif
            object ev0 = environment.Argument0Value;

            if (!(ev0 is char)) {

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

    class PCondIsCharA1 : PCondIsCharA
    {
        protected PCondIsCharA1 (PrimitiveIsCharA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.arg0);

#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 is char)) {
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

        internal static SCode Make (PrimitiveIsCharA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsCharA1L.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondIsCharA1Q.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondIsCharA1SL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsCharA1SQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsCharA1 (predicate, consequent, alternative);
        }
    }

    class PCondIsCharA1L : PCondIsCharA1
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsCharA1L (PrimitiveIsCharA1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;

        }

        internal static SCode Make (PrimitiveIsCharA1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsCharA1LL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsCharA1LQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsCharA1L (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();

#endif
            object ev0 = environment.Argument1Value;

            if (!(ev0 is char)) {
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

    class PCondIsCharA1LL : PCondIsCharA1L
    {
        protected PCondIsCharA1LL (PrimitiveIsCharA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsCharA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsCharA1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsCharA1LQ : PCondIsCharA1L
    {
        public readonly object alternativeValue;

        protected PCondIsCharA1LQ (PrimitiveIsCharA1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsCharA1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsCharA1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 is char)) {

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

    class PCondIsCharA1Q : PCondIsCharA1
    {
        public readonly object consequentValue;
        protected PCondIsCharA1Q (PrimitiveIsCharA1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }
        internal static SCode Make (PrimitiveIsCharA1 predicate, Quotation consequent, SCode alternative)
        {
            return
    (alternative is LexicalVariable) ? PCondIsCharA1QL.Make (predicate, consequent, (LexicalVariable) alternative)
    : (alternative is Quotation) ? PCondIsCharA1QQ.Make (predicate, consequent, (Quotation) alternative)
    : new PCondIsCharA1Q (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsCharA1Q.EvalStep");
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 is char)) {
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

    class PCondIsCharA1QL : PCondIsCharA1Q
    {
        protected PCondIsCharA1QL (PrimitiveIsCharA1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsCharA1 predicate, Quotation quotation, LexicalVariable alternative)
        {

            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsCharA1QQ : PCondIsCharA1Q
    {
        protected PCondIsCharA1QQ (PrimitiveIsCharA1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsCharA1 predicate, Quotation consequent, Quotation alternative)
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

    class PCondIsCharA1SL : PCondIsCharA1
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsCharA1SL (PrimitiveIsCharA1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }
        internal static SCode Make (PrimitiveIsCharA1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsCharA1SL (predicate, consequent, alternative);

        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;

            if (!(ev0 is char)) {
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

    class PCondIsCharA1SQ : PCondIsCharA1
    {
        public readonly object alternativeValue;

        protected PCondIsCharA1SQ (PrimitiveIsCharA1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsCharA1 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsCharA1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsCharA1SQ.EvalStep");
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 is char)) {

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

    class PCondIsCharAL : PCondIsCharA
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsCharAL (PrimitiveIsCharA predicate, LexicalVariable consequent, SCode alternative)
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

            if (!(ev0 is char)) {
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

        internal static SCode Make (PrimitiveIsCharA predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsCharALL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsCharALQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsCharAL (predicate, consequent, alternative);
        }
    }

    class PCondIsCharAA : PCondIsCharAL
    {
        protected PCondIsCharAA (PrimitiveIsCharA predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        { }

        static public SCode Make (PrimitiveIsCharA predicate, Argument consequent, SCode alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

    }

    class PCondIsCharAL1 : PCondIsCharAL
    {
        protected PCondIsCharAL1 (PrimitiveIsCharA predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsCharA predicate, LexicalVariable1 consequent, SCode alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsCharALL : PCondIsCharAL
    {
        protected PCondIsCharALL (PrimitiveIsCharA predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsCharA predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsCharALQ : PCondIsCharAL
    {
        protected PCondIsCharALQ (PrimitiveIsCharA predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsCharA predicate, LexicalVariable consequent, Quotation alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsCharAQ : PCondIsCharA
    {
        public readonly object consequentValue;

        protected PCondIsCharAQ (PrimitiveIsCharA predicate, Quotation consequent, SCode alternative)
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


            if (!(ev0 is char)) {
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

        internal static SCode Make (PrimitiveIsCharA predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsCharAQL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsCharAQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsCharAQ (predicate, consequent, alternative);
        }
    }

    class PCondIsCharAQL : PCondIsCharAQ
    {
        protected PCondIsCharAQL (PrimitiveIsCharA predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        internal static SCode Make (PrimitiveIsCharA predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsCharAQL (predicate, consequent, alternative);
        }
    }

    class PCondIsCharAQQ : PCondIsCharAQ
    {
        protected PCondIsCharAQQ (PrimitiveIsCharA predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {

            throw new NotImplementedException ();
        }

        internal static SCode Make (PrimitiveIsCharA predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsCharAQQ (predicate, consequent, alternative);
        }
    }

    class PCondIsCharASL : PCondIsCharA
    {
        protected PCondIsCharASL (PrimitiveIsCharA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        internal static PCondIsCharA Make (PrimitiveIsCharA predicate, SCode consequent, LexicalVariable lexicalVariable)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsCharASQ : PCondIsCharA
    {
        public readonly object alternativeValue;

        protected PCondIsCharASQ (PrimitiveIsCharA predicate, SCode consequent, Quotation alternative)
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


            if (!(ev0 is char)) {

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

        internal static PCondIsCharA Make (PrimitiveIsCharA predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsCharASQ (predicate, consequent, alternative);
        }
    }

    class PCondIsCharL1 : PCondIsCharL
    {
        protected PCondIsCharL1 (PrimitiveIsCharL1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsCharL1 predicate, SCode consequent, SCode alternative)
        {
            return
       (consequent is LexicalVariable) ? PCondIsCharL1L.Make (predicate, (LexicalVariable) consequent, alternative)
       : (consequent is Quotation) ? PCondIsCharL1Q.Make (predicate, (Quotation) consequent, alternative)
       : (alternative is LexicalVariable) ? PCondIsCharL1SL.Make (predicate, consequent, (LexicalVariable) alternative)
       : (alternative is Quotation) ? PCondIsCharL1SQ.Make (predicate, consequent, (Quotation) alternative)
       : new PCondIsCharL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ("PCondIsCharL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();


            if (!(ev0 is char)) {
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

    class PCondIsCharL1L : PCondIsCharL1
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsCharL1L (PrimitiveIsCharL1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;

        }

        internal static SCode Make (PrimitiveIsCharL1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsCharL1LL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsCharL1LQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsCharL1L (predicate, consequent, alternative);
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

            if (!(ev0 is char)) {
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

    class PCondIsCharL1LL : PCondIsCharL1L
    {
        protected PCondIsCharL1LL (PrimitiveIsCharL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsCharL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsCharL1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsCharL1LQ : PCondIsCharL1L
    {
        public readonly object alternativeValue;

        protected PCondIsCharL1LQ (PrimitiveIsCharL1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsCharL1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsCharL1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 is char)) {

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

    class PCondIsCharL1Q : PCondIsCharL1
    {
        public readonly object consequentValue;

        protected PCondIsCharL1Q (PrimitiveIsCharL1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }
        internal static SCode Make (PrimitiveIsCharL1 predicate, Quotation consequent, SCode alternative)
        {
            return
    (alternative is LexicalVariable) ? PCondIsCharL1QL.Make (predicate, consequent, (LexicalVariable) alternative)
    : (alternative is Quotation) ? PCondIsCharL1QQ.Make (predicate, consequent, (Quotation) alternative)
    : new PCondIsCharL1Q (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsCharL1Q.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();


            if (!(ev0 is char)) {
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

    class PCondIsCharL1QL : PCondIsCharL1Q
    {
        protected PCondIsCharL1QL (PrimitiveIsCharL1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsCharL1 predicate, Quotation quotation, LexicalVariable alternative)
        {

            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsCharL1QQ : PCondIsCharL1Q
    {
        protected PCondIsCharL1QQ (PrimitiveIsCharL1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsCharL1 predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsCharL1QQ (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsCharL1SL : PCondIsCharL1
    {
        protected PCondIsCharL1SL (PrimitiveIsCharL1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsCharL1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsCharL1SL (predicate, consequent, alternative);

        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsCharL1SQ : PCondIsCharL1
    {
        public readonly object alternativeValue;

        protected PCondIsCharL1SQ (PrimitiveIsCharL1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsCharL1 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsCharL1SQ (predicate, consequent, alternative);
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


            if (!(ev0 is char)) {
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

    class PCondIsCharLL : PCondIsCharL
    {
        protected PCondIsCharLL (PrimitiveIsCharL predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsCharL predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsCharLLL.Make (predicate, consequent, alternative)
                : (alternative is Quotation) ? PCondIsCharLLQ.Make (predicate, consequent, alternative)
                : new PCondIsCharLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsCharLLL : PCondIsCharLL
    {
        protected PCondIsCharLLL (PrimitiveIsCharL predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsCharL predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsCharLLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsCharLLQ : PCondIsCharLL
    {
        protected PCondIsCharLLQ (PrimitiveIsCharL predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsCharL predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsCharLLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsCharLQ : PCondIsCharL
    {
        protected readonly object consequentValue;

        protected PCondIsCharLQ (PrimitiveIsCharL predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsCharL predicate, Quotation consequent, SCode alternative)
        {
            return
              (alternative is LexicalVariable) ? PCondIsCharLQL.Make (predicate, consequent, (LexicalVariable) alternative)
              : (alternative is Quotation) ? PCondIsCharLQQ.Make (predicate, consequent, (Quotation) alternative)
              : new PCondIsCharLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsCharLQ.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();


            if (!(ev0 is char)) {
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

    class PCondIsCharLQL : PCondIsCharLQ
    {
        protected PCondIsCharLQL (PrimitiveIsCharL predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsCharL predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsCharLQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsCharLQQ : PCondIsCharLQ
    {
        protected PCondIsCharLQQ (PrimitiveIsCharL predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsCharL predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsCharLQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsCharLSL : PCondIsCharL
    {
        protected PCondIsCharLSL (PrimitiveIsCharL predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsCharL predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsCharLSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsCharLSQ : PCondIsCharL
    {
        public readonly object alternativeValue;

        protected PCondIsCharLSQ (PrimitiveIsCharL predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsCharL predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsCharLSQ (predicate, consequent, alternative);
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

            if (!(ev0 is char)) {
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

    class PCondIsCharSL : PCondIsChar
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsCharSL (PrimitiveIsChar predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsChar predicate, LexicalVariable consequent, SCode alternative)
        {
            return
     (alternative is LexicalVariable) ? PCondIsCharSLL.Make (predicate, consequent, (LexicalVariable) alternative)
     : (alternative is Quotation) ? PCondIsCharSLQ.Make (predicate, consequent, (Quotation) alternative)
     : new PCondIsCharSL (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsCharFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is char)) {
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

    class PCondIsCharSLL : PCondIsCharSL
    {

        protected PCondIsCharSLL (PrimitiveIsChar predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsChar predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsCharSLQ : PCondIsCharSL
    {
        public readonly object alternativeValue;

        protected PCondIsCharSLQ (PrimitiveIsChar predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }


        public static SCode Make (PrimitiveIsChar predicate, LexicalVariable consequent, Quotation alternative)
        {

            return new PCondIsCharSLQ (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsCharFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is char)) {
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

    class PCondIsCharSQ : PCondIsChar
    {
        public readonly object consequentValue;

        protected PCondIsCharSQ (PrimitiveIsChar predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsChar predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsCharSQL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsCharSQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsCharSQ (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsCharFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is char)) {
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

    class PCondIsCharSQL : PCondIsCharSQ
    {
        protected PCondIsCharSQL (PrimitiveIsChar predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsChar predicate, Quotation consequent, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsCharSQQ : PCondIsCharSQ
    {
        public readonly object alternativeValue;

        protected PCondIsCharSQQ (PrimitiveIsChar predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }


        public static SCode Make (PrimitiveIsChar predicate, Quotation consequent, Quotation alternative)
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

            return new PCondIsCharSQQ (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsCharFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 is char)) {
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

    class PCondIsCharSSL : PCondIsChar
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsCharSSL (PrimitiveIsChar predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveIsChar predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsCharSSL (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsCharFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }



            if (!(ev0 is char)) {
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

    class PCondIsCharSSQ : PCondIsChar
    {
        public readonly object alternativeValue;

        protected PCondIsCharSSQ (PrimitiveIsChar predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsChar predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsCharSSQ (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsCharFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (!(ev0 is char)) {
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
