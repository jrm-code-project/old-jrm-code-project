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
                (predicate is PrimitiveIsPairA) ? PCondIsPairA.Make ((PrimitiveIsPairA) predicate, consequent, alternative) :
                 new PCondIsPair (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPair.EvalStep");
            NoteCalls (this.arg0);
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


            if (ev0 is Cons) {
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
    class PCondIsPairA : PCondIsPair
    {
        public readonly int predicateOffset;
        protected PCondIsPairA (PrimitiveIsPairA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateOffset = predicate.offset;
        }

        public static SCode Make (PrimitiveIsPairA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsPairA0) ? PCondIsPairA0.Make ((PrimitiveIsPairA0) predicate, consequent, alternative)
                : (predicate is PrimitiveIsPairA1) ? PCondIsPairA1.Make ((PrimitiveIsPairA1) predicate, consequent, alternative)
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
            NoteCalls ((SCode) expression);
#endif
            return true;
        }
    }

    [Serializable]
    class PCondIsPairA0 : PCondIsPairA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsPairA0 (PrimitiveIsPairA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsPairA0 predicate, SCode consequent, SCode alternative)
        {
            return
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
            NoteCalls ((SCode) expression);
#endif
            return true;
        }
    }

//    [Serializable]
//    class PCondIsPairA0A : PCondIsPairA0
//    {
//        protected PCondIsPairA0A (PrimitiveIsPairA0 predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsPairA0 predicate, Argument consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument0) ? PCondIsPairA0A0.Make (predicate, (Argument0) consequent, alternative) :
//                (consequent is Argument1) ? Unimplemented() :
//                (alternative is LexicalVariable) ? Unimplemented() :
//                (alternative is Quotation) ? Unimplemented() :
//                new PCondIsPairA0A (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            object ev0 = environment.Argument0Value;


//            if (!(ev0 is Cons)) {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;

//                return true;
//            }
//            else {
//                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCondIsPairA0A0 : PCondIsPairA0A
//    {
//        protected PCondIsPairA0A0 (PrimitiveIsPairA0 predicate, Argument0 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsPairA0 predicate, Argument0 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? PCondIsPairA0A0Q.Make (predicate, consequent, (Quotation) alternative) :
//                new PCondIsPairA0A0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsPairA0A0.EvalStep");
//#endif
//            object ev0 = environment.Argument0Value;
//            if (ev0 is Cons) {
//                answer = ev0;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCondIsPairA0A0Q : PCondIsPairA0A0
//    {
//        readonly object alternativeValue;

//        PCondIsPairA0A0Q (PrimitiveIsPairA0 predicate, Argument0 consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsPairA0 predicate, Argument0 consequent, Quotation alternative)
//        {
//            return
//                new PCondIsPairA0A0Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsPairA0A0Q.EvalStep");
//#endif
//            object ev0 = environment.Argument0Value;
//            answer = (ev0 is Cons) ? ev0 : this.alternativeValue;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PCondIsPairA0QQ : PCondIsPairA0Q
//    {
//        PCondIsPairA0QQ (PrimitiveIsPairA0 predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        { }
//        internal static SCode Make (PrimitiveIsPairA0 predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            return new PCondIsPairA0QQ (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//        }


//    }

//    [Serializable]
//    class PCondIsPairA0SA : PCondIsPairA0S
//    {
//        protected PCondIsPairA0SA (PrimitiveIsPairA0 predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsPairA0 predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCondIsPairA0SA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? PCondIsPairA0SA1.Make (predicate, consequent, (Argument1) alternative) :
//                new PCondIsPairA0SA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsPairA0SA.EvalStep");
//#endif
//            throw new NotImplementedException ();
//            object ev0 = environment.Argument0Value;

//            if (!(ev0 is Cons)) {
//                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);

//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }


//        }


//    }

//    [Serializable]
//    sealed class PCondIsPairA0SA0 : PCondIsPairA0SA
//    {
//        PCondIsPairA0SA0 (PrimitiveIsPairA0 predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsPairA0 predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new PCondIsPairA0SA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsPairA0SA1.EvalStep");
//#endif
//            object temp = environment.Argument0Value;
//            if (temp is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = temp;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCondIsPairA0SA1 : PCondIsPairA0SA
//    {
//        PCondIsPairA0SA1 (PrimitiveIsPairA0 predicate, SCode consequent, Argument1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsPairA0 predicate, SCode consequent, Argument1 alternative)
//        {
//            return
//                new PCondIsPairA0SA1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsPairA0SA1.EvalStep");
//#endif
//            if (environment.Argument0Value is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = environment.Argument1Value;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCondIsPairA0SQ : PCondIsPairA0
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
//#endif
//        public readonly object alternativeValue;

//        PCondIsPairA0SQ (PrimitiveIsPairA0 predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsPairA0 predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsPairA0SQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsPairA0SQ.EvalStep");
//#endif
//            if (environment.Argument0Value is Cons) {
//#if DEBUG
//                SCode.location = "-";
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//                SCode.location = "PCondIsPairA0SQ.EvalStep";
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = this.alternativeValue;
//                return false;
//            }
//        }
//    }

    [Serializable]
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

        internal static SCode Make (PrimitiveIsPairA1 predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondIsPairA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsPairA1.EvalStep");
#endif
            if (environment.Argument1Value is Cons) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsPairA1.EvalStep";
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
                SCode.location = "PCondIsPairA1.EvalStep";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

//    [Serializable]
//    class PCondIsPairA1Q : PCondIsPairA1
//    {
//        public readonly object consequentValue;
//        protected PCondIsPairA1Q (PrimitiveIsPairA1 predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }
//        internal static SCode Make (PrimitiveIsPairA1 predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCondIsPairA1QL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCondIsPairA1QQ.Make (predicate, consequent, (Quotation) alternative) :
//                new PCondIsPairA1Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsPairA1Q.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;

//            if (ev0 is Cons) {
//                answer = this.consequentValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }  
//        }
//    }

//    [Serializable]
//    class PCondIsPairA1QQ : PCondIsPairA1Q
//    {
//        protected PCondIsPairA1QQ (PrimitiveIsPairA1 predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        { }
//        internal static SCode Make (PrimitiveIsPairA1 predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            throw new NotImplementedException ();
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//        }


//    }


//    [Serializable]
//    class PCondIsPairA1SA : PCondIsPairA1S
//    {

//        protected PCondIsPairA1SA (PrimitiveIsPairA1 predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }
//        internal static SCode Make (PrimitiveIsPairA1 predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCondIsPairA1SA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? Unimplemented () :
//                new PCondIsPairA1SA (predicate, consequent, alternative);

//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("PCondIsPairA1SL.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;

//            if (!(ev0 is Cons)) {
//                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);

//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }

//        }


//    }

//    [Serializable]
//    sealed class PCondIsPairA1SA0 : PCondIsPairA1SA
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
//#endif

//        PCondIsPairA1SA0 (PrimitiveIsPairA1 predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsPairA1 predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new PCondIsPairA1SA0 (predicate, consequent, alternative);

//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsPairA1SA0.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;

//            if (ev0 is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = environment.Argument0Value;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCondIsPairA1SQ : PCondIsPairA1
//    {
//        public readonly object alternativeValue;

//        protected PCondIsPairA1SQ (PrimitiveIsPairA1 predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsPairA1 predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsPairA1SQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsPairA1SQ.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;


//            if (!(ev0 is Cons)) {

//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PCondIsPairAA : PCondIsPairA
//    {
//        protected PCondIsPairAA (PrimitiveIsPairA predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        { }

//        static public SCode Make (PrimitiveIsPairA predicate, Argument consequent, SCode alternative)
//        {
//            throw new NotImplementedException ();
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//        }

//    }

//    [Serializable]
//    class PCondIsPairAQ : PCondIsPairA
//    {
//        public readonly object consequentValue;

//        protected PCondIsPairAQ (PrimitiveIsPairA predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            #region EvalStepBody
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg0);
//#endif
//            object ev0 = environment.ArgumentValue (this.predicateOffset);


//            if (!(ev0 is Cons)) {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = this.consequentValue;
//                return false;
//            }
//            #endregion

//        }

//        internal static SCode Make (PrimitiveIsPairA predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCondIsPairAQL.Make (predicate, consequent, (LexicalVariable) alternative)
//                : (alternative is Quotation) ? PCondIsPairAQQ.Make (predicate, consequent, (Quotation) alternative)
//                : new PCondIsPairAQ (predicate, consequent, alternative);
//        }
//    }

//    [Serializable]
//    class PCondIsPairAQQ : PCondIsPairAQ
//    {
//        protected PCondIsPairAQQ (PrimitiveIsPairA predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {

//            throw new NotImplementedException ();
//        }

//        internal static SCode Make (PrimitiveIsPairA predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            return new PCondIsPairAQQ (predicate, consequent, alternative);
//        }
//    }

//    [Serializable]
//    class PCondIsPairASQ : PCondIsPairA
//    {
//        public readonly object alternativeValue;

//        protected PCondIsPairASQ (PrimitiveIsPairA predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsPairASQ.EvalStep");
//#endif
//            object ev0 = environment.ArgumentValue (predicateOffset);


//            if (!(ev0 is Cons)) {

//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//        }

//        internal static PCondIsPairA Make (PrimitiveIsPairA predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsPairASQ (predicate, consequent, alternative);
//        }
//    }

//    [Serializable]
//    class PCondIsPairSQ : PCondIsPair
//    {
//        public readonly object consequentValue;

//        protected PCondIsPairSQ (PrimitiveIsPair predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }

//        public static SCode Make (PrimitiveIsPair predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCondIsPairSQL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCondIsPairSQQ.Make (predicate, consequent, (Quotation) alternative) :
//                new PCondIsPairSQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            noteCalls (this.arg0);
//            SCode.location = "PCondIsPairSQ.EvalStep";
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//#if DEBUG
//                        SCode.location = "PCondIsPairSQ.EvalStep.1";
//#endif
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsPairFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            if (ev0 is Cons) {
//                answer = this.consequentValue;
//                return false;
//            } 
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//        }
//    }


//    [Serializable]
//    class PCondIsPairSQQ : PCondIsPairSQ
//    {
//        public readonly object alternativeValue;

//        protected PCondIsPairSQQ (PrimitiveIsPair predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }


//        public static SCode Make (PrimitiveIsPair predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }

//            return new PCondIsPairSQQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            #region EvalStepBody
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsPairFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }


//            if (!(ev0 is Cons)) {
//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//                answer = this.consequentValue;
//                return false;
//            }
//            #endregion
//        }
//    }



//    [Serializable] class PCondIsPairSSA : PCondIsPairSSL
//    {

//        protected PCondIsPairSSA (PrimitiveIsPair predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveIsPair predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCondIsPairSSA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? Unimplemented () :
//                new PCondIsPairSSA (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("PCondIsPairSSL.EvalStep");
//            noteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsPairFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }



//            if (!(ev0 is Cons)) {
//                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCondIsPairSSA0 : PCondIsPairSSA
//    {

//        PCondIsPairSSA0 (PrimitiveIsPair predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveIsPair predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new PCondIsPairSSA0 (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            noteCalls (this.arg0);
//            SCode.location = "PCondIsPairSSA0.EvalStep";
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsPairFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            if (ev0 is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = environment.Argument0Value;
//                return false;
//            }
//        }
//    }


//    [Serializable]
//    sealed class PCondIsPairSSQ : PCondIsPair
//    {
//        public readonly object alternativeValue;

//        PCondIsPairSSQ (PrimitiveIsPair predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (PrimitiveIsPair predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsPairSSQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsPairSSQ.EvalStep");
//            noteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsPairFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            if (ev0 is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = this.alternativeValue;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCondIsPairCar : PCondIsPair
//    {
//#if DEBUG
//        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected PCondIsPairCar (PrimitiveIsPairCar predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveIsPairCar predicate, SCode consequent, SCode alternative)
//        {
//            return
//                //(predicate is PrimitiveIsPairCaar) ? PCondIsPairCarCar.Make ((PrimitiveIsPairCarCar) predicate, consequent, alternative) :
//                (predicate is PrimitiveIsPairCarL) ? PCondIsPairCarL.Make ((PrimitiveIsPairCarL) predicate, consequent, alternative) :
//                (consequent is LexicalVariable) ? PCondIsPairCarSL.Make (predicate, (LexicalVariable) consequent, alternative) :
//                (consequent is Quotation) ? PCondIsPairCarSQ.Make (predicate, (Quotation) consequent, alternative) :
//                 (alternative is LexicalVariable) ? PCondIsPairCarSSL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                 (alternative is Quotation) ? PCondIsPairCarSSQ.Make (predicate, consequent, (Quotation) alternative) :
//                 new PCondIsPairCar (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsPairCar.EvalStep");
//            noteCalls (this.arg0);
//            arg0TypeHistogram.Note (this.arg0Type);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsPairCarFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }


//            if (!(ev0 is Cons)) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//        }
//    }



//    [Serializable] class PCondIsPairCarA : PCondIsPairCarL
//    {
//        protected PCondIsPairCarA (PrimitiveIsPairCarA predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveIsPairCarA predicate, SCode consequent, SCode alternative)
//        {
//            return
//                (predicate is PrimitiveIsPairCarA0) ? PCondIsPairCarA0.Make ((PrimitiveIsPairCarA0) predicate, consequent, alternative)
//                : (predicate is PrimitiveIsPairCarA1) ? PCondIsPairCarA1.Make ((PrimitiveIsPairCarA1) predicate, consequent, alternative)
//                : (consequent is LexicalVariable) ? PCondIsPairCarAL.Make (predicate, (LexicalVariable) consequent, alternative)
//                : (consequent is Quotation) ? PCondIsPairCarAQ.Make (predicate, (Quotation) consequent, alternative)
//                : (alternative is LexicalVariable) ? PCondIsPairCarASL.Make (predicate, consequent, (LexicalVariable) alternative)
//                : (alternative is Quotation) ? PCondIsPairCarASQ.Make (predicate, consequent, (Quotation) alternative)
//                : new PCondIsPairCarA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsPairCarA.EvalStep");
//#endif
//            expression = environment.ArgumentValue (this.predicateOffset) is Cons
//                ? this.consequent
//                : this.alternative;
//            answer = null;
//#if DEBUG
//            noteCalls ((SCode) expression);
//#endif
//            return true;
//        }
//    }

//    [Serializable] class PCondIsPairCarA0 : PCondIsPairCarA
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
//#endif
//        protected PCondIsPairCarA0 (PrimitiveIsPairCarA0 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsPairCarA0 predicate, SCode consequent, SCode alternative)
//        {
//            return
//                (consequent is LexicalVariable) ? PCondIsPairCarA0L.Make (predicate, (LexicalVariable) consequent, alternative) :
//                //(consequent is SimpleLet1CarA0) ? PCondIsPairCarA0SimpleLet1CarA0.Make (predicate, (SimpleLet1CarA0) consequent, alternative) :
//                //(consequent is PCondIsEqCarA0LA0) ? PCondIsPairCarA0Fragment6.Make (predicate, (PCondIsEqCarA0LA0) consequent, alternative) :
//                //: (consequent is SComb1Fragment3) ? PCondIsPairCarFragment4.Make (predicate, (SComb1Fragment3) consequent, alternative) :
//                (consequent is Quotation) ? PCondIsPairCarA0Q.Make (predicate, (Quotation) consequent, alternative) :
//                (alternative is LexicalVariable) ? PCondIsPairCarA0SL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCondIsPairCarA0SQ.Make (predicate, consequent, (Quotation) alternative) :
//               new PCondIsPairCarA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            SCode.location = "PCondIsPairCarA0.EvalStep";
//#endif
//            Cons temp = environment.Argument0Value as Cons;
//            if (temp == null) throw new NotImplementedException ();
//            if (temp.Car is Cons) {
//                expression = this.consequent;
//#if DEBUG
//                SCode.location = "-";
//                consequentTypeHistogram.Note (this.consequentType);
//                noteCalls (this.consequent);
//#endif
//            }
//            else {
//                expression = this.alternative;
//#if DEBUG
//                SCode.location = "-";
//                alternativeTypeHistogram.Note (this.alternativeType);
//                noteCalls (this.alternative);
//#endif
//            }
//            answer = null;
//            return true;
//        }
//    }



//    [Serializable] class PCondIsPairCarA0A : PCondIsPairCarA0L
//    {
//        protected PCondIsPairCarA0A (PrimitiveIsPairCarA0 predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsPairCarA0 predicate, Argument consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument0) ? PCondIsPairCarA0A0.Make (predicate, (Argument0) consequent, alternative) :
//                (consequent is Argument1) ? Unimplemented() :
//                (alternative is LexicalVariable) ? Unimplemented() :
//                (alternative is Quotation) ? Unimplemented() :
//                new PCondIsPairCarA0A (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ();
//#endif
//            object ev0 = environment.Argument0Value;


//            if (!(ev0 is Cons)) {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;

//                return true;
//            }
//            else {
//                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable] class PCondIsPairCarA0A0 : PCondIsPairCarA0A
//    {
//        protected PCondIsPairCarA0A0 (PrimitiveIsPairCarA0 predicate, Argument0 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsPairCarA0 predicate, Argument0 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? PCondIsPairCarA0A0Q.Make (predicate, consequent, (Quotation) alternative) :
//                new PCondIsPairCarA0A0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsPairCarA0A0.EvalStep");
//#endif
//            object ev0 = environment.Argument0Value;
//            if (ev0 is Cons) {
//                answer = ev0;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCondIsPairCarA0A0Q : PCondIsPairCarA0A0
//    {
//        readonly object alternativeValue;

//        PCondIsPairCarA0A0Q (PrimitiveIsPairCarA0 predicate, Argument0 consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsPairCarA0 predicate, Argument0 consequent, Quotation alternative)
//        {
//            return
//                new PCondIsPairCarA0A0Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsPairCarA0A0Q.EvalStep");
//#endif
//            object ev0 = environment.Argument0Value;
//            answer = (ev0 is Cons) ? ev0 : this.alternativeValue;
//            return false;
//        }
//    }



//    [Serializable] class PCondIsPairCarA0Q : PCondIsPairCarA0
//    {
//        public readonly object consequentValue;

//        protected PCondIsPairCarA0Q (PrimitiveIsPairCarA0 predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsPairCarA0 predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCondIsPairCarA0QL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCondIsPairCarA0QQ.Make (predicate, consequent, (Quotation) alternative) :
//                new PCondIsPairCarA0Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsPairCarA0Q.EvalStep");
//#endif
//            Cons temp = environment.Argument0Value as Cons;
//            if (temp == null) throw new NotImplementedException ();
//            if (temp.Car is Cons) {
//                answer = this.consequentValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//        }
//    }


//    [Serializable] class PCondIsPairCarA0QQ : PCondIsPairCarA0Q
//    {
//        protected PCondIsPairCarA0QQ (PrimitiveIsPairCarA0 predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        { }
//        internal static SCode Make (PrimitiveIsPairCarA0 predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            return new PCondIsPairCarA0QQ (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//        }


//    }



//    [Serializable] class PCondIsPairCarA0SA : PCondIsPairCarA0SL
//    {
//        protected PCondIsPairCarA0SA (PrimitiveIsPairCarA0 predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsPairCarA0 predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCondIsPairCarA0SA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? PCondIsPairCarA0SA1.Make (predicate, consequent, (Argument1) alternative) :
//                new PCondIsPairCarA0SA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();

//#if DEBUG
//            Warm ("PCondIsPairCarA0SA.EvalStep");
//#endif
//            throw new NotImplementedException ();
//            object ev0 = environment.Argument0Value;

//            if (!(ev0 is Cons)) {
//                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);

//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }


//        }


//    }

//    [Serializable]
//    sealed class PCondIsPairCarA0SA0 : PCondIsPairCarA0SA
//    {
//        PCondIsPairCarA0SA0 (PrimitiveIsPairCarA0 predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsPairCarA0 predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new PCondIsPairCarA0SA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();

//#if DEBUG
//            Warm ("PCondIsPairCarA0SA1.EvalStep");
//#endif
//            object temp = environment.Argument0Value;
//            if (temp is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = temp;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCondIsPairCarA0SA1 : PCondIsPairCarA0SA
//    {
//        PCondIsPairCarA0SA1 (PrimitiveIsPairCarA0 predicate, SCode consequent, Argument1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsPairCarA0 predicate, SCode consequent, Argument1 alternative)
//        {
//            return
//                new PCondIsPairCarA0SA1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();

//#if DEBUG
//            Warm ("PCondIsPairCarA0SA1.EvalStep");
//#endif
//            if (environment.Argument0Value is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = environment.Argument1Value;
//                return false;
//            }
//        }
//    }



//    [Serializable]
//    sealed class PCondIsPairCarA0SQ : PCondIsPairCarA0
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
//#endif
//        public readonly object alternativeValue;

//        PCondIsPairCarA0SQ (PrimitiveIsPairCarA0 predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsPairCarA0 predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsPairCarA0SQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsPairCarA0SQ.EvalStep");
//#endif
//            Cons temp = environment.Argument0Value as Cons;
//            if (temp == null) throw new NotImplementedException ();
//            if (temp.Car is Cons) {
//#if DEBUG
//                SCode.location = "-";
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//                SCode.location = "PCondIsPairCarA0SQ.EvalStep";
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = this.alternativeValue;
//                return false;
//            }
//        }
//    }

//    [Serializable] class PCondIsPairCarA1 : PCondIsPairCarA
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram <Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected PCondIsPairCarA1 (PrimitiveIsPairCarA1 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();

//#if DEBUG
//            Warm ("PCondIsPairCarA1.EvalStep");
//#endif
//            if (environment.Argument1Value is Cons) {
//#if DEBUG
//                SCode.location = "-";
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//                SCode.location = "PCondIsPairCarA1.EvalStep.1";
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            } else {
//#if DEBUG
//                SCode.location = "-";
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//                SCode.location = "PCondIsPairCarA1.EvalStep.2";
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//        }

//        internal static SCode Make (PrimitiveIsPairCarA1 predicate, SCode consequent, SCode alternative)
//        {
//            return
//                (consequent is LexicalVariable) ? PCondIsPairCarA1L.Make (predicate, (LexicalVariable) consequent, alternative)
//                : (consequent is Quotation) ? PCondIsPairCarA1Q.Make (predicate, (Quotation) consequent, alternative)
//                : (alternative is LexicalVariable) ? PCondIsPairCarA1SL.Make (predicate, consequent, (LexicalVariable) alternative)
//                : (alternative is Quotation) ? PCondIsPairCarA1SQ.Make (predicate, consequent, (Quotation) alternative)
//                : new PCondIsPairCarA1 (predicate, consequent, alternative);
//        }
//    }



//    [Serializable] class PCondIsPairCarA1Q : PCondIsPairCarA1
//    {
//        public readonly object consequentValue;
//        protected PCondIsPairCarA1Q (PrimitiveIsPairCarA1 predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }
//        internal static SCode Make (PrimitiveIsPairCarA1 predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCondIsPairCarA1QL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCondIsPairCarA1QQ.Make (predicate, consequent, (Quotation) alternative) :
//                new PCondIsPairCarA1Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsPairCarA1Q.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;

//            if (ev0 is Cons) {
//                answer = this.consequentValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }  
//        }
//    }


//    [Serializable] class PCondIsPairCarA1SA : PCondIsPairCarA1SL
//    {

//        protected PCondIsPairCarA1SA (PrimitiveIsPairCarA1 predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }
//        internal static SCode Make (PrimitiveIsPairCarA1 predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCondIsPairCarA1SA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? Unimplemented () :
//                new PCondIsPairCarA1SA (predicate, consequent, alternative);

//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("PCondIsPairCarA1SL.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;

//            if (!(ev0 is Cons)) {
//                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);

//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }

//        }


//    }

//    [Serializable]
//    sealed class PCondIsPairCarA1SA0 : PCondIsPairCarA1SA
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
//#endif

//        PCondIsPairCarA1SA0 (PrimitiveIsPairCarA1 predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsPairCarA1 predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new PCondIsPairCarA1SA0 (predicate, consequent, alternative);

//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsPairCarA1SA0.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;

//            if (ev0 is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = environment.Argument0Value;
//                return false;
//            }
//        }
//    }

//    [Serializable] class PCondIsPairCarA1SQ : PCondIsPairCarA1
//    {
//        public readonly object alternativeValue;

//        protected PCondIsPairCarA1SQ (PrimitiveIsPairCarA1 predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsPairCarA1 predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsPairCarA1SQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsPairCarA1SQ.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;


//            if (!(ev0 is Cons)) {

//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//        }
//    }



//    [Serializable] class PCondIsPairCarAA : PCondIsPairCarAL
//    {
//        protected PCondIsPairCarAA (PrimitiveIsPairCarA predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        { }

//        static public SCode Make (PrimitiveIsPairCarA predicate, Argument consequent, SCode alternative)
//        {
//            throw new NotImplementedException ();
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//        }

//    }



//    [Serializable] class PCondIsPairCarAQ : PCondIsPairCarA
//    {
//        public readonly object consequentValue;

//        protected PCondIsPairCarAQ (PrimitiveIsPairCarA predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//            #region EvalStepBody
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg0);
//#endif
//            object ev0 = environment.ArgumentValue (this.predicateOffset);


//            if (!(ev0 is Cons)) {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = this.consequentValue;
//                return false;
//            }
//            #endregion

//        }

//        internal static SCode Make (PrimitiveIsPairCarA predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCondIsPairCarAQL.Make (predicate, consequent, (LexicalVariable) alternative)
//                : (alternative is Quotation) ? PCondIsPairCarAQQ.Make (predicate, consequent, (Quotation) alternative)
//                : new PCondIsPairCarAQ (predicate, consequent, alternative);
//        }
//    }



//    [Serializable] class PCondIsPairCarAQQ : PCondIsPairCarAQ
//    {
//        protected PCondIsPairCarAQQ (PrimitiveIsPairCarA predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {

//            throw new NotImplementedException ();
//        }

//        internal static SCode Make (PrimitiveIsPairCarA predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            return new PCondIsPairCarAQQ (predicate, consequent, alternative);
//        }
//    }


//    [Serializable] class PCondIsPairCarASQ : PCondIsPairCarA
//    {
//        public readonly object alternativeValue;

//        protected PCondIsPairCarASQ (PrimitiveIsPairCarA predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsPairCarASQ.EvalStep");
//#endif
//            object ev0 = environment.ArgumentValue (predicateOffset);


//            if (!(ev0 is Cons)) {

//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//        }

//        internal static PCondIsPairCarA Make (PrimitiveIsPairCarA predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsPairCarASQ (predicate, consequent, alternative);
//        }
//    }


//    [Serializable]
//    class PCondIsPairCarSQ : PCondIsPairCar
//    {
//        public readonly object consequentValue;

//        protected PCondIsPairCarSQ (PrimitiveIsPairCar predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }

//        public static SCode Make (PrimitiveIsPairCar predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCondIsPairCarSQL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCondIsPairCarSQQ.Make (predicate, consequent, (Quotation) alternative) :
//                new PCondIsPairCarSQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("-");
//            noteCalls (this.arg0);
//            SCode.location = "PCondIsPairCarSQ.EvalStep";
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//#if DEBUG
//                        SCode.location = "PCondIsPairCarSQ.EvalStep.1";
//#endif
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsPairCarFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            if (ev0 is Cons) {
//                answer = this.consequentValue;
//                return false;
//            } 
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//        }
//    }



//    [Serializable]
//    class PCondIsPairCarSQQ : PCondIsPairCarSQ
//    {
//        public readonly object alternativeValue;

//        protected PCondIsPairCarSQQ (PrimitiveIsPairCar predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }


//        public static SCode Make (PrimitiveIsPairCar predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }

//            return new PCondIsPairCarSQQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//            #region EvalStepBody
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsPairCarFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }


//            if (!(ev0 is Cons)) {
//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//                answer = this.consequentValue;
//                return false;
//            }
//            #endregion
//        }
//    }



//    [Serializable] class PCondIsPairCarSSA : PCondIsPairCarSSL
//    {

//        protected PCondIsPairCarSSA (PrimitiveIsPairCar predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveIsPairCar predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCondIsPairCarSSA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? Unimplemented () :
//                new PCondIsPairCarSSA (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("PCondIsPairCarSSL.EvalStep");
//            noteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsPairCarFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }



//            if (!(ev0 is Cons)) {
//                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCondIsPairCarSSA0 : PCondIsPairCarSSA
//    {

//        PCondIsPairCarSSA0 (PrimitiveIsPairCar predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveIsPairCar predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new PCondIsPairCarSSA0 (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("-");
//            noteCalls (this.arg0);
//            SCode.location = "PCondIsPairCarSSA0.EvalStep";
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsPairCarFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            if (ev0 is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = environment.Argument0Value;
//                return false;
//            }
//        }
//    }


//    [Serializable]
//    sealed class PCondIsPairCarSSQ : PCondIsPairCar
//    {
//        public readonly object alternativeValue;

//        PCondIsPairCarSSQ (PrimitiveIsPairCar predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (PrimitiveIsPairCar predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsPairCarSSQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsPairCarSSQ.EvalStep");
//            noteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsPairCarFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            if (ev0 is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = this.alternativeValue;
//                return false;
//           }
//        }
//    }

}
