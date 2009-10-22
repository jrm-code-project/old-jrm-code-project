using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PCondIsNull : PCond1
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNull (PrimitiveIsNull predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNull predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsNullA) ? PCondIsNullA.Make ((PrimitiveIsNullA) predicate, consequent, alternative) :
                new PCondIsNull (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCondIsNull.EvalStep";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsNull.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ev0 == null) {
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
    class PCondIsNullA : PCondIsNull
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int predicateArgumentOffset;

        protected PCondIsNullA (PrimitiveIsNullA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateArgumentOffset = predicate.arg0Offset;
        }

        public static SCode Make (PrimitiveIsNullA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsNullA0) ? PCondIsNullA0.Make ((PrimitiveIsNullA0) predicate, consequent, alternative) :
                (predicate is PrimitiveIsNullA1) ? PCondIsNullA1.Make ((PrimitiveIsNullA1) predicate, consequent, alternative) :
                new PCondIsNullA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA.EvalStep");
#endif
            expression = environment.ArgumentValue (this.predicateArgumentOffset) == null
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
    class PCondIsNullA0 : PCondIsNullA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullA0 (PrimitiveIsNullA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsNullA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Quotation) ? PCondIsNullA0Q.Make (predicate, (Quotation) consequent, alternative) :
                new PCondIsNullA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0.EvalStep");
#endif
            if (environment.Argument0Value == null) {
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
    class PCondIsNullA0Q : PCondIsNullA0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondIsNullA0Q (PrimitiveIsNullA0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        internal static SCode Make (PrimitiveIsNullA0 predicate, Quotation consequent, SCode alternative)
        {
            return
                new PCondIsNullA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0Q.EvalStep");
#endif
            if (environment.Argument0Value == null) {
                expression = this.consequent;
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
    class PCondIsNullA1 : PCondIsNullA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullA1 (PrimitiveIsNullA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsNullA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsNullA1A.Make (predicate, (Argument) consequent, alternative) :
                new PCondIsNullA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA1.EvalStep");
#endif
            if (environment.Argument1Value == null) {
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
    class PCondIsNullA1A : PCondIsNullA1
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCondIsNullA1A (PrimitiveIsNullA1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        internal static SCode Make (PrimitiveIsNullA1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsNullA1A0.Make (predicate, (Argument0) consequent, alternative) :
                new PCondIsNullA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA1A.EvalStep");
#endif
            if (environment.Argument1Value == null) {
                answer = environment.ArgumentValue(this.consequentOffset);
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
    class PCondIsNullA1A0 : PCondIsNullA1A
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullA1A0 (PrimitiveIsNullA1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsNullA1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                new PCondIsNullA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA1A0.EvalStep");
#endif
            if (environment.Argument1Value == null) {
                answer = environment.Argument0Value;
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

//    [Serializable]
//    class PCondIsNullA0A : PCondIsNullA0L
//    {
//        protected PCondIsNullA0A (PrimitiveIsNullA0 predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsNullA0 predicate, Argument consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument0) ? PCondIsNullA0A0.Make (predicate, (Argument0) consequent, alternative) :
//                (consequent is Argument1) ? PCondIsNullA0A1.Make (predicate, (Argument1) consequent, alternative) :
//                (alternative is LexicalVariable) ? Unimplemented() :
//                (alternative is Quotation) ? PCondIsNullA0AQ.Make (predicate, consequent, (Quotation) alternative) :
//                new PCondIsNullA0A (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsNullA0A.EvalStep");
//#endif
//            if (environment.Argument0Value == null) {
//                answer = environment.ArgumentValue (this.consequentOffset);
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
//    class PCondIsNullA0A0 : PCondIsNullA0A
//    {

//        protected PCondIsNullA0A0 (PrimitiveIsNullA0 predicate, Argument0 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsNullA0 predicate, Argument0 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? Unimplemented () :
//                new PCondIsNullA0A0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("PCondIsNullA0L.EvalStep");
//#endif
//            object ev0 = environment.Argument0Value;


//            if (!(ev0 == null)) {
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
//    class PCondIsNullA0A1 : PCondIsNullA0A
//    {
//        protected PCondIsNullA0A1 (PrimitiveIsNullA0 predicate, Argument1 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsNullA0 predicate, Argument1 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? Unimplemented() :
//                new PCondIsNullA0A1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsNullA0A1.EvalStep");
//#endif
//            if (environment.Argument0Value == null) {
//                answer = environment.Argument1Value;
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
//    sealed class PCondIsNullA0AQ : PCondIsNullA0A
//    {
//        public readonly object alternativeValue;

//        PCondIsNullA0AQ (PrimitiveIsNullA0 predicate, Argument consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsNullA0 predicate, Argument consequent, Quotation alternative)
//        {
//            return
//                new PCondIsNullA0AQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsNullA0AQ.EvalStep");
//#endif
//            object ev0 = environment.Argument0Value;
//            answer = (ev0 == null) ? environment.ArgumentValue (this.consequentOffset) : this.alternativeValue;
//            return false;
//        }
//    }

//    [Serializable]
//    class PCondIsNullA0Q : PCondIsNullA0
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
//#endif
//        public readonly object consequentValue;

//        protected PCondIsNullA0Q (PrimitiveIsNullA0 predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsNullA0 predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCondIsNullA0QL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCondIsNullA0QQ.Make (predicate, consequent, (Quotation) alternative) :
//                new PCondIsNullA0Q (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsNullA0Q.EvalStep");
//#endif
//            if (environment.Argument0Value == null) {
//                answer = this.consequentValue;
//                return false;
//            } 
//            else {
//#if DEBUG
//                SCode.location = "-";
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//                SCode.location = "PCondIsNullA0Q.EvalStep";
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PCondIsNullA0QQ : PCondIsNullA0Q
//    {
//        protected PCondIsNullA0QQ (PrimitiveIsNullA0 predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        { }
//        internal static SCode Make (PrimitiveIsNullA0 predicate, Quotation consequent, Quotation alternative)
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
//            return new PCondIsNullA0QQ (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//        }


//    }

//    [Serializable]
//    class PCondIsNullA0SQ : PCondIsNullA0
//    {
//        public readonly object alternativeValue;

//        protected PCondIsNullA0SQ (PrimitiveIsNullA0 predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsNullA0 predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsNullA0SQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsNullA0SQ.EvalStep");
//#endif
//            object ev0 = environment.Argument0Value;


//            if (!(ev0 == null)) {

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
//    class PCondIsNullA1 : PCondIsNullA
//    {
//        protected PCondIsNullA1 (PrimitiveIsNullA1 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsNullA1 predicate, SCode consequent, SCode alternative)
//        {
//            return
//                (consequent is LexicalVariable) ? PCondIsNullA1L.Make (predicate, (LexicalVariable) consequent, alternative) :
//                (consequent is Quotation) ? PCondIsNullA1Q.Make (predicate, (Quotation) consequent, alternative):
//                (alternative is LexicalVariable) ? PCondIsNullA1SL.Make (predicate, consequent, (LexicalVariable) alternative):
//                (alternative is Quotation) ? PCondIsNullA1SQ.Make (predicate, consequent, (Quotation) alternative):
//                new PCondIsNullA1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsNullA1.EvalStep");
//#endif
//            if (environment.Argument1Value == null) {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
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

//     }

//    [Serializable]
//    class PCondIsNullA1A : PCondIsNullA1L
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected PCondIsNullA1A (PrimitiveIsNullA1 predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsNullA1 predicate, Argument consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument0) ? PCondIsNullA1A0.Make (predicate, (Argument0) consequent, alternative) :
//                (consequent is Argument1) ? Unimplemented () :
//                (alternative is LexicalVariable) ? Unimplemented() :
//                (alternative is Quotation) ? Unimplemented() :
//                new PCondIsNullA1A (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsNullA1A.EvalStep");
//#endif
//            if (environment.Argument1Value == null) {
//                answer = environment.ArgumentValue (this.consequentOffset);
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PCondIsNullA1A0 : PCondIsNullA1A
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected PCondIsNullA1A0 (PrimitiveIsNullA1 predicate, Argument0 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsNullA1 predicate, Argument0 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? Unimplemented () :
//                new PCondIsNullA1A0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsNullA1A0.EvalStep");
//#endif
//            if (environment.Argument1Value == null) {
//                answer = environment.Argument0Value;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PCondIsNullA1Q : PCondIsNullA1
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
//#endif
//        public readonly object consequentValue;

//        protected PCondIsNullA1Q (PrimitiveIsNullA1 predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsNullA1 predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCondIsNullA1QL.Make (predicate, consequent, (LexicalVariable) alternative) : 
//                (alternative is Quotation) ? PCondIsNullA1QQ.Make (predicate, consequent, (Quotation) alternative) :
//                new PCondIsNullA1Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsNullA1Q.EvalStep");
//#endif
//            if (environment.Argument1Value == null) {
//                answer = this.consequentValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                SCode.location = "-";
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//                SCode.location = "PCondIsNullA1Q.EvalStep.1";
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCondIsNullA1QQ : PCondIsNullA1Q
//    {
//        PCondIsNullA1QQ (PrimitiveIsNullA1 predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        { }
//        internal static SCode Make (PrimitiveIsNullA1 predicate, Quotation consequent, Quotation alternative)
//        {
//            throw new NotImplementedException ();
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class PCondIsNullA1SQ : PCondIsNullA1
//    {
//        public readonly object alternativeValue;

//        protected PCondIsNullA1SQ (PrimitiveIsNullA1 predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsNullA1 predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsNullA1SQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            object ev0 = environment.Argument1Value;


//            if (!(ev0 == null)) {

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
//    class PCondIsNullAA : PCondIsNullA
//    {
//        protected PCondIsNullAA (PrimitiveIsNullA predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        { }

//        static public SCode Make (PrimitiveIsNullA predicate, Argument consequent, SCode alternative)
//        {
//            throw new NotImplementedException ();
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//        }

//    }

//    [Serializable]
//    class PCondIsNullAQ : PCondIsNullA
//    {
//        public readonly object consequentValue;

//        protected PCondIsNullAQ (PrimitiveIsNullA predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            #region EvalStepBody
//#if DEBUG
//            Warm ("PCondIsNullAQ.EvalStep");
//#endif
//            object ev0 = environment.ArgumentValue (this.predicateOffset);


//            if (!(ev0 == null)) {
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

//        internal static SCode Make (PrimitiveIsNullA predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCondIsNullAQL.Make (predicate, consequent, (LexicalVariable) alternative)
//                : (alternative is Quotation) ? PCondIsNullAQQ.Make (predicate, consequent, (Quotation) alternative)
//                : new PCondIsNullAQ (predicate, consequent, alternative);
//        }
//    }

//    [Serializable]
//    class PCondIsNullAQQ : PCondIsNullAQ
//    {
//        protected PCondIsNullAQQ (PrimitiveIsNullA predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {

//            throw new NotImplementedException ();
//        }

//        internal static SCode Make (PrimitiveIsNullA predicate, Quotation consequent, Quotation alternative)
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
//            return new PCondIsNullAQQ (predicate, consequent, alternative);
//        }
//    }

//    [Serializable]
//    class PCondIsNullASQ : PCondIsNullA
//    {
//        public readonly object alternativeValue;

//        protected PCondIsNullASQ (PrimitiveIsNullA predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsNullASQ.EvalStep");
//#endif
//            object ev0 = environment.ArgumentValue (predicateOffset);


//            if (!(ev0 == null)) {

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

//        internal static PCondIsNullA Make (PrimitiveIsNullA predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsNullASQ (predicate, consequent, alternative);
//        }
//    }


//    [Serializable]
//    class PCondIsNullSQ : PCondIsNull
//    {
//        public readonly object consequentValue;

//        protected PCondIsNullSQ (PrimitiveIsNull predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }

//        public static SCode Make (PrimitiveIsNull predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCondIsNullSQL.Make (predicate, consequent, (LexicalVariable) alternative)
//                : (alternative is Quotation) ? PCondIsNullSQQ.Make (predicate, consequent, (Quotation) alternative)
//                : new PCondIsNullSQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            noteCalls (this.arg0);
//            SCode.location = "PCondIsNullSQ.EvalStep";
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }


//            if (!(ev0 == null)) {
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
//        }
//    }


//    [Serializable]
//    class PCondIsNullSQA : PCondIsNullSQ
//    {
//        protected PCondIsNullSQA (PrimitiveIsNull predicate, Quotation consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }


//        public static SCode Make (PrimitiveIsNull predicate, Quotation consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCondIsNullSQA0.Make (predicate, consequent, (Argument0) alternative) :
//                new PCondIsNullSQA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    sealed class PCondIsNullSQA0 : PCondIsNullSQA
//    {
        
//        PCondIsNullSQA0 (PrimitiveIsNull predicate, Quotation consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }


//        public static SCode Make (PrimitiveIsNull predicate, Quotation consequent, Argument0 alternative)
//        {
//            return new PCondIsNullSQA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class PCondIsNullSQQ : PCondIsNullSQ
//    {
//        public readonly object alternativeValue;

//        protected PCondIsNullSQQ (PrimitiveIsNull predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (PrimitiveIsNull predicate, Quotation consequent, Quotation alternative)
//        {
//            return new PCondIsNullSQQ (predicate, consequent, alternative);
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
//                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }


//            if (!(ev0 == null)) {
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

}

