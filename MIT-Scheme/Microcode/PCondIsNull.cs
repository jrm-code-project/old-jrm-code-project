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
                (predicate is PrimitiveIsNullL) ? PCondIsNullL.Make ((PrimitiveIsNullL) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCondIsNullSL.Make (predicate, (LexicalVariable) consequent, alternative):
                (consequent is Quotation) ? PCondIsNullSQ.Make (predicate, (Quotation) consequent, alternative):
                (alternative is LexicalVariable) ? PCondIsNullSSL.Make (predicate, consequent, (LexicalVariable) alternative):
                (alternative is Quotation) ? PCondIsNullSSQ.Make (predicate, consequent, (Quotation) alternative):
                new PCondIsNull (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCondIsNull.EvalStep";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
                        SCode.location = "PCondIsNull.EvalStep.1";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 == null)) {
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
    class PCondIsNullL : PCondIsNull
    {
        public readonly object predicateName;
        public readonly int predicateDepth;
        public readonly int predicateOffset;

        protected PCondIsNullL (PrimitiveIsNullL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateName = ((LexicalVariable) predicate.Operand).Name;
            this.predicateDepth = ((LexicalVariable) predicate.Operand).Depth;
            this.predicateOffset = ((LexicalVariable) predicate.Operand).Offset;
        }

        public static SCode Make (PrimitiveIsNullL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsNullA) ? PCondIsNullA.Make ((PrimitiveIsNullA) predicate, consequent, alternative) :
                (predicate is PrimitiveIsNullL1) ? PCondIsNullL1.Make ((PrimitiveIsNullL1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCondIsNullLL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondIsNullLQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCondIsNullLSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondIsNullLSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

            if (ev0 == null) {
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

    [Serializable]
    class PCondIsNullA : PCondIsNullL
    {
        protected PCondIsNullA (PrimitiveIsNullA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsNullA0) ? PCondIsNullA0.Make ((PrimitiveIsNullA0) predicate, consequent, alternative) :
                (predicate is PrimitiveIsNullA1) ? PCondIsNullA1.Make ((PrimitiveIsNullA1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCondIsNullAL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondIsNullAQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondIsNullASQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA.EvalStep");
#endif
            expression = environment.ArgumentValue (this.predicateOffset) == null
                ? this.consequent
                : this.alternative;
            answer = null;
#if DEBUG
            noteCalls ((SCode) expression);
#endif
            return true;
        }
    }

    [Serializable]
    class PCondIsNullA0 : PCondIsNullA
    {
        protected PCondIsNullA0 (PrimitiveIsNullA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsNullA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsNullA0L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondIsNullA0Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCondIsNullA0SL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondIsNullA0SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0.EvalStep");
#endif
            object ev0 = environment.Argument0Value;



            if (!(ev0 == null)) {
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


    }

    [Serializable]
    class PCondIsNullA0L : PCondIsNullA0
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsNullA0L (PrimitiveIsNullA0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        internal static SCode Make (PrimitiveIsNullA0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsNullA0A.Make (predicate, (Argument) consequent, alternative):
                (consequent is LexicalVariable1) ? PCondIsNullA0L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCondIsNullA0LL.Make (predicate, consequent, (LexicalVariable) alternative):
                (alternative is Quotation) ? PCondIsNullA0LQ.Make (predicate, consequent, (Quotation) alternative):
                new PCondIsNullA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0L.EvalStep");
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 == null)) {
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
    class PCondIsNullA0A : PCondIsNullA0L
    {
        protected PCondIsNullA0A (PrimitiveIsNullA0 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsNullA0 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsNullA0A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCondIsNullA0A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented() :
                (alternative is Quotation) ? PCondIsNullA0AQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0A.EvalStep");
#endif
            if (environment.Argument0Value == null) {
                answer = environment.ArgumentValue (this.consequentOffset);
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
    class PCondIsNullA0A0 : PCondIsNullA0A
    {

        protected PCondIsNullA0A0 (PrimitiveIsNullA0 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsNullA0 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsNullA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsNullA0L.EvalStep");
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 == null)) {
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
    class PCondIsNullA0A1 : PCondIsNullA0A
    {
        protected PCondIsNullA0A1 (PrimitiveIsNullA0 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsNullA0 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented() :
                new PCondIsNullA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0A1.EvalStep");
#endif
            if (environment.Argument0Value == null) {
                answer = environment.Argument1Value;
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
    class PCondIsNullA0L1 : PCondIsNullA0L
    {

        protected PCondIsNullA0L1 (PrimitiveIsNullA0 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsNullA0 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented() :
                new PCondIsNullA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0L1.EvalStep");
#endif
            if (environment.Argument0Value == null) {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
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
    sealed class PCondIsNullA0AQ : PCondIsNullA0A
    {
        public readonly object alternativeValue;

        PCondIsNullA0AQ (PrimitiveIsNullA0 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsNullA0 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCondIsNullA0AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0AQ.EvalStep");
#endif
            object ev0 = environment.Argument0Value;
            answer = (ev0 == null) ? environment.ArgumentValue (this.consequentOffset) : this.alternativeValue;
            return false;
        }
    }

    [Serializable]
    class PCondIsNullA0LL : PCondIsNullA0L
    {
        protected PCondIsNullA0LL (PrimitiveIsNullA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsNullA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsNullA0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsNullA0LQ : PCondIsNullA0L
    {
        public readonly object alternativeValue;

        protected PCondIsNullA0LQ (PrimitiveIsNullA0 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsNullA0 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsNullA0LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0LQ.EvalStep");
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 == null)) {

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
    class PCondIsNullA0Q : PCondIsNullA0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
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
                (alternative is LexicalVariable) ? PCondIsNullA0QL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondIsNullA0QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA0Q (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0Q.EvalStep");
#endif
            if (environment.Argument0Value == null) {
                answer = this.consequentValue;
                return false;
            } 
            else {
#if DEBUG
                SCode.location = "-";
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullA0Q.EvalStep";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullA0QL : PCondIsNullA0Q
    {
        protected PCondIsNullA0QL (PrimitiveIsNullA0 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsNullA0 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsNullA0QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    [Serializable]
    class PCondIsNullA0QQ : PCondIsNullA0Q
    {
        protected PCondIsNullA0QQ (PrimitiveIsNullA0 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsNullA0 predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsNullA0QQ (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    [Serializable]
    class PCondIsNullA0SL : PCondIsNullA0
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsNullA0SL (PrimitiveIsNullA0 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }
        internal static SCode Make (PrimitiveIsNullA0 predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsNullA0SL (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument0Value;

            if (!(ev0 == null)) {
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
    class PCondIsNullA0SQ : PCondIsNullA0
    {
        public readonly object alternativeValue;

        protected PCondIsNullA0SQ (PrimitiveIsNullA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsNullA0 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsNullA0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0SQ.EvalStep");
#endif
            object ev0 = environment.Argument0Value;


            if (!(ev0 == null)) {

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
    class PCondIsNullA1 : PCondIsNullA
    {
        protected PCondIsNullA1 (PrimitiveIsNullA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsNullA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsNullA1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondIsNullA1Q.Make (predicate, (Quotation) consequent, alternative):
                (alternative is LexicalVariable) ? PCondIsNullA1SL.Make (predicate, consequent, (LexicalVariable) alternative):
                (alternative is Quotation) ? PCondIsNullA1SQ.Make (predicate, consequent, (Quotation) alternative):
                new PCondIsNullA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA1.EvalStep");
#endif
            if (environment.Argument1Value == null) {
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

    [Serializable]
    class PCondIsNullA1L : PCondIsNullA1
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsNullA1L (PrimitiveIsNullA1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;

        }

        internal static SCode Make (PrimitiveIsNullA1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsNullA1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented() :
                (alternative is LexicalVariable) ? PCondIsNullA1LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondIsNullA1LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA1L.EvalStep");
#endif
            if (environment.Argument1Value == null) {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else{
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
    class PCondIsNullA1A : PCondIsNullA1L
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullA1A (PrimitiveIsNullA1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsNullA1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsNullA1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented() :
                new PCondIsNullA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA1A.EvalStep");
#endif
            if (environment.Argument1Value == null) {
                answer = environment.ArgumentValue (this.consequentOffset);
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
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
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
    class PCondIsNullA1LL : PCondIsNullA1L
    {
        protected PCondIsNullA1LL (PrimitiveIsNullA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsNullA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsNullA1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class PCondIsNullA1LQ : PCondIsNullA1L
    {
        public readonly object alternativeValue;

        PCondIsNullA1LQ (PrimitiveIsNullA1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsNullA1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsNullA1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 == null)) {

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
    class PCondIsNullA1Q : PCondIsNullA1
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
#endif
        public readonly object consequentValue;

        protected PCondIsNullA1Q (PrimitiveIsNullA1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        internal static SCode Make (PrimitiveIsNullA1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsNullA1QL.Make (predicate, consequent, (LexicalVariable) alternative) : 
                (alternative is Quotation) ? PCondIsNullA1QQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA1Q.EvalStep");
#endif
            if (environment.Argument1Value == null) {
                answer = this.consequentValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullA1Q.EvalStep.1";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullA1QL : PCondIsNullA1Q
    {
        protected PCondIsNullA1QL (PrimitiveIsNullA1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsNullA1 predicate, Quotation quotation, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    [Serializable]
    sealed class PCondIsNullA1QQ : PCondIsNullA1Q
    {
        PCondIsNullA1QQ (PrimitiveIsNullA1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsNullA1 predicate, Quotation consequent, Quotation alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsNullA1SL : PCondIsNullA1
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsNullA1SL (PrimitiveIsNullA1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }
        internal static SCode Make (PrimitiveIsNullA1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsNullA1SL (predicate, consequent, alternative);

        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;

            if (!(ev0 == null)) {
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
    class PCondIsNullA1SQ : PCondIsNullA1
    {
        public readonly object alternativeValue;

        protected PCondIsNullA1SQ (PrimitiveIsNullA1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsNullA1 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsNullA1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 == null)) {

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
    class PCondIsNullAL : PCondIsNullA
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsNullAL (PrimitiveIsNullA predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullAL.EvalStep");
#endif
            object ev0 = environment.ArgumentValue (this.predicateOffset);

            if (!(ev0 == null)) {
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

        internal static SCode Make (PrimitiveIsNullA predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsNullALL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsNullALQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsNullAL (predicate, consequent, alternative);
        }
    }

    [Serializable]
    class PCondIsNullAA : PCondIsNullAL
    {
        protected PCondIsNullAA (PrimitiveIsNullA predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        { }

        static public SCode Make (PrimitiveIsNullA predicate, Argument consequent, SCode alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

    }

    [Serializable]
    class PCondIsNullAL1 : PCondIsNullAL
    {
        protected PCondIsNullAL1 (PrimitiveIsNullA predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsNullA predicate, LexicalVariable1 consequent, SCode alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsNullALL : PCondIsNullAL
    {
        protected PCondIsNullALL (PrimitiveIsNullA predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsNullA predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsNullALQ : PCondIsNullAL
    {
        protected PCondIsNullALQ (PrimitiveIsNullA predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsNullA predicate, LexicalVariable consequent, Quotation alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsNullAQ : PCondIsNullA
    {
        public readonly object consequentValue;

        protected PCondIsNullAQ (PrimitiveIsNullA predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ("PCondIsNullAQ.EvalStep");
#endif
            object ev0 = environment.ArgumentValue (this.predicateOffset);


            if (!(ev0 == null)) {
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

        internal static SCode Make (PrimitiveIsNullA predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsNullAQL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsNullAQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsNullAQ (predicate, consequent, alternative);
        }
    }

    [Serializable]
    class PCondIsNullAQL : PCondIsNullAQ
    {
        protected PCondIsNullAQL (PrimitiveIsNullA predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        internal static SCode Make (PrimitiveIsNullA predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsNullAQL (predicate, consequent, alternative);
        }
    }

    [Serializable]
    class PCondIsNullAQQ : PCondIsNullAQ
    {
        protected PCondIsNullAQQ (PrimitiveIsNullA predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {

            throw new NotImplementedException ();
        }

        internal static SCode Make (PrimitiveIsNullA predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsNullAQQ (predicate, consequent, alternative);
        }
    }

    [Serializable]
    class PCondIsNullASL : PCondIsNullA
    {
        protected PCondIsNullASL (PrimitiveIsNullA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        internal static PCondIsNullA Make (PrimitiveIsNullA predicate, SCode consequent, LexicalVariable lexicalVariable)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsNullASQ : PCondIsNullA
    {
        public readonly object alternativeValue;

        protected PCondIsNullASQ (PrimitiveIsNullA predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullASQ.EvalStep");
#endif
            object ev0 = environment.ArgumentValue (predicateOffset);


            if (!(ev0 == null)) {

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

        internal static PCondIsNullA Make (PrimitiveIsNullA predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsNullASQ (predicate, consequent, alternative);
        }
    }

    [Serializable]
    class PCondIsNullL1 : PCondIsNullL
    {
        protected PCondIsNullL1 (PrimitiveIsNullL1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullL1 predicate, SCode consequent, SCode alternative)
        {
            return
       (consequent is LexicalVariable) ? PCondIsNullL1L.Make (predicate, (LexicalVariable) consequent, alternative)
       : (consequent is Quotation) ? PCondIsNullL1Q.Make (predicate, (Quotation) consequent, alternative)
       : (alternative is LexicalVariable) ? PCondIsNullL1SL.Make (predicate, consequent, (LexicalVariable) alternative)
       : (alternative is Quotation) ? PCondIsNullL1SQ.Make (predicate, consequent, (Quotation) alternative)
       : new PCondIsNullL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ("PCondIsNullL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();


            if (!(ev0 == null)) {
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
    class PCondIsNullL1L : PCondIsNullL1
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsNullL1L (PrimitiveIsNullL1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        internal static SCode Make (PrimitiveIsNullL1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsNullL1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? PCondIsNullL1L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
                (alternative is LexicalVariable) ? PCondIsNullL1LL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? PCondIsNullL1LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullL1L.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (!(ev0 == null)) {
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
    class PCondIsNullL1A : PCondIsNullL1L
    {
        protected PCondIsNullL1A (PrimitiveIsNullL1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsNullL1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsNullL1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCondIsNullL1A1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented() :
                new PCondIsNullL1A (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsNullL1L.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (!(ev0 == null)) {
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
    class PCondIsNullL1A0 : PCondIsNullL1A
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
#endif
        protected PCondIsNullL1A0 (PrimitiveIsNullL1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsNullL1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsNullL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullL1A0.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (ev0 == null) {
                answer = environment.Argument0Value;
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

    [Serializable]
    class PCondIsNullL1A1 : PCondIsNullL1A
    {
        protected PCondIsNullL1A1 (PrimitiveIsNullL1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsNullL1 predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsNullL1A1 (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullL1A1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (ev0 == null) {
                answer = environment.Argument1Value;
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
    class PCondIsNullL1L1 : PCondIsNullL1L
    {

        protected PCondIsNullL1L1 (PrimitiveIsNullL1 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        internal static SCode Make (PrimitiveIsNullL1 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (((LexicalVariable) predicate.Operand).Offset == consequent.Offset) ? PCondIsNullL1Q.Make (predicate, Quotation.Make (null), alternative) :
                (alternative is LexicalVariable) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented() :
                new PCondIsNullL1L1 (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullL1L1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (ev0 == null) {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException();
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
    class PCondIsNullL1LL : PCondIsNullL1L
    {
        protected PCondIsNullL1LL (PrimitiveIsNullL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsNullL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsNullL1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsNullL1LQ : PCondIsNullL1L
    {
        public readonly object alternativeValue;

        protected PCondIsNullL1LQ (PrimitiveIsNullL1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsNullL1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsNullL1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;


            if (!(ev0 == null)) {

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
    class PCondIsNullL1Q : PCondIsNullL1
    {
        public readonly object consequentValue;

        protected PCondIsNullL1Q (PrimitiveIsNullL1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }
        internal static SCode Make (PrimitiveIsNullL1 predicate, Quotation consequent, SCode alternative)
        {
            return
    (alternative is LexicalVariable) ? PCondIsNullL1QL.Make (predicate, consequent, (LexicalVariable) alternative)
    : (alternative is Quotation) ? PCondIsNullL1QQ.Make (predicate, consequent, (Quotation) alternative)
    : new PCondIsNullL1Q (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullL1Q.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();


            if (ev0 == null) {
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
    class PCondIsNullL1QL : PCondIsNullL1Q
    {
        protected PCondIsNullL1QL (PrimitiveIsNullL1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsNullL1 predicate, Quotation quotation, LexicalVariable alternative)
        {

            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    [Serializable]
    class PCondIsNullL1QQ : PCondIsNullL1Q
    {
        protected PCondIsNullL1QQ (PrimitiveIsNullL1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsNullL1 predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsNullL1QQ (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    [Serializable]
    class PCondIsNullL1SL : PCondIsNullL1
    {
        protected PCondIsNullL1SL (PrimitiveIsNullL1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsNullL1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsNullL1SL (predicate, consequent, alternative);

        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    [Serializable]
    class PCondIsNullL1SQ : PCondIsNullL1
    {
        public readonly object alternativeValue;

        protected PCondIsNullL1SQ (PrimitiveIsNullL1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsNullL1 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsNullL1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullL1SQ.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (!(ev0 == null)) {
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
    class PCondIsNullLL : PCondIsNullL
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsNullLL (PrimitiveIsNullL predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsNullL predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? Unimplemented() :
                (consequent is LexicalVariable1) ? Unimplemented ():
                (alternative is LexicalVariable) ? PCondIsNullLLL.Make (predicate, consequent, alternative):
                (alternative is Quotation) ? PCondIsNullLLQ.Make (predicate, consequent, alternative):
                new PCondIsNullLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullLL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

            if (ev0 == null) {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
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

    [Serializable]
    class PCondIsNullLLL : PCondIsNullLL
    {
        protected PCondIsNullLLL (PrimitiveIsNullL predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullL predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsNullLLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsNullLLQ : PCondIsNullLL
    {
        protected PCondIsNullLLQ (PrimitiveIsNullL predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullL predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsNullLLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsNullLQ : PCondIsNullL
    {
        public readonly object consequentValue;

        protected PCondIsNullLQ (PrimitiveIsNullL predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsNullL predicate, Quotation consequent, SCode alternative)
        {
            return
              (alternative is LexicalVariable) ? PCondIsNullLQL.Make (predicate, consequent, (LexicalVariable) alternative)
              : (alternative is Quotation) ? PCondIsNullLQQ.Make (predicate, consequent, (Quotation) alternative)
              : new PCondIsNullLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullLQ");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

            if (ev0 == null) {
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
    class PCondIsNullLQL : PCondIsNullLQ
    {
        protected PCondIsNullLQL (PrimitiveIsNullL predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullL predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsNullLQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsNullLQQ : PCondIsNullLQ
    {
        protected PCondIsNullLQQ (PrimitiveIsNullL predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullL predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsNullLQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsNullLSL : PCondIsNullL
    {
        protected PCondIsNullLSL (PrimitiveIsNullL predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullL predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsNullLSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class PCondIsNullLSQ : PCondIsNullL
    {
        public readonly object alternativeValue;

        PCondIsNullLSQ (PrimitiveIsNullL predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsNullL predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsNullLSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullLSQ.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

            if (ev0 == null) {
#if DEBUG
                noteCalls (this.consequent);
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
    class PCondIsNullSL : PCondIsNull
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsNullSL (PrimitiveIsNull predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsNull predicate, LexicalVariable consequent, SCode alternative)
        {
            return
     (alternative is LexicalVariable) ? PCondIsNullSLL.Make (predicate, consequent, (LexicalVariable) alternative)
     : (alternative is Quotation) ? PCondIsNullSLQ.Make (predicate, consequent, (Quotation) alternative)
     : new PCondIsNullSL (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ("PCondIsNullSL.EvalStep");
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 == null)) {
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
    class PCondIsNullSLL : PCondIsNullSL
    {

        protected PCondIsNullSLL (PrimitiveIsNull predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsNull predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsNullSLQ : PCondIsNullSL
    {
        public readonly object alternativeValue;

        protected PCondIsNullSLQ (PrimitiveIsNull predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }


        public static SCode Make (PrimitiveIsNull predicate, LexicalVariable consequent, Quotation alternative)
        {

            return new PCondIsNullSLQ (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 == null)) {
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
    class PCondIsNullSQ : PCondIsNull
    {
        public readonly object consequentValue;

        protected PCondIsNullSQ (PrimitiveIsNull predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsNull predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsNullSQL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsNullSQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsNullSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.arg0);
            SCode.location = "PCondIsNullSQ.EvalStep";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 == null)) {
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
    class PCondIsNullSQL : PCondIsNullSQ
    {
        protected PCondIsNullSQL (PrimitiveIsNull predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsNull predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondIsNullSQA.Make (predicate, consequent, (Argument) alternative) :
                new PCondIsNullSQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsNullSQA : PCondIsNullSQL
    {
        protected PCondIsNullSQA (PrimitiveIsNull predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsNull predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsNullSQA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsNullSQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class PCondIsNullSQA0 : PCondIsNullSQA
    {
        
        PCondIsNullSQA0 (PrimitiveIsNull predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsNull predicate, Quotation consequent, Argument0 alternative)
        {
            return new PCondIsNullSQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsNullSQQ : PCondIsNullSQ
    {
        public readonly object alternativeValue;

        protected PCondIsNullSQQ (PrimitiveIsNull predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsNull predicate, Quotation consequent, Quotation alternative)
        {
            return new PCondIsNullSQQ (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (!(ev0 == null)) {
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
    class PCondIsNullSSL : PCondIsNull
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsNullSSL (PrimitiveIsNull predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveIsNull predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? Unimplemented() :
                (alternative is LexicalVariable1) ? Unimplemented() :
                new PCondIsNullSSL (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }



            if (!(ev0 == null)) {
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
    sealed class PCondIsNullSSQ : PCondIsNull
    {
        public readonly object alternativeValue;

        PCondIsNullSSQ (PrimitiveIsNull predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsNull predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsNullSSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.arg0);
            SCode.location = "PCondIsNullSSQ.EvalStep";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
                        SCode.location = "PCondIsNullSSQ.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (!(ev0 == null)) {
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

