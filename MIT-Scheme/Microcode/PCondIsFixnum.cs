using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PCondIsFixnum : PCond1
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsFixnum (PrimitiveIsFixnum predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnum predicate, SCode consequent, SCode alternative)
        {
            return
                 (predicate is PrimitiveIsFixnumL) ? PCondIsFixnumL.Make ((PrimitiveIsFixnumL) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? PCondIsFixnumSL.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondIsFixnumSQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondIsFixnumSSL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsFixnumSSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsFixnum (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsFixnumFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (! (ev0 is Int32)) {
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

    class PCondIsFixnumL : PCondIsFixnum
    {
        public readonly object predicateName;
        public readonly int predicateDepth;
        public readonly int predicateOffset;

        protected PCondIsFixnumL (PrimitiveIsFixnumL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateName = ((LexicalVariable) predicate.Operand).Name;
            this.predicateDepth = ((LexicalVariable) predicate.Operand).Depth;
            this.predicateOffset = ((LexicalVariable) predicate.Operand).Offset;
        }

        public static SCode Make (PrimitiveIsFixnumL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsFixnumA) ? PCondIsFixnumA.Make ((PrimitiveIsFixnumA) predicate, consequent, alternative)
                : (predicate is PrimitiveIsFixnumL1) ? PCondIsFixnumL1.Make ((PrimitiveIsFixnumL1) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? PCondIsFixnumLL.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondIsFixnumLQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondIsFixnumLSL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsFixnumLSQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsFixnumL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ("PCondIsFixnumL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();


            if (! (ev0 is Int32)) {
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

    class PCondIsFixnumA : PCondIsFixnumL
    {
        protected PCondIsFixnumA (PrimitiveIsFixnumA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsFixnumA0) ? PCondIsFixnumA0.Make ((PrimitiveIsFixnumA0) predicate, consequent, alternative)
                : (predicate is PrimitiveIsFixnumA1) ? PCondIsFixnumA1.Make ((PrimitiveIsFixnumA1) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? PCondIsFixnumAL.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondIsFixnumAQ.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondIsFixnumASL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsFixnumASQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsFixnumA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();

#endif
            object ev0 = environment.ArgumentValue (this.predicateOffset);


            if (! (ev0 is Int32)) {
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

    class PCondIsFixnumA0 : PCondIsFixnumA
    {
        protected PCondIsFixnumA0 (PrimitiveIsFixnumA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumA0.EvalStep");
#endif

            object ev0 = environment.Argument0Value;



            if (! (ev0 is Int32)) {
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

        internal static SCode Make (PrimitiveIsFixnumA0 predicate, SCode consequent, SCode alternative)
        {
            return
       (consequent is LexicalVariable) ? PCondIsFixnumA0L.Make (predicate, (LexicalVariable) consequent, alternative)
       : (consequent is Quotation) ? PCondIsFixnumA0Q.Make (predicate, (Quotation) consequent, alternative)
       : (alternative is LexicalVariable) ? PCondIsFixnumA0SL.Make (predicate, consequent, (LexicalVariable) alternative)
       : (alternative is Quotation) ? PCondIsFixnumA0SQ.Make (predicate, consequent, (Quotation) alternative)
       : new PCondIsFixnumA0 (predicate, consequent, alternative);
        }
    }

    class PCondIsFixnumA0L : PCondIsFixnumA0
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsFixnumA0L (PrimitiveIsFixnumA0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        internal static SCode Make (PrimitiveIsFixnumA0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsFixnumA0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented():
                (alternative is LexicalVariable) ? PCondIsFixnumA0LL.Make (predicate, consequent, (LexicalVariable) alternative):
                (alternative is Quotation) ? PCondIsFixnumA0LQ.Make (predicate, consequent, (Quotation) alternative):
                new PCondIsFixnumA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumA0L.EvalStep");
#endif
            if (environment.Argument0Value is Int32) {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
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

    class PCondIsFixnumA0A : PCondIsFixnumA0L
    {

        protected PCondIsFixnumA0A (PrimitiveIsFixnumA0 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsFixnumA0 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsFixnumA0A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented() :
                new PCondIsFixnumA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsFixnumA0L.EvalStep");
#endif
            if (environment.Argument0Value is Int32) {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
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

    class PCondIsFixnumA0A0 : PCondIsFixnumA0A
    {

        protected PCondIsFixnumA0A0 (PrimitiveIsFixnumA0 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsFixnumA0 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsFixnumA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumA0A0.EvalStep");
#endif
            object test = environment.Argument0Value;

            if (test is Int32) {
                answer = test;
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



    class PCondIsFixnumA0LL : PCondIsFixnumA0L
    {
        protected PCondIsFixnumA0LL (PrimitiveIsFixnumA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsFixnumA0 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsFixnumA0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsFixnumA0LQ : PCondIsFixnumA0L
    {
        public readonly object alternativeValue;

        protected PCondIsFixnumA0LQ (PrimitiveIsFixnumA0 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsFixnumA0 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsFixnumA0LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument0Value;


            if (! (ev0 is Int32)) {

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

    class PCondIsFixnumA0Q : PCondIsFixnumA0
    {
        public readonly object consequentValue;

        protected PCondIsFixnumA0Q (PrimitiveIsFixnumA0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }
        internal static SCode Make (PrimitiveIsFixnumA0 predicate, Quotation consequent, SCode alternative)
        {
            return
    (alternative is LexicalVariable) ? PCondIsFixnumA0QL.Make (predicate, consequent, (LexicalVariable) alternative)
    : (alternative is Quotation) ? PCondIsFixnumA0QQ.Make (predicate, consequent, (Quotation) alternative)
    : new PCondIsFixnumA0Q (predicate, consequent, alternative);

        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumA0Q.EvalStep");
#endif
            object ev0 = environment.Argument0Value;


            if (! (ev0 is Int32)) {
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

    class PCondIsFixnumA0QL : PCondIsFixnumA0Q
    {
        protected PCondIsFixnumA0QL (PrimitiveIsFixnumA0 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsFixnumA0 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsFixnumA0QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsFixnumA0QQ : PCondIsFixnumA0Q
    {
        protected PCondIsFixnumA0QQ (PrimitiveIsFixnumA0 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsFixnumA0 predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsFixnumA0QQ (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsFixnumA0SL : PCondIsFixnumA0
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsFixnumA0SL (PrimitiveIsFixnumA0 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        internal static SCode Make (PrimitiveIsFixnumA0 predicate, SCode consequent, LexicalVariable alternative)
        {
            return 
                (alternative is Argument) ? PCondIsFixnumA0SA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? Unimplemented() :
                new PCondIsFixnumA0SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumA0SL.EvalStep");
#endif
            object ev0 = environment.Argument0Value;

            if (! (ev0 is Int32)) {
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

    class PCondIsFixnumA0SA : PCondIsFixnumA0SL
    {

        protected PCondIsFixnumA0SA (PrimitiveIsFixnumA0 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsFixnumA0 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsFixnumA0SA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? Unimplemented () :
                new PCondIsFixnumA0SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsFixnumA0SL.EvalStep");
#endif
            object ev0 = environment.Argument0Value;

            if (!(ev0 is Int32)) {
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

    sealed class PCondIsFixnumA0SA0 : PCondIsFixnumA0SA
    {

        PCondIsFixnumA0SA0 (PrimitiveIsFixnumA0 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsFixnumA0 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsFixnumA0SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumA0SA0.EvalStep");
#endif
            object ev0 = environment.Argument0Value;

            if (ev0 is Int32) {
#if DEBUG
                noteCalls (this.consequent);
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

    class PCondIsFixnumA0SQ : PCondIsFixnumA0
    {
        public readonly object alternativeValue;

        protected PCondIsFixnumA0SQ (PrimitiveIsFixnumA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsFixnumA0 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsFixnumA0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnum.EvalStepA0SQ");
#endif
            object ev0 = environment.Argument0Value;

            if (! (ev0 is Int32)) {

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

    class PCondIsFixnumA1 : PCondIsFixnumA
    {
        protected PCondIsFixnumA1 (PrimitiveIsFixnumA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumA1.EvalStep");
#endif
            if (environment.Argument1Value is Int32) 
            {
#if DEBUG
                noteCalls (this.consequent);

#endif
                expression = this.consequent;
                answer = null;
                return true;
            } 
            else 
            {
#if DEBUG
                noteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }

        internal static SCode Make (PrimitiveIsFixnumA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsFixnumA1L.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? PCondIsFixnumA1Q.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? PCondIsFixnumA1SL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsFixnumA1SQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsFixnumA1 (predicate, consequent, alternative);
        }
    }

    class PCondIsFixnumA1L : PCondIsFixnumA1
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsFixnumA1L (PrimitiveIsFixnumA1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;

        }

        internal static SCode Make (PrimitiveIsFixnumA1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsFixnumA1LL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsFixnumA1LQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsFixnumA1L (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();

#endif
            object ev0 = environment.Argument1Value;

            if (! (ev0 is Int32)) {
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

    class PCondIsFixnumA1LL : PCondIsFixnumA1L
    {
        protected PCondIsFixnumA1LL (PrimitiveIsFixnumA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsFixnumA1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsFixnumA1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsFixnumA1LQ : PCondIsFixnumA1L
    {
        public readonly object alternativeValue;

        protected PCondIsFixnumA1LQ (PrimitiveIsFixnumA1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsFixnumA1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsFixnumA1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;


            if (! (ev0 is Int32)) {

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

    class PCondIsFixnumA1Q : PCondIsFixnumA1
    {
        public readonly object consequentValue;
        protected PCondIsFixnumA1Q (PrimitiveIsFixnumA1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }
        internal static SCode Make (PrimitiveIsFixnumA1 predicate, Quotation consequent, SCode alternative)
        {
            return
    (alternative is LexicalVariable) ? PCondIsFixnumA1QL.Make (predicate, consequent, (LexicalVariable) alternative)
    : (alternative is Quotation) ? PCondIsFixnumA1QQ.Make (predicate, consequent, (Quotation) alternative)
    : new PCondIsFixnumA1Q (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumA1Q.EvalStep");
#endif
            object ev0 = environment.Argument1Value;


            if (! (ev0 is Int32)) {
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

    class PCondIsFixnumA1QL : PCondIsFixnumA1Q
    {
        protected PCondIsFixnumA1QL (PrimitiveIsFixnumA1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsFixnumA1 predicate, Quotation quotation, LexicalVariable alternative)
        {

            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsFixnumA1QQ : PCondIsFixnumA1Q
    {
        protected PCondIsFixnumA1QQ (PrimitiveIsFixnumA1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsFixnumA1 predicate, Quotation consequent, Quotation alternative)
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

    class PCondIsFixnumA1SL : PCondIsFixnumA1
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsFixnumA1SL (PrimitiveIsFixnumA1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }
        internal static SCode Make (PrimitiveIsFixnumA1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsFixnumA1SL (predicate, consequent, alternative);

        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;

            if (! (ev0 is Int32)) {
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

    class PCondIsFixnumA1SQ : PCondIsFixnumA1
    {
        public readonly object alternativeValue;

        protected PCondIsFixnumA1SQ (PrimitiveIsFixnumA1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsFixnumA1 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsFixnumA1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumA1SQ.EvalStep");
#endif
            object ev0 = environment.Argument1Value;


            if (! (ev0 is Int32)) {

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

    class PCondIsFixnumAL : PCondIsFixnumA
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsFixnumAL (PrimitiveIsFixnumA predicate, LexicalVariable consequent, SCode alternative)
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

            if (! (ev0 is Int32)) {
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

        internal static SCode Make (PrimitiveIsFixnumA predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsFixnumALL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsFixnumALQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsFixnumAL (predicate, consequent, alternative);
        }
    }

    class PCondIsFixnumAA : PCondIsFixnumAL
    {
        protected PCondIsFixnumAA (PrimitiveIsFixnumA predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        { }

        static public SCode Make (PrimitiveIsFixnumA predicate, Argument consequent, SCode alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

    }

    class PCondIsFixnumAL1 : PCondIsFixnumAL
    {
        protected PCondIsFixnumAL1 (PrimitiveIsFixnumA predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsFixnumA predicate, LexicalVariable1 consequent, SCode alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsFixnumALL : PCondIsFixnumAL
    {
        protected PCondIsFixnumALL (PrimitiveIsFixnumA predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsFixnumA predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsFixnumALQ : PCondIsFixnumAL
    {
        protected PCondIsFixnumALQ (PrimitiveIsFixnumA predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        static public SCode Make (PrimitiveIsFixnumA predicate, LexicalVariable consequent, Quotation alternative)
        {
            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsFixnumAQ : PCondIsFixnumA
    {
        public readonly object consequentValue;

        protected PCondIsFixnumAQ (PrimitiveIsFixnumA predicate, Quotation consequent, SCode alternative)
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


            if (! (ev0 is Int32)) {
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

        internal static SCode Make (PrimitiveIsFixnumA predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsFixnumAQL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsFixnumAQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsFixnumAQ (predicate, consequent, alternative);
        }
    }

    class PCondIsFixnumAQL : PCondIsFixnumAQ
    {
        protected PCondIsFixnumAQL (PrimitiveIsFixnumA predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        internal static SCode Make (PrimitiveIsFixnumA predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsFixnumAQL (predicate, consequent, alternative);
        }
    }

    class PCondIsFixnumAQQ : PCondIsFixnumAQ
    {
        protected PCondIsFixnumAQQ (PrimitiveIsFixnumA predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {

            throw new NotImplementedException ();
        }

        internal static SCode Make (PrimitiveIsFixnumA predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsFixnumAQQ (predicate, consequent, alternative);
        }
    }

    class PCondIsFixnumASL : PCondIsFixnumA
    {
        protected PCondIsFixnumASL (PrimitiveIsFixnumA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        internal static PCondIsFixnumA Make (PrimitiveIsFixnumA predicate, SCode consequent, LexicalVariable lexicalVariable)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsFixnumASQ : PCondIsFixnumA
    {
        public readonly object alternativeValue;

        protected PCondIsFixnumASQ (PrimitiveIsFixnumA predicate, SCode consequent, Quotation alternative)
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


            if (! (ev0 is Int32)) {

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

        internal static PCondIsFixnumA Make (PrimitiveIsFixnumA predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsFixnumASQ (predicate, consequent, alternative);
        }
    }

    class PCondIsFixnumL1 : PCondIsFixnumL
    {
        protected PCondIsFixnumL1 (PrimitiveIsFixnumL1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumL1 predicate, SCode consequent, SCode alternative)
        {
            return
       (consequent is LexicalVariable) ? PCondIsFixnumL1L.Make (predicate, (LexicalVariable) consequent, alternative)
       : (consequent is Quotation) ? PCondIsFixnumL1Q.Make (predicate, (Quotation) consequent, alternative)
       : (alternative is LexicalVariable) ? PCondIsFixnumL1SL.Make (predicate, consequent, (LexicalVariable) alternative)
       : (alternative is Quotation) ? PCondIsFixnumL1SQ.Make (predicate, consequent, (Quotation) alternative)
       : new PCondIsFixnumL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            #region EvalStepBody
#if DEBUG
            Warm ("PCondIsFixnumL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();


            if (ev0 is Int32) {
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
            #endregion

        }
    }

    class PCondIsFixnumL1L : PCondIsFixnumL1
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsFixnumL1L (PrimitiveIsFixnumL1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;

        }

        internal static SCode Make (PrimitiveIsFixnumL1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsFixnumL1LL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsFixnumL1LQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsFixnumL1L (predicate, consequent, alternative);
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

            if (! (ev0 is Int32)) {
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

    class PCondIsFixnumL1LL : PCondIsFixnumL1L
    {
        protected PCondIsFixnumL1LL (PrimitiveIsFixnumL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }

        internal static SCode Make (PrimitiveIsFixnumL1 predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsFixnumL1LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsFixnumL1LQ : PCondIsFixnumL1L
    {
        public readonly object alternativeValue;

        protected PCondIsFixnumL1LQ (PrimitiveIsFixnumL1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsFixnumL1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsFixnumL1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0 = environment.Argument1Value;


            if (! (ev0 is Int32)) {

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

    class PCondIsFixnumL1Q : PCondIsFixnumL1
    {
        public readonly object consequentValue;

        protected PCondIsFixnumL1Q (PrimitiveIsFixnumL1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }
        internal static SCode Make (PrimitiveIsFixnumL1 predicate, Quotation consequent, SCode alternative)
        {
            return
    (alternative is LexicalVariable) ? PCondIsFixnumL1QL.Make (predicate, consequent, (LexicalVariable) alternative)
    : (alternative is Quotation) ? PCondIsFixnumL1QQ.Make (predicate, consequent, (Quotation) alternative)
    : new PCondIsFixnumL1Q (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumL1Q.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();


            if (! (ev0 is Int32)) {
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

    class PCondIsFixnumL1QL : PCondIsFixnumL1Q
    {
        protected PCondIsFixnumL1QL (PrimitiveIsFixnumL1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsFixnumL1 predicate, Quotation quotation, LexicalVariable alternative)
        {

            throw new NotImplementedException ();
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsFixnumL1QQ : PCondIsFixnumL1Q
    {
        protected PCondIsFixnumL1QQ (PrimitiveIsFixnumL1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsFixnumL1 predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsFixnumL1QQ (predicate, consequent, alternative);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsFixnumL1SL : PCondIsFixnumL1
    {
        protected PCondIsFixnumL1SL (PrimitiveIsFixnumL1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        { }
        internal static SCode Make (PrimitiveIsFixnumL1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsFixnumL1SL (predicate, consequent, alternative);

        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    class PCondIsFixnumL1SQ : PCondIsFixnumL1
    {
        public readonly object alternativeValue;

        protected PCondIsFixnumL1SQ (PrimitiveIsFixnumL1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsFixnumL1 predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsFixnumL1SQ (predicate, consequent, alternative);
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


            if (! (ev0 is Int32)) {
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

    class PCondIsFixnumLL : PCondIsFixnumL
    {
        protected PCondIsFixnumLL (PrimitiveIsFixnumL predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumL predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsFixnumLLL.Make (predicate, consequent, alternative)
                : (alternative is Quotation) ? PCondIsFixnumLLQ.Make (predicate, consequent, alternative)
                : new PCondIsFixnumLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsFixnumLLL : PCondIsFixnumLL
    {
        protected PCondIsFixnumLLL (PrimitiveIsFixnumL predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumL predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new PCondIsFixnumLLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsFixnumLLQ : PCondIsFixnumLL
    {
        protected PCondIsFixnumLLQ (PrimitiveIsFixnumL predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumL predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new PCondIsFixnumLLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsFixnumLQ : PCondIsFixnumL
    {
        protected readonly object consequentValue;

        protected PCondIsFixnumLQ (PrimitiveIsFixnumL predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsFixnumL predicate, Quotation consequent, SCode alternative)
        {
            return
              (alternative is LexicalVariable) ? PCondIsFixnumLQL.Make (predicate, consequent, (LexicalVariable) alternative)
              : (alternative is Quotation) ? PCondIsFixnumLQQ.Make (predicate, consequent, (Quotation) alternative)
              : new PCondIsFixnumLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsFixnumLQ.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();


            if (!(ev0 is Int32)) {
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

    class PCondIsFixnumLQL : PCondIsFixnumLQ
    {
        protected PCondIsFixnumLQL (PrimitiveIsFixnumL predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumL predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new PCondIsFixnumLQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsFixnumLQQ : PCondIsFixnumLQ
    {
        protected PCondIsFixnumLQQ (PrimitiveIsFixnumL predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumL predicate, Quotation consequent, Quotation alternative)
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
            return new PCondIsFixnumLQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsFixnumLSL : PCondIsFixnumL
    {
        protected PCondIsFixnumLSL (PrimitiveIsFixnumL predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsFixnumL predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsFixnumLSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsFixnumLSQ : PCondIsFixnumL
    {
        public readonly object alternativeValue;

        protected PCondIsFixnumLSQ (PrimitiveIsFixnumL predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsFixnumL predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsFixnumLSQ (predicate, consequent, alternative);
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

            if (! (ev0 is Int32)) {
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

    class PCondIsFixnumSL : PCondIsFixnum
    {
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsFixnumSL (PrimitiveIsFixnum predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsFixnum predicate, LexicalVariable consequent, SCode alternative)
        {
            return
     (alternative is LexicalVariable) ? PCondIsFixnumSLL.Make (predicate, consequent, (LexicalVariable) alternative)
     : (alternative is Quotation) ? PCondIsFixnumSLQ.Make (predicate, consequent, (Quotation) alternative)
     : new PCondIsFixnumSL (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsFixnumFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (! (ev0 is Int32)) {
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

    class PCondIsFixnumSLL : PCondIsFixnumSL
    {

        protected PCondIsFixnumSLL (PrimitiveIsFixnum predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsFixnum predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsFixnumSLQ : PCondIsFixnumSL
    {
        public readonly object alternativeValue;

        protected PCondIsFixnumSLQ (PrimitiveIsFixnum predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }


        public static SCode Make (PrimitiveIsFixnum predicate, LexicalVariable consequent, Quotation alternative)
        {

            return new PCondIsFixnumSLQ (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsFixnumFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (! (ev0 is Int32)) {
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

    class PCondIsFixnumSQ : PCondIsFixnum
    {
        public readonly object consequentValue;

        protected PCondIsFixnumSQ (PrimitiveIsFixnum predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsFixnum predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? PCondIsFixnumSQL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? PCondIsFixnumSQQ.Make (predicate, consequent, (Quotation) alternative)
                : new PCondIsFixnumSQ (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsFixnumFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (! (ev0 is Int32)) {
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

    class PCondIsFixnumSQL : PCondIsFixnumSQ
    {
        protected PCondIsFixnumSQL (PrimitiveIsFixnum predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsFixnum predicate, Quotation consequent, LexicalVariable alternative)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsFixnumSQQ : PCondIsFixnumSQ
    {
        public readonly object alternativeValue;

        protected PCondIsFixnumSQQ (PrimitiveIsFixnum predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }


        public static SCode Make (PrimitiveIsFixnum predicate, Quotation consequent, Quotation alternative)
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

            return new PCondIsFixnumSQQ (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsFixnumFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (! (ev0 is Int32)) {
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

    class PCondIsFixnumSSL : PCondIsFixnum
    {
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsFixnumSSL (PrimitiveIsFixnum predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveIsFixnum predicate, SCode consequent, LexicalVariable alternative)
        {
            return new PCondIsFixnumSSL (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsFixnumFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }



            if (! (ev0 is Int32)) {
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

    class PCondIsFixnumSSQ : PCondIsFixnum
    {
        public readonly object alternativeValue;

        protected PCondIsFixnumSSQ (PrimitiveIsFixnum predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsFixnum predicate, SCode consequent, Quotation alternative)
        {
            return new PCondIsFixnumSSQ (predicate, consequent, alternative);
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
                //((UnwinderState) env).AddFrame (new PrimitiveIsFixnumFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (! (ev0 is Int32)) {
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
