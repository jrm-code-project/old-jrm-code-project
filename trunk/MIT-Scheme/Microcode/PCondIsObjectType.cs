using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PCondIsObjectType : PCond2
    {
#if DEBUG
            static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
            static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
            static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
            static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsObjectType (PrimitiveIsObjectType predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsObjectType predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsObjectTypeQ) ? PCondIsObjectTypeQ.Make ((PrimitiveIsObjectTypeQ) predicate, consequent, alternative) :
                new PCondIsObjectType (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsObjectType";
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

            ObjectModel.IsPrimitiveObjectType (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
 
    }

    [Serializable]
    class PCondIsObjectTypeQ : PCondIsObjectType
    {
#if DEBUG
        static Histogram<TC> rand0Histogram = new Histogram<TC> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly TC rand0Value;

        protected PCondIsObjectTypeQ (PrimitiveIsObjectTypeQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Value =  predicate.rand0Value;
        }

        static SCode Rewrite (TC oldType, Primitive1 newPred, SCode arg)
        {
            Debug.Write ("\n; Flatten (object-type? =>  " + newPred.Name);
            return PrimitiveCombination1.Make (newPred, arg);
        }

        public static SCode Make (PrimitiveIsObjectTypeQ predicate, SCode consequent, SCode alternative)
        {
            return
                //((TC) predicate.rand0Value == TC.BIG_FLONUM) ? PCondIsBigFlonum.Make (Flatten ((TC) predicate.rand0Value, Primitive.IsBigFlonum, predicate.Rand1), consequent, alternative) :
                //((TC) predicate.rand0Value == TC.RATNUM) ? PCondIsBigFlonum.Make (Flatten ((TC) predicate.rand0Value, Primitive.IsRatnum, predicate.Rand1), consequent, alternative) :
                //((TC) predicate.rand0Value == TC.VECTOR) ? PCondIsVector.Make (Flatten ((TC) predicate.rand0Value, Primitive.IsVector, predicate.Rand1), consequent, alternative) :
                //(predicate is PrimitiveIsObjectTypeQL) ? PCondIsObjectTypeQL.Make ((PrimitiveIsObjectTypeQL) predicate, consequent, alternative) :
                //(consequent is Quotation) ? Unimplemented() :
                //(alternative is Quotation) ? Unimplemented() :
                new PCondIsObjectTypeQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand0Histogram.Note (this.rand0Value);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsObjectTypeQ.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.IsPrimitiveObjectType (out answer, this.rand0Value, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

}
