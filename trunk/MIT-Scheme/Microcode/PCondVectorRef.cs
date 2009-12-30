using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
            [Serializable]
    class PCondVectorRef : PCond2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondVectorRef (PrimitiveVectorRef predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveVectorRef predicate, SCode consequent, SCode alternative)
        {
            return
                //(predicate is PrimitiveVectorRefA) ? PCondVectorRefA.Make ((PrimitiveVectorRefA) predicate, consequent, alternative) :
                //(predicate is PrimitiveVectorRefS) ? PCondVectorRefS.Make ((PrimitiveVectorRefS) predicate, consequent, alternative) :
                //(predicate is PrimitiveVectorRefXQ) ? PCondVectorRefXQ.Make ((PrimitiveVectorRefXQ) predicate, consequent, alternative) :
                new PCondVectorRef (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondVectorRef";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondVectorRef";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondVectorRef";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object result = ((object []) ev0)[((int) ev1)];

            if (result is Boolean && (((bool) result) == false)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondVectorRef";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            } else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondVectorRef";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

}
