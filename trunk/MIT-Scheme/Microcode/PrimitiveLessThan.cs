using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PrimitiveLessThan : PrimitiveCombination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PrimitiveLessThan (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                //(rand0 is Argument) ? PrimitiveLessThanFixnumA.Make (rator, (Argument) rand0, rand1) :
                //(rand0 is StaticVariable) ? PrimitiveLessThanFixnumS.Make (rator, (StaticVariable) rand0, rand1) :
                //(rand1 is Quotation) ? PrimitiveLessThanFixnumXQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveLessThan (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveLessThan";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveLessThan";
#endif
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
#if DEBUG
            SCode.location = "PrimitiveLessThan";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // less-than-fixnum?
            // Shortcut for fixnums
            if (ev0 is int && ev1 is int) {
                answer = ((int) ev0 < (int) ev1) ? Constant.sharpT : Constant.sharpF;
                return false;
            }
            else
                answer = new TailCallInterpreter (CallFromPrimitive.Make ((IApplicable) FixedObjectsVector.GenericLessP, ev0, ev1), null);
            return true;
        }
    }

}
