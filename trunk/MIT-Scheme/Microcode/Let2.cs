using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class Let2 : Combination2
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();

        protected Type bodyType;
#endif
        protected Let2 (Lambda rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
#if DEBUG
            if (rator.Body != null)
                this.bodyType = rator.Body.GetType ();
#endif
        }

        static SCode StandardMake (Lambda rator, SCode arg0, SCode arg1)
        {
            return
                new Let2 (rator, arg0, arg1);
        }

        public static SCode Make (Lambda rator, SCode arg0, SCode arg1)
        {
            return StandardMake (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            ratorTypeHistogram.Note (this.ratorType);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Let2";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Let2";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Let2";
#endif
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }

    }
}
