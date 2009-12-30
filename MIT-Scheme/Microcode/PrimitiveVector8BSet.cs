using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    [Serializable]
    class PrimitiveVector8BSet : PrimitiveCombination3
    {
        #if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveVector8BSet (SCode arg0, SCode arg1, SCode arg2)
            : base (Primitive.Vector8BSet, arg0, arg1, arg2)
        {
        }
        public static SCode Make (SCode arg0, SCode arg1, SCode arg2)
        {
            return
                new PrimitiveVector8BSet (arg0, arg1, arg2);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            NoteCalls (this.arg1);
            NoteCalls (this.arg2);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveVector8BSet";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveVector8BSet";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev1;
            env = environment;
            unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveVector8BSet";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveVector8BSet";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

            answer = ((char []) ev0) [(int) ev1];
            ((char []) ev0) [(int) ev1] = (char) (int) ev2;

            return false;
        }

    }
}
