using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class Combination2S : Combination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol ratorName;
        public readonly int ratorOffset;

        protected Combination2S (StaticVariable rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.ratorName = rator.Name;
            this.ratorOffset = rator.Offset;        
        }

        public static SCode Make (StaticVariable rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? Combination2SA.Make (rator, (Argument) rand0, rand1) :
                new Combination2S (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2S.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2S.EvalStep";
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
            SCode.location = "Combination2S.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2SA : Combination2S
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;

        protected Combination2SA (StaticVariable rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (StaticVariable rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? Combination2SA0.Make (rator, (Argument0) rand0, rand1) :
                (rand0 is Argument1) ? Combination2SA1.Make (rator, (Argument1) rand0, rand1) :
                new Combination2SA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2SA.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SA.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2SA0 : Combination2SA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2SA0 (StaticVariable rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (StaticVariable rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? Combination2SA0A.Make (rator, rand0, (Argument) rand1) :
                new Combination2SA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2SA0.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SA0.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0 = environment.Argument0Value;

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2SA0A : Combination2SA0
    {
        public readonly int rand1Offset;

        protected Combination2SA0A (StaticVariable rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (StaticVariable rator, Argument0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument1) ? Combination2SA0A1.Make (rator, rand0, (Argument1) rand1) :
                new Combination2SA0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2SA0A.EvalStep");
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2SA0A1 : Combination2SA0A
    {
        protected Combination2SA0A1 (StaticVariable rator, Argument0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (StaticVariable rator, Argument0 rand0, Argument1 rand1)
        {
            return
                new Combination2SA0A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2SA0A1.EvalStep");
#endif
            object ev1 = environment.Argument1Value;

            object ev0 = environment.Argument0Value;

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2SA1 : Combination2SA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2SA1 (StaticVariable rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (StaticVariable rator, Argument1 rand0, SCode rand1)
        {
            return
                new Combination2SA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2SA1.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SA1.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0 = environment.Argument1Value;

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

}
