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
                (rand0 is Quotation) ? Combination2SQ.Make (rator, (Quotation) rand0, rand1) :
                (rand0 is StaticVariable) ? Combination2SS.Make (rator, (StaticVariable) rand0, rand1) :
                (rand1 is Argument) ? Combination2SXA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? new Combination2SXQ (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new Combination2SXS (rator, rand0, (StaticVariable) rand1) :
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
                (rand1 is Quotation) ? new Combination2SA0Q (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new Combination2SA0S (rator, rand0, (StaticVariable) rand1) :
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
    sealed class Combination2SA0Q : Combination2SA0
    {
        public readonly object rand1Value;
        internal Combination2SA0Q (StaticVariable rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2SA0Q.EvalStep";
#endif
            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, this.rand1Value);
        }
    }

    [Serializable]
    sealed class Combination2SA0S : Combination2SA0
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        internal Combination2SA0S (StaticVariable rator, Argument0 rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2SA0S.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, ev1);
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

    [Serializable]
    class Combination2SQ : Combination2S
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;

        protected Combination2SQ (StaticVariable rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        public static SCode Make (StaticVariable rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is StaticVariable) ? new Combination2SQS (rator, rand0, (StaticVariable) rand1) :
                new Combination2SQ (rator, rand0, rand1);
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

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
        }
    }

    [Serializable]
    sealed class Combination2SQS : Combination2SQ
    {

        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal Combination2SQS (StaticVariable rator, Quotation rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2SQS.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
        }
    }

    [Serializable]
    class Combination2SS : Combination2S
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected Combination2SS (StaticVariable rator, StaticVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (StaticVariable rator, StaticVariable rand0, SCode rand1)
        {
            return
               // (rand1 is Argument) ? Combination2SXA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? new Combination2SSQ (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new Combination2SSS (rator, rand0, (StaticVariable) rand1) :
                new Combination2SS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2SS.EvalStep";
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
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2SSQ : Combination2SS
    {

        public readonly object rand1Value;

        internal Combination2SSQ (StaticVariable rator, StaticVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2SSQ.EvalStep";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
        }
    }

    [Serializable]
    sealed class Combination2SSS : Combination2SS
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal Combination2SSS (StaticVariable rator, StaticVariable rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2SSS.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }


    [Serializable]
    class Combination2SXA : Combination2S
    {
        public readonly int rand1Offset;

        protected Combination2SXA (StaticVariable rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (StaticVariable rator, SCode rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2SXA0.Make (rator,  rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? Combination2SXA1.Make (rator,  rand0, (Argument1) rand1) :
                new Combination2SXA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            SCode.location = "Combination2SXA.EvalStep";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SA.EvalStep";
#endif



            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2SXA0 : Combination2SXA
    {
        protected Combination2SXA0 (StaticVariable rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (StaticVariable rator, SCode rand0, Argument0 rand1)
        {
            return
                new Combination2SXA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            SCode.location = "Combination2SXA0";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SXA0";
#endif
            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2SXA1 : Combination2SXA
    {
        protected Combination2SXA1 (StaticVariable rator, SCode rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (StaticVariable rator, SCode rand0, Argument1 rand1)
        {
            return
                new Combination2SXA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            SCode.location = "Combination2SXA1";
#endif
            object ev1 = environment.Argument1Value;

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SXA1";
#endif
            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2SXQ : Combination2S
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        internal Combination2SXQ (StaticVariable rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2SXQ.EvalStep";
#endif
            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SXQ.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
        }
    }

    [Serializable]
    sealed class Combination2SXS : Combination2S
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal Combination2SXS (StaticVariable rator, SCode rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2SXS.EvalStep";
#endif
            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SXQ.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2A : Combination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int ratorOffset;

        protected Combination2A (Argument rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.ratorOffset =  rator.Offset;
        }

        public static SCode Make (Argument rator, SCode rand0, SCode rand1)
        {
            return
                (rator is Argument0) ? Combination2A0.Make ((Argument0) rator, rand0, rand1) :
                (rator is Argument1) ? Combination2A1.Make ((Argument1) rator, rand0, rand1) :
                (rand0 is Quotation) ? Combination2AQ.Make (rator, (Quotation) rand0, rand1) :
                (rand1 is Quotation) ? new Combination2AXQ (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new Combination2AXS (rator, rand0, (StaticVariable) rand1) :
                new Combination2A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2A.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2A.EvalStep";
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
            SCode.location = "Combination2A.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), ev0, ev1);
        }
    }

    [Serializable]
    class Combination2A0 : Combination2A
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2A0 (Argument0 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Argument0 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is Quotation) ? Combination2A0Q.Make (rator, (Quotation) rand0, rand1) :
                (rand0 is StaticVariable) ? Combination2A0S.Make (rator, (StaticVariable) rand0, rand1) :
                (rand1 is Quotation) ? new Combination2A0XQ (rator, rand0, (Quotation) rand1) :
                new Combination2A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2A0.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2A0.EvalStep";
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
            SCode.location = "Combination2A0.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2A0Q : Combination2A0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;
        protected Combination2A0Q (Argument0 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        public static SCode Make (Argument0 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? new Combination2A0QQ (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new Combination2A0QS (rator, rand0, (StaticVariable) rand1) :
                new Combination2A0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2A0Q.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2A0Q.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value, this.rand0Value, ev1);
        }
    }

    [Serializable]
    sealed class Combination2A0QQ : Combination2A0Q
    {
        public readonly object rand1Value;
        internal Combination2A0QQ (Argument0 rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2A0QQ.EvalStep";
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value, this.rand0Value, this.rand1Value);
        }
    }

    [Serializable]
    sealed class Combination2A0QS : Combination2A0Q
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal Combination2A0QS (Argument0 rator, Quotation rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2A0QS.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
 
            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value, this.rand0Value, ev1);
        }
    }

    [Serializable]
    class Combination2A0S : Combination2A0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected Combination2A0S (Argument0 rator, StaticVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Argument0 rator, StaticVariable rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? new Combination2A0SQ (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new Combination2A0SS (rator, rand0, (StaticVariable) rand1):
                new Combination2A0S (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2A0S";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2A0S";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();


            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2A0SQ : Combination2A0S
    {
        public readonly object rand1Value;

        internal Combination2A0SQ (Argument0 rator, StaticVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2A0SQ";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();


            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value, ev0, this.rand1Value);
        }
    }

    [Serializable]
    sealed class Combination2A0SS : Combination2A0S
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal Combination2A0SS (Argument0 rator, StaticVariable rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2A0SS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value, ev0, ev1);
        }
    }



    [Serializable]
    sealed class Combination2A0XQ : Combination2A0
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;
        internal Combination2A0XQ (Argument0 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2A0XQ.EvalStep";
#endif

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2A0XQ.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, this.rand1Value));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value, ev0, this.rand1Value);
        }
    }

    [Serializable]
    class Combination2A1 : Combination2A
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2A1 (Argument1 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Argument1 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is Quotation) ? Combination2A1Q.Make (rator, (Quotation) rand0, rand1) :
                (rand0 is StaticVariable) ? Combination2A1S.Make (rator, (StaticVariable) rand0, rand1) :
                (rand1 is Quotation) ? new Combination2A1XQ (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new Combination2A1XS (rator,rand0,(StaticVariable) rand1) :
                new Combination2A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2A1.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2A1.EvalStep";
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
            SCode.location = "Combination2A1.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument1Value, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2A1Q : Combination2A1
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;
        protected Combination2A1Q (Argument1 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        public static SCode Make (Argument1 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? new Combination2A1QQ (rator, rand0, (Quotation) rand1) :
                new Combination2A1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2A1Q.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2A1Q.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument1Value, this.rand0Value, ev1);
        }
    }

    [Serializable]
    sealed class Combination2A1QQ : Combination2A1Q
    {
        public readonly object rand1Value;
        internal Combination2A1QQ (Argument1 rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2A1QQ.EvalStep";
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument1Value, this.rand0Value, this.rand1Value);
        }
    }

    [Serializable]
    class Combination2A1S : Combination2A1
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;
        protected Combination2A1S (Argument1 rator, StaticVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Argument1 rator, StaticVariable rand0, SCode rand1)
        {
            return
                (rand1 is StaticVariable) ? new Combination2A1SS (rator, rand0, (StaticVariable) rand1) :
                new Combination2A1S (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2A1S.EvalStep";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2A1Q.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument1Value, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2A1SS : Combination2A1S
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal Combination2A1SS (Argument1 rator, StaticVariable rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2A1SS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument1Value, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2A1XQ : Combination2A1
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;
        internal Combination2A1XQ (Argument1 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2A1XQ.EvalStep";
#endif

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2A1XQ.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, this.rand1Value));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument1Value, ev0, this.rand1Value);
        }
    }

    [Serializable]
    sealed class Combination2A1XS : Combination2A1
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        internal Combination2A1XS (Argument1 rator, SCode rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2A1XS.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2A1XS.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument1Value, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2AQ : Combination2A
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;

        protected Combination2AQ (Argument rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        public static SCode Make (Argument rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? new Combination2AQQ (rator, rand0, (Quotation) rand1) :
                new Combination2AQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2AQ.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2AQ.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), this.rand0Value, ev1);
        }
    }

    [Serializable]
    sealed class Combination2AQQ : Combination2AQ
    {
        public readonly object rand1Value;

        internal Combination2AQQ (Argument rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2AQQ.EvalStep";
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), this.rand0Value, this.rand1Value);
        }
    }

    [Serializable]
    sealed class Combination2AXQ : Combination2A
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;
        internal Combination2AXQ (Argument rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2AXQ.EvalStep";
#endif

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2AXQ.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, this.rand1Value));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue(this.ratorOffset), ev0, this.rand1Value);
        }
    }

    [Serializable]
    sealed class Combination2AXS : Combination2A
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal Combination2AXS (Argument rator, SCode rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2AXS.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2AXS.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), ev0, ev1);
        }
    }

    [Serializable]
    class Combination2Q : Combination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly IApplicable ratorValue;

        protected Combination2Q (Quotation rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.ratorValue = (IApplicable) rator.Quoted;
        }

        public static SCode Make (Quotation rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is Quotation) ? Combination2QQ.Make (rator, (Quotation) rand0, rand1) :
                (rand0 is StaticVariable) ? Combination2QS.Make (rator, (StaticVariable) rand0, rand1) :
                new Combination2Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2Q.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2Q.EvalStep";
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
            SCode.location = "Combination2Q.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return this.ratorValue.Call (out answer, ref expression, ref environment,  ev0, ev1);
        }
    }

    [Serializable]
    class Combination2QQ : Combination2Q
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;

        protected Combination2QQ (Quotation rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        public static SCode Make (Quotation rator, Quotation rand0, SCode rand1)
        {
            return
                //(rand0 is Quotation) ? Combination2QQ.Make (rator, (Quotation) rand0, rand1) :
                //(rand0 is StaticVariable) ? Combination2QS.Make (rator, (StaticVariable) rand0, rand1) :
                new Combination2QQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2QQ";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2QQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return this.ratorValue.Call (out answer, ref expression, ref environment, this.rand0Value, ev1);
        }
    }

    [Serializable]
    class Combination2QS : Combination2Q
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected Combination2QS (Quotation rator, StaticVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Quotation rator, StaticVariable rand0, SCode rand1)
        {
            return
                //(rand0 is Quotation) ? Combination2QQ.Make (rator, (Quotation) rand0, rand1) :
                //(rand0 is StaticVariable) ? Combination2QS.Make (rator, (StaticVariable) rand0, rand1) :
                (rand1 is Argument) ? Combination2QSA.Make (rator, rand0, (Argument) rand1) :
                new Combination2QS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2QS";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2QS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return this.ratorValue.Call (out answer, ref expression, ref environment, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2QSA : Combination2QS
    {
        public readonly int rand1Offset;

        protected Combination2QSA (Quotation rator, StaticVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand0.Offset;
        }

        public static SCode Make (Quotation rator, StaticVariable rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? new Combination2QSA0 (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? new Combination2QSA1 (rator, rand0, (Argument1) rand1) :
                new Combination2QSA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2QSA";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            return this.ratorValue.Call (out answer, ref expression, ref environment, ev0, environment.ArgumentValue (this.rand1Offset));
        }
    }

    [Serializable]
    sealed class Combination2QSA0 : Combination2QSA
    {
        internal Combination2QSA0 (Quotation rator, StaticVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2QSA0";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            return this.ratorValue.Call (out answer, ref expression, ref environment, ev0, environment.Argument0Value);
        }
    }

    [Serializable]
    sealed class Combination2QSA1 : Combination2QSA
    {
        internal Combination2QSA1 (Quotation rator, StaticVariable rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2QSA1";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            return this.ratorValue.Call (out answer, ref expression, ref environment, ev0, environment.Argument1Value);
        }
    }

}
