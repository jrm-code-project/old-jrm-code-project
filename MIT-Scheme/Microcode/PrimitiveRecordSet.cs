using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    [Serializable]
    class PrimitiveRecordSet : PrimitiveCombination3
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveRecordSet (SCode arg0, SCode arg1, SCode arg2)
            : base (Primitive.RecordSet, arg0, arg1, arg2)
        {
        }

        public static PrimitiveRecordSet Make (SCode arg0, SCode arg1, SCode arg2)
        {
            return
                (arg0 is Argument) ? PrimitiveRecordSetA.Make ((Argument) arg0, arg1, arg2) :
                (arg0 is StaticVariable) ? PrimitiveRecordSetS.Make ((StaticVariable) arg0, arg1, arg2) :
                new PrimitiveRecordSet (arg0, arg1, arg2);
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
            SCode.location = "PrimitiveRecordSet";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveRecordSet";
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
            SCode.location = "PrimitiveRecordSet";
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
            SCode.location = "PrimitiveRecordSet";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

            answer = ((Record) ev0).Set ((int) ev1, ev2);
            return false;
        }
    }

    [Serializable]
    class PrimitiveRecordSetA : PrimitiveRecordSet
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;

        protected PrimitiveRecordSetA (Argument arg0, SCode arg1, SCode arg2)
            : base (arg0, arg1, arg2)
        {
            this.rand0Offset = arg0.Offset;
        }
        public static PrimitiveRecordSetA Make (Argument arg0, SCode arg1, SCode arg2)
        {
            return
                (arg0 is Argument0) ? PrimitiveRecordSetA0.Make ((Argument0) arg0, arg1, arg2) :
                new PrimitiveRecordSetA (arg0, arg1, arg2);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            NoteCalls (this.arg2);
            rand1TypeHistogram.Note (this.rand1Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveRecordSetA";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveRecordSetA";
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
            SCode.location = "PrimitiveRecordSetA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = ((Record) environment.ArgumentValue(this.rand0Offset)).Set ((int) ev1, ev2);
            return false;
        }
    }

    [Serializable]
    class PrimitiveRecordSetA0 : PrimitiveRecordSetA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveRecordSetA0 (Argument0 arg0, SCode arg1, SCode arg2)
            : base (arg0, arg1, arg2)
        {
        }
        public static PrimitiveRecordSetA0 Make (Argument0 arg0, SCode arg1, SCode arg2)
        {
            return
                (arg1 is Quotation) ? PrimitiveRecordSetA0Q.Make (arg0, (Quotation) arg1, arg2) :
                new PrimitiveRecordSetA0 (arg0, arg1, arg2);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            NoteCalls (this.arg2);
            rand1TypeHistogram.Note (this.rand1Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveRecordSetA0";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveRecordSetA0";
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
            SCode.location = "PrimitiveRecordSetA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = ((Record) environment.Argument0Value).Set ((int) ev1, ev2);
            return false;
        }
    }

    [Serializable]
    class PrimitiveRecordSetA0Q : PrimitiveRecordSetA0
    {
#if DEBUG
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PrimitiveRecordSetA0Q (Argument0 arg0, Quotation arg1, SCode arg2)
            : base (arg0, arg1, arg2)
        {
            this.rand1Value = (int) arg1.Quoted;
        }

        public static PrimitiveRecordSetA0 Make (Argument0 arg0, Quotation arg1, SCode arg2)
        {
            return
                new PrimitiveRecordSetA0Q (arg0, arg1, arg2);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveRecordSetA0Q";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveRecordSetA0Q";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = ((Record) environment.Argument0Value).Set (this.rand1Value, ev2);
            return false;
        }
    }

    [Serializable]
    class PrimitiveRecordSetS : PrimitiveRecordSet
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected PrimitiveRecordSetS (StaticVariable arg0, SCode arg1, SCode arg2)
            : base (arg0, arg1, arg2)
        {
            this.rand0Name = arg0.Name;
            this.rand0Offset = arg0.Offset;
        }
        public static PrimitiveRecordSetS Make (StaticVariable arg0, SCode arg1, SCode arg2)
        {
            return
                (arg1 is Quotation) ? PrimitiveRecordSetSQ.Make (arg0, (Quotation) arg1, arg2) :
                new PrimitiveRecordSetS (arg0, arg1, arg2);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            NoteCalls (this.arg2);
            rand1TypeHistogram.Note (this.rand1Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveRecordSetS";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveRecordSetS";
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
            SCode.location = "PrimitiveRecordSetS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((Record) ev0).Set ((int) ev1, ev2);
            return false;
        }
    }

    [Serializable]
    class PrimitiveRecordSetSQ : PrimitiveRecordSetS
    {
#if DEBUG
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PrimitiveRecordSetSQ (StaticVariable arg0, Quotation arg1, SCode arg2)
            : base (arg0, arg1, arg2)
        {
            this.rand1Value = (int) arg1.Quoted;
        }
        public static PrimitiveRecordSetSQ Make (StaticVariable arg0, Quotation arg1, SCode arg2)
        {
            return
                (arg2 is Quotation) ? new PrimitiveRecordSetSQQ (arg0, arg1, (Quotation) arg2) :
                (arg2 is StaticVariable) ? new PrimitiveRecordSetSQS (arg0, arg1, (StaticVariable) arg2) :
                new PrimitiveRecordSetSQ (arg0, arg1, arg2);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveRecordSetSQ";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveRecordSetSQ";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((Record) ev0).Set ((int) this.rand1Value, ev2);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveRecordSetSQS : PrimitiveRecordSetSQ
    {
        public readonly Symbol rand2Name;
        public readonly int rand2Offset;

        internal PrimitiveRecordSetSQS (StaticVariable arg0, Quotation arg1, StaticVariable arg2)
            : base (arg0, arg1, arg2)
        {
            this.rand2Name = arg2.Name;
            this.rand2Offset = arg2.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PrimitiveRecordSetSQS";
#endif
            object ev2;
            if (environment.StaticValue (out ev2, this.rand2Name, this.rand2Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((Record) ev0).Set (this.rand1Value, ev2);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveRecordSetSQQ : PrimitiveRecordSetSQ
    {
        public readonly object rand2Value;

        internal PrimitiveRecordSetSQQ (StaticVariable arg0, Quotation arg1, Quotation arg2)
            : base (arg0, arg1, arg2)
        {
            this.rand2Value = arg2.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveRecordSetSQQ");
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((Record) ev0).Set (this.rand1Value, this.rand2Value);
            return false;
        }
    }
}

