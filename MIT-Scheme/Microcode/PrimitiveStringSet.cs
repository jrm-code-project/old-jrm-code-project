using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    [Serializable]
    class PrimitiveStringSet : PrimitiveCombination3
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveStringSet (SCode arg0, SCode arg1, SCode arg2)
            : base (Primitive.StringSet, arg0, arg1, arg2)
        {
        }
        public static PrimitiveStringSet Make (SCode arg0, SCode arg1, SCode arg2)
        {
            return
                (arg0 is Argument) ? PrimitiveStringSetA.Make ((Argument) arg0, arg1, arg2) :
                (arg0 is StaticVariable) ? PrimitiveStringSetS.Make ((StaticVariable) arg0, arg1, arg2) :
                new PrimitiveStringSet (arg0, arg1, arg2);
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
            SCode.location = "PrimitiveStringSet";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveStringSet";
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
            SCode.location = "PrimitiveStringSet";
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
            SCode.location = "PrimitiveStringSet";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

            answer = ((char []) ev0)[(int) ev1];
            ((char []) ev0)[(int) ev1] = (char) ev2;
            return false;
        }
    }

    [Serializable]
    class PrimitiveStringSetA : PrimitiveStringSet
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;

        protected PrimitiveStringSetA (Argument arg0, SCode arg1, SCode arg2)
            : base (arg0, arg1, arg2)
        {
            this.rand0Offset = arg0.Offset;
        }
        public static PrimitiveStringSetA Make (Argument arg0, SCode arg1, SCode arg2)
        {
            return
                (arg0 is Argument0) ? PrimitiveStringSetA0.Make ((Argument0) arg0, arg1, arg2) :
                new PrimitiveStringSetA (arg0, arg1, arg2);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            NoteCalls (this.arg2);
            rand1TypeHistogram.Note (this.rand1Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveStringSetA";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveStringSetA";
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
            SCode.location = "PrimitiveStringSetA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);
            answer = ((char []) ev0) [(int) ev1];
            ((char []) ev0) [(int) ev1] = (char) ev2;
            return false;
        }
    }

    [Serializable]
    class PrimitiveStringSetA0 : PrimitiveStringSetA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveStringSetA0 (Argument0 arg0, SCode arg1, SCode arg2)
            : base (arg0, arg1, arg2)
        {
        }
        public static PrimitiveStringSetA0 Make (Argument0 arg0, SCode arg1, SCode arg2)
        {
            return
                (arg1 is Quotation) ? PrimitiveStringSetA0Q.Make (arg0, (Quotation) arg1, arg2) :
                new PrimitiveStringSetA0 (arg0, arg1, arg2);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            NoteCalls (this.arg2);
            rand1TypeHistogram.Note (this.rand1Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveStringSetA0";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveStringSetA0";
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
            SCode.location = "PrimitiveStringSetA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0 = environment.Argument0Value;
            answer = ((char []) ev0) [(int) ev1];
            ((char []) ev0) [(int) ev1] = (char) ev2;
            return false;
        }
    }

    [Serializable]
    class PrimitiveStringSetA0Q : PrimitiveStringSetA0
    {
#if DEBUG
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PrimitiveStringSetA0Q (Argument0 arg0, Quotation arg1, SCode arg2)
            : base (arg0, arg1, arg2)
        {
            this.rand1Value = (int) arg1.Quoted;
        }

        public static PrimitiveStringSetA0 Make (Argument0 arg0, Quotation arg1, SCode arg2)
        {
            return
                new PrimitiveStringSetA0Q (arg0, arg1, arg2);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveStringSetA0Q";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveStringSetA0Q";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0 = environment.Argument0Value;
            object ev1 = this.rand1Value;
            answer = ((char []) ev0) [(int) ev1];
            ((char []) ev0) [(int) ev1] = (char) ev2;
            return false;
        }
    }

    [Serializable]
    class PrimitiveStringSetS : PrimitiveStringSet
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected PrimitiveStringSetS (StaticVariable arg0, SCode arg1, SCode arg2)
            : base (arg0, arg1, arg2)
        {
            this.rand0Name = arg0.Name;
            this.rand0Offset = arg0.Offset;
        }
        public static PrimitiveStringSetS Make (StaticVariable arg0, SCode arg1, SCode arg2)
        {
            return
                (arg1 is Quotation) ? PrimitiveStringSetSQ.Make (arg0, (Quotation) arg1, arg2) :
                new PrimitiveStringSetS (arg0, arg1, arg2);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            NoteCalls (this.arg2);
            rand1TypeHistogram.Note (this.rand1Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveStringSetS";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveStringSetS";
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
            SCode.location = "PrimitiveStringSetS";
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

            answer = ((char []) ev0) [(int) ev1];
            ((char []) ev0) [(int) ev1] = (char) ev2;
            return false;
        }
    }

    [Serializable]
    class PrimitiveStringSetSQ : PrimitiveStringSetS
    {
#if DEBUG
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected PrimitiveStringSetSQ (StaticVariable arg0, Quotation arg1, SCode arg2)
            : base (arg0, arg1, arg2)
        {
            this.rand1Value = (int) arg1.Quoted;
        }
        public static PrimitiveStringSetSQ Make (StaticVariable arg0, Quotation arg1, SCode arg2)
        {
            return
                (arg2 is Quotation) ? PrimitiveStringSetSQQ.Make (arg0, arg1, (Quotation) arg2) :
                new PrimitiveStringSetSQ (arg0, arg1, arg2);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveStringSetSQ";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveStringSetSQ";
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

            object ev1 = this.rand1Value;
            answer = ((char []) ev0) [(int) ev1];
            ((char []) ev0) [(int) ev1] = (char) ev2;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveStringSetSQQ : PrimitiveStringSetSQ
    {
        public readonly object rand2Value;

        protected PrimitiveStringSetSQQ (StaticVariable arg0, Quotation arg1, Quotation arg2)
            : base (arg0, arg1, arg2)
        {
            this.rand2Value = arg2.Quoted;
        }
        public static PrimitiveStringSetSQQ Make (StaticVariable arg0, Quotation arg1, Quotation arg2)
        {
            return
                new PrimitiveStringSetSQQ (arg0, arg1, arg2);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveStringSetSQQ");
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object ev1 = this.rand1Value;
            object ev2 = this.rand2Value;
            answer = ((char []) ev0) [(int) ev1];
            ((char []) ev0) [(int) ev1] = (char) ev2;
            return false;
        }
    }
}

