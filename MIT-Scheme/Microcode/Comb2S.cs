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
                (rand0 is PrimitiveCar) ? Combination2SCar.Make (rator, (PrimitiveCar) rand0, rand1) :
                (rand0 is PrimitiveCdr) ? Combination2SCdr.Make (rator, (PrimitiveCdr) rand0, rand1) :
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
                (rand1 is Argument) ? Combination2SAA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? new Combination2SAQ (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new Combination2SAS (rator, rand0, (StaticVariable) rand1) :
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
    class Combination2SAA : Combination2SA
    {
        public readonly int rand1Offset;

        protected Combination2SAA (StaticVariable rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (StaticVariable rator, Argument rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? new Combination2SAA0 (rator, rand0, (Argument0) rand1) :
                new Combination2SAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2SAA.EvalStep");
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2SAA0 : Combination2SAA
    {
        internal Combination2SAA0 (StaticVariable rator, Argument rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2SAA0.EvalStep");
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2SAQ : Combination2SA
    {
        public readonly object rand1Value;
        internal Combination2SAQ (StaticVariable rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2SAQ");
#endif
            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue(this.rand0Offset), this.rand1Value);
        }
    }

    [Serializable]
    sealed class Combination2SAS : Combination2SA
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        internal Combination2SAS (StaticVariable rator, Argument rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2SAS");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue(this.rand0Offset), ev1);
        }
    }

    [Serializable]
    class Combination2SCar : Combination2S
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        protected readonly Type rand0ArgType;
#endif
        public readonly SCode rand0Arg;

        protected Combination2SCar (StaticVariable rator, PrimitiveCar rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
#if DEBUG
            this.rand0ArgType = rand0.Operand.GetType();
#endif
            this.rand0Arg = rand0.Operand;        
        }

        public static SCode Make (StaticVariable rator, PrimitiveCar rand0, SCode rand1)
        {
            return
                (rand0.Operand is Argument) ? Combination2SCarA.Make (rator, (PrimitiveCarA) rand0, rand1) :
                (rand0.Operand is StaticVariable) ? Combination2SCarS.Make (rator, rand0, rand1) :
                (rand1 is Quotation) ? Combination2SCarXQ.Make (rator, rand0, (Quotation) rand1) :
                new Combination2SCar (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0Arg);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0ArgType);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2SCar.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SCar.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.rand0Arg;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SCar.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Car, ev1);
        }
    }

    [Serializable]
    class Combination2SCarA : Combination2SCar
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;

        protected Combination2SCarA (StaticVariable rator, PrimitiveCarA rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Offset = ((Argument) rand0.Operand).Offset;
        }

        public static SCode Make (StaticVariable rator, PrimitiveCarA rand0, SCode rand1)
        {
            return
                (rand0.Operand is Argument0) ? Combination2SCarA0.Make (rator, (PrimitiveCarA0) rand0, rand1) :
                (rand1 is Argument) ? Combination2SCarAA.Make (rator, rand0, (Argument) rand1) :
                new Combination2SCarA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2SCarA.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SCarA.EvalStep";
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Car, ev1);
        }
    }

    [Serializable]
    class Combination2SCarA0 : Combination2SCarA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected Combination2SCarA0 (StaticVariable rator, PrimitiveCarA0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (StaticVariable rator, PrimitiveCarA0 rand0, SCode rand1)
        {
            return
                (rand1 is PrimitiveCar) ? Combination2SCarA0Car.Make (rator, rand0, (PrimitiveCar) rand1) :
                (rand1 is StaticVariable) ? Combination2SCarA0S.Make (rator, rand0, (StaticVariable) rand1) :
                new Combination2SCarA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2SCarA0.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SCarA0.EvalStep";
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Car, ev1);
        }
    }

    class Combination2SCarA0Car : Combination2SCarA0
    {
        public readonly SCode rand1Inner;

        protected Combination2SCarA0Car (StaticVariable rator, PrimitiveCarA0 rand0, PrimitiveCar rand1)
            : base (rator, rand0, rand1)
        {
            rand1Inner = rand1.Operand;
        }

        public static SCode Make (StaticVariable rator, PrimitiveCarA0 rand0, PrimitiveCar rand1)
        {
            return
                (rand1.Operand is Argument) ? Combination2SCarA0CarA.Make (rator, rand0, rand1) :
                new Combination2SCarA0Car (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1Inner);
            SCode.location = "Combination2SCarA0Car.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1Inner;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SCarA0Car.EvalStep";
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Car, ((Cons)ev1).Car);
        }
    }

    class Combination2SCarA0CarA : Combination2SCarA0Car
    {
        public readonly int rand1InnerOffset;

        protected Combination2SCarA0CarA (StaticVariable rator, PrimitiveCarA0 rand0, PrimitiveCar rand1)
            : base (rator, rand0, rand1)
        {
            rand1InnerOffset = ((Argument) rand1.Operand).Offset;
        }

        public static SCode Make (StaticVariable rator, PrimitiveCarA0 rand0, PrimitiveCar rand1)
        {
            return
                new Combination2SCarA0CarA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2SCarA0CarA.EvalStep");
#endif
            object ev1 = environment.ArgumentValue (this.rand1InnerOffset);

            object ev0 = environment.Argument0Value;

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Car, ((Cons) ev1).Car);
        }
    }


    [Serializable]
    class Combination2SCarA0S : Combination2SCarA0
    {

        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        protected Combination2SCarA0S (StaticVariable rator, PrimitiveCarA0 rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (StaticVariable rator, PrimitiveCarA0 rand0, StaticVariable rand1)
        {
            return
                new Combination2SCarA0S (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2SCarA0S.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Car, ev1);
        }
    }


    [Serializable]
    class Combination2SCarAA : Combination2SCarA
    {
        public readonly int rand1Offset;

        protected Combination2SCarAA (StaticVariable rator, PrimitiveCarA rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (StaticVariable rator, PrimitiveCarA rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2SCarAA0.Make (rator, rand0, (Argument0) rand1) :
                new Combination2SCarAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2SCarAA.EvalStep";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Car, ev1);
        }
    }

    [Serializable]
    class Combination2SCarAA0 : Combination2SCarAA
    {
        protected Combination2SCarAA0 (StaticVariable rator, PrimitiveCarA rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (StaticVariable rator, PrimitiveCarA rand0, Argument0 rand1)
        {
            return
                new Combination2SCarAA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2SCarAA0.EvalStep";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Car, ev1);
        }
    }

    [Serializable]
    class Combination2SCarS : Combination2SCar
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected Combination2SCarS (StaticVariable rator, PrimitiveCar rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = ((StaticVariable) rand0.Operand).Name;
            this.rand0Offset = ((StaticVariable) rand0.Operand).Offset;
        }

        public static SCode Make (StaticVariable rator, PrimitiveCar rand0, SCode rand1)
        {
            return
                new Combination2SCarS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2SCarS.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SCarS.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException();

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Car, ev1);
        }
    }

    [Serializable]
    class Combination2SCarXQ : Combination2SCar
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected Combination2SCarXQ (StaticVariable rator, PrimitiveCar rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (StaticVariable rator, PrimitiveCar rand0, Quotation rand1)
        {
            return
                new Combination2SCarXQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0Arg);
            rand0TypeHistogram.Note (this.rand0ArgType);
            SCode.location = "Combination2SCarXQ.EvalStep";
#endif
            object ev1 = this.rand1Value;

            object ev0;
            Environment env = environment;
            Control unev = this.rand0Arg;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SCar.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Car, ev1);
        }
    }


    [Serializable]
    class Combination2SCdr : Combination2S
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        protected readonly Type rand0ArgType;
#endif
        public readonly SCode rand0Arg;

        protected Combination2SCdr (StaticVariable rator, PrimitiveCdr rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
#if DEBUG
            this.rand0ArgType = rand0.Operand.GetType ();
#endif
            this.rand0Arg = rand0.Operand;
        }

        public static SCode Make (StaticVariable rator, PrimitiveCdr rand0, SCode rand1)
        {
            return
                (rand0.Operand is Argument) ? Combination2SCdrA.Make (rator, rand0, rand1) :
                (rand0.Operand is StaticVariable) ? Combination2SCdrS.Make (rator, rand0, rand1) :
                new Combination2SCdr (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0Arg);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0ArgType);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2SCdr.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SCdr.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.rand0Arg;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SCdr.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Cdr, ev1);
        }
    }

    [Serializable]
    class Combination2SCdrA : Combination2SCdr
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;

        protected Combination2SCdrA (StaticVariable rator, PrimitiveCdr rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Offset = ((Argument) rand0.Operand).Offset;
        }

        public static SCode Make (StaticVariable rator, PrimitiveCdr rand0, SCode rand1)
        {
            return
                (rand0.Operand is Argument0) ? Combination2SCdrA0.Make (rator, rand0, rand1) :
                new Combination2SCdrA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2SCdrA.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SCdrA.EvalStep";
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Cdr, ev1);
        }
    }

    [Serializable]
    class Combination2SCdrA0 : Combination2SCdrA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected Combination2SCdrA0 (StaticVariable rator, PrimitiveCdr rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (StaticVariable rator, PrimitiveCdr rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? Combination2SCdrA0A.Make (rator, rand0, (Argument) rand1) :
                new Combination2SCdrA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2SCdrA0.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SCdrA0.EvalStep";
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Cdr, ev1);
        }
    }

    [Serializable]
    class Combination2SCdrA0A : Combination2SCdrA0
    {

        public readonly int rand1Offset;
        protected Combination2SCdrA0A (StaticVariable rator, PrimitiveCdr rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (StaticVariable rator, PrimitiveCdr rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2SCdrA0A0.Make (rator, rand0, (Argument0) rand1) :
                new Combination2SCdrA0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2SCdrA0A.EvalStep";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Cdr, ev1);
        }
    }

    [Serializable]
    class Combination2SCdrA0A0 : Combination2SCdrA0A
    {

        protected Combination2SCdrA0A0 (StaticVariable rator, PrimitiveCdr rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (StaticVariable rator, PrimitiveCdr rand0, Argument0 rand1)
        {
            return
                new Combination2SCdrA0A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2SCdrA0A0.EvalStep";
#endif

            object ev0 = environment.Argument0Value;

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Cdr, ev0);
        }
    }

    [Serializable]
    class Combination2SCdrS : Combination2SCdr
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected Combination2SCdrS (StaticVariable rator, PrimitiveCdr rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = ((StaticVariable) rand0.Operand).Name;
            this.rand0Offset = ((StaticVariable) rand0.Operand).Offset;
        }

        public static SCode Make (StaticVariable rator, PrimitiveCdr rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? Combination2SCdrSA.Make (rator, rand0, (Argument) rand1) :
                new Combination2SCdrS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2SCdrS.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SCdrS.EvalStep";
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Cdr, ev1);
        }
    }

    [Serializable]
    class Combination2SCdrSA : Combination2SCdrS
    {
        public readonly int rand1Offset;

        protected Combination2SCdrSA (StaticVariable rator, PrimitiveCdr rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (StaticVariable rator, PrimitiveCdr rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2SCdrSA0.Make (rator, rand0, (Argument0) rand1) :
                new Combination2SCdrSA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2SCdrSA.EvalStep";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Cdr, ev1);
        }
    }

    [Serializable]
    class Combination2SCdrSA0 : Combination2SCdrSA
    {
        protected Combination2SCdrSA0 (StaticVariable rator, PrimitiveCdr rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (StaticVariable rator, PrimitiveCdr rand0, Argument0 rand1)
        {
            return
                new Combination2SCdrSA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2SCdrSA0.EvalStep";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) ev0).Cdr, ev1);
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
                (rand1 is Argument) ? Combination2SQA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? new Combination2SQS (rator, rand0, (StaticVariable) rand1) :
                new Combination2SQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2SQ";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SQ";
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
    class Combination2SQA : Combination2SQ
    {

        public readonly int rand1Offset;

        protected Combination2SQA (StaticVariable rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (StaticVariable rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2SQA0.Make (rator, rand0, (Argument0) rand1) :
                new Combination2SQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2SQA.EvalStep";
#endif


            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, environment.ArgumentValue (this.rand1Offset));
        }
    }

    [Serializable]
    class Combination2SQA0 : Combination2SQA
    {

        protected Combination2SQA0 (StaticVariable rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (StaticVariable rator, Quotation rand0, Argument0 rand1)
        {
            return

                new Combination2SQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2SQA0.EvalStep";
#endif


            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, environment.Argument0Value);
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
                (rand1 is Argument) ? Combination2SSA.Make (rator, rand0, (Argument) rand1) :
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
    class Combination2SSA : Combination2SS
    {

        public readonly int rand1Offset;

        protected Combination2SSA (StaticVariable rator, StaticVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (StaticVariable rator, StaticVariable rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2SSA0.Make (rator, rand0, (Argument0) rand1) :
                new Combination2SSA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");

            SCode.location = "Combination2SSA.EvalStep";
#endif


            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.ArgumentValue (this.rand1Offset));
        }
    }

    [Serializable]
    class Combination2SSA0 : Combination2SSA
    {


        protected Combination2SSA0 (StaticVariable rator, StaticVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (StaticVariable rator, StaticVariable rand0, Argument0 rand1)
        {
            return
                new Combination2SSA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");

            SCode.location = "Combination2SSA0.EvalStep";
#endif


            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument0Value);
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
                (rand0 is Argument) ? Combination2AA.Make (rator, (Argument) rand0, rand1) :
                (rand0 is Quotation) ? Combination2AQ.Make (rator, (Quotation) rand0, rand1) :
                (rand0 is StaticVariable) ? Combination2AS.Make (rator, (StaticVariable) rand0, rand1) :
                (rand1 is Argument) ? Combination2AXA.Make (rator, rand0, (Argument) rand1):
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
                (rand1 is StaticVariable) ? new Combination2A0XS (rator, rand0, (StaticVariable) rand1) :
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
    sealed class Combination2A0XS : Combination2A0
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        internal Combination2A0XS (Argument0 rator, SCode rand0, StaticVariable rand1)
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
            SCode.location = "Combination2A0XS.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2A0XS.EvalStep";
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
    class Combination2AS : Combination2A
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;
        protected Combination2AS (Argument rator, StaticVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Argument rator, StaticVariable rand0, SCode rand1)
        {
            return
                (rand1 is StaticVariable) ? new Combination2ASS (rator, rand0, (StaticVariable) rand1) :
                (rand1 is Quotation) ? new Combination2ASQ (rator, rand0, (Quotation) rand1) :
                new Combination2AS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2AS.EvalStep";
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2AS.EvalStep";
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

            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue(this.ratorOffset), ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2ASS : Combination2AS
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal Combination2ASS (Argument rator, StaticVariable rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2ASS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue(this.ratorOffset), ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2ASQ : Combination2AS
    {
        public readonly object rand1Value;

        internal Combination2ASQ (Argument rator, StaticVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2ASQ";
#endif
            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), ev0, ev1);
        }
    }


    [Serializable]
    class Combination2AA : Combination2A
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;

        protected Combination2AA (Argument rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Argument rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? Combination2AA0.Make (rator, (Argument0) rand0, rand1) :
                (rand1 is Quotation) ? new Combination2AAQ (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new Combination2AAS (rator, rand0, (StaticVariable) rand1) :
                new Combination2AA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2AA.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2AA.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), environment.ArgumentValue(this.rand0Offset), ev1);
        }
    }

    [Serializable]
    class Combination2AA0 : Combination2AA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2AA0 (Argument rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Argument rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? Combination2AA0A.Make (rator, rand1, (Argument) rand1):
                (rand1 is Quotation) ? Combination2AA0Q.Make (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? Combination2AA0S.Make (rator, rand0, (StaticVariable) rand1) :
                new Combination2AA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2AA0.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2AA0.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), environment.Argument0Value, ev1);
        }
    }

    [Serializable]
    class Combination2AA0A : Combination2AA0
    {
        public readonly int rand1Offset;

        protected Combination2AA0A (Argument rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Argument rator, Argument0 rand0, Argument rand1)
        {
            return
                new Combination2AA0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2AA0A.EvalStep";
#endif

            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), environment.Argument0Value, environment.ArgumentValue(this.rand1Offset));
        }
    }

    [Serializable]
    class Combination2AA0A0 : Combination2AA0A
    {
        protected Combination2AA0A0 (Argument rator, Argument0 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Argument rator, Argument0 rand0, Argument0 rand1)
        {
            return
                new Combination2AA0A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2AA0A0.EvalStep";
#endif

            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), environment.Argument0Value, environment.Argument0Value);
        }
    }

    [Serializable]
    class Combination2AA0Q : Combination2AA0
    {
        public readonly object rand1Value;
        protected Combination2AA0Q (Argument rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
        this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Argument rator, Argument0 rand0, Quotation rand1)
        {
            return
                new Combination2AA0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2AA0Q.EvalStep";
#endif

            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), environment.Argument0Value, this.rand1Value);
        }
    }

    [Serializable]
    class Combination2AA0S : Combination2AA0
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected Combination2AA0S (Argument rator, Argument0 rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Argument rator, Argument0 rand0, StaticVariable rand1)
        {
            return
                new Combination2AA0S (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2AA0S.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), environment.Argument0Value, ev1);
        }
    }

    [Serializable]
    sealed class Combination2AAQ : Combination2AA
    {
        public readonly object rand1Value;
        internal Combination2AAQ (Argument rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2AAQ.EvalStep";
#endif

            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), environment.ArgumentValue(this.rand0Offset), this.rand1Value);
        }
    }

    [Serializable]
    class Combination2AAS : Combination2AA
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal Combination2AAS (Argument rator, Argument rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Argument rator, Argument rand0, StaticVariable rand1)
        {
            return
                new Combination2AAS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2AAS.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), environment.ArgumentValue(this.rand0Offset), ev1);
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
                (rand1 is Argument) ? Combination2AQA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? new Combination2AQQ (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new Combination2AQS (rator, rand0, (StaticVariable) rand1) :
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
    class Combination2AQA : Combination2AQ
    {
        public readonly int rand1Offset;

        protected Combination2AQA (Argument rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Argument rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2AQA0.Make (rator, rand0, (Argument0) rand1) :
                new Combination2AQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2AQA.EvalStep";
#endif


            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), this.rand0Value, environment.ArgumentValue(this.rand1Offset));
        }
    }

    [Serializable]
    class Combination2AQA0 : Combination2AQA
    {
        protected Combination2AQA0 (Argument rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Argument rator, Quotation rand0, Argument0 rand1)
        {
            return
                new Combination2AQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2AQA0.EvalStep";
#endif


            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), this.rand0Value, environment.Argument0Value);
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
    sealed class Combination2AQS : Combination2AQ
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal Combination2AQS (Argument rator, Quotation rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2AQS.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue(this.ratorOffset), this.rand0Value, ev1);
        }
    }

    [Serializable]
    class Combination2AXA : Combination2A
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;

        internal Combination2AXA (Argument rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Argument rator, SCode rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2AXA0.Make (rator, rand0, (Argument0) rand1) :
               new Combination2AXA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2AXA.EvalStep";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2AXA.EvalStep";
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
    class Combination2AXA0 : Combination2AXA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif


        internal Combination2AXA0 (Argument rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Argument rator, SCode rand0, Argument0 rand1)
        {
            return
                new Combination2AXA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2AXA0.EvalStep";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2AXA0.EvalStep";
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
                (rand1 is Argument) ? Combination2QQA.Make (rator, rand0, (Argument) rand1) :
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
    class Combination2QQA : Combination2QQ
    {

        public readonly int rand1Offset;

        protected Combination2QQA (Quotation rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Quotation rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2QQA0.Make (rator, rand0, (Argument0) rand1) :
                new Combination2QQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2QQA";
#endif
 

            return this.ratorValue.Call (out answer, ref expression, ref environment, this.rand0Value, environment.ArgumentValue(this.rand1Offset));
        }
    }

    [Serializable]
    class Combination2QQA0 : Combination2QQA
    {


        protected Combination2QQA0 (Quotation rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Quotation rator, Quotation rand0, Argument0 rand1)
        {
            return
                new Combination2QQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2QQA0";
#endif


            return this.ratorValue.Call (out answer, ref expression, ref environment, this.rand0Value, environment.Argument0Value);
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
                (rand1 is Quotation) ? Combination2QSQ.Make (rator, rand0, (Quotation) rand1) :
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
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Quotation rator, StaticVariable rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? new Combination2QSA0 (rator, rand0, (Argument0) rand1) :
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
    class Combination2QSQ : Combination2QS
    {
        public readonly object rand1Value;

        protected Combination2QSQ (Quotation rator, StaticVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Quotation rator, StaticVariable rand0, Quotation rand1)
        {
            return
                new Combination2QSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2QSQ";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            return this.ratorValue.Call (out answer, ref expression, ref environment, ev0, this.rand1Value);
        }
    }

}
