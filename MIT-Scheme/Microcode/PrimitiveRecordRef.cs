using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PrimitiveRecordRef : PrimitiveCombination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PrimitiveRecordRef (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? PrimitiveRecordRefA.Make(rator, (Argument) rand0, rand1) :
                (rand0 is StaticVariable) ? PrimitiveRecordRefS.Make (rator, (StaticVariable) rand0, rand1) :
                (rand1 is Quotation) ? PrimitiveRecordRefXQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveRecordRef (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveRecordRef";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
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
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            answer = ((Record) ev0).Ref((int)ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveRecordRefA : PrimitiveRecordRef
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;
        protected PrimitiveRecordRefA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveRecordRefA0.Make (rator, (Argument0) rand0, rand1) :
                (rand1 is Quotation) ? PrimitiveRecordRefAQ.Make (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new PrimitiveRecordRefAS (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveRecordRefA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveRecordRefA";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            answer = ((Record) environment.ArgumentValue (this.rand0Offset)).Ref ((int) ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveRecordRefA0 : PrimitiveRecordRefA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PrimitiveRecordRefA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? PrimitiveRecordRefA0Q.Make (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? PrimitiveRecordRefA0S.Make (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveRecordRefA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveRecordRefA0";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            answer = ((Record) environment.Argument0Value).Ref ((int) ev1);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveRecordRefA0Q : PrimitiveRecordRefA0
    {
        public readonly int offset;
        PrimitiveRecordRefA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.offset = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitiveRecordRefA0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveRecordRefA0Q");
#endif
            answer = ((Record) environment.Argument0Value).Ref (this.offset);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveRecordRefA0S : PrimitiveRecordRefA0
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        PrimitiveRecordRefA0S (Primitive2 rator, Argument0 rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, StaticVariable rand1)
        {
            return
                new PrimitiveRecordRefA0S (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveRecordRefA0S");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            answer = ((Record) environment.Argument0Value).Ref ((int) ev1);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveRecordRefAQ : PrimitiveRecordRefA
    {
        public readonly int offset;
        PrimitiveRecordRefAQ (Primitive2 rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.offset = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Quotation rand1)
        {
            return
                new PrimitiveRecordRefAQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveRecordRefAQ");
#endif
            answer = ((Record) environment.ArgumentValue(this.rand0Offset)).Ref (this.offset);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveRecordRefAS : PrimitiveRecordRefA
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        internal PrimitiveRecordRefAS (Primitive2 rator, Argument rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveRecordRefAS");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            answer = ((Record) environment.ArgumentValue(this.rand0Offset)).Ref ((int) ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveRecordRefS : PrimitiveRecordRef
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;
        protected PrimitiveRecordRefS (Primitive2 rator, StaticVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, StaticVariable rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? PrimitiveRecordRefSQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveRecordRefS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveRecordRefS";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            // Eval argument0
            answer = ((Record) ev0).Ref ((int) ev1);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveRecordRefSQ : PrimitiveRecordRefS
    {
        public readonly int offset;
        protected PrimitiveRecordRefSQ (Primitive2 rator, StaticVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.offset = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, StaticVariable rand0, Quotation rand1)
        {
            return
                new PrimitiveRecordRefSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveRecordRefSQ");
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            // Eval argument0
            answer = ((Record) ev0).Ref (this.offset);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveRecordRefXQ : PrimitiveRecordRef
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static int [] offsetHistogram = new int [64];
#endif
        public readonly int offset;
        protected PrimitiveRecordRefXQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.offset = (int) rand1.Quoted;
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveRecordRefXQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            offsetHistogram [this.offset] += 1;
            SCode.location = "PrimitiveRecordRefXQ";
#endif
            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            answer = ((Record) ev0).Ref (this.offset);
            return false;
        }
    }

}
