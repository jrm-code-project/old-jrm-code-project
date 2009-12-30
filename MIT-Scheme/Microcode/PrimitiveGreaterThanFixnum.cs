using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PrimitiveGreaterThanFixnum : PrimitiveCombination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PrimitiveGreaterThanFixnum (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? PrimitiveGreaterThanFixnumA.Make (rator, (Argument) rand0, rand1) :
                (rand0 is StaticVariable) ? PrimitiveGreaterThanFixnumS.Make (rator, (StaticVariable) rand0, rand1) :
                (rand0 is Quotation) ? PrimitiveGreaterThanFixnumQ.Make (rator, (Quotation) rand0, rand1) :
                (rand1 is Quotation) ? new PrimitiveGreaterThanFixnumXQ (rator, rand0, (Quotation) rand1) :
                new PrimitiveGreaterThanFixnum (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveGreaterThanFixnum";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveGreaterThanFixnum";
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
            SCode.location = "PrimitiveGreaterThanFixnum";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveGreaterThanFixnumA : PrimitiveGreaterThanFixnum
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;
        protected PrimitiveGreaterThanFixnumA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Offset = rand0.Offset;
        }

        public static new SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                //(rand0 is LexicalVariable) ? PrimitiveGreaterThanFixnumL.Make (rator, (LexicalVariable) rand0, rand1) :
                //(rand0 is Quotation) ? PrimitiveGreaterThanFixnumQ.Make (rator, (Quotation) rand0, rand1) :
                //(rand1 is LexicalVariable) ? PrimitiveGreaterThanFixnumSL.Make (rator, rand0, (LexicalVariable) rand1) :
                //(rand1 is Quotation) ? PrimitiveGreaterThanFixnumSQ.Make (rator, rand0, (Quotation) rand1) :
                (rand0 is Argument0) ? PrimitiveGreaterThanFixnumA0.Make (rator, (Argument0) rand0, rand1) :
                (rand0 is Argument1) ? PrimitiveGreaterThanFixnumA1.Make (rator, (Argument1) rand0, rand1) :
                new PrimitiveGreaterThanFixnumA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveGreaterThanFixnumA";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveGreaterThanFixnumA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveGreaterThanFixnumA0 : PrimitiveGreaterThanFixnumA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PrimitiveGreaterThanFixnumA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is StaticVariable) ? PrimitiveGreaterThanFixnumA0S.Make (rator, rand0, (StaticVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveGreaterThanFixnumA0Q.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveGreaterThanFixnumA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveGreaterThanFixnumA0";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveGreaterThanFixnumA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            object ev0 = environment.Argument0Value;

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveGreaterThanFixnumA0Q : PrimitiveGreaterThanFixnumA0
    {
        public readonly int rand1Value;

        PrimitiveGreaterThanFixnumA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static new SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitiveGreaterThanFixnumA0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumA0Q");
#endif
            // Greater-than-fixnum?
            answer = (int) environment.Argument0Value > this.rand1Value ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveGreaterThanFixnumA0S : PrimitiveGreaterThanFixnumA0
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        PrimitiveGreaterThanFixnumA0S (Primitive2 rator, Argument0 rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static new SCode Make (Primitive2 rator, Argument0 rand0, StaticVariable rand1)
        {
            return
                new PrimitiveGreaterThanFixnumA0S (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumA0S");
#endif
            // Eval argument1
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0 = environment.Argument0Value;

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveGreaterThanFixnumA1 : PrimitiveGreaterThanFixnumA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveGreaterThanFixnumA1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                //(rand0 is LexicalVariable) ? PrimitiveGreaterThanFixnumL.Make (rator, (LexicalVariable) rand0, rand1) :
                //(rand0 is Quotation) ? PrimitiveGreaterThanFixnumQ.Make (rator, (Quotation) rand0, rand1) :
                //(rand1 is LexicalVariable) ? PrimitiveGreaterThanFixnumSL.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? new PrimitiveGreaterThanFixnumA1Q (rator, rand0, (Quotation) rand1) :
                new PrimitiveGreaterThanFixnumA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveGreaterThanFixnumA1";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveGreaterThanFixnumA1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            object ev0 = environment.Argument1Value;

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveGreaterThanFixnumA1Q : PrimitiveGreaterThanFixnumA1
    {
        public readonly int rand1Value;
        internal PrimitiveGreaterThanFixnumA1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumA1Q");
#endif
            // Greater-than-fixnum?
            answer = (int) environment.Argument1Value > this.rand1Value ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveGreaterThanFixnumS : PrimitiveGreaterThanFixnum
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;
        protected PrimitiveGreaterThanFixnumS (Primitive2 rator, StaticVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Offset = rand0.Offset;
        }

        public static new SCode Make (Primitive2 rator, StaticVariable rand0, SCode rand1)
        {
            return
                new PrimitiveGreaterThanFixnumS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveGreaterThanFixnumS";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveGreaterThanFixnumS";
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
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveGreaterThanFixnumQ : PrimitiveGreaterThanFixnum
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Value;
        protected PrimitiveGreaterThanFixnumQ (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = (int) rand0.Quoted;
        }

        public static new SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                new PrimitiveGreaterThanFixnumQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveGreaterThanFixnumQ";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveGreaterThanFixnumQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Greater-than-fixnum?
            answer = this.rand0Value > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveGreaterThanFixnumQA : PrimitiveGreaterThanFixnumQ
    {
        public readonly int rand1Offset;

        protected PrimitiveGreaterThanFixnumQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static new SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? new PrimitiveGreaterThanFixnumQA0 (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? new PrimitiveGreaterThanFixnumQA1 (rator, rand0, (Argument1) rand1) :
                new PrimitiveGreaterThanFixnumQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PrimitiveGreaterThanFixnumQA";
#endif
            // Eval argument1
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            // Greater-than-fixnum?
            answer = this.rand0Value > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveGreaterThanFixnumQA0 : PrimitiveGreaterThanFixnumQA
    {
        internal PrimitiveGreaterThanFixnumQA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumQA0");
#endif
            // Greater-than-fixnum?
            answer = this.rand0Value > (int) environment.Argument0Value ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveGreaterThanFixnumQA1 : PrimitiveGreaterThanFixnumQA
    {
        internal PrimitiveGreaterThanFixnumQA1 (Primitive2 rator, Quotation rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumQA1");
#endif
            // Greater-than-fixnum?
            answer = this.rand0Value > (int) environment.Argument1Value ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveGreaterThanFixnumXQ : PrimitiveGreaterThanFixnum
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;
        internal PrimitiveGreaterThanFixnumXQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveGreaterThanFixnumXQ";
#endif
            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveGreaterThanFixnumXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Greater-than-fixnum?
            answer = (int) ev0 > this.rand1Value ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

}
