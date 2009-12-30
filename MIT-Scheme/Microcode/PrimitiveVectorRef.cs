using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PrimitiveVectorRef : PrimitiveCombination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PrimitiveVectorRef (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? PrimitiveVectorRefA.Make (rator, (Argument) rand0, rand1) :
                (rand0 is StaticVariable) ? PrimitiveVectorRefS.Make (rator, (StaticVariable) rand0, rand1) :
                (rand0 is TopLevelVariable) ? PrimitiveVectorRefT.Make (rator, (TopLevelVariable) rand0, rand1) :
                (rand1 is Argument) ? PrimitiveVectorRefXA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? PrimitiveVectorRefXQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveVectorRef (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveVectorRef";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG            
            SCode.location = "PrimitiveVectorRef";
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
            SCode.location = "PrimitiveVectorRef";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            answer = ((object []) ev0)[(int)ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefA : PrimitiveVectorRef
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;
        protected PrimitiveVectorRefA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Offset = rand0.Offset;
        }

        public static new SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveVectorRefA0.Make (rator, (Argument0) rand0, rand1) :
                (rand0 is Argument1) ? PrimitiveVectorRefA1.Make (rator, (Argument1) rand0, rand1) :
                (rand1 is Quotation) ? new PrimitiveVectorRefAQ (rator, rand0, (Quotation) rand1) :
                new PrimitiveVectorRefA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveVectorRefA";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveVectorRefA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            object ev0 = environment.ArgumentValue(this.rand0Offset);

            answer = ((object []) ev0) [(int) ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefA0 : PrimitiveVectorRefA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PrimitiveVectorRefA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? PrimitiveVectorRefA0Q.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveVectorRefA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveVectorRefA0";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveVectorRefA0";
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

            answer = ((object []) ev0) [(int) ev1];
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveVectorRefA0Q : PrimitiveVectorRefA0
    {
        public readonly int rand1Value;
        protected PrimitiveVectorRefA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static new SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitiveVectorRefA0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefA0Q");
#endif

            // Eval argument0
            object ev0 = environment.Argument0Value;

            answer = ((object []) ev0) [this.rand1Value];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefA1 : PrimitiveVectorRefA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;
        protected PrimitiveVectorRefA1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? new PrimitiveVectorRefA1Q (rator, rand0, (Quotation) rand1) :
                new PrimitiveVectorRefA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveVectorRefA1";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveVectorRefA1";
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

            answer = ((object []) ev0) [(int) ev1];
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveVectorRefA1Q : PrimitiveVectorRefA1
    {
        public readonly int rand1Value;
        internal PrimitiveVectorRefA1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
        this.rand1Value = (int) rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefA1Q");
#endif
            answer = ((object []) environment.Argument1Value) [this.rand1Value];
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveVectorRefAQ : PrimitiveVectorRefA
    {
        public readonly int rand1Value;
        internal PrimitiveVectorRefAQ (Primitive2 rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PrimitiveVectorRefAQ";
#endif
            answer = ((object []) environment.ArgumentValue (this.rand0Offset)) [this.rand1Value];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefS : PrimitiveVectorRef
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;
        protected PrimitiveVectorRefS (Primitive2 rator, StaticVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Offset = rand0.Offset;
        }

        public static new SCode Make (Primitive2 rator, StaticVariable rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveVectorRefSA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? PrimitiveVectorRefSQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveVectorRefS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveVectorRefS";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveVectorRefS";
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

            answer = ((object []) ev0) [(int) ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefSA : PrimitiveVectorRefS
    {
        public readonly int rand1Offset;
        protected PrimitiveVectorRefSA (Primitive2 rator, StaticVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static new SCode Make (Primitive2 rator, StaticVariable rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveVectorRefSA0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? PrimitiveVectorRefSA1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveVectorRefSA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefSA");
#endif
            // Eval argument1
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            // Eval argument0
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((object []) ev0) [(int) ev1];
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveVectorRefSA0 : PrimitiveVectorRefSA
    {
        PrimitiveVectorRefSA0 (Primitive2 rator, StaticVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, StaticVariable rand0, Argument0 rand1)
        {
            return
                new PrimitiveVectorRefSA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefSA0");
#endif
            // Eval argument1
            object ev1 = environment.Argument0Value;

            // Eval argument0
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((object []) ev0) [(int) ev1];
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveVectorRefSA1 : PrimitiveVectorRefSA
    {
        PrimitiveVectorRefSA1 (Primitive2 rator, StaticVariable rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, StaticVariable rand0, Argument1 rand1)
        {
            return

                new PrimitiveVectorRefSA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefSA");
#endif
            // Eval argument1
            object ev1 = environment.Argument1Value;

            // Eval argument0
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((object []) ev0) [(int) ev1];
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveVectorRefSQ : PrimitiveVectorRefS
    {
        public readonly int rand1Value;
        PrimitiveVectorRefSQ (Primitive2 rator, StaticVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static new SCode Make (Primitive2 rator, StaticVariable rand0, Quotation rand1)
        {
            return
                new PrimitiveVectorRefSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefSQ");
#endif
            // Eval argument0
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((object []) ev0) [this.rand1Value];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefT : PrimitiveVectorRef
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly ValueCell rand0Cell;

        protected PrimitiveVectorRefT (Primitive2 rator, TopLevelVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Cell = rand0.valueCell;
        }

        public static new SCode Make (Primitive2 rator, TopLevelVariable rand0, SCode rand1)
        {
            return
                new PrimitiveVectorRefT (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveVectorRefT";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveVectorRefT";
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
            if (this.rand0Cell.GetValue (out ev0))
                throw new NotImplementedException ();

            answer = ((object []) ev0) [(int) ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefXA : PrimitiveCombination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;
        protected PrimitiveVectorRefXA (Primitive2 rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveVectorRefXA0.Make (rator, rand0, (Argument0) rand1) :
                                (rand1 is Argument1) ? PrimitiveVectorRefXA1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveVectorRefXA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveVectorRefXA";
#endif
            // Eval argument1
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveVectorRefXA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            answer = ((object []) ev0) [(int) ev1];
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveVectorRefXA0 : PrimitiveVectorRefXA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        PrimitiveVectorRefXA0 (Primitive2 rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, Argument0 rand1)
        {
            return
                new PrimitiveVectorRefXA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveVectorRefXA0";
#endif
            // Eval argument1
            object ev1 = environment.Argument0Value;

            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveVectorRefXA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            answer = ((object []) ev0) [(int) ev1];
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveVectorRefXA1 : PrimitiveVectorRefXA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        PrimitiveVectorRefXA1 (Primitive2 rator, SCode rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, Argument1 rand1)
        {
            return
                new PrimitiveVectorRefXA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveVectorRefXA1";
#endif
            // Eval argument1
            object ev1 = environment.Argument1Value;

            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveVectorRefXA1";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            answer = ((object []) ev0) [(int) ev1];
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveVectorRefXQ : PrimitiveCombination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;
        protected PrimitiveVectorRefXQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveVectorRefXQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveVectorRefXQ";
#endif

            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveVectorRefXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            answer = ((object []) ev0) [this.rand1Value];
            return false;
        }
    }

#if NIL
    [Serializable]
    class PrimitiveVectorRefL : PrimitiveVectorRef
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PrimitiveVectorRefL (Primitive2 rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? PrimitiveVectorRefA.Make (rator, (Argument) rand0, rand1)
                : (rand0 is LexicalVariable1) ? PrimitiveVectorRefL1.Make (rator, (LexicalVariable1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveVectorRefLL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveVectorRefLQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveVectorRefL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PrimitiveVectorRefL.EvalStep";
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
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((object []) ev0)[(int)ev1];
            return false;
        }

    }

    [Serializable]
    class PrimitiveVectorRefA : PrimitiveVectorRefL
    {
        protected PrimitiveVectorRefA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveVectorRefA0.Make (rator, (Argument0) rand0, rand1)
                : (rand0 is Argument1) ? PrimitiveVectorRefA1.Make (rator, (Argument1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveVectorRefAL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveVectorRefAQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveVectorRefA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand1);
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
            answer = ((object []) environment.ArgumentValue(this.rand0Offset))[(int)ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefA0 : PrimitiveVectorRefA
    {
        protected PrimitiveVectorRefA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveVectorRefA0L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveVectorRefA0Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveVectorRefA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PrimitiveVectorRefA0.EvalStep";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveVectorRefA0.EvalStep.1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            answer = ((object []) environment.Argument0Value)[(int)ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefA0L : PrimitiveVectorRefA0
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveVectorRefA0L (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveVectorRefA0A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveVectorRefA0L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveVectorRefA0L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefA0L.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            answer = ((object []) environment.Argument0Value)[(int)ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefA0A : PrimitiveVectorRefA0L
    {
        protected PrimitiveVectorRefA0A (Primitive2 rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveVectorRefA0A0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? PrimitiveVectorRefA0A1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveVectorRefA0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefA0A.EvalStep");
#endif
            answer = ((object []) environment.Argument0Value) [(int) environment.ArgumentValue (this.rand1Offset)];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefA0A0 : PrimitiveVectorRefA0A
    {
        protected PrimitiveVectorRefA0A0 (Primitive2 rator, Argument0 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument0 rand1)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }



    [Serializable]
    class PrimitiveVectorRefA0A1 : PrimitiveVectorRefA0A
    {
        protected PrimitiveVectorRefA0A1 (Primitive2 rator, Argument0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            return
                new PrimitiveVectorRefA0A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefA0A1.EvalStep");
#endif
            answer = ((object []) environment.Argument0Value)[(int)environment.Argument1Value];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefA0L1 : PrimitiveVectorRefA0L
    {
        protected PrimitiveVectorRefA0L1 (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveVectorRefA0L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefA0L1.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            answer = ((object []) environment.Argument0Value)[(int)ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefA0Q : PrimitiveVectorRefA0
    {
        public readonly int rand1Value;

        protected PrimitiveVectorRefA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitiveVectorRefA0Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefA0Q.EvalStep");
#endif
            answer = ((object []) environment.Argument0Value)[this.rand1Value];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefA1 : PrimitiveVectorRefA
    {
        protected PrimitiveVectorRefA1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveVectorRefA1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveVectorRefA1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveVectorRefA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand1);
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

            answer = ((object []) environment.Argument1Value)[(int)ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefA1L : PrimitiveVectorRefA1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveVectorRefA1L (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveVectorRefA1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveVectorRefA1L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveVectorRefA1L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();

#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            answer = ((object []) environment.Argument1Value)[(int)ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefA1A : PrimitiveVectorRefA1L
    {
        protected PrimitiveVectorRefA1A (Primitive2 rator, Argument1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveVectorRefA1A0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? Unimplemented () :
                new PrimitiveVectorRefA1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefA1A.EvalStep");
#endif
            answer = ((object []) environment.Argument1Value)[(int)environment.ArgumentValue(this.rand1Offset)];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefA1A0 : PrimitiveVectorRefA1A
    {
        protected PrimitiveVectorRefA1A0 (Primitive2 rator, Argument1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument0 rand1)
        {
            return
                 new PrimitiveVectorRefA1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefA1A0.EvalStep");
#endif
            answer = ((object []) environment.Argument1Value)[(int)environment.Argument0Value];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefA1A1 : PrimitiveVectorRefA1A
    {
        protected PrimitiveVectorRefA1A1 (Primitive2 rator, Argument1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            throw new NotImplementedException ();
            //new PrimitiveVectorRefA1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveVectorRefA1L1 : PrimitiveVectorRefA1L
    {
        protected PrimitiveVectorRefA1L1 (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveVectorRefA1L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefA1L1.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0 = environment.Argument1Value;

            // Greater-than-fixnum?
            answer = ((object []) ev0)[(int)ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefA1Q : PrimitiveVectorRefA1
    {
        public readonly int rand1Value;

        protected PrimitiveVectorRefA1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Quotation rand1)
        {
            return
                new PrimitiveVectorRefA1Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefA1Q.EvalStep");
#endif
            answer = ((object []) environment.Argument1Value)[this.rand1Value];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefAL : PrimitiveVectorRefA
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveVectorRefAL (Primitive2 rator, Argument rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveVectorRefAA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitiveVectorRefAL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveVectorRefAA : PrimitiveVectorRefAL
    {
        protected PrimitiveVectorRefAA (Primitive2 rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveVectorRefAA0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? PrimitiveVectorRefAA1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveVectorRefAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefAA.EvalStep");
#endif
            answer = ((object []) environment.ArgumentValue (this.rand0Offset))[(int)environment.ArgumentValue(this.rand1Offset)];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefAA0 : PrimitiveVectorRefAA
    {
        protected PrimitiveVectorRefAA0 (Primitive2 rator, Argument rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument0 rand1)
        {
            return new PrimitiveVectorRefAA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefAA0.EvalStep");
#endif
            answer = ((object []) environment.ArgumentValue (this.rand0Offset)) [(int) environment.Argument0Value];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefAA1 : PrimitiveVectorRefAA
    {
        protected PrimitiveVectorRefAA1 (Primitive2 rator, Argument rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument1 rand1)
        {
            return new PrimitiveVectorRefAA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveVectorRefAL1 : PrimitiveVectorRefAL
    {
        protected PrimitiveVectorRefAL1 (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
        {
            return new PrimitiveVectorRefAL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveVectorRefAQ : PrimitiveVectorRefA
    {
        public readonly int rand1Value;

        protected PrimitiveVectorRefAQ (Primitive2 rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Quotation rand1)
        {
            return
                new PrimitiveVectorRefAQ (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefAQ.EvalStep");
#endif
            answer = ((object []) environment.ArgumentValue(this.rand0Offset))[this.rand1Value];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefL1 : PrimitiveVectorRefL
    {
        protected PrimitiveVectorRefL1 (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveVectorRefL1L.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveVectorRefL1Q.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveVectorRefL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PrimitiveVectorRefL1.EvalStep";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveVectorRefL1.EvalStep.1";
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
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((object []) ev0)[(int)ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefL1L : PrimitiveVectorRefL1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveVectorRefL1L (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveVectorRefL1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveVectorRefL1L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveVectorRefL1L (rator, rand0, rand1);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();


            answer = ((object []) ev0)[(int)ev1];
            return false;
        }

    }

    [Serializable]
    class PrimitiveVectorRefL1A : PrimitiveVectorRefL1L
    {
        protected PrimitiveVectorRefL1A (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveVectorRefL1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveVectorRefL1A1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveVectorRefL1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveVectorRefL1A0 : PrimitiveVectorRefL1A
    {
        protected PrimitiveVectorRefL1A0 (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
        {
            return
                 new PrimitiveVectorRefL1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefL1A0.EvalStep");
#endif
            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((object []) ev0)[(int)environment.Argument0Value];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefL1A1 : PrimitiveVectorRefL1A
    {
        protected PrimitiveVectorRefL1A1 (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
        {
            return
                 new PrimitiveVectorRefL1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefL1A1.EvalStep");
#endif

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((object []) ev0)[(int)environment.Argument1Value];
            return false;
        }

    }

    [Serializable]
    class PrimitiveVectorRefL1L1 : PrimitiveVectorRefL1L
    {
        protected PrimitiveVectorRefL1L1 (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveVectorRefL1L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefL1L.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((object []) ev0)[(int)ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefL1Q : PrimitiveVectorRefL1
    {
        public readonly int rand1Value;

        protected PrimitiveVectorRefL1Q (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
        {
            return
                  new PrimitiveVectorRefL1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefL1Q.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            answer = ((object []) ev0)[this.rand1Value];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefLL : PrimitiveVectorRefL
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveVectorRefLL (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveVectorRefLA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveVectorRefLL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveVectorRefLL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefLL.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((object []) ev0)[(int)ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefLA : PrimitiveVectorRefLL
    {
        protected PrimitiveVectorRefLA (Primitive2 rator, LexicalVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveVectorRefLA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveVectorRefLA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveVectorRefLA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefLA.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            answer = ((object []) ev0) [(int) environment.ArgumentValue(this.rand1Offset)];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefLA0 : PrimitiveVectorRefLA
    {
        protected PrimitiveVectorRefLA0 (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
        {
            return
                 new PrimitiveVectorRefLA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefLA0.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            answer = ((object []) ev0)[(int)environment.Argument0Value];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefLA1 : PrimitiveVectorRefLA
    {
        protected PrimitiveVectorRefLA1 (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
        {
            return
                 new PrimitiveVectorRefLA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefLA1.EvalStep");
#endif
            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((object []) ev0)[(int)environment.Argument1Value];
            return false;
        }

    }

    [Serializable]
    class PrimitiveVectorRefLL1 : PrimitiveVectorRefLL
    {
        protected PrimitiveVectorRefLL1 (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveVectorRefLL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefLL1.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((object []) ev0)[(int)ev1];
            return false;
        }


    }

    [Serializable]
    class PrimitiveVectorRefLQ : PrimitiveVectorRefL
    {
        public readonly int rand1Value;

        protected PrimitiveVectorRefLQ (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
        {
            return new PrimitiveVectorRefLQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefLQ.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            answer = ((object []) ev0)[this.rand1Value];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefQ : PrimitiveVectorRef
    {
        public readonly object [] rand0Value;

        protected PrimitiveVectorRefQ (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = (object []) rand0.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveVectorRefQL.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveVectorRefQQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveVectorRefQ (rator, rand0, rand1);
        }


        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PrimitiveVectorRefQ.EvalStep";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
                        SCode.location = "PrimitiveVectorRefQ.EvalStep.1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            answer = this.rand0Value[(int)ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefQL : PrimitiveVectorRefQ
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveVectorRefQL (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveVectorRefQA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveVectorRefQL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveVectorRefQL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveVectorRefQA : PrimitiveVectorRefQL
    {
        protected PrimitiveVectorRefQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveVectorRefQA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveVectorRefQA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveVectorRefQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            answer = this.rand0Value [(int) environment.ArgumentValue(this.rand1Offset)];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefQA0 : PrimitiveVectorRefQA
    {
        protected PrimitiveVectorRefQA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument0 rand1)
        {
            return
                 new PrimitiveVectorRefQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            answer = this.rand0Value [(int)environment.Argument0Value];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefQA1 : PrimitiveVectorRefQA
    {
        protected PrimitiveVectorRefQA1 (Primitive2 rator, Quotation rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument1 rand1)
        {
            return
                 new PrimitiveVectorRefQA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            answer = this.rand0Value [(int)environment.Argument1Value];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefQL1 : PrimitiveVectorRefQL
    {
        protected PrimitiveVectorRefQL1 (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveVectorRefQL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();


            answer = this.rand0Value[(int)ev1];
            return false;
        }

    }



    [Serializable]
    class PrimitiveVectorRefQQ : PrimitiveVectorRefQ
    {
        public readonly int rand1Value;

        protected PrimitiveVectorRefQQ (Primitive2 rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Quotation rand1)
        {
            return
                 new PrimitiveVectorRefQQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // refetch because vector may be mutable
            answer = this.rand0Value [this.rand1Value];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefSL : PrimitiveVectorRef
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveVectorRefSL (Primitive2 rator, SCode rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveVectorRefSA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveVectorRefSL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveVectorRefSL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

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

            answer = ((object []) ev0)[(int)ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefSA : PrimitiveVectorRefSL
    {
        protected PrimitiveVectorRefSA (Primitive2 rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveVectorRefSA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveVectorRefSA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveVectorRefSA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand1);
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

            answer = ((object []) ev0)[(int)environment.ArgumentValue(this.rand1Offset)];
            return false;
        }

    }

    [Serializable]
    class PrimitiveVectorRefSA0 : PrimitiveVectorRefSA
    {
        protected PrimitiveVectorRefSA0 (Primitive2 rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument0 rand1)
        {
            return

                new PrimitiveVectorRefSA0 (rator, rand0, rand1);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefSA0.EvalStep");
            noteCalls (this.rand0);
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

            answer = ((object []) ev0)[(int)environment.Argument0Value];
            return false;
        }

    }

    [Serializable]
    class PrimitiveVectorRefSA1 : PrimitiveVectorRefSA
    {
        protected PrimitiveVectorRefSA1 (Primitive2 rator, SCode rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument1 rand1)
        {
            return

                new PrimitiveVectorRefSA1 (rator, rand0, rand1);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PrimitiveVectorRefSA1.EvalStep";
#endif

            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveVectorRefSA1.EvalStep.1";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            answer = ((object []) ev0)[(int)environment.Argument1Value];
            return false;
        }

    }


    [Serializable]
    class PrimitiveVectorRefSL1 : PrimitiveVectorRefSL
    {
        protected PrimitiveVectorRefSL1 (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
        {
            return

                new PrimitiveVectorRefSL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PrimitiveVectorRefSL1.EvalStep";
#endif
            // Eval argument1
            object ev1;

            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

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

            answer = ((object []) ev0)[(int)ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVectorRefSQ : PrimitiveVectorRef
    {
        public readonly int rand1Value;

        protected PrimitiveVectorRefSQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveVectorRefSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorRefSQ.EvalStep");
            noteCalls (this.rand0);
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

            answer = ((object []) ev0)[this.rand1Value];
            return false;
        }
    }
#endif
}

