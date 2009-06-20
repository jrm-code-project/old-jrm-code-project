using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PrimitiveLessThanFixnum : PrimitiveCombination2
    {
        protected PrimitiveLessThanFixnum (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is LexicalVariable) ? PrimitiveLessThanFixnumL.Make (rator, (LexicalVariable) rand0, rand1) :
                (rand0 is Quotation) ? PrimitiveLessThanFixnumQ.Make (rator, (Quotation) rand0, rand1) :
                (rand1 is LexicalVariable) ? PrimitiveLessThanFixnumSL.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveLessThanFixnumSQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveLessThanFixnum (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnum.EvalStep");
            noteCalls (this.rand0);
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

            // Greater-than-fixnum?
            answer = (int) ev0 < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumL : PrimitiveLessThanFixnum
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PrimitiveLessThanFixnumL (Primitive2 rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? PrimitiveLessThanFixnumA.Make (rator, (Argument) rand0, rand1)
                : (rand0 is LexicalVariable1) ? PrimitiveLessThanFixnumL1.Make (rator, (LexicalVariable1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveLessThanFixnumLL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveLessThanFixnumLQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveLessThanFixnumL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumL.EvalStep");
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
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            // Greater-than-fixnum?
            answer = (int) ev0 < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }

    }

    [Serializable]
    class PrimitiveLessThanFixnumA : PrimitiveLessThanFixnumL
    {
        protected PrimitiveLessThanFixnumA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveLessThanFixnumA0.Make (rator, (Argument0) rand0, rand1)
                : (rand0 is Argument1) ? PrimitiveLessThanFixnumA1.Make (rator, (Argument1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveLessThanFixnumAL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveLessThanFixnumAQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveLessThanFixnumA (rator, rand0, rand1);
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
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // Greater-than-fixnum?
            answer = (int) ev0 < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumA0 : PrimitiveLessThanFixnumA
    {
        protected PrimitiveLessThanFixnumA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveLessThanFixnumA0L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveLessThanFixnumA0Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveLessThanFixnumA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumA0.EvalStep");
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

            // Greater-than-fixnum?
            answer = (int) environment.Argument0Value < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumA0L : PrimitiveLessThanFixnumA0
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveLessThanFixnumA0L (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveLessThanFixnumA0A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveLessThanFixnumA0L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveLessThanFixnumA0L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumA0L.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0 = (int) environment.Argument0Value;

            // Greater-than-fixnum?
            answer = (int) ev0 < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumA0A : PrimitiveLessThanFixnumA0L
    {
        protected PrimitiveLessThanFixnumA0A (Primitive2 rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Unimplemented ()
                : (rand1 is Argument1) ? PrimitiveLessThanFixnumA0A1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveLessThanFixnumA0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    // class PrimitiveLessThanFixnumA0A0 : PrimitiveLessThanFixnumA0A

    [Serializable]
    class PrimitiveLessThanFixnumA0A1 : PrimitiveLessThanFixnumA0A
    {
        protected PrimitiveLessThanFixnumA0A1 (Primitive2 rator, Argument0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            return
                new PrimitiveLessThanFixnumA0A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumA0A1.EvalStep");
#endif
            // Eval argument1
            int ev1 = (int) environment.Argument1Value;

            // Eval argument0
            int ev0 = (int) environment.Argument0Value;

            // Greater-than-fixnum?
            answer = ev0 < ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumA0L1 : PrimitiveLessThanFixnumA0L
    {
        protected PrimitiveLessThanFixnumA0L1 (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveLessThanFixnumA0L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumA0L1.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0 = (int) environment.Argument0Value;

            // Greater-than-fixnum?
            answer = (int) ev0 < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false; throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumA0Q : PrimitiveLessThanFixnumA0
    {
        public readonly int rand1Value;

        protected PrimitiveLessThanFixnumA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitiveLessThanFixnumA0Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumA0Q.EvalStep");
#endif
            answer = (int) environment.Argument0Value < this.rand1Value ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumA1 : PrimitiveLessThanFixnumA
    {
        protected PrimitiveLessThanFixnumA1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveLessThanFixnumA1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveLessThanFixnumA1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveLessThanFixnumA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumA1.EvalStep");
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

            // Greater-than-fixnum?
            answer = (int) environment.Argument1Value < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumA1L : PrimitiveLessThanFixnumA1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveLessThanFixnumA1L (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveLessThanFixnumA1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitiveLessThanFixnumA1L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumA1L.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // Greater-than-fixnum?
            answer = (int) environment.Argument1Value < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumA1A : PrimitiveLessThanFixnumA1L
    {
        protected PrimitiveLessThanFixnumA1A (Primitive2 rator, Argument1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveLessThanFixnumA1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented ()
                : new PrimitiveLessThanFixnumA1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumA1A.EvalStep");
#endif
            // Eval argument1
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            // Eval argument0
            object ev0 = environment.Argument1Value;

            // Greater-than-fixnum?
            answer = (int) ev0 < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumA1A0 : PrimitiveLessThanFixnumA1A
    {
        protected PrimitiveLessThanFixnumA1A0 (Primitive2 rator, Argument1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument0 rand1)
        {
            return
                 new PrimitiveLessThanFixnumA1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument1
            object ev1 = environment.Argument0Value;

            // Eval argument0
            object ev0 = environment.Argument1Value;

            // Greater-than-fixnum?
            answer = (int) ev0 < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumA1A1 : PrimitiveLessThanFixnumA1A
    {
        protected PrimitiveLessThanFixnumA1A1 (Primitive2 rator, Argument1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            throw new NotImplementedException ();
            //new PrimitiveLessThanFixnumA1A1 (rator, rand0, rand1);
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
    class PrimitiveLessThanFixnumA1L1 : PrimitiveLessThanFixnumA1L
    {
        protected PrimitiveLessThanFixnumA1L1 (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveLessThanFixnumA1L1 (rator, rand0, rand1);
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

            // Eval argument0
            object ev0 = (int) environment.Argument1Value;

            // Greater-than-fixnum?
            answer = (int) ev0 < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false; throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumA1Q : PrimitiveLessThanFixnumA1
    {
        public readonly int rand1Value;

        protected PrimitiveLessThanFixnumA1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Quotation rand1)
        {
            return
                new PrimitiveLessThanFixnumA1Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumA1Q.EvalStep");
#endif
            // Eval argument1
            int ev1 = this.rand1Value;

            // Eval argument0
            object ev0 = environment.Argument1Value;

            // Greater-than-fixnum?
            answer = (int) ev0 < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumAL : PrimitiveLessThanFixnumA
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveLessThanFixnumAL (Primitive2 rator, Argument rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveLessThanFixnumAA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitiveLessThanFixnumAL (rator, rand0, rand1);
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
    class PrimitiveLessThanFixnumAA : PrimitiveLessThanFixnumAL
    {
        protected PrimitiveLessThanFixnumAA (Primitive2 rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Unimplemented ()
                : (rand1 is Argument1) ? PrimitiveLessThanFixnumAA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveLessThanFixnumAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumAA.EvalStep");
#endif
            // Eval argument1
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            // Eval argument0
            object ev0 =  environment.ArgumentValue (this.rand0Offset);

            // Greater-than-fixnum?
            answer = (int) ev0 < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumAA0 : PrimitiveLessThanFixnumAA
    {
        protected PrimitiveLessThanFixnumAA0 (Primitive2 rator, Argument rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument0 rand1)
        {
            return new PrimitiveLessThanFixnumAA0 (rator, rand0, rand1);
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
    class PrimitiveLessThanFixnumAA1 : PrimitiveLessThanFixnumAA
    {
        protected PrimitiveLessThanFixnumAA1 (Primitive2 rator, Argument rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument1 rand1)
        {
            return new PrimitiveLessThanFixnumAA1 (rator, rand0, rand1);
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
    class PrimitiveLessThanFixnumAL1 : PrimitiveLessThanFixnumAL
    {
        protected PrimitiveLessThanFixnumAL1 (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
        {
            return new PrimitiveLessThanFixnumAL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumAQ : PrimitiveLessThanFixnumA
    {
        public readonly int rand1Value;

        protected PrimitiveLessThanFixnumAQ (Primitive2 rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Quotation rand1)
        {
            return
                new PrimitiveLessThanFixnumAQ (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument1
            int ev1 = this.rand1Value;

            // Eval argument0
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            // Greater-than-fixnum?
            answer = (int) ev0 < ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumL1 : PrimitiveLessThanFixnumL
    {
        protected PrimitiveLessThanFixnumL1 (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveLessThanFixnumL1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveLessThanFixnumL1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveLessThanFixnumL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumL1.EvalStep");
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
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // Greater-than-fixnum?
            answer = (int) ev0 < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumL1L : PrimitiveLessThanFixnumL1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveLessThanFixnumL1L (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveLessThanFixnumL1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveLessThanFixnumL1L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveLessThanFixnumL1L (rator, rand0, rand1);
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
    class PrimitiveLessThanFixnumL1A : PrimitiveLessThanFixnumL1L
    {
        protected PrimitiveLessThanFixnumL1A (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveLessThanFixnumL1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented ()
                : new PrimitiveLessThanFixnumL1A (rator, rand0, rand1);
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
    class PrimitiveLessThanFixnumL1A0 : PrimitiveLessThanFixnumL1A
    {
        protected PrimitiveLessThanFixnumL1A0 (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
        {
            return
                 new PrimitiveLessThanFixnumL1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumL1A0.EvalStep");
#endif
            // Eval argument1
            object ev1 = environment.Argument0Value;

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // Greater-than-fixnum?
            answer = (int) ev0 < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumL1A1 : PrimitiveLessThanFixnumL1A
    {
        protected PrimitiveLessThanFixnumL1A1 (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
        {
            return
                 new PrimitiveLessThanFixnumL1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumL1L1 : PrimitiveLessThanFixnumL1L
    {
        protected PrimitiveLessThanFixnumL1L1 (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveLessThanFixnumL1L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumL1L1.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // Greater-than-fixnum?
            answer = (int) ev0 < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumL1Q : PrimitiveLessThanFixnumL1
    {
        public readonly int rand1Value;

        protected PrimitiveLessThanFixnumL1Q (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
        {
            return
                  new PrimitiveLessThanFixnumL1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumL1Q");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            answer = ((int) ev0 < this.rand1Value) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumLL : PrimitiveLessThanFixnumL
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveLessThanFixnumLL (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveLessThanFixnumLA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitiveLessThanFixnumLL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumLL.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            // Greater-than-fixnum?
            answer = (int) ev0 < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumLA : PrimitiveLessThanFixnumLL
    {
        protected PrimitiveLessThanFixnumLA (Primitive2 rator, LexicalVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveLessThanFixnumLA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented ()
                : new PrimitiveLessThanFixnumLA (rator, rand0, rand1);
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
    class PrimitiveLessThanFixnumLA0 : PrimitiveLessThanFixnumLA
    {
        protected PrimitiveLessThanFixnumLA0 (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
        {
            return
                 new PrimitiveLessThanFixnumLA0 (rator, rand0, rand1);
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
    class PrimitiveLessThanFixnumLA1 : PrimitiveLessThanFixnumLA
    {
        protected PrimitiveLessThanFixnumLA1 (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
        {
            return
                 new PrimitiveLessThanFixnumLA1 (rator, rand0, rand1);
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
    class PrimitiveLessThanFixnumLL1 : PrimitiveLessThanFixnumLL
    {
        protected PrimitiveLessThanFixnumLL1 (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveLessThanFixnumLL1 (rator, rand0, rand1);
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
    class PrimitiveLessThanFixnumLQ : PrimitiveLessThanFixnumL
    {
        public readonly int rand1Value;

        protected PrimitiveLessThanFixnumLQ (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
        {
            return new PrimitiveLessThanFixnumLQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumLQ.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            answer = ((int) ev0 < this.rand1Value) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumQ : PrimitiveLessThanFixnum
    {
        public readonly int rand0Value;

        protected PrimitiveLessThanFixnumQ (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = (int) rand0.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveLessThanFixnumQL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? Unimplemented ()
                : new PrimitiveLessThanFixnumQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand1);
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumQL : PrimitiveLessThanFixnumQ
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveLessThanFixnumQL (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveLessThanFixnumQA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitiveLessThanFixnumQL (rator, rand0, rand1);
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
    class PrimitiveLessThanFixnumQA : PrimitiveLessThanFixnumQL
    {
        protected PrimitiveLessThanFixnumQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveLessThanFixnumQA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveLessThanFixnumQA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveLessThanFixnumQA (rator, rand0, rand1);
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
    class PrimitiveLessThanFixnumQA0 : PrimitiveLessThanFixnumQA
    {
        protected PrimitiveLessThanFixnumQA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument0 rand1)
        {
            return
                 new PrimitiveLessThanFixnumQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumQA0.EvalStep");
#endif
            // Eval argument1
            object ev1 = environment.Argument0Value;

            // Eval argument0
            int ev0 = this.rand0Value;

            // Greater-than-fixnum?
            answer = (int) ev0 < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumQA1 : PrimitiveLessThanFixnumQA
    {
        protected PrimitiveLessThanFixnumQA1 (Primitive2 rator, Quotation rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument1 rand1)
        {
            return
                 new PrimitiveLessThanFixnumQA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument1
            object ev1 = environment.Argument1Value;

            // Eval argument0
            int ev0 = this.rand0Value;

            // Greater-than-fixnum?
            answer = (int) ev0 < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumQL1 : PrimitiveLessThanFixnumQL
    {
        protected PrimitiveLessThanFixnumQL1 (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveLessThanFixnumQL1 (rator, rand0, rand1);
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
    class PrimitiveLessThanFixnumSL : PrimitiveLessThanFixnum
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveLessThanFixnumSL (Primitive2 rator, SCode rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveLessThanFixnumSA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveLessThanFixnumSL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveLessThanFixnumSL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumSA : PrimitiveLessThanFixnumSL
    {
        protected PrimitiveLessThanFixnumSA (Primitive2 rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveLessThanFixnumSA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented ()
                : new PrimitiveLessThanFixnumSA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumSA0 : PrimitiveLessThanFixnumSA
    {
        protected PrimitiveLessThanFixnumSA0 (Primitive2 rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument0 rand1)
        {
            return

                new PrimitiveLessThanFixnumSA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumSL1 : PrimitiveLessThanFixnumSL
    {
        protected PrimitiveLessThanFixnumSL1 (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
        {
            return

                new PrimitiveLessThanFixnumSL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveLessThanFixnumSQ : PrimitiveLessThanFixnum
    {
        public readonly int rand1Value;

        protected PrimitiveLessThanFixnumSQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveLessThanFixnumSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveLessThanFixnumSQ.EvalStep");
            noteCalls (this.rand0);
#endif
            // Eval argument1
            int ev1 = this.rand1Value;

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

            // Greater-than-fixnum?
            answer = (int) ev0 < ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
            throw new NotImplementedException ();
        }
    }

}
