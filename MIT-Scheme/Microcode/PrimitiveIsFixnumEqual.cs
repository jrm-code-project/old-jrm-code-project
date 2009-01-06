using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PrimitiveIsFixnumEqual : PrimitiveCombination2
    {
        protected PrimitiveIsFixnumEqual (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is LexicalVariable) ? PrimitiveIsFixnumEqualL.Make (rator, (LexicalVariable) rand0, rand1) :
                (rand0 is Quotation) ? PrimitiveIsFixnumEqualQ.Make (rator, (Quotation) rand0, rand1) :
                (rand1 is LexicalVariable) ? PrimitiveIsFixnumEqualSL.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveIsFixnumEqualSQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveIsFixnumEqual (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqual.EvalStep");
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

            answer = (int) ev0 == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualL : PrimitiveIsFixnumEqual
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PrimitiveIsFixnumEqualL (Primitive2 rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? PrimitiveIsFixnumEqualA.Make (rator, (Argument) rand0, rand1)
                : (rand0 is LexicalVariable1) ? PrimitiveIsFixnumEqualL1.Make (rator, (LexicalVariable1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveIsFixnumEqualLL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveIsFixnumEqualLQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsFixnumEqualL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualL.EvalStep");
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
            answer = (int) ev0 == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }

    }

    class PrimitiveIsFixnumEqualA : PrimitiveIsFixnumEqualL
    {
        protected PrimitiveIsFixnumEqualA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveIsFixnumEqualA0.Make (rator, (Argument0) rand0, rand1)
                : (rand0 is Argument1) ? PrimitiveIsFixnumEqualA1.Make (rator, (Argument1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveIsFixnumEqualAL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveIsFixnumEqualAQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsFixnumEqualA (rator, rand0, rand1);
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
            answer = (int) ev0 == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualA0 : PrimitiveIsFixnumEqualA
    {
        protected PrimitiveIsFixnumEqualA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveIsFixnumEqualA0L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveIsFixnumEqualA0Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsFixnumEqualA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualA0.EvalStep");
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
            answer = (int) environment.Argument0Value == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualA0L : PrimitiveIsFixnumEqualA0
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsFixnumEqualA0L (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsFixnumEqualA0A.Make (rator, rand0, (Argument) rand1) :
                (rand1 is LexicalVariable1) ? PrimitiveIsFixnumEqualA0L1.Make (rator, rand0, (LexicalVariable1) rand1) :
                new PrimitiveIsFixnumEqualA0L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualA0L.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // fixnum-equal
            answer = ((int) environment.Argument0Value == (int) ev1) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualA0A : PrimitiveIsFixnumEqualA0L
    {
        protected PrimitiveIsFixnumEqualA0A (Primitive2 rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Unimplemented ()
                : (rand1 is Argument1) ? PrimitiveIsFixnumEqualA0A1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveIsFixnumEqualA0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    // class PrimitiveIsFixnumEqualA0A0 : PrimitiveIsFixnumEqualA0A

    class PrimitiveIsFixnumEqualA0A1 : PrimitiveIsFixnumEqualA0A
    {
        protected PrimitiveIsFixnumEqualA0A1 (Primitive2 rator, Argument0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            return
                new PrimitiveIsFixnumEqualA0A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualA0A1.EvalStep");
#endif
            // Eval argument1
            int ev1 = (int) environment.Argument1Value;

            // Eval argument0
            int ev0 = (int) environment.Argument0Value;

            // Greater-than-fixnum?
            answer = ev0 == ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualA0L1 : PrimitiveIsFixnumEqualA0L
    {
        protected PrimitiveIsFixnumEqualA0L1 (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveIsFixnumEqualA0L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualA0L1.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0 = (int) environment.Argument0Value;

            // Greater-than-fixnum?
            answer = (int) ev0 == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false; throw new NotImplementedException ();
        }
    }

    class PrimitiveIsFixnumEqualA0Q : PrimitiveIsFixnumEqualA0
    {
        public readonly int rand1Value;

        protected PrimitiveIsFixnumEqualA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitiveIsFixnumEqualA0Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualA0Q.EvalStep");
#endif
            answer = (int) environment.Argument0Value == this.rand1Value ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualA1 : PrimitiveIsFixnumEqualA
    {
        protected PrimitiveIsFixnumEqualA1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveIsFixnumEqualA1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveIsFixnumEqualA1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsFixnumEqualA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualA1.EvalStep");
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
            answer = (int) environment.Argument1Value == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualA1L : PrimitiveIsFixnumEqualA1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsFixnumEqualA1L (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsFixnumEqualA1A.Make (rator, rand0, (Argument) rand1) :
                //(rand1 is LexicalVariable1) ? Unimplemented () :
                new PrimitiveIsFixnumEqualA1L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualA1L.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // Greater-than-fixnum?
            answer = (int) environment.Argument1Value == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualA1A : PrimitiveIsFixnumEqualA1L
    {
        protected PrimitiveIsFixnumEqualA1A (Primitive2 rator, Argument1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsFixnumEqualA1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented ()
                : new PrimitiveIsFixnumEqualA1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualA1A.EvalStep");
#endif
            // Eval argument1
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            // Eval argument0
            object ev0 = environment.Argument1Value;

            // Greater-than-fixnum?
            answer = (int) ev0 == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualA1A0 : PrimitiveIsFixnumEqualA1A
    {
        protected PrimitiveIsFixnumEqualA1A0 (Primitive2 rator, Argument1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument0 rand1)
        {
            return
                 new PrimitiveIsFixnumEqualA1A0 (rator, rand0, rand1);
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
            answer = (int) ev0 == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualA1A1 : PrimitiveIsFixnumEqualA1A
    {
        protected PrimitiveIsFixnumEqualA1A1 (Primitive2 rator, Argument1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            throw new NotImplementedException ();
            //new PrimitiveIsFixnumEqualA1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsFixnumEqualA1L1 : PrimitiveIsFixnumEqualA1L
    {
        protected PrimitiveIsFixnumEqualA1L1 (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveIsFixnumEqualA1L1 (rator, rand0, rand1);
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
            answer = (int) ev0 == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false; throw new NotImplementedException ();
        }
    }

    class PrimitiveIsFixnumEqualA1Q : PrimitiveIsFixnumEqualA1
    {
        public readonly int rand1Value;

        protected PrimitiveIsFixnumEqualA1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Quotation rand1)
        {
            return
                new PrimitiveIsFixnumEqualA1Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualA1Q.EvalStep");
#endif
            // Eval argument1
            int ev1 = this.rand1Value;

            // Eval argument0
            object ev0 = environment.Argument1Value;

            // Greater-than-fixnum?
            answer = (int) ev0 == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualAL : PrimitiveIsFixnumEqualA
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsFixnumEqualAL (Primitive2 rator, Argument rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsFixnumEqualAA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitiveIsFixnumEqualAL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualAL.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.ArgumentValue (this.rand0Offset);
            answer = ((int) ev0 == (int) ev1) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualAA : PrimitiveIsFixnumEqualAL
    {
        protected PrimitiveIsFixnumEqualAA (Primitive2 rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Unimplemented ()
                : (rand1 is Argument1) ? PrimitiveIsFixnumEqualAA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveIsFixnumEqualAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualAA.EvalStep");
#endif
            // Eval argument1
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            // Eval argument0
            object ev0 =  environment.ArgumentValue (this.rand0Offset);

            // Greater-than-fixnum?
            answer = (int) ev0 == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualAA0 : PrimitiveIsFixnumEqualAA
    {
        protected PrimitiveIsFixnumEqualAA0 (Primitive2 rator, Argument rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument0 rand1)
        {
            return new PrimitiveIsFixnumEqualAA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsFixnumEqualAA1 : PrimitiveIsFixnumEqualAA
    {
        protected PrimitiveIsFixnumEqualAA1 (Primitive2 rator, Argument rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument1 rand1)
        {
            return new PrimitiveIsFixnumEqualAA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsFixnumEqualAL1 : PrimitiveIsFixnumEqualAL
    {
        protected PrimitiveIsFixnumEqualAL1 (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
        {
            return new PrimitiveIsFixnumEqualAL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsFixnumEqualAQ : PrimitiveIsFixnumEqualA
    {
        public readonly int rand1Value;

        protected PrimitiveIsFixnumEqualAQ (Primitive2 rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Quotation rand1)
        {
            return
                new PrimitiveIsFixnumEqualAQ (rator, rand0, (Quotation) rand1);
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
            answer = (int) ev0 == ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualL1 : PrimitiveIsFixnumEqualL
    {
        protected PrimitiveIsFixnumEqualL1 (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveIsFixnumEqualL1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveIsFixnumEqualL1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsFixnumEqualL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualL1.EvalStep");
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
            answer = (int) ev0 == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualL1L : PrimitiveIsFixnumEqualL1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsFixnumEqualL1L (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveIsFixnumEqualL1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveIsFixnumEqualL1L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveIsFixnumEqualL1L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsFixnumEqualL1A : PrimitiveIsFixnumEqualL1L
    {
        protected PrimitiveIsFixnumEqualL1A (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveIsFixnumEqualL1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented ()
                : new PrimitiveIsFixnumEqualL1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsFixnumEqualL1A0 : PrimitiveIsFixnumEqualL1A
    {
        protected PrimitiveIsFixnumEqualL1A0 (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
        {
            return
                 new PrimitiveIsFixnumEqualL1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualL1A0.EvalStep");
#endif
            // Eval argument1
            object ev1 = environment.Argument0Value;

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // Greater-than-fixnum?
            answer = (int) ev0 == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualL1A1 : PrimitiveIsFixnumEqualL1A
    {
        protected PrimitiveIsFixnumEqualL1A1 (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
        {
            return
                 new PrimitiveIsFixnumEqualL1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsFixnumEqualL1L1 : PrimitiveIsFixnumEqualL1L
    {
        protected PrimitiveIsFixnumEqualL1L1 (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveIsFixnumEqualL1L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualL1L1.EvalStep");
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
            answer = (int) ev0 == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualL1Q : PrimitiveIsFixnumEqualL1
    {
        public readonly int rand1Value;

        protected PrimitiveIsFixnumEqualL1Q (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
        {
            return
                  new PrimitiveIsFixnumEqualL1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsFixnumEqualLL : PrimitiveIsFixnumEqualL
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsFixnumEqualLL (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveIsFixnumEqualLA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitiveIsFixnumEqualLL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualLL.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            answer = (int) ev0 == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualLA : PrimitiveIsFixnumEqualLL
    {
        protected PrimitiveIsFixnumEqualLA (Primitive2 rator, LexicalVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveIsFixnumEqualLA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented ()
                : new PrimitiveIsFixnumEqualLA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsFixnumEqualLA0 : PrimitiveIsFixnumEqualLA
    {
        protected PrimitiveIsFixnumEqualLA0 (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
        {
            return
                 new PrimitiveIsFixnumEqualLA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualLA0.EvalStep");
#endif
            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            answer = (int) ev0 == (int) environment.Argument0Value ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualLA1 : PrimitiveIsFixnumEqualLA
    {
        protected PrimitiveIsFixnumEqualLA1 (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
        {
            return
                 new PrimitiveIsFixnumEqualLA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsFixnumEqualLL1 : PrimitiveIsFixnumEqualLL
    {
        protected PrimitiveIsFixnumEqualLL1 (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveIsFixnumEqualLL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsFixnumEqualLQ : PrimitiveIsFixnumEqualL
    {
        public readonly int rand1Value;

        protected PrimitiveIsFixnumEqualLQ (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
        {
            return new PrimitiveIsFixnumEqualLQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsFixnumEqualQ : PrimitiveIsFixnumEqual
    {
        public readonly int rand0Value;

        protected PrimitiveIsFixnumEqualQ (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = (int) rand0.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveIsFixnumEqualQL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? Unimplemented ()
                : new PrimitiveIsFixnumEqualQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PrimitiveIsFixnumEqualQ.EvalStep";
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
            answer = this.rand0Value == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualQL : PrimitiveIsFixnumEqualQ
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsFixnumEqualQL (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsFixnumEqualQA.Make (rator, rand0, (Argument) rand1) :
                //(rand1 is LexicalVariable1) ? Unimplemented () :
                new PrimitiveIsFixnumEqualQL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualQL.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            int ev0 = this.rand0Value;

            // equal?
            answer =  (ev0 == (int) ev1) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualQA : PrimitiveIsFixnumEqualQL
    {
        protected PrimitiveIsFixnumEqualQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsFixnumEqualQA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveIsFixnumEqualQA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveIsFixnumEqualQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsFixnumEqualQA0 : PrimitiveIsFixnumEqualQA
    {
        protected PrimitiveIsFixnumEqualQA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument0 rand1)
        {
            return
                 new PrimitiveIsFixnumEqualQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualQA0.EvalStep");
#endif
            // Eval argument1
            object ev1 = environment.Argument0Value;

            // Eval argument0
            int ev0 = this.rand0Value;

            // Greater-than-fixnum?
            answer = (int) ev0 == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualQA1 : PrimitiveIsFixnumEqualQA
    {
        protected PrimitiveIsFixnumEqualQA1 (Primitive2 rator, Quotation rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument1 rand1)
        {
            return
                 new PrimitiveIsFixnumEqualQA1 (rator, rand0, rand1);
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
            answer = (int) ev0 == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualQL1 : PrimitiveIsFixnumEqualQL
    {
        protected PrimitiveIsFixnumEqualQL1 (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveIsFixnumEqualQL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsFixnumEqualSL : PrimitiveIsFixnumEqual
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsFixnumEqualSL (Primitive2 rator, SCode rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsFixnumEqualSA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveIsFixnumEqualSL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveIsFixnumEqualSL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualL.EvalStep");
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

            answer = (int) ev0 == (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsFixnumEqualSA : PrimitiveIsFixnumEqualSL
    {
        protected PrimitiveIsFixnumEqualSA (Primitive2 rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsFixnumEqualSA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented ()
                : new PrimitiveIsFixnumEqualSA (rator, rand0, rand1);
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

    class PrimitiveIsFixnumEqualSA0 : PrimitiveIsFixnumEqualSA
    {
        protected PrimitiveIsFixnumEqualSA0 (Primitive2 rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument0 rand1)
        {
            return

                new PrimitiveIsFixnumEqualSA0 (rator, rand0, rand1);
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

    class PrimitiveIsFixnumEqualSL1 : PrimitiveIsFixnumEqualSL
    {
        protected PrimitiveIsFixnumEqualSL1 (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
        {
            return

                new PrimitiveIsFixnumEqualSL1 (rator, rand0, rand1);
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
    sealed class PrimitiveIsFixnumEqualSQ : PrimitiveIsFixnumEqual
    {
        public readonly int rand1Value;

        PrimitiveIsFixnumEqualSQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveIsFixnumEqualSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumEqualSQ.EvalStep");
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

            answer = ((int) ev0 == this.rand1Value) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }
}

