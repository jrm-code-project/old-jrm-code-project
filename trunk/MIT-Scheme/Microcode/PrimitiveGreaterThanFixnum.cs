using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    class PrimitiveGreaterThanFixnum : PrimitiveCombination2
    {
        protected PrimitiveGreaterThanFixnum (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is LexicalVariable) ? PrimitiveGreaterThanFixnumL.Make (rator, (LexicalVariable) rand0, rand1)
                : (rand0 is Quotation) ? PrimitiveGreaterThanFixnumQ.Make (rator, (Quotation) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveGreaterThanFixnumSL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveGreaterThanFixnumSQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveGreaterThanFixnum (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            noteCalls (this.rand1);
            SCode.location = "PrimitiveGreaterThanFixnum.EvalStep";
#endif
            // Eval argument1
            object ev1;      
            
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
                        SCode.location = "PrimitiveGreaterThanFixnum.EvalStep.1";
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
            SCode.location = "PrimitiveGreaterThanFixnum.EvalStep.2";
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

    class PrimitiveGreaterThanFixnumL : PrimitiveGreaterThanFixnum
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PrimitiveGreaterThanFixnumL (Primitive2 rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? PrimitiveGreaterThanFixnumA.Make (rator, (Argument) rand0, rand1)
                : (rand0 is LexicalVariable1) ? PrimitiveGreaterThanFixnumL1.Make (rator, (LexicalVariable1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveGreaterThanFixnumLL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveGreaterThanFixnumLQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveGreaterThanFixnumL (rator, rand0, rand1);
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

    class PrimitiveGreaterThanFixnumA : PrimitiveGreaterThanFixnumL
    {
        protected PrimitiveGreaterThanFixnumA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveGreaterThanFixnumA0.Make (rator, (Argument0) rand0, rand1)
                : (rand0 is Argument1) ? PrimitiveGreaterThanFixnumA1.Make (rator, (Argument1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveGreaterThanFixnumAL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveGreaterThanFixnumAQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveGreaterThanFixnumA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumA.EvalStep");
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
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveGreaterThanFixnumA0 : PrimitiveGreaterThanFixnumA
    {
        protected PrimitiveGreaterThanFixnumA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveGreaterThanFixnumA0L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveGreaterThanFixnumA0Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveGreaterThanFixnumA0 (rator, rand0, rand1);
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

    class PrimitiveGreaterThanFixnumA0L : PrimitiveGreaterThanFixnumA0
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveGreaterThanFixnumA0L (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveGreaterThanFixnumA0A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveGreaterThanFixnumA0L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveGreaterThanFixnumA0L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumA0L.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0 = (int) environment.Argument0Value;

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveGreaterThanFixnumA0A : PrimitiveGreaterThanFixnumA0L
    {
        protected PrimitiveGreaterThanFixnumA0A (Primitive2 rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Unimplemented ()
                : (rand1 is Argument1) ? PrimitiveGreaterThanFixnumA0A1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveGreaterThanFixnumA0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    // class PrimitiveGreaterThanFixnumA0A0 : PrimitiveGreaterThanFixnumA0A

    class PrimitiveGreaterThanFixnumA0A1 : PrimitiveGreaterThanFixnumA0A
    {
        protected PrimitiveGreaterThanFixnumA0A1 (Primitive2 rator, Argument0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            return
                new PrimitiveGreaterThanFixnumA0A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumA0A1.EvalStep");
#endif
            // Eval argument1
            int ev1 = (int) environment.Argument1Value;

            // Eval argument0
            int ev0 = (int) environment.Argument0Value;

            // Greater-than-fixnum?
            answer = ev0 >  ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveGreaterThanFixnumA0L1 : PrimitiveGreaterThanFixnumA0L
    {
        protected PrimitiveGreaterThanFixnumA0L1 (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveGreaterThanFixnumA0L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumA0L1.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0 = (int) environment.Argument0Value;

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false; throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumA0Q : PrimitiveGreaterThanFixnumA0
    {
        public readonly int rand1Value;

        protected PrimitiveGreaterThanFixnumA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitiveGreaterThanFixnumA0Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumA0Q.EvalStep");
#endif
            // Greater-than-fixnum?
            answer = (int) environment.Argument0Value > this.rand1Value ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveGreaterThanFixnumA1 : PrimitiveGreaterThanFixnumA
    {
        protected PrimitiveGreaterThanFixnumA1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveGreaterThanFixnumA1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveGreaterThanFixnumA1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveGreaterThanFixnumA1 (rator, rand0, rand1);
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

    class PrimitiveGreaterThanFixnumA1L : PrimitiveGreaterThanFixnumA1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveGreaterThanFixnumA1L (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveGreaterThanFixnumA1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitiveGreaterThanFixnumA1L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumA1A : PrimitiveGreaterThanFixnumA1L
    {
        protected PrimitiveGreaterThanFixnumA1A (Primitive2 rator, Argument1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveGreaterThanFixnumA1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented()
                : new PrimitiveGreaterThanFixnumA1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumA1A.EvalStep");
#endif
            answer = (int) environment.Argument1Value > (int) environment.ArgumentValue (this.rand1Offset) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveGreaterThanFixnumA1A0 : PrimitiveGreaterThanFixnumA1A
    {
        protected PrimitiveGreaterThanFixnumA1A0 (Primitive2 rator, Argument1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument0 rand1)
        {
            return
                 new PrimitiveGreaterThanFixnumA1A0 (rator, rand0, rand1);
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
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveGreaterThanFixnumA1A1 : PrimitiveGreaterThanFixnumA1A
    {
        protected PrimitiveGreaterThanFixnumA1A1 (Primitive2 rator, Argument1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
                throw new NotImplementedException();
                //new PrimitiveGreaterThanFixnumA1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumA1L1 : PrimitiveGreaterThanFixnumA1L
    {
        protected PrimitiveGreaterThanFixnumA1L1 (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveGreaterThanFixnumA1L1 (rator, rand0, rand1);
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
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false; throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumA1Q : PrimitiveGreaterThanFixnumA1
    {
        public readonly int rand1Value;

        protected PrimitiveGreaterThanFixnumA1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Quotation rand1)
        {
            return
                new PrimitiveGreaterThanFixnumA1Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumA1Q.EvalStep");
#endif
            // Eval argument1
            int ev1 = this.rand1Value;

            // Eval argument0
            object ev0 = environment.Argument1Value;

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveGreaterThanFixnumAL : PrimitiveGreaterThanFixnumA
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveGreaterThanFixnumAL (Primitive2 rator, Argument rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveGreaterThanFixnumAA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitiveGreaterThanFixnumAL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumAA : PrimitiveGreaterThanFixnumAL
    {
        protected PrimitiveGreaterThanFixnumAA (Primitive2 rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Unimplemented ()
                : (rand1 is Argument1) ? PrimitiveGreaterThanFixnumAA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveGreaterThanFixnumAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumAA.EvalStep");
#endif
            // Eval argument1
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            // Eval argument0
            object ev0 =  environment.ArgumentValue (this.rand0Offset);

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveGreaterThanFixnumAA0 : PrimitiveGreaterThanFixnumAA
    {
        protected PrimitiveGreaterThanFixnumAA0 (Primitive2 rator, Argument rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument0 rand1)
        {
            return new PrimitiveGreaterThanFixnumAA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumAA1 : PrimitiveGreaterThanFixnumAA
    {
        protected PrimitiveGreaterThanFixnumAA1 (Primitive2 rator, Argument rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument1 rand1)
        {
            return new PrimitiveGreaterThanFixnumAA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumAL1 : PrimitiveGreaterThanFixnumAL
    {
        protected PrimitiveGreaterThanFixnumAL1 (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
        {
            return new PrimitiveGreaterThanFixnumAL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumAQ : PrimitiveGreaterThanFixnumA
    {
        public readonly int rand1Value;

        protected PrimitiveGreaterThanFixnumAQ (Primitive2 rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Quotation rand1)
        {
            return
                new PrimitiveGreaterThanFixnumAQ (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumAQ.EvalStep");
#endif
            // Eval argument1
            int ev1 = this.rand1Value;

            // Eval argument0
            object ev0 = environment.ArgumentValue(this.rand0Offset);

            // Greater-than-fixnum?
            answer = (int) ev0 >  ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveGreaterThanFixnumL1 : PrimitiveGreaterThanFixnumL
    {
        protected PrimitiveGreaterThanFixnumL1 (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveGreaterThanFixnumL1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveGreaterThanFixnumL1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveGreaterThanFixnumL1 (rator, rand0, rand1);
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

    class PrimitiveGreaterThanFixnumL1L : PrimitiveGreaterThanFixnumL1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveGreaterThanFixnumL1L (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveGreaterThanFixnumL1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitiveGreaterThanFixnumL1L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumL1A : PrimitiveGreaterThanFixnumL1L
    {
        protected PrimitiveGreaterThanFixnumL1A (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveGreaterThanFixnumL1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented ()
                : new PrimitiveGreaterThanFixnumL1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumL1A0 : PrimitiveGreaterThanFixnumL1A
    {
        protected PrimitiveGreaterThanFixnumL1A0 (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
        {
            return
                 new PrimitiveGreaterThanFixnumL1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumL1A0.EvalStep");
#endif
            // Eval argument1
            object ev1 = environment.Argument0Value;

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveGreaterThanFixnumL1A1 : PrimitiveGreaterThanFixnumL1A
    {
        protected PrimitiveGreaterThanFixnumL1A1 (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
        {
            return
                 new PrimitiveGreaterThanFixnumL1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumL1L1 : PrimitiveGreaterThanFixnumL1L
    {
        protected PrimitiveGreaterThanFixnumL1L1 (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveGreaterThanFixnumL1L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumL1Q : PrimitiveGreaterThanFixnumL1
    {
        public readonly int rand1Value;

        protected PrimitiveGreaterThanFixnumL1Q (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
        {
            return
                  new PrimitiveGreaterThanFixnumL1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumLL : PrimitiveGreaterThanFixnumL
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveGreaterThanFixnumLL (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveGreaterThanFixnumLA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitiveGreaterThanFixnumLL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumLA : PrimitiveGreaterThanFixnumLL
    {
        protected PrimitiveGreaterThanFixnumLA (Primitive2 rator, LexicalVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveGreaterThanFixnumLA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented ()
                : new PrimitiveGreaterThanFixnumLA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumLA0 : PrimitiveGreaterThanFixnumLA
    {
        protected PrimitiveGreaterThanFixnumLA0 (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
        {
            return
                 new PrimitiveGreaterThanFixnumLA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumLA1 : PrimitiveGreaterThanFixnumLA
    {
        protected PrimitiveGreaterThanFixnumLA1 (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
        {
            return
                 new PrimitiveGreaterThanFixnumLA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumLL1 : PrimitiveGreaterThanFixnumLL
    {
        protected PrimitiveGreaterThanFixnumLL1 (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveGreaterThanFixnumLL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumLQ : PrimitiveGreaterThanFixnumL
    {
        public readonly int rand1Value;

        protected PrimitiveGreaterThanFixnumLQ (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
        {
            return new PrimitiveGreaterThanFixnumLQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumQ : PrimitiveGreaterThanFixnum 
    {
        public readonly int rand0Value;

        protected PrimitiveGreaterThanFixnumQ (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = (int) rand0.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveGreaterThanFixnumQL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? Unimplemented ()
                : new PrimitiveGreaterThanFixnumQ (rator, rand0, rand1);
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

    class PrimitiveGreaterThanFixnumQL : PrimitiveGreaterThanFixnumQ
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveGreaterThanFixnumQL (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveGreaterThanFixnumQA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitiveGreaterThanFixnumQL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumQA : PrimitiveGreaterThanFixnumQL
    {
        protected PrimitiveGreaterThanFixnumQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveGreaterThanFixnumQA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveGreaterThanFixnumQA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveGreaterThanFixnumQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumQA0 : PrimitiveGreaterThanFixnumQA
    {
        protected PrimitiveGreaterThanFixnumQA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument0 rand1)
        {
            return
                 new PrimitiveGreaterThanFixnumQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumQA0.EvalStep");
#endif
            // Eval argument1
            object ev1 = environment.Argument0Value;

            // Eval argument0
            int ev0 = this.rand0Value;

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveGreaterThanFixnumQA1 : PrimitiveGreaterThanFixnumQA
    {
        protected PrimitiveGreaterThanFixnumQA1 (Primitive2 rator, Quotation rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument1 rand1)
        {
            return
                 new PrimitiveGreaterThanFixnumQA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumQA1.EvalStep");
#endif
            // Eval argument1
            object ev1 = environment.Argument1Value;

            // Eval argument0
            int ev0 = this.rand0Value;

            // Greater-than-fixnum?
            answer = (int) ev0 > (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveGreaterThanFixnumQL1 : PrimitiveGreaterThanFixnumQL
    {
        protected PrimitiveGreaterThanFixnumQL1 (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveGreaterThanFixnumQL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveGreaterThanFixnumSL : PrimitiveGreaterThanFixnum
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveGreaterThanFixnumSL (Primitive2 rator, SCode rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveGreaterThanFixnumSA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveGreaterThanFixnumSL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveGreaterThanFixnumSL (rator, rand0, rand1);
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

    class PrimitiveGreaterThanFixnumSA : PrimitiveGreaterThanFixnumSL
    {
        protected PrimitiveGreaterThanFixnumSA (Primitive2 rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveGreaterThanFixnumSA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented()
                : new PrimitiveGreaterThanFixnumSA (rator, rand0, rand1);
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

    class PrimitiveGreaterThanFixnumSA0 : PrimitiveGreaterThanFixnumSA
    {
        protected PrimitiveGreaterThanFixnumSA0 (Primitive2 rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument0 rand1)
        {
            return
               
                new PrimitiveGreaterThanFixnumSA0 (rator, rand0, rand1);
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

    class PrimitiveGreaterThanFixnumSL1 : PrimitiveGreaterThanFixnumSL
    {
        protected PrimitiveGreaterThanFixnumSL1 (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
        {
            return

                new PrimitiveGreaterThanFixnumSL1 (rator, rand0, rand1);
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

    class PrimitiveGreaterThanFixnumSQ : PrimitiveGreaterThanFixnum
    {
        public readonly int rand1Value;

        protected PrimitiveGreaterThanFixnumSQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveGreaterThanFixnumSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGreaterThanFixnumSQ.EvalStep");
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
            answer = (int) ev0 > ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
            throw new NotImplementedException ();
        }
    }
}
