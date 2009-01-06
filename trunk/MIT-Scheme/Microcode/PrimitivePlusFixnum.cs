using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PrimitivePlusFixnum : PrimitiveCombination2
    {
        protected PrimitivePlusFixnum (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

 
        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is LexicalVariable) ? PrimitivePlusFixnumL.Make (rator, (LexicalVariable) rand0, rand1) :
                (rand0 is Quotation) ? PrimitivePlusFixnumQ.Make (rator, (Quotation) rand0, rand1) :
                (rand1 is LexicalVariable) ? PrimitivePlusFixnumSL.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitivePlusFixnumSQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitivePlusFixnum (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnum.EvalStep");
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

            // Compute answer
            answer = (int) ev0 + (int) ev1;
            return false;
        }
    }

    class PrimitivePlusFixnumL : PrimitivePlusFixnum
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PrimitivePlusFixnumL (Primitive2 rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? PrimitivePlusFixnumA.Make (rator, (Argument) rand0, rand1)
                : (rand0 is LexicalVariable1) ? PrimitivePlusFixnumL1.Make (rator, (LexicalVariable1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitivePlusFixnumLL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitivePlusFixnumLQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitivePlusFixnumL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PrimitivePlusFixnumL.EvalStep";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitivePlusFixnumL.EvalStep.1";
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
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            // Compute answer
            answer = (int) ev0 + (int) ev1;
            return false;
        }
    }

    class PrimitivePlusFixnumA : PrimitivePlusFixnumL
    {
        protected PrimitivePlusFixnumA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitivePlusFixnumA0.Make (rator, (Argument0) rand0, rand1)
                : (rand0 is Argument1) ? PrimitivePlusFixnumA1.Make (rator, (Argument1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitivePlusFixnumAL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitivePlusFixnumAQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitivePlusFixnumA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PrimitivePlusFixnumA.EvalStep";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
                        SCode.location = "PrimitivePlusFixnumA.EvalStep.1";
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

            // Compute answer
            answer = (int) ev0 + (int) ev1;
            return false;
        }
    }

    class PrimitivePlusFixnumA0 : PrimitivePlusFixnumA
    {
        protected PrimitivePlusFixnumA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitivePlusFixnumA0L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitivePlusFixnumA0Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitivePlusFixnumA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumA0.EvalStep");
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
            object ev0 = environment.Argument0Value;

            // Compute answer
            answer = (int) ev0 + (int) ev1;
            return false;
        }
    }

    class PrimitivePlusFixnumA0L : PrimitivePlusFixnumA0
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitivePlusFixnumA0L (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitivePlusFixnumA0A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitivePlusFixnumA0L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitivePlusFixnumA0L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumA0L.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            int ev0 = (int) environment.Argument0Value;

            // Compute answer
            answer = ev0 + (int) ev1;
            return false;
        }
    }

    class PrimitivePlusFixnumA0A : PrimitivePlusFixnumA0L
    {
        protected PrimitivePlusFixnumA0A (Primitive2 rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitivePlusFixnumA0A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitivePlusFixnumA0A1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitivePlusFixnumA0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    sealed class PrimitivePlusFixnumA0A0 : PrimitivePlusFixnumA0A
    {
        PrimitivePlusFixnumA0A0 (Primitive2 rator, Argument0 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument0 rand1)
        {
            return
                new PrimitivePlusFixnumA0A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumA0A0.EvalStep");
#endif
            // Eval argument1
            int ev1 = (int) environment.Argument0Value;

            answer = ev1 + ev1;
            return false;
        }
    }


    class PrimitivePlusFixnumA0A1 : PrimitivePlusFixnumA0A
    {
        protected PrimitivePlusFixnumA0A1 (Primitive2 rator, Argument0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            return
                new PrimitivePlusFixnumA0A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument1
            object ev1 = environment.Argument1Value;

            // Eval argument0
            object ev0 = environment.Argument0Value;

            // Compute result
            throw new NotImplementedException();
        }
    }

    class PrimitivePlusFixnumA0L1 : PrimitivePlusFixnumA0L
    {
        protected PrimitivePlusFixnumA0L1 (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitivePlusFixnumA0L1 (rator, rand0, rand1);
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
            object ev0 = (int) environment.Argument0Value;

            // Compute answer
            throw new NotImplementedException ();
        }
    }

    class PrimitivePlusFixnumA0Q : PrimitivePlusFixnumA0
    {
        public readonly int rand1Value;

        protected PrimitivePlusFixnumA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitivePlusFixnumA0Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumA0Q.EvalStep");
#endif
            answer = (int) environment.Argument0Value + this.rand1Value;
            return false;
        }
    }

    class PrimitivePlusFixnumA1 : PrimitivePlusFixnumA
    {
        protected PrimitivePlusFixnumA1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitivePlusFixnumA1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitivePlusFixnumA1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitivePlusFixnumA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumA1.EvalStep");
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
            object ev0 = environment.Argument1Value;

            // Compute answer
            answer = (int) ev0 + (int) ev1;
            return false;
        }
    }

    class PrimitivePlusFixnumA1L : PrimitivePlusFixnumA1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitivePlusFixnumA1L (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitivePlusFixnumA1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitivePlusFixnumA1L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumA1L.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            answer = (int) environment.Argument1Value + (int) ev1;
            return false;
        }
    }

    class PrimitivePlusFixnumA1A : PrimitivePlusFixnumA1L
    {
        protected PrimitivePlusFixnumA1A (Primitive2 rator, Argument1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitivePlusFixnumA1A0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? Unimplemented() :
                new PrimitivePlusFixnumA1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumA1A.EvalStep");
#endif
            answer = (int) environment.Argument1Value + (int) environment.ArgumentValue (this.rand1Offset);
            return false;
        }
    }

    sealed class PrimitivePlusFixnumA1A0 : PrimitivePlusFixnumA1A
    {
        PrimitivePlusFixnumA1A0 (Primitive2 rator, Argument1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument0 rand1)
        {
            return
                 new PrimitivePlusFixnumA1A0 (rator, rand0, rand1);
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

            // Compute answer
            throw new NotImplementedException();
        }
    }


//    class PrimitivePlusFixnumA1A1 : PrimitivePlusFixnumA1A
//    {
//        protected PrimitivePlusFixnumA1A1 (Primitive2 rator, Argument1 rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
//        {
//            return
//                new PrimitivePlusFixnumA1A1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            throw new NotImplementedException ();
//        }
//    }

    class PrimitivePlusFixnumA1Q : PrimitivePlusFixnumA1
    {
        public readonly int rand1Value;

        protected PrimitivePlusFixnumA1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Quotation rand1)
        {
            return
                new PrimitivePlusFixnumA1Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumA1Q.EvalStep");
#endif
            answer = (int) environment.Argument1Value + this.rand1Value;
            return false;
        }
    }

    class PrimitivePlusFixnumAL : PrimitivePlusFixnumA
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitivePlusFixnumAL (Primitive2 rator, Argument rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitivePlusFixnumAA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitivePlusFixnumAL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitivePlusFixnumAA : PrimitivePlusFixnumAL
    {
        protected PrimitivePlusFixnumAA (Primitive2 rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Unimplemented ()
                : (rand1 is Argument1) ? PrimitivePlusFixnumAA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitivePlusFixnumAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumAA.EvalStep");
#endif
            // Eval argument1
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            // Eval argument0
            object ev0 =  environment.ArgumentValue (this.rand0Offset);

            // Compute answer
            answer = (int) ev0 + (int) ev1;
            return false;
        }
    }

    class PrimitivePlusFixnumAA1 : PrimitivePlusFixnumAA
    {
        protected PrimitivePlusFixnumAA1 (Primitive2 rator, Argument rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument1 rand1)
        {
            return new PrimitivePlusFixnumAA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }


    class PrimitivePlusFixnumAQ : PrimitivePlusFixnumA
    {
        public readonly int rand1Value;

        protected PrimitivePlusFixnumAQ (Primitive2 rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Quotation rand1)
        {
            return
                new PrimitivePlusFixnumAQ (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumAQ.EvalStep");
#endif
            // Eval argument1
            int ev1 = this.rand1Value;

            // Eval argument0
            object ev0 = environment.ArgumentValue(this.rand0Offset);

            // Compute answer
            answer = (int) ev0 + ev1;
            return false;
        }
    }

    class PrimitivePlusFixnumL1 : PrimitivePlusFixnumL
    {
        protected PrimitivePlusFixnumL1 (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitivePlusFixnumL1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitivePlusFixnumL1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitivePlusFixnumL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumL1.EvalStep");
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

            // Compute answer
            answer = (int) ev0 + (int) ev1;
            return false;
        }
    }

    class PrimitivePlusFixnumL1L : PrimitivePlusFixnumL1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitivePlusFixnumL1L (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitivePlusFixnumL1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitivePlusFixnumL1L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitivePlusFixnumL1L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitivePlusFixnumL1A : PrimitivePlusFixnumL1L
    {
        protected PrimitivePlusFixnumL1A (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitivePlusFixnumL1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented ()
                : new PrimitivePlusFixnumL1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitivePlusFixnumL1A0 : PrimitivePlusFixnumL1A
    {
        protected PrimitivePlusFixnumL1A0 (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
        {
            return
                 new PrimitivePlusFixnumL1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumL1A0.EvalStep");
#endif
            // Eval argument1
            object ev1 = environment.Argument0Value;

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // Compute answer
            answer = (int) ev0 + (int) ev1;
            return false;
        }
    }

    class PrimitivePlusFixnumL1L1 : PrimitivePlusFixnumL1L
    {
        protected PrimitivePlusFixnumL1L1 (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitivePlusFixnumL1L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumL1L1.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // Compute answer
            answer = (int) ev0 + (int) ev1;
            return false;
        }
    }


    class PrimitivePlusFixnumL1Q : PrimitivePlusFixnumL1
    {
        public readonly int rand1Value;

        protected PrimitivePlusFixnumL1Q (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
        {
            return
                  new PrimitivePlusFixnumL1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumL1Q.EvalStep");
#endif
            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // Compute answer
            answer = (int) ev0 + this.rand1Value;
            return false;
        }
    }


    class PrimitivePlusFixnumLL : PrimitivePlusFixnumL
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitivePlusFixnumLL (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitivePlusFixnumLA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitivePlusFixnumLL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitivePlusFixnumLL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitivePlusFixnumLA : PrimitivePlusFixnumLL
    {
        protected PrimitivePlusFixnumLA (Primitive2 rator, LexicalVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitivePlusFixnumLA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitivePlusFixnumLA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitivePlusFixnumLA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitivePlusFixnumLA0 : PrimitivePlusFixnumLA
    {
        protected PrimitivePlusFixnumLA0 (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
        {
            return
                 new PrimitivePlusFixnumLA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumLA0.EvalStep");
#endif
            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            // Compute answer
            answer = (int) ev0 + (int) environment.Argument0Value;
            return false;
        }
    }

    class PrimitivePlusFixnumLA1 : PrimitivePlusFixnumLA
    {
        protected PrimitivePlusFixnumLA1 (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
        {
            return
                 new PrimitivePlusFixnumLA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitivePlusFixnumLL1 : PrimitivePlusFixnumLL
    {
        protected PrimitivePlusFixnumLL1 (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitivePlusFixnumLL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }



    class PrimitivePlusFixnumLQ : PrimitivePlusFixnumL
    {
        public readonly int rand1Value;

        protected PrimitivePlusFixnumLQ (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
        {
            return
                 new PrimitivePlusFixnumLQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumLQ.EvalStep");
#endif
            // Eval argument1
            int ev1 = this.rand1Value;

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset)) 
                throw new NotImplementedException ();

            // Compute answer
            answer = (int) ev0 + ev1;
            return false;
        }
    }

    class PrimitivePlusFixnumQ : PrimitivePlusFixnum 
    {
        public readonly int rand0Value;

        protected PrimitivePlusFixnumQ (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = (int) rand0.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitivePlusFixnumQL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? Unimplemented ()
                : new PrimitivePlusFixnumQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumQ.EvalStep");
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
            int ev0 = this.rand0Value;

            // Compute answer
            answer = ev0 + (int) ev1;
            return false;
        }
    }

    class PrimitivePlusFixnumQL : PrimitivePlusFixnumQ
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitivePlusFixnumQL (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitivePlusFixnumQA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitivePlusFixnumQL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitivePlusFixnumQL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitivePlusFixnumQA : PrimitivePlusFixnumQL
    {
        protected PrimitivePlusFixnumQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitivePlusFixnumQA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitivePlusFixnumQA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitivePlusFixnumQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitivePlusFixnumQA0 : PrimitivePlusFixnumQA
    {
        protected PrimitivePlusFixnumQA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument0 rand1)
        {
            return
                 new PrimitivePlusFixnumQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument1
            object ev1 = environment.Argument0Value;

            // Eval argument0
            object ev0 = this.rand0Value;

            // Compute result
            throw new NotImplementedException();
        }
    }

    class PrimitivePlusFixnumQA1 : PrimitivePlusFixnumQA
    {
        protected PrimitivePlusFixnumQA1 (Primitive2 rator, Quotation rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument1 rand1)
        {
            return
                 new PrimitivePlusFixnumQA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument1
            object ev1 = environment.Argument1Value;

            // Eval argument0
            object ev0 = this.rand0Value;

            // Compute answer
            throw new NotImplementedException();
        }
    }

    class PrimitivePlusFixnumQL1 : PrimitivePlusFixnumQL
    {
        protected PrimitivePlusFixnumQL1 (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitivePlusFixnumQL1 (rator, rand0, rand1);
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
            object ev0 = this.rand0Value;

            // Compute answer
            throw new NotImplementedException ();
        }
    }


    class PrimitivePlusFixnumSL : PrimitivePlusFixnum
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitivePlusFixnumSL (Primitive2 rator, SCode rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitivePlusFixnumSA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitivePlusFixnumSL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitivePlusFixnumSL (rator, rand0, rand1);
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

    class PrimitivePlusFixnumSA : PrimitivePlusFixnumSL
    {
        protected PrimitivePlusFixnumSA (Primitive2 rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitivePlusFixnumSA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitivePlusFixnumSA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitivePlusFixnumSA (rator, rand0, rand1);
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

    class PrimitivePlusFixnumSA0 : PrimitivePlusFixnumSA
    {
        protected PrimitivePlusFixnumSA0 (Primitive2 rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument0 rand1)
        {
            return
               
                new PrimitivePlusFixnumSA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumSA0.EvalStep");
            noteCalls (this.rand0);
#endif
            // Eval argument1
            object ev1 = environment.Argument0Value;

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

            // Compute answer
            answer = (int) ev0 + (int) ev1;
            return false;
        }
    }

    class PrimitivePlusFixnumSA1 : PrimitivePlusFixnumSA
    {
        protected PrimitivePlusFixnumSA1 (Primitive2 rator, SCode rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument1 rand1)
        {
            return

                new PrimitivePlusFixnumSA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumSA1.EvalStep");
            noteCalls (this.rand0);
#endif
            // Eval argument1
            object ev1 = environment.Argument1Value;

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

            // Compute answer
            answer = (int) ev0 + (int) ev1;
            return false;
        }
    }


    class PrimitivePlusFixnumSL1 : PrimitivePlusFixnumSL
    {
        protected PrimitivePlusFixnumSL1 (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
        {
            return

                new PrimitivePlusFixnumSL1 (rator, rand0, rand1);
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



    class PrimitivePlusFixnumSQ : PrimitivePlusFixnum
    {
        public readonly int rand1Value;

        protected PrimitivePlusFixnumSQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitivePlusFixnumSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitivePlusFixnumSQ.EvalStep");
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

            // Compute answer
            answer = (int) ev0 + ev1;
            return false;
        }
    }
}
