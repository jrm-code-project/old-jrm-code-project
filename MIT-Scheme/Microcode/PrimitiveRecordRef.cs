using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PrimitiveRecordRef : PrimitiveCombination2
    {
        protected PrimitiveRecordRef (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }


        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is LexicalVariable) ? PrimitiveRecordRefL.Make (rator, (LexicalVariable) rand0, rand1) :
                (rand0 is Quotation) ? PrimitiveRecordRefQ.Make (rator, (Quotation) rand0, rand1) :
                (rand1 is LexicalVariable) ? PrimitiveRecordRefSL.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveRecordRefSQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveRecordRef (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
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

            answer = ((Record) ev0).Ref((int)ev1);
            return false;
        }
    }

    class PrimitiveRecordRefL : PrimitiveRecordRef
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PrimitiveRecordRefL (Primitive2 rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? PrimitiveRecordRefA.Make (rator, (Argument) rand0, rand1)
                : (rand0 is LexicalVariable1) ? PrimitiveRecordRefL1.Make (rator, (LexicalVariable1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveRecordRefLL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveRecordRefLQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveRecordRefL (rator, rand0, rand1);
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
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((Record) ev0).Ref((int)ev1);
            return false;
        }

    }

    class PrimitiveRecordRefA : PrimitiveRecordRefL
    {
        protected PrimitiveRecordRefA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveRecordRefA0.Make (rator, (Argument0) rand0, rand1)
                : (rand0 is Argument1) ? PrimitiveRecordRefA1.Make (rator, (Argument1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveRecordRefAL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveRecordRefAQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveRecordRefA (rator, rand0, rand1);
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
            answer = ((Record) environment.ArgumentValue(this.rand0Offset)).Ref((int)ev1);
            return false;
        }
    }

    class PrimitiveRecordRefA0 : PrimitiveRecordRefA
    {
        protected PrimitiveRecordRefA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveRecordRefA0L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveRecordRefA0Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveRecordRefA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
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

            answer = ((Record) environment.Argument0Value).Ref((int)ev1);
            return false;
        }
    }

    class PrimitiveRecordRefA0L : PrimitiveRecordRefA0
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveRecordRefA0L (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveRecordRefA0A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveRecordRefA0L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveRecordRefA0L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveRecordRefA0L.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            answer = ((Record) environment.Argument0Value).Ref((int)ev1);
            return false;
        }
    }

    class PrimitiveRecordRefA0A : PrimitiveRecordRefA0L
    {
        protected PrimitiveRecordRefA0A (Primitive2 rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveRecordRefA0A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveRecordRefA0A1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveRecordRefA0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveRecordRefA0A0 : PrimitiveRecordRefA0A
    {
        protected PrimitiveRecordRefA0A0 (Primitive2 rator, Argument0 rand0, Argument0 rand1)
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



    class PrimitiveRecordRefA0A1 : PrimitiveRecordRefA0A
    {
        protected PrimitiveRecordRefA0A1 (Primitive2 rator, Argument0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            return
                new PrimitiveRecordRefA0A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            answer = ((Record) environment.Argument0Value).Ref((int)environment.Argument1Value);
            return false;
        }
    }

    class PrimitiveRecordRefA0L1 : PrimitiveRecordRefA0L
    {
        protected PrimitiveRecordRefA0L1 (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveRecordRefA0L1 (rator, rand0, rand1);
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

            answer = ((Record) environment.Argument0Value).Ref((int)ev1);
            return false;
        }
    }

    class PrimitiveRecordRefA0Q : PrimitiveRecordRefA0
    {
        public readonly int rand1Value;

        protected PrimitiveRecordRefA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitiveRecordRefA0Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveRecordRefA0Q.EvalStep");
#endif
            answer = ((Record) environment.Argument0Value).Ref(this.rand1Value);
            return false;
        }
    }

    class PrimitiveRecordRefA1 : PrimitiveRecordRefA
    {
        protected PrimitiveRecordRefA1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveRecordRefA1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveRecordRefA1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveRecordRefA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PrimitiveRecordRefA1.EvalStep";
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

            answer = ((Record) environment.Argument1Value).Ref((int)ev1);
            return false;
        }
    }

    class PrimitiveRecordRefA1L : PrimitiveRecordRefA1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveRecordRefA1L (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveRecordRefA1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveRecordRefA1L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveRecordRefA1L (rator, rand0, rand1);
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

            answer = ((Record) environment.Argument1Value).Ref((int)ev1);
            return false;
        }
    }

    class PrimitiveRecordRefA1A : PrimitiveRecordRefA1L
    {
        protected PrimitiveRecordRefA1A (Primitive2 rator, Argument1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveRecordRefA1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented ()
                : new PrimitiveRecordRefA1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif

            answer = ((Record) environment.Argument1Value).Ref((int)environment.ArgumentValue(this.rand1Offset));
            return false;
        }

    }



    class PrimitiveRecordRefA1A0 : PrimitiveRecordRefA1A
    {
        protected PrimitiveRecordRefA1A0 (Primitive2 rator, Argument1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument0 rand1)
        {
            return
                 new PrimitiveRecordRefA1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            answer = ((Record) environment.Argument1Value).Ref((int)environment.Argument0Value);
            return false;
        }
    }

    class PrimitiveRecordRefA1A1 : PrimitiveRecordRefA1A
    {
        protected PrimitiveRecordRefA1A1 (Primitive2 rator, Argument1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            throw new NotImplementedException ();
            //new PrimitiveRecordRefA1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveRecordRefA1L1 : PrimitiveRecordRefA1L
    {
        protected PrimitiveRecordRefA1L1 (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveRecordRefA1L1 (rator, rand0, rand1);
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
            answer = ((Record) ev0).Ref((int)ev1);
            return false; throw new NotImplementedException ();
        }
    }

    class PrimitiveRecordRefA1Q : PrimitiveRecordRefA1
    {
        public readonly int rand1Value;

        protected PrimitiveRecordRefA1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Quotation rand1)
        {
            return
                new PrimitiveRecordRefA1Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveRecordRefA1Q.EvalStep");
#endif
            answer = ((Record) environment.Argument1Value).Ref(this.rand1Value);
            return false;
        }
    }

    class PrimitiveRecordRefAL : PrimitiveRecordRefA
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveRecordRefAL (Primitive2 rator, Argument rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveRecordRefAA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitiveRecordRefAL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveRecordRefAA : PrimitiveRecordRefAL
    {
        protected PrimitiveRecordRefAA (Primitive2 rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Unimplemented ()
                : (rand1 is Argument1) ? PrimitiveRecordRefAA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveRecordRefAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            answer = ((Record) environment.ArgumentValue (this.rand0Offset)).Ref((int)environment.ArgumentValue(this.rand1Offset));
            return false;
        }
    }

    class PrimitiveRecordRefAA0 : PrimitiveRecordRefAA
    {
        protected PrimitiveRecordRefAA0 (Primitive2 rator, Argument rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument0 rand1)
        {
            return new PrimitiveRecordRefAA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveRecordRefAA1 : PrimitiveRecordRefAA
    {
        protected PrimitiveRecordRefAA1 (Primitive2 rator, Argument rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument1 rand1)
        {
            return new PrimitiveRecordRefAA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveRecordRefAL1 : PrimitiveRecordRefAL
    {
        protected PrimitiveRecordRefAL1 (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
        {
            return new PrimitiveRecordRefAL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PrimitiveRecordRefAQ : PrimitiveRecordRefA
    {
        public readonly int rand1Value;

        protected PrimitiveRecordRefAQ (Primitive2 rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Quotation rand1)
        {
            return
                new PrimitiveRecordRefAQ (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveRecordRefAQ.EvalStep");
#endif
            answer = ((Record) environment.ArgumentValue(this.rand0Offset)).Ref(this.rand1Value);
            return false;
        }
    }

    class PrimitiveRecordRefL1 : PrimitiveRecordRefL
    {
        protected PrimitiveRecordRefL1 (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveRecordRefL1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveRecordRefL1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveRecordRefL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PrimitiveRecordRefL1.EvalStep";
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

            answer = ((Record) ev0).Ref((int)ev1);
            return false;
        }
    }

    class PrimitiveRecordRefL1L : PrimitiveRecordRefL1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveRecordRefL1L (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveRecordRefL1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveRecordRefL1L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveRecordRefL1L (rator, rand0, rand1);
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


            answer = ((Record) ev0).Ref((int)ev1);
            return false;
        }

    }

    class PrimitiveRecordRefL1A : PrimitiveRecordRefL1L
    {
        protected PrimitiveRecordRefL1A (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveRecordRefL1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveRecordRefL1A1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveRecordRefL1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveRecordRefL1A0 : PrimitiveRecordRefL1A
    {
        protected PrimitiveRecordRefL1A0 (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
        {
            return
                 new PrimitiveRecordRefL1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((Record) ev0).Ref((int)environment.Argument0Value);
            return false;
        }
    }

    class PrimitiveRecordRefL1A1 : PrimitiveRecordRefL1A
    {
        protected PrimitiveRecordRefL1A1 (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
        {
            return
                 new PrimitiveRecordRefL1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((Record) ev0).Ref((int)environment.Argument1Value);
            return false;
        }

    }

    class PrimitiveRecordRefL1L1 : PrimitiveRecordRefL1L
    {
        protected PrimitiveRecordRefL1L1 (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveRecordRefL1L1 (rator, rand0, rand1);
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
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((Record) ev0).Ref((int)ev1);
            return false;
        }
    }

    class PrimitiveRecordRefL1Q : PrimitiveRecordRefL1
    {
        public readonly int rand1Value;

        protected PrimitiveRecordRefL1Q (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
        {
            return
                  new PrimitiveRecordRefL1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveRecordRefL1Q.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            answer = ((Record) ev0).Ref(this.rand1Value);
            return false;
        }
    }

    class PrimitiveRecordRefLL : PrimitiveRecordRefL
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveRecordRefLL (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveRecordRefLA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveRecordRefLL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveRecordRefLL (rator, rand0, rand1);
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
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((Record) ev0).Ref((int)ev1);
            return false;
        }
    }

    class PrimitiveRecordRefLA : PrimitiveRecordRefLL
    {
        protected PrimitiveRecordRefLA (Primitive2 rator, LexicalVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveRecordRefLA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveRecordRefLA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveRecordRefLA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            answer = ((Record) ev0) .Ref((int) environment.ArgumentValue(this.rand1Offset));
            return false;
        }
    }

    class PrimitiveRecordRefLA0 : PrimitiveRecordRefLA
    {
        protected PrimitiveRecordRefLA0 (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
        {
            return
                 new PrimitiveRecordRefLA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            answer = ((Record) ev0).Ref((int)environment.Argument0Value);
            return false;
        }
    }

    class PrimitiveRecordRefLA1 : PrimitiveRecordRefLA
    {
        protected PrimitiveRecordRefLA1 (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
        {
            return
                 new PrimitiveRecordRefLA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((Record) ev0).Ref((int)environment.Argument1Value);
            return false;
        }

    }

    class PrimitiveRecordRefLL1 : PrimitiveRecordRefLL
    {
        protected PrimitiveRecordRefLL1 (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveRecordRefLL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
            noteCalls (this.rand1);
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            answer = ((Record) ev0).Ref((int)ev1);
            return false;
        }


    }

    class PrimitiveRecordRefLQ : PrimitiveRecordRefL
    {
        public readonly int rand1Value;

        protected PrimitiveRecordRefLQ (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
        {
            return new PrimitiveRecordRefLQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveRecordRefLQ.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            answer = ((Record) ev0).Ref(this.rand1Value);
            return false;
        }
    }



    class PrimitiveRecordRefQ : PrimitiveRecordRef
    {
        public readonly Record rand0Value;

        protected PrimitiveRecordRefQ (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = (Record) rand0.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveRecordRefQL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveRecordRefQQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveRecordRefQ (rator, rand0, rand1);
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

            answer = this.rand0Value.Ref((int)ev1);
            return false;
        }
    }

    class PrimitiveRecordRefQL : PrimitiveRecordRefQ
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveRecordRefQL (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveRecordRefQA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveRecordRefQL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveRecordRefQL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveRecordRefQA : PrimitiveRecordRefQL
    {
        protected PrimitiveRecordRefQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveRecordRefQA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveRecordRefQA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveRecordRefQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            answer = this.rand0Value .Ref((int) environment.ArgumentValue(this.rand1Offset));
            return false;
        }
    }

    class PrimitiveRecordRefQA0 : PrimitiveRecordRefQA
    {
        protected PrimitiveRecordRefQA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument0 rand1)
        {
            return
                 new PrimitiveRecordRefQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            answer = this.rand0Value .Ref((int)environment.Argument0Value);
            return false;
        }
    }

    class PrimitiveRecordRefQA1 : PrimitiveRecordRefQA
    {
        protected PrimitiveRecordRefQA1 (Primitive2 rator, Quotation rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument1 rand1)
        {
            return
                 new PrimitiveRecordRefQA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            answer = this.rand0Value .Ref((int)environment.Argument1Value);
            return false;
        }
    }

    class PrimitiveRecordRefQL1 : PrimitiveRecordRefQL
    {
        protected PrimitiveRecordRefQL1 (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveRecordRefQL1 (rator, rand0, rand1);
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


            answer = this.rand0Value.Ref((int)ev1);
            return false;
        }

    }



    class PrimitiveRecordRefQQ : PrimitiveRecordRefQ
    {
        public readonly int rand1Value;

        protected PrimitiveRecordRefQQ (Primitive2 rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Quotation rand1)
        {
            return
                 new PrimitiveRecordRefQQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // refetch because vector may be mutable
            answer = this.rand0Value .Ref(this.rand1Value);
            return false;
        }
    }

    class PrimitiveRecordRefSL : PrimitiveRecordRef
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveRecordRefSL (Primitive2 rator, SCode rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveRecordRefSA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveRecordRefSL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveRecordRefSL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PrimitiveRecordRefSL.EvalStep";
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

            answer = ((Record) ev0).Ref((int)ev1);
            return false;
        }
    }

    class PrimitiveRecordRefSA : PrimitiveRecordRefSL
    {
        protected PrimitiveRecordRefSA (Primitive2 rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveRecordRefSA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveRecordRefSA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveRecordRefSA (rator, rand0, rand1);
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

            answer = ((Record) ev0).Ref((int)environment.ArgumentValue(this.rand1Offset));
            return false;
        }

    }

    class PrimitiveRecordRefSA0 : PrimitiveRecordRefSA
    {
        protected PrimitiveRecordRefSA0 (Primitive2 rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument0 rand1)
        {
            return

                new PrimitiveRecordRefSA0 (rator, rand0, rand1);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
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

            answer = ((Record) ev0).Ref((int)environment.Argument0Value);
            return false;
        }

    }

    class PrimitiveRecordRefSA1 : PrimitiveRecordRefSA
    {
        protected PrimitiveRecordRefSA1 (Primitive2 rator, SCode rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument1 rand1)
        {
            return

                new PrimitiveRecordRefSA1 (rator, rand0, rand1);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
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

            answer = ((Record) ev0).Ref((int)environment.Argument1Value);
            return false;
        }

    }


    class PrimitiveRecordRefSL1 : PrimitiveRecordRefSL
    {
        protected PrimitiveRecordRefSL1 (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
        {
            return

                new PrimitiveRecordRefSL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
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

            answer = ((Record) ev0).Ref((int)ev1);
            return false;
        }

    }

    [Serializable]
    sealed class PrimitiveRecordRefSQ : PrimitiveRecordRef
    {
        public readonly int rand1Value;

        PrimitiveRecordRefSQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveRecordRefSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveRecordRefSQ.EvalStep");
            noteCalls (this.rand0);
#endif
            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveRecordRefSQFrame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
                throw new NotImplementedException ();

            }

            answer = ((Record) ev0).Ref(this.rand1Value);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveRecordRefSQFrame0 : SubproblemContinuation<PrimitiveRecordRefSQ>, ISystemVector
    {
        internal PrimitiveRecordRefSQFrame0 (PrimitiveRecordRefSQ expression, Environment environment)
            : base (expression, environment)
        {
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { throw new NotImplementedException (); }
        }

        public object SystemVectorRef (int index)
        {
            throw new NotImplementedException ();
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            throw new NotImplementedException ();
        }
    }

}
