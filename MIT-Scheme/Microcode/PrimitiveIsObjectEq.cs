using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    /// <summary>
    /// True iff both args are objects and they are = to each other.
    /// False otherwise.  Does not throw an error.
    /// </summary>
    [Serializable]
    class PrimitiveIsObjectEq : PrimitiveCombination2
    {
        protected PrimitiveIsObjectEq (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is LexicalVariable) ? PrimitiveIsObjectEqL.Make (rator, (LexicalVariable) rand0, rand1) :
                (rand0 is Quotation) ? PrimitiveIsObjectEqQ.Make (rator, (Quotation) rand0, rand1) :
                (rand1 is LexicalVariable) ? PrimitiveIsObjectEqSL.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveIsObjectEqSQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveIsObjectEq (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEq.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsObjectEqL : PrimitiveIsObjectEq
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PrimitiveIsObjectEqL (Primitive2 rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? PrimitiveIsObjectEqA.Make (rator, (Argument) rand0, rand1)
                : (rand0 is LexicalVariable1) ? PrimitiveIsObjectEqL1.Make (rator, (LexicalVariable1) rand0, rand1)
                : (rand1 is LexicalVariable) ? Unimplemented()
                : (rand1 is Quotation) ? PrimitiveIsObjectEqLQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsObjectEqL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqL.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsObjectEqA : PrimitiveIsObjectEqL
    {
        protected PrimitiveIsObjectEqA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveIsObjectEqA0.Make (rator, (Argument0) rand0, rand1)
                : (rand0 is Argument1) ? PrimitiveIsObjectEqA1.Make (rator, (Argument1) rand0, rand1)
                : (rand1 is LexicalVariable) ? Unimplemented()
                : (rand1 is Quotation) ? PrimitiveIsObjectEqAQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsObjectEqA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqA.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsObjectEqA0 : PrimitiveIsObjectEqA
    {
        protected PrimitiveIsObjectEqA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? PrimitiveIsObjectEqA0Q.Make (rator, rand0, (Quotation) rand1)
                : Unimplemented ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqA0.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsObjectEqA0Q : PrimitiveIsObjectEqA0
    {
        public readonly object rand1Value;

        protected PrimitiveIsObjectEqA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitiveIsObjectEqA0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqA0Q.EvalStep");
#endif
            answer = (environment.Argument0Value == this.rand1Value) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsObjectEqA1 : PrimitiveIsObjectEqA
    {
        protected PrimitiveIsObjectEqA1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? PrimitiveIsObjectEqA1Q.Make (rator, rand0, (Quotation) rand1)
                : Unimplemented ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqA0.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsObjectEqA1Q : PrimitiveIsObjectEqA1
    {
        public readonly object rand1Value;

        protected PrimitiveIsObjectEqA1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Quotation rand1)
        {
            return
                new PrimitiveIsObjectEqA1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqA1Q.EvalStep");
#endif
            answer = (environment.Argument1Value == this.rand1Value) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    sealed class PrimitiveIsObjectEqAQ : PrimitiveIsObjectEqA
    {
        public readonly object rand1Value;

        PrimitiveIsObjectEqAQ (Primitive2 rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Quotation rand1)
        {
            return
                new PrimitiveIsObjectEqAQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqAQ.EvalStep");
#endif
            answer = (environment.ArgumentValue(this.rand0Offset) == this.rand1Value) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }



    class PrimitiveIsObjectEqL1 : PrimitiveIsObjectEqL
    {

        protected PrimitiveIsObjectEqL1 (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? PrimitiveIsObjectEqL1Q.Make (rator, rand0, (Quotation) rand1)
                : Unimplemented ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqA.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsObjectEqL1Q : PrimitiveIsObjectEqL1
    {
        public readonly object rand1Value;

        protected PrimitiveIsObjectEqL1Q (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
        {
            return
                new PrimitiveIsObjectEqL1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqL1Q.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            answer = (ev0 == this.rand1Value) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    sealed class PrimitiveIsObjectEqLQ : PrimitiveIsObjectEqL
    {
        public readonly object rand1Value;

        PrimitiveIsObjectEqLQ (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
        {
            return
                 new PrimitiveIsObjectEqLQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqLQ.EvalStep");
#endif
            object ev0;
            Control expr = this.rand0;
            Environment env = environment;
            while (expr.EvalStep (out ev0, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsObjectLQ.EvalStep.1";
#endif
            answer = (ev0 == this.rand1Value) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectEqQ : PrimitiveIsObjectEq
    {
        public readonly object rand0Value;

        protected PrimitiveIsObjectEqQ (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveIsObjectEqQL.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? Unimplemented () :
                new PrimitiveIsObjectEqQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqQ.EvalStep");
#endif
            object ev1;
            Control expr = this.rand1;
            Environment env = environment;
            while (expr.EvalStep (out ev1, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsObjectEqQ.EvalStep.1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }
            answer = (this.rand0Value == ev1) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsObjectEqQL : PrimitiveIsObjectEqQ
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsObjectEqQL (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsObjectEqQA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is LexicalVariable1) ? PrimitiveIsObjectEqQL1.Make (rator, rand0, (LexicalVariable1) rand1) :
                new PrimitiveIsObjectEqQL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqQL.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            answer = (this.rand0Value == ev1) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsObjectEqQA : PrimitiveIsObjectEqQL
    {

        protected PrimitiveIsObjectEqQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {

        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsObjectEqQA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveIsObjectEqQA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveIsObjectEqQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqQA.EvalStep");
#endif
            answer = (environment.ArgumentValue (this.rand1Offset) == this.rand0Value) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    sealed class PrimitiveIsObjectEqQA0 : PrimitiveIsObjectEqQA
    {

        PrimitiveIsObjectEqQA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {

        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument0 rand1)
        {
            return
                 new PrimitiveIsObjectEqQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqQA0.EvalStep");
#endif
            answer = (environment.Argument0Value == this.rand0Value) ? Constant.sharpT : Constant.sharpF;
            return false;

        }
    }

    sealed class PrimitiveIsObjectEqQA1 : PrimitiveIsObjectEqQA
    {
        PrimitiveIsObjectEqQA1 (Primitive2 rator, Quotation rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument1 rand1)
        {
            return
                 new PrimitiveIsObjectEqQA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqQA1.EvalStep");
#endif
            answer = (environment.Argument1Value == this.rand0Value) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }



    sealed class PrimitiveIsObjectEqQL1 : PrimitiveIsObjectEqQL
    {
        PrimitiveIsObjectEqQL1 (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveIsObjectEqQL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqQL1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            answer = (ev1 == this.rand0Value) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsObjectEqSL : PrimitiveIsObjectEq
    {
        protected PrimitiveIsObjectEqSL (Primitive2 rator, SCode rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable rand1)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqSL.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class PrimitiveIsObjectEqSQ : PrimitiveIsObjectEq
    {
        public readonly object rand1Value;

        PrimitiveIsObjectEqSQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveIsObjectEqSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqSQ.EvalStep");
#endif
            object ev0;
            Control expr = this.rand0;
            Environment env = environment;
            while (expr.EvalStep (out ev0, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsObjectEqSQ.EvalStep.1";
#endif
            answer = (ev0 == this.rand1Value) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }
}

