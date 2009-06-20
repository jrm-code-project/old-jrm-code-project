using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    /// <summary>
    /// True iff both args are chars and they are = to each other.
    /// False otherwise.  Does not throw an error.
    /// </summary>
    class PrimitiveIsCharEq : PrimitiveCombination2
    {
        protected PrimitiveIsCharEq (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is LexicalVariable) ? PrimitiveIsCharEqL.Make (rator, (LexicalVariable) rand0, rand1)
                : (rand0 is Quotation) ? PrimitiveIsCharEqQ.Make (rator, (Quotation) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveIsCharEqSL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveIsCharEqSQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsCharEq (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEq.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsCharEqL : PrimitiveIsCharEq
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PrimitiveIsCharEqL (Primitive2 rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? PrimitiveIsCharEqA.Make (rator, (Argument) rand0, rand1)
                : (rand0 is LexicalVariable1) ? PrimitiveIsCharEqL1.Make (rator, (LexicalVariable1) rand0, rand1) :
                (rand1 is LexicalVariable) ? Unimplemented () :
                (rand1 is Quotation) ? PrimitiveIsCharEqLQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveIsCharEqL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEqL.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsCharEqA : PrimitiveIsCharEqL
    {
        protected PrimitiveIsCharEqA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveIsCharEqA0.Make (rator, (Argument0) rand0, rand1)
                : (rand0 is Argument1) ? PrimitiveIsCharEqA1.Make (rator, (Argument1) rand0, rand1)
                : Unimplemented ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEqA.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsCharEqA0 : PrimitiveIsCharEqA
    {
        protected PrimitiveIsCharEqA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? PrimitiveIsCharEqA0Q.Make (rator, rand0, (Quotation) rand1)
                : Unimplemented ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEqA0.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsCharEqA0Q : PrimitiveIsCharEqA0
    {
        public readonly char rand1Value;

        protected PrimitiveIsCharEqA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (char) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitiveIsCharEqA0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEqA0Q.EvalStep");
#endif
            object ev0 = environment.Argument0Value;
            answer = (ev0 is char && ((char) ev0 == this.rand1Value)) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsCharEqA1 : PrimitiveIsCharEqA
    {
        protected PrimitiveIsCharEqA1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? PrimitiveIsCharEqA1Q.Make (rator, rand0, (Quotation) rand1)
                : Unimplemented ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEqA0.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsCharEqA1Q : PrimitiveIsCharEqA1
    {
        public readonly char rand1Value;

        protected PrimitiveIsCharEqA1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (char) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Quotation rand1)
        {
            return
                new PrimitiveIsCharEqA1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEqA0Q.EvalStep");
#endif
            object ev0 = environment.Argument0Value;
            answer = (ev0 is char && ((char) ev0 == this.rand1Value)) ? Constant.sharpT : Constant.sharpF;
            return false; 
        }
    }


    class PrimitiveIsCharEqL1 : PrimitiveIsCharEqL
    {

        protected PrimitiveIsCharEqL1 (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? PrimitiveIsCharEqL1Q.Make (rator, rand0, (Quotation) rand1)
                : Unimplemented ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEqA.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsCharEqL1Q : PrimitiveIsCharEqL1
    {
        public readonly char rand1Value;

        protected PrimitiveIsCharEqL1Q (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (char) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
        {
            return
                new PrimitiveIsCharEqL1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEqL1Q.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            answer = (ev0 is char && ((char) ev0 == this.rand1Value)) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsCharEqLQ : PrimitiveIsCharEqL
    {
        public readonly char rand1Value;

        protected PrimitiveIsCharEqLQ (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (char) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
        {
            return
                new PrimitiveIsCharEqLQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEqLQ.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            answer = (((char) ev0) == this.rand1Value) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsCharEqQ : PrimitiveIsCharEq
    {
        public readonly char rand0Value;

        protected PrimitiveIsCharEqQ (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = (char) rand0.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveIsCharEqQL.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? Unimplemented () : 
                new PrimitiveIsCharEqQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEqQ.EvalStep");
#endif
            object ev1;
            Control expr = this.rand1;
            Environment env = environment;
            while (expr.EvalStep (out ev1, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsCharEqQ.EvalStep.1";
#endif
            answer = (ev1 is char && ((char) ev1 == this.rand0Value)) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsCharEqQL : PrimitiveIsCharEqQ
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsCharEqQL (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsCharEqQA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveIsCharEqQL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveIsCharEqQL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEqQL.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            answer = (ev1 is char && this.rand0Value == (char) ev1) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsCharEqQA : PrimitiveIsCharEqQL
    {

        protected PrimitiveIsCharEqQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {

        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsCharEqQA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented ()
                : new PrimitiveIsCharEqQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEqQA.EvalStep");
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
            answer = (ev1 is char && ((char) ev1 == this.rand0Value)) ? Constant.sharpT : Constant.sharpF;
            return false;

        }
    }

    class PrimitiveIsCharEqQA0 : PrimitiveIsCharEqQA
    {

        protected PrimitiveIsCharEqQA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {

        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument0 rand1)
        {
            return
                 new PrimitiveIsCharEqQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEqQA0.EvalStep");
#endif
            object ev1 = environment.Argument0Value;
            answer = (ev1 is char && ((char) ev1 == this.rand0Value)) ? Constant.sharpT : Constant.sharpF;
            return false;

        }
    }



    class PrimitiveIsCharEqQL1 : PrimitiveIsCharEqQL
    {
        protected PrimitiveIsCharEqQL1 (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveIsCharEqQL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEqQL1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            answer = (this.rand0Value == (char) ev1) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsCharEqSL : PrimitiveIsCharEq
    {
        protected PrimitiveIsCharEqSL (Primitive2 rator, SCode rand0, LexicalVariable rand1)
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
            Warm ("PrimitiveIsCharEqSL.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveIsCharEqSQ : PrimitiveIsCharEq
    {
        public readonly char rand1Value;

        protected PrimitiveIsCharEqSQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (char) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveIsCharEqSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEqSQ.EvalStep");
#endif
            object ev0;
            Control expr = this.rand0;
            Environment env = environment;
            while (expr.EvalStep (out ev0, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsCharEqSQ.EvalStep.1";
#endif
            answer = (ev0 is char && ((char) ev0 == this.rand1Value)) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }
}

