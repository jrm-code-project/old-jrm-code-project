using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    /// <summary>
    /// True iff both args are integers and they are = to each other.
    /// False otherwise.  Does not throw an error.
    /// </summary>
    class PrimitiveIsIntEq : PrimitiveCombination2
    {
        protected PrimitiveIsIntEq (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is LexicalVariable) ? PrimitiveIsIntEqL.Make (rator, (LexicalVariable) rand0, rand1)
                : (rand0 is Quotation) ? PrimitiveIsIntEqQ.Make (rator, (Quotation) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveIsIntEqSL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveIsIntEqSQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsIntEq (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntEq.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsIntEqL : PrimitiveIsIntEq
    {
        protected PrimitiveIsIntEqL (Primitive2 rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? PrimitiveIsIntEqA.Make (rator, (Argument) rand0, rand1)
                : (rand0 is LexicalVariable1) ? PrimitiveIsIntEqL1.Make (rator, (LexicalVariable1) rand0, rand1)
                : Unimplemented ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntEqL.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsIntEqA : PrimitiveIsIntEqL
    {
        protected PrimitiveIsIntEqA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveIsIntEqA0.Make (rator, (Argument0) rand0, rand1)
                : (rand0 is Argument1) ? PrimitiveIsIntEqA1.Make (rator, (Argument1) rand0, rand1)
                : Unimplemented ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntEqA.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsIntEqA0 : PrimitiveIsIntEqA
    {
        protected PrimitiveIsIntEqA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? PrimitiveIsIntEqA0Q.Make (rator, rand0, (Quotation) rand1)
                : Unimplemented ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntEqA0.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsIntEqA0Q : PrimitiveIsIntEqA0
    {
        public readonly int rand1Value;

        protected PrimitiveIsIntEqA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitiveIsIntEqA0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntEqA0Q.EvalStep");
#endif
            object ev0 = environment.Argument0Value;
            answer = (ev0 is int && ((int) ev0 == this.rand1Value)) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsIntEqA1 : PrimitiveIsIntEqA
    {
        protected PrimitiveIsIntEqA1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? PrimitiveIsIntEqA1Q.Make (rator, rand0, (Quotation) rand1)
                : Unimplemented ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntEqA0.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsIntEqA1Q : PrimitiveIsIntEqA1
    {
        protected PrimitiveIsIntEqA1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Quotation rand1)
        {
            return
                new PrimitiveIsIntEqA1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntEqA0Q.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }


    class PrimitiveIsIntEqL1 : PrimitiveIsIntEqL
    {
        protected PrimitiveIsIntEqL1 (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? PrimitiveIsIntEqL1Q.Make (rator, rand0, (Quotation) rand1)
                : Unimplemented ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntEqA.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsIntEqL1Q : PrimitiveIsIntEqL1
    {
        protected PrimitiveIsIntEqL1Q (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
        {
            return
                new PrimitiveIsIntEqL1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntEqA.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }


 
    class PrimitiveIsIntEqQ : PrimitiveIsIntEq
    {
        public readonly int rand0Value;

        protected PrimitiveIsIntEqQ (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = (int) rand0.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveIsIntEqQL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? Unimplemented()
                : new PrimitiveIsIntEqQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntEqQ.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsIntEqQL : PrimitiveIsIntEqQ
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsIntEqQL (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsIntEqQA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented()
                : new PrimitiveIsIntEqQL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntEqQ.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsIntEqQA : PrimitiveIsIntEqQL
    {

        protected PrimitiveIsIntEqQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {

        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsIntEqQA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented ()
                : new PrimitiveIsIntEqQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntEqQA.EvalStep");
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
            answer = (ev1 is int && ((int) ev1 == this.rand0Value)) ? Constant.sharpT : Constant.sharpF;
            return false;

        }
    }

        class PrimitiveIsIntEqQA0 : PrimitiveIsIntEqQA
    {
        protected PrimitiveIsIntEqQA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument0 rand1)
        {
            return
                 new PrimitiveIsIntEqQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntEqQ.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }



    class PrimitiveIsIntEqSL : PrimitiveIsIntEq
    {
        protected PrimitiveIsIntEqSL (Primitive2 rator, SCode rand0, LexicalVariable rand1)
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
            Warm ("PrimitiveIsIntEqSL.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    sealed class PrimitiveIsIntEqSQ : PrimitiveIsIntEq
    {
        public readonly int rand1Value;

        PrimitiveIsIntEqSQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveIsIntEqSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntEqSQ.EvalStep");
#endif
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsIntEqSQ.EvalStep";
#endif
            answer = ((int) ev0 == this.rand1Value) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }



}
