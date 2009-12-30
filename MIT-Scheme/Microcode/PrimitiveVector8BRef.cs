using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PrimitiveVector8BRef : PrimitiveCombination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PrimitiveVector8BRef (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                new PrimitiveVector8BRef (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveVector8BRef";
#endif
            // Eval argument1
            object ev1;      
            
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveVector8BRef";
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
            SCode.location = "PrimitiveVector8BRef";
#endif

            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Vector-8b-ref
            answer = (int) ((char []) ev0) [(int) ev1];
            return false;
        }
    }

#if NIL
    [Serializable]
    class PrimitiveVector8BRefL : PrimitiveVector8BRef
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PrimitiveVector8BRefL (Primitive2 rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? PrimitiveVector8BRefA.Make (rator, (Argument) rand0, rand1)
                : (rand0 is LexicalVariable1) ? PrimitiveVector8BRefL1.Make (rator, (LexicalVariable1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveVector8BRefLL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? Unimplemented ()
                : new PrimitiveVector8BRefL (rator, rand0, rand1);
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
    class PrimitiveVector8BRefA : PrimitiveVector8BRefL
    {
        protected PrimitiveVector8BRefA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveVector8BRefA0.Make (rator, (Argument0) rand0, rand1)
                : (rand0 is Argument1) ? PrimitiveVector8BRefA1.Make (rator, (Argument1) rand0, rand1)
                : (rand1 is LexicalVariable) ? Unimplemented ()
                : (rand1 is Quotation) ? Unimplemented ()
                : new PrimitiveVector8BRefA (rator, rand0, rand1);
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
    class PrimitiveVector8BRefA0 : PrimitiveVector8BRefA
    {
        protected PrimitiveVector8BRefA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveVector8BRefA0L.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveVector8BRefA0Q.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveVector8BRefA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVector8BRefA0.EvalStep");
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

            // Vector-8b-ref
            answer = (int) ((char []) environment.Argument0Value) [(int) ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVector8BRefA0L : PrimitiveVector8BRefA0
    {
        protected PrimitiveVector8BRefA0L (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveVector8BRefA0A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveVector8BRefA0L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveVector8BRefA0L (rator, rand0, rand1);
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
    class PrimitiveVector8BRefA0A : PrimitiveVector8BRefA0L
    {
        protected PrimitiveVector8BRefA0A (Primitive2 rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Unimplemented()
                : (rand1 is Argument1) ? PrimitiveVector8BRefA0A1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveVector8BRefA0A (rator, rand0, rand1);
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
    sealed class PrimitiveVector8BRefA0A1 : PrimitiveVector8BRefA0A
    {
        PrimitiveVector8BRefA0A1 (Primitive2 rator, Argument0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            return
                new PrimitiveVector8BRefA0A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVector8BRefA0A1.EvalStep");
#endif
            // Vector-8b-ref
            answer = (int) ((char []) environment.Argument0Value) [(int) environment.Argument1Value];
            return false; 
        }
    }

    [Serializable]
    class PrimitiveVector8BRefA0L1 : PrimitiveVector8BRefA0L
    {
        protected PrimitiveVector8BRefA0L1 (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveVector8BRefA0L1 (rator, rand0, rand1);
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
    class PrimitiveVector8BRefA0Q : PrimitiveVector8BRefA0
    {
        public readonly int rand1Value;

        protected PrimitiveVector8BRefA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitiveVector8BRefA0Q (rator, rand0, (Quotation) rand1);
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
    class PrimitiveVector8BRefA1 : PrimitiveVector8BRefA
    {
        protected PrimitiveVector8BRefA1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? Unimplemented ()
                : (rand1 is Quotation) ? PrimitiveVector8BRefA1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveVector8BRefA1 (rator, rand0, rand1);
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
    class PrimitiveVector8BRefA1Q : PrimitiveVector8BRefA1
    {
        public readonly int rand1Value;

        protected PrimitiveVector8BRefA1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Quotation rand1)
        {
            return
                new PrimitiveVector8BRefA1Q (rator, rand0, (Quotation) rand1);
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
    class PrimitiveVector8BRefL1 : PrimitiveVector8BRefL
    {
        protected PrimitiveVector8BRefL1 (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveVector8BRefL1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? Unimplemented ()
                : new PrimitiveVector8BRefL1 (rator, rand0, rand1);
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
    class PrimitiveVector8BRefL1L : PrimitiveVector8BRefL1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveVector8BRefL1L (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveVector8BRefL1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveVector8BRefL1L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveVector8BRefL1L (rator, rand0, rand1);
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
    class PrimitiveVector8BRefL1A : PrimitiveVector8BRefL1L
    {
        protected PrimitiveVector8BRefL1A (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveVector8BRefL1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveVector8BRefL1A1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveVector8BRefL1A (rator, rand0, rand1);
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
    class PrimitiveVector8BRefL1A0 : PrimitiveVector8BRefL1A
    {
        protected PrimitiveVector8BRefL1A0 (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
        {
            return
                 new PrimitiveVector8BRefL1A0 (rator, rand0, rand1);
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
    class PrimitiveVector8BRefL1A1 : PrimitiveVector8BRefL1A
    {
        protected PrimitiveVector8BRefL1A1 (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
        {
            return
                 new PrimitiveVector8BRefL1A1 (rator, rand0, rand1);
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
    class PrimitiveVector8BRefL1L1 : PrimitiveVector8BRefL1L
    {
        protected PrimitiveVector8BRefL1L1 (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveVector8BRefL1L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVector8BRefL1L1.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // Vector-8b-ref
            answer = (int) ((char []) ev0) [(int) ev1];
            return false; 
        }
    }


    [Serializable]
    class PrimitiveVector8BRefLL : PrimitiveVector8BRefL
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveVector8BRefLL (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveVector8BRefLA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveVector8BRefLL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveVector8BRefLL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVector8BRefLL.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            // Vector-8b-ref
            answer = (int) ((char []) ev0) [(int) ev1];
            return false; 
        }
    }

    [Serializable]
    class PrimitiveVector8BRefLA : PrimitiveVector8BRefLL
    {
        protected PrimitiveVector8BRefLA (Primitive2 rator, LexicalVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveVector8BRefLA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveVector8BRefLA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveVector8BRefLA (rator, rand0, rand1);
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
    sealed class PrimitiveVector8BRefLA0 : PrimitiveVector8BRefLA
    {
        PrimitiveVector8BRefLA0 (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
        {
            return
                 new PrimitiveVector8BRefLA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVector8BRefLA0.EvalStep");
#endif
            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            // Vector-8b-ref
            answer = (int) ((char []) ev0) [(int) environment.Argument0Value];
            return false; 
        }
    }

    [Serializable]
    class PrimitiveVector8BRefLA1 : PrimitiveVector8BRefLA
    {
        protected PrimitiveVector8BRefLA1 (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
        {
            return
                 new PrimitiveVector8BRefLA1 (rator, rand0, rand1);
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
    class PrimitiveVector8BRefLL1 : PrimitiveVector8BRefLL
    {
        protected PrimitiveVector8BRefLL1 (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveVector8BRefLL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVector8BRefLL1.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            // Vector-8b-ref
            answer = (int) ((char []) ev0) [(int) ev1];
            return false; 
        }
    }


    //class PrimitiveVector8BRefQ : PrimitiveVector8BRef { }

    //class PrimitiveVector8BRefSL : PrimitiveVector8BRef { }

    [Serializable]
    class PrimitiveVector8BRefSQ : PrimitiveVector8BRef
    {
        public readonly int rand1Value;

        protected PrimitiveVector8BRefSQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveVector8BRefSQ (rator, rand0, rand1);
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
#endif
}
