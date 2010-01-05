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
                (rand0 is Argument) ? PrimitiveVector8BRefA.Make (rator, (Argument) rand0, rand1) :
                (rand0 is StaticVariable) ? PrimitiveVector8BRefS.Make (rator, (StaticVariable) rand0, rand1) :
                (rand1 is Argument) ? PrimitiveVector8BRefXA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? PrimitiveVector8BRefXS.Make (rator, rand0, (StaticVariable) rand1) :
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

    [Serializable]
    class PrimitiveVector8BRefA : PrimitiveVector8BRef
    {
        protected PrimitiveVector8BRefA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveVector8BRefA0.Make (rator, (Argument0) rand0, rand1) :
                (rand0 is Argument1) ? PrimitiveVector8BRefA1.Make (rator, (Argument1) rand0, rand1) :
                new PrimitiveVector8BRefA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVector8BRefA");
            NoteCalls (this.rand1);
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
                new PrimitiveVector8BRefA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVector8BRefA0.EvalStep");
            NoteCalls (this.rand1);
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
    class PrimitiveVector8BRefA0A : PrimitiveVector8BRefA0
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
            Warm ("-");
            NoteCalls (this.rand1);
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
            Warm ("-");
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
                 (rand1 is Quotation) ? PrimitiveVector8BRefA1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveVector8BRefA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
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
            Warm ("-");
#endif
            throw new NotImplementedException ();
        }
    }


    //class PrimitiveVector8BRefQ : PrimitiveVector8BRef { }

    [Serializable]
    class PrimitiveVector8BRefS : PrimitiveVector8BRef
    {
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected PrimitiveVector8BRefS (Primitive2 rator, StaticVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, StaticVariable rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveVector8BRefSA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? new PrimitiveVector8BRefSS (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveVector8BRefS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVector8BRefS");
            NoteCalls (this.rand1);
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveVector8BRefSA : PrimitiveVector8BRefS
    {
        protected PrimitiveVector8BRefSA (Primitive2 rator, StaticVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, StaticVariable rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? new PrimitiveVector8BRefSA0 (rator, rand0, (Argument0) rand1) :
                 (rand1 is Argument1) ? PrimitiveVector8BRefSA1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveVector8BRefSA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVector8BRefSA");
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class PrimitiveVector8BRefSA0 : PrimitiveVector8BRefSA
    {
        internal PrimitiveVector8BRefSA0 (Primitive2 rator, StaticVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVector8BRefSA0");
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // Vector-8b-ref
            answer = (int) ((char []) ev0) [(int) environment.Argument0Value];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVector8BRefSA1 : PrimitiveVector8BRefSA
    {
        protected PrimitiveVector8BRefSA1 (Primitive2 rator, StaticVariable rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, StaticVariable rand0, Argument1 rand1)
        {
            return
                new PrimitiveVector8BRefSA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVector8BRefSA1");
            NoteCalls (this.rand1);
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class PrimitiveVector8BRefSS : PrimitiveVector8BRefS
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal PrimitiveVector8BRefSS (Primitive2 rator, StaticVariable rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVector8BRefSS");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            // Vector-8b-ref
            answer = (int) ((char []) ev0) [(int) ev1];
            return false;
        }
    }

    [Serializable]
    class PrimitiveVector8BRefXA : PrimitiveVector8BRef
    {
        protected PrimitiveVector8BRefXA (Primitive2 rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument rand1)
        {
            return
                new PrimitiveVector8BRefXA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVector8BRefXA");
            NoteCalls (this.rand1);
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveVector8BRefXA0 : PrimitiveVector8BRefXA
    {
        protected PrimitiveVector8BRefXA0 (Primitive2 rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument0 rand1)
        {
            return
                new PrimitiveVector8BRefXA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVector8BRefXA0");
            NoteCalls (this.rand1);
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveVector8BRefXA1 : PrimitiveVector8BRefXA
    {
        protected PrimitiveVector8BRefXA1 (Primitive2 rator, SCode rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument1 rand1)
        {
            return
                new PrimitiveVector8BRefXA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVector8BRefXA1");
            NoteCalls (this.rand1);
#endif
            throw new NotImplementedException ();
        }
    }

        [Serializable]
    class PrimitiveVector8BRefXS : PrimitiveVector8BRef
    {
        protected PrimitiveVector8BRefXS (Primitive2 rator, SCode rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, StaticVariable rand1)
        {
            return
                new PrimitiveVector8BRefXS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVector8BRefXS");
            NoteCalls (this.rand1);
#endif
            throw new NotImplementedException ();
        }
    }

}
