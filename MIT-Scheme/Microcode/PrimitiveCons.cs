using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PrimitiveCons : PrimitiveCombination2
    {
        protected PrimitiveCons (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is LexicalVariable) ? PrimitiveConsL.Make (rator, (LexicalVariable) rand0, rand1) :
                (rand0 is PrimitiveCar) ? PrimitiveConsCar.Make (rator, (PrimitiveCar) rand0, rand1) :
                (rand0 is Quotation) ? PrimitiveConsQ.Make (rator, (Quotation) rand0, rand1) :
                (rand1 is LexicalVariable) ? PrimitiveConsSL.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveConsSQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveCons (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCons.EvalStep");
            noteCalls (this.rand0);
            noteCalls (this.rand1);
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveConsFrame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            // Eval argument0
            object ev0;

            unev = this.rand0;
            env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveConsFrame1 (this, environment, ev1));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = new Cons (ev0, ev1);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveConsFrame0 : SubproblemContinuation<PrimitiveCons>, ISystemVector
    {
        public PrimitiveConsFrame0 (PrimitiveCons expression, Environment environment)
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev1)
        {
            Control unev = this.expression.Rand0;
            Environment env = this.environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveConsFrame1 (this.expression, this.environment, ev1));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = new Cons (ev0, ev1);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveConsFrame1 : SubproblemContinuation<PrimitiveCons>, ISystemVector
    {
        readonly object ev1;

        internal PrimitiveConsFrame1 (PrimitiveCons expression, Environment environment, object ev1)
            : base (expression, environment)
        {
            this.ev1 = ev1;
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev0)
        {
            answer = new Cons (ev0, this.ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsL : PrimitiveCons
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PrimitiveConsL (Primitive2 rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? PrimitiveConsA.Make (rator, (Argument) rand0, rand1)
                : (rand0 is LexicalVariable1) ? PrimitiveConsL1.Make (rator, (LexicalVariable1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveConsLL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveConsLQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveConsL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsL.EvalStep");
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

            answer = new Cons (ev0, ev1);
            return false;
        }

    }

    [Serializable]
    class PrimitiveConsA : PrimitiveConsL
    {
        protected PrimitiveConsA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveConsA0.Make (rator, (Argument0) rand0, rand1)
                : (rand0 is Argument1) ? PrimitiveConsA1.Make (rator, (Argument1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveConsAL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveConsAQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveConsA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsA.EvalStep");
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
            answer = new Cons (environment.ArgumentValue (this.rand0Offset), ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsA0 : PrimitiveConsA
    {
        protected PrimitiveConsA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveConsA0L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveConsA0Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveConsA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsA0.EvalStep");
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

            answer = new Cons (environment.Argument0Value, ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsA0L : PrimitiveConsA0
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveConsA0L (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveConsA0A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveConsA0L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveConsA0L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsA0L.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            answer = new Cons (environment.Argument0Value, ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsA0A : PrimitiveConsA0L
    {
        protected PrimitiveConsA0A (Primitive2 rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveConsA0A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveConsA0A1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveConsA0A (rator, rand0, rand1);
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
    class PrimitiveConsA0A0 : PrimitiveConsA0A
    {
        protected PrimitiveConsA0A0 (Primitive2 rator, Argument0 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument0 rand1)
        {
            return
                new PrimitiveConsA0A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsA0A0.EvalStep");
#endif
            object it = environment.Argument0Value;
            answer = new Cons (it, it);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsA0A1 : PrimitiveConsA0A
    {
        protected PrimitiveConsA0A1 (Primitive2 rator, Argument0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            return
                new PrimitiveConsA0A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsA0A1.EvalStep");
#endif
            answer = new Cons (environment.Argument0Value, environment.Argument1Value);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsA0L1 : PrimitiveConsA0L
    {
        protected PrimitiveConsA0L1 (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveConsA0L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsA0L1.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            answer = new Cons (environment.Argument0Value, ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsA0Q : PrimitiveConsA0
    {
        public readonly object rand1Value;

        protected PrimitiveConsA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitiveConsA0Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsA0Q.EvalStep");
#endif
            answer = new Cons (environment.Argument0Value, this.rand1Value);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsA1 : PrimitiveConsA
    {
        protected PrimitiveConsA1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveConsA1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveConsA1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveConsA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsA1.EvalStep");
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

            answer = new Cons (environment.Argument1Value, ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsA1L : PrimitiveConsA1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveConsA1L (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveConsA1A.Make (rator, rand0, (Argument) rand1) :
                (rand1 is LexicalVariable1) ? PrimitiveConsA1L1.Make (rator, rand0, (LexicalVariable1) rand1) :
                new PrimitiveConsA1L (rator, rand0, rand1);
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

            // Greater-than-fixnum?
            answer = (int) environment.Argument1Value < (int) ev1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsA1A : PrimitiveConsA1L
    {
        protected PrimitiveConsA1A (Primitive2 rator, Argument1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveConsA1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented ()
                : new PrimitiveConsA1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsA1A.EvalStep");
#endif
            answer = new Cons (environment.Argument1Value, environment.ArgumentValue (this.rand1Offset));
            return false;
        }

    }

    [Serializable]
    class PrimitiveConsA1A0 : PrimitiveConsA1A
    {
        protected PrimitiveConsA1A0 (Primitive2 rator, Argument1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument0 rand1)
        {
            return
                 new PrimitiveConsA1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsA1A0.EvalStep");
#endif
            answer = new Cons (environment.Argument1Value, environment.Argument0Value);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsA1A1 : PrimitiveConsA1A
    {
        protected PrimitiveConsA1A1 (Primitive2 rator, Argument1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            throw new NotImplementedException ();
            //new PrimitiveConsA1A1 (rator, rand0, rand1);
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
    sealed class PrimitiveConsA1L1 : PrimitiveConsA1L
    {
        PrimitiveConsA1L1 (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        internal static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveConsA1L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsA1L1.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            answer = new Cons (environment.Argument1Value, ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsA1Q : PrimitiveConsA1
    {
        public readonly object rand1Value;

        protected PrimitiveConsA1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Quotation rand1)
        {
            return
                new PrimitiveConsA1Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsA1Q.EvalStep");
#endif
            answer = new Cons (environment.Argument1Value, this.rand1Value);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsAL : PrimitiveConsA
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveConsAL (Primitive2 rator, Argument rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveConsAA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitiveConsAL (rator, rand0, rand1);
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
    class PrimitiveConsAA : PrimitiveConsAL
    {
        protected PrimitiveConsAA (Primitive2 rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Unimplemented ()
                : (rand1 is Argument1) ? PrimitiveConsAA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveConsAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsAA.EvalStep");
#endif
            answer = new Cons (environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset));
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsAA0 : PrimitiveConsAA
    {
        protected PrimitiveConsAA0 (Primitive2 rator, Argument rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument0 rand1)
        {
            return new PrimitiveConsAA0 (rator, rand0, rand1);
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
    class PrimitiveConsAA1 : PrimitiveConsAA
    {
        protected PrimitiveConsAA1 (Primitive2 rator, Argument rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument1 rand1)
        {
            return new PrimitiveConsAA1 (rator, rand0, rand1);
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
    class PrimitiveConsAL1 : PrimitiveConsAL
    {
        protected PrimitiveConsAL1 (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
        {
            return new PrimitiveConsAL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveConsAQ : PrimitiveConsA
    {
        public readonly object rand1Value;

        protected PrimitiveConsAQ (Primitive2 rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Quotation rand1)
        {
            return
                new PrimitiveConsAQ (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsAQ.EvalStep");
#endif
            answer = new Cons (environment.ArgumentValue (this.rand0Offset), this.rand1Value);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsL1 : PrimitiveConsL
    {
        protected PrimitiveConsL1 (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveConsL1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveConsL1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveConsL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsL1.EvalStep");
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

            answer = new Cons (ev0, ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsL1L : PrimitiveConsL1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveConsL1L (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveConsL1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveConsL1L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveConsL1L (rator, rand0, rand1);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsL1L.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();


            answer = new Cons (ev0, ev1);
            return false;
        }

    }

    [Serializable]
    class PrimitiveConsL1A : PrimitiveConsL1L
    {
        protected PrimitiveConsL1A (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveConsL1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveConsL1A1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveConsL1A (rator, rand0, rand1);
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
    class PrimitiveConsL1A0 : PrimitiveConsL1A
    {
        protected PrimitiveConsL1A0 (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
        {
            return
                 new PrimitiveConsL1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsL1A0.EvalStep");
#endif
            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = new Cons (ev0, environment.Argument0Value);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsL1A1 : PrimitiveConsL1A
    {
        protected PrimitiveConsL1A1 (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
        {
            return
                 new PrimitiveConsL1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsL1A1.EvalStep");
#endif

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = new Cons (ev0, environment.Argument1Value);
            return false;
        }

    }

    [Serializable]
    class PrimitiveConsL1L1 : PrimitiveConsL1L
    {
        protected PrimitiveConsL1L1 (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveConsL1L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsL1L1.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            answer = new Cons (ev0, ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsL1Q : PrimitiveConsL1
    {
        public readonly object rand1Value;

        protected PrimitiveConsL1Q (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
        {
            return
                  new PrimitiveConsL1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsL1Q.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            answer = new Cons (ev0, this.rand1Value);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsLL : PrimitiveConsL
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveConsLL (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveConsLA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveConsLL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveConsLL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsLL.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            answer = new Cons (ev0, ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsLA : PrimitiveConsLL
    {
        protected PrimitiveConsLA (Primitive2 rator, LexicalVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveConsLA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveConsLA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveConsLA (rator, rand0, rand1);
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
    class PrimitiveConsLA0 : PrimitiveConsLA
    {
        protected PrimitiveConsLA0 (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
        {
            return
                 new PrimitiveConsLA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsLA0.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            answer = new Cons (ev0, environment.Argument0Value);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsLA1 : PrimitiveConsLA
    {
        protected PrimitiveConsLA1 (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
        {
            return
                 new PrimitiveConsLA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsLA1.EvalStep");
#endif
            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            answer = new Cons (ev0, environment.Argument1Value);
            return false;
        }

    }

    [Serializable]
    class PrimitiveConsLL1 : PrimitiveConsLL
    {
        protected PrimitiveConsLL1 (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveConsLL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsLL1.EvalStep");
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

            answer = new Cons (ev0, ev1);
            return false;
        }


    }

    [Serializable]
    class PrimitiveConsLQ : PrimitiveConsL
    {
        public readonly object rand1Value;

        protected PrimitiveConsLQ (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
        {
            return new PrimitiveConsLQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsLQ.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            answer = new Cons (ev0, this.rand1Value);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsQ : PrimitiveCons
    {
        public readonly object rand0Value;

        protected PrimitiveConsQ (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveConsQL.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveConsQQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveConsQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsQ.EvalStep");
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

            answer = new Cons (this.rand0Value, ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsQL : PrimitiveConsQ
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveConsQL (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveConsQA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveConsQL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveConsQL (rator, rand0, rand1);
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
    class PrimitiveConsQA : PrimitiveConsQL
    {
        protected PrimitiveConsQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveConsQA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveConsQA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveConsQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsQA.EvalStep");
#endif
            answer = new Cons (this.rand0Value, environment.ArgumentValue (this.rand1Offset));
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsQA0 : PrimitiveConsQA
    {
        protected PrimitiveConsQA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument0 rand1)
        {
            return
                 new PrimitiveConsQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsQA0.EvalStep");
#endif
            answer = new Cons (this.rand0Value, environment.Argument0Value);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveConsQA1 : PrimitiveConsQA
    {
        PrimitiveConsQA1 (Primitive2 rator, Quotation rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        internal static SCode Make (Primitive2 rator, Quotation rand0, Argument1 rand1)
        {
            return
                 new PrimitiveConsQA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsQA1.EvalStep");
#endif
            answer = new Cons (this.rand0Value, environment.Argument1Value);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsQL1 : PrimitiveConsQL
    {
        protected PrimitiveConsQL1 (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveConsQL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsQL1.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();


            answer = new Cons (this.rand0Value, ev1);
            return false;
        }

    }

    [Serializable]
    sealed class PrimitiveConsQQ : PrimitiveConsQ
    {
        public readonly object rand1Value;

        PrimitiveConsQQ (Primitive2 rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Quotation rand1)
        {
            return
                 new PrimitiveConsQQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsQQ");
#endif
            // need a new cons to avoid problems with EQ
            answer = new Cons (this.rand0Value, this.rand1Value);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsSL : PrimitiveCons
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveConsSL (Primitive2 rator, SCode rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveConsSA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveConsSL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveConsSL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PrimitiveConsSL.EvalStep";
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

            answer = new Cons (ev0, ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsSA : PrimitiveConsSL
    {
        protected PrimitiveConsSA (Primitive2 rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveConsSA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveConsSA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveConsSA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsSA.EvalStep");
            noteCalls (this.rand0);
#endif
            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveConsSAFrame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = new Cons (ev0, environment.ArgumentValue(this.rand1Offset));
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveConsSAFrame0 : SubproblemContinuation<PrimitiveConsSA>, ISystemVector
    {
        public PrimitiveConsSAFrame0 (PrimitiveConsSA expr, Environment env)
            : base (expr, env)
        {}

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev0)
        {
            answer = new Cons (ev0, this.environment.ArgumentValue (this.expression.rand1Offset));
            return false;
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
    }

    [Serializable]
    class PrimitiveConsSA0 : PrimitiveConsSA
    {
        protected PrimitiveConsSA0 (Primitive2 rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument0 rand1)
        {
            return

                new PrimitiveConsSA0 (rator, rand0, rand1);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PrimitiveConsSA0.EvalStep";
#endif

            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
                        SCode.location = "PrimitiveConsSA0.EvalStep.1";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            answer = new Cons (ev0, environment.Argument0Value);
            return false;
        }

    }

    [Serializable]
    class PrimitiveConsSA1 : PrimitiveConsSA
    {
        protected PrimitiveConsSA1 (Primitive2 rator, SCode rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument1 rand1)
        {
            return

                new PrimitiveConsSA1 (rator, rand0, rand1);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsSA1.EvalStep");
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

            answer = new Cons (ev0, environment.Argument1Value);
            return false;
        }

    }

    [Serializable]
    class PrimitiveConsSL1 : PrimitiveConsSL
    {
        protected PrimitiveConsSL1 (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
        {
            return

                new PrimitiveConsSL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsSL1.EvalStep");
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

            answer = new Cons (ev0, ev1);
            return false;
        }

    }

    [Serializable]
    sealed class PrimitiveConsSQ : PrimitiveCons
    {
        public readonly object rand1Value;

        PrimitiveConsSQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveConsSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PrimitiveConsSQ.EvalStep";
#endif
            // Eval argument0
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveConsSQFrame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = new Cons (ev0, this.rand1Value);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveConsSQFrame0 : SubproblemContinuation<PrimitiveConsSQ>, ISystemVector
    {
        public PrimitiveConsSQFrame0 (PrimitiveConsSQ expression, Environment env)
            : base (expression, env)
        {
        }

        public override bool Continue (out object answer, ref Control expression, ref Environment env, object ev0)
        {
            answer = new Cons (ev0, this.expression.rand1Value);
            return false;
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
    }

    [Serializable]
    class PrimitiveConsCar : PrimitiveCons
    {
#if DEBUG
        static Histogram<Type> carArgTypeHistogram = new Histogram<Type>();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        protected Type carArgType;
#endif
        public readonly SCode carArg;

        protected PrimitiveConsCar (Primitive2 rator, PrimitiveCar rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.carArg = rand0.Operand;
#if DEBUG
            this.carArgType = rand0.Operand.GetType();
#endif
        }

        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveCarL) ? PrimitiveConsCarL.Make (rator, (PrimitiveCarL) rand0, rand1) :
                new PrimitiveConsCar (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.carArg);
            noteCalls (this.rand1);
            carArgTypeHistogram.Note (this.carArgType);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCons.EvalStep";
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

            unev = this.carArg;
            env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            answer = new Cons (((Cons)ev0).Car, ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsCarL : PrimitiveConsCar
    {
#if DEBUG
        static Histogram<Type> carArgTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PrimitiveConsCarL (Primitive2 rator, PrimitiveCarL rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.OperandName;
            this.rand0Depth = rand0.OperandDepth;
            this.rand0Offset = rand0.OperandOffset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveCarA) ? PrimitiveConsCarA.Make (rator, (PrimitiveCarA) rand0, rand1) :
                (rand0 is PrimitiveCarL1) ? PrimitiveConsCarL1.Make (rator, (PrimitiveCarL1) rand0, rand1) :
                new PrimitiveConsCarL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.carArg);
            noteCalls (this.rand1);
            carArgTypeHistogram.Note (this.carArgType);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCons.EvalStep";
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

            answer = new Cons (((Cons) ev0).Car, ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsCarA : PrimitiveConsCarL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveConsCarA (Primitive2 rator, PrimitiveCarA rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveCarA0) ? PrimitiveConsCarA0.Make (rator, (PrimitiveCarA0) rand0, rand1) :
                (rand0 is PrimitiveCarA1) ? PrimitiveConsCarA1.Make (rator, (PrimitiveCarA1) rand0, rand1) :
                new PrimitiveConsCarA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveConsCarA.EvalStep";
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
            answer = new Cons (((Cons) ev0).Car, ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsCarA0 : PrimitiveConsCarA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveConsCarA0 (Primitive2 rator, PrimitiveCarA0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? PrimitiveConsCarA0Q.Make (rator, rand0, (Quotation) rand1) :
                 new PrimitiveConsCarA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveConsCarA0.EvalStep";
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

            answer = new Cons (((Cons) environment.Argument0Value).Car, ev1);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveConsCarA0Q : PrimitiveConsCarA0
    {
        public readonly object rand1Value;

        PrimitiveConsCarA0Q (Primitive2 rator, PrimitiveCarA0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, Quotation rand1)
        {
            return
                new PrimitiveConsCarA0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            SCode.location = "PrimitiveConsCarA0.EvalStep";
#endif
            answer = new Cons (((Cons) environment.Argument0Value).Car, this.rand1Value);
            return false;
        }
    }

    [Serializable]
    class PrimitiveConsCarA1 : PrimitiveConsCarA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PrimitiveConsCarA1 (Primitive2 rator, PrimitiveCarA1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA1 rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? PrimitiveConsCarA1Q.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveConsCarA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveConsCarA1.EvalStep";
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

            answer = new Cons (((Cons) environment.Argument1Value).Car, ev1);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveConsCarA1Q : PrimitiveConsCarA1
    {
        public readonly object rand1Value;
        PrimitiveConsCarA1Q (Primitive2 rator, PrimitiveCarA1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA1 rand0, Quotation rand1)
        {
            return
                new PrimitiveConsCarA1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConsCarA1Q.EvalStep");
#endif
            answer = new Cons (((Cons) environment.Argument1Value).Car, this.rand1Value);
            return false;
            // Eval argument1
        }
    }

    [Serializable]
    class PrimitiveConsCarL1 : PrimitiveConsCarL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveConsCarL1 (Primitive2 rator, PrimitiveCarL1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarL1 rand0, SCode rand1)
        {
            return
                 new PrimitiveConsCarL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveConsCarL1.EvalStep";
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

            answer = new Cons (((Cons) ev0).Car, ev1);
            return false;
        }
    }



}
