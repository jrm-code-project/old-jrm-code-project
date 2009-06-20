using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PrimitiveGeneralCarCdr : PrimitiveCombination2
    {
        protected PrimitiveGeneralCarCdr (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is LexicalVariable) ? PrimitiveGeneralCarCdrL.Make (rator, (LexicalVariable) rand0, rand1) :
                (rand0 is Quotation) ? Unimplemented() : // PrimitiveGeneralCarCdrQ.Make (rator, (Quotation) rand0, rand1)
                (rand1 is LexicalVariable) ? Unimplemented() : // PrimitiveGeneralCarCdrSL.Make (rator, rand0, (LexicalVariable) rand1)
                (rand1 is Quotation) ?  PrimitiveGeneralCarCdrSQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveGeneralCarCdr (rator, rand0, rand1);
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

            // Compute answer
            if (Cons.GeneralCarCdr (out answer, ev0, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveGeneralCarCdrL : PrimitiveGeneralCarCdr
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PrimitiveGeneralCarCdrL (Primitive2 rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? PrimitiveGeneralCarCdrA.Make (rator, (Argument) rand0, rand1)
                : (rand0 is LexicalVariable1) ? PrimitiveGeneralCarCdrL1.Make (rator, (LexicalVariable1) rand0, rand1)
                : (rand1 is LexicalVariable) ? Unimplemented () //PrimitiveGeneralCarCdrLL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveGeneralCarCdrLQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveGeneralCarCdrL (rator, rand0, rand1);
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

            // Compute answer
            answer = (int) ev0 + (int) ev1;
            return false;
        }
    }

    [Serializable]
    class PrimitiveGeneralCarCdrA : PrimitiveGeneralCarCdrL
    {
        protected PrimitiveGeneralCarCdrA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveGeneralCarCdrA0.Make (rator, (Argument0) rand0, rand1)
                : (rand0 is Argument1) ? PrimitiveGeneralCarCdrA1.Make (rator, (Argument1) rand0, rand1)
                : (rand1 is LexicalVariable) ? Unimplemented () //PrimitiveGeneralCarCdrAL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveGeneralCarCdrAQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveGeneralCarCdrA (rator, rand0, rand1);
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

            // Compute answer
            answer = (int) ev0 + (int) ev1;
            return false;
        }
    }

    [Serializable]
    class PrimitiveGeneralCarCdrA0 : PrimitiveGeneralCarCdrA
    {
        protected PrimitiveGeneralCarCdrA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveGeneralCarCdrA0L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveGeneralCarCdrA0Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveGeneralCarCdrA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
            answer = null;
            return false;
        }
    }

    [Serializable]
    class PrimitiveGeneralCarCdrA0L : PrimitiveGeneralCarCdrA0
    {
        object rand1Name;
        int rand1Depth;
        int rand1Offset;

        protected PrimitiveGeneralCarCdrA0L (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable rand1) {
            return
                (rand1 is Argument) ? PrimitiveGeneralCarCdrA0A.Make (rator, rand0, (Argument) rand1) :
                (rand1 is LexicalVariable1) ? Unimplemented() :
                new PrimitiveGeneralCarCdrA0L (rator, rand0, rand1);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
            answer = null;
            return false;
        }
    }

    [Serializable]
    class PrimitiveGeneralCarCdrA0A : PrimitiveGeneralCarCdrA0L
    {
        protected PrimitiveGeneralCarCdrA0A (Primitive2 rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument rand1) {
            return
                (rand1 is Argument0) ? Unimplemented() :
                (rand1 is Argument1) ? PrimitiveGeneralCarCdrA0A1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveGeneralCarCdrA0A (rator, rand0, rand1);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
            answer = null;
            return false;
        }
    }

    [Serializable]
    class PrimitiveGeneralCarCdrA0A1 : PrimitiveGeneralCarCdrA0A
    {
        PrimitiveGeneralCarCdrA0A1 (Primitive2 rator, Argument0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1) {
            return
                new PrimitiveGeneralCarCdrA0A1 (rator, rand0, rand1);
        }
    }


    [Serializable]
    class PrimitiveGeneralCarCdrA0Q : PrimitiveGeneralCarCdrA0
    {
        public readonly object rand1Value;

        protected PrimitiveGeneralCarCdrA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitiveGeneralCarCdrA0Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGeneralCarCdrA0Q.EvalStep");
#endif
            if (Cons.GeneralCarCdr (out answer, environment.Argument0Value, this.rand1Value))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveGeneralCarCdrA1 : PrimitiveGeneralCarCdrA
    {
        protected PrimitiveGeneralCarCdrA1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? Unimplemented () //PrimitiveGeneralCarCdrA0L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveGeneralCarCdrA1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveGeneralCarCdrA1 (rator, rand0, rand1);
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
            object ev0 = environment.Argument1Value;

            // Compute answer
            throw new NotImplementedException ();
            answer = (int) ev0 + (int) ev1;
            return false;
        }
    }

    [Serializable]
    class PrimitiveGeneralCarCdrA1Q : PrimitiveGeneralCarCdrA1
    {
        public readonly object rand1Value;

        protected PrimitiveGeneralCarCdrA1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Quotation rand1)
        {
            return
                new PrimitiveGeneralCarCdrA1Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGeneralCarCdrA1Q.EvalStep");
#endif
            // Compute answer
            if (Cons.GeneralCarCdr (out answer, environment.Argument1Value, this.rand1Value))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveGeneralCarCdrAQ : PrimitiveGeneralCarCdrA
    {
        public readonly object rand1Value;

        protected PrimitiveGeneralCarCdrAQ (Primitive2 rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Quotation rand1)
        {
            return
                new PrimitiveGeneralCarCdrAQ (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGeneralCarCdrAQ.EvalStep");
#endif
            // Compute answer
            if (Cons.GeneralCarCdr (out answer, environment.ArgumentValue(this.rand0Offset), this.rand1Value))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveGeneralCarCdrL1 : PrimitiveGeneralCarCdrL
    {
        protected PrimitiveGeneralCarCdrL1 (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? Unimplemented () //PrimitiveGeneralCarCdrA0L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveGeneralCarCdrL1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveGeneralCarCdrL1 (rator, rand0, rand1);
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
            object ev0 = environment.Argument1Value;

            // Compute answer
            throw new NotImplementedException ();
            answer = (int) ev0 + (int) ev1;
            return false;
        }
    }

    [Serializable]
    class PrimitiveGeneralCarCdrL1Q : PrimitiveGeneralCarCdrL1
    {
        public readonly object rand1Value;

        protected PrimitiveGeneralCarCdrL1Q (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
        {
            return
                new PrimitiveGeneralCarCdrL1Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGeneralCarCdrL1Q.EvalStep");
#endif
            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // Compute answer
            if (Cons.GeneralCarCdr (out answer, ev0, this.rand1Value))
                throw new NotImplementedException ();
            return false;

        }
    }

    [Serializable]
    class PrimitiveGeneralCarCdrLQ : PrimitiveGeneralCarCdrL
    {
        public readonly object rand1Value;

        protected PrimitiveGeneralCarCdrLQ (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
        {
            return
                new PrimitiveGeneralCarCdrLQ (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveGeneralCarCdrLQ.EvalStep");
#endif
            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            // Compute answer
            if (Cons.GeneralCarCdr (out answer, ev0, this.rand1Value))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveGeneralCarCdrSQ : PrimitiveGeneralCarCdr
    {
        public readonly int rand1Value;

        protected PrimitiveGeneralCarCdrSQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        static SCode AddCar (SCode rand, int code)
        {
            return BuildGCC (rand, code * 2 + 1);
        }

        static SCode AddCdr (SCode rand, int code)
        {
            return BuildGCC (rand, code * 2 + 0);
        }

        static SCode BuildGCC (SCode rand, int code)
        {
            Debug.Write ("\n; => GeneralCarCdr " + code.ToString ());
            return PrimitiveCombination2.Make (Primitive.GeneralCarCdr, rand, Quotation.Make (code));
        }

        static SCode CombineGCC (SCode rand, int innerCode, int outerCode)
        {
            if (innerCode == 6 && outerCode == 6)
                return BuildGCC (rand, 26);
            else if (innerCode == 6 && outerCode == 12) // 110 1100 => 110010
                return BuildGCC (rand, 50);
            else if (innerCode == 25 && outerCode == 4) // 11001 100 => 1001001
                return BuildGCC (rand, 0x49);
            else if (innerCode == 25 && outerCode == 10) // 11001 1010 => 1010 1001
                return BuildGCC (rand, 0xA9);
            throw new NotImplementedException ();
        }

        static bool isFoldableCxr (SCode form, Primitive prim)
        {
            return Configuration.EnableFoldCarCdr &&
                ((form is PrimitiveCombination1 && ((PrimitiveCombination1) form).Operator == prim) ||
                 (form is PrimitiveCombination2 &&
                 ((PrimitiveCombination2) form).Rator == prim &&
                 ((PrimitiveCombination2) form).Rand1 is Quotation));
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                isFoldableCxr (rand0, Primitive.Car) ? AddCar (((PrimitiveCombination1)rand0).Operand, (int) rand1.Quoted) :
                isFoldableCxr (rand0, Primitive.Cdr) ? AddCdr (((PrimitiveCombination1) rand0).Operand, (int) rand1.Quoted) :
                isFoldableCxr (rand0, Primitive.GeneralCarCdr) ? CombineGCC (((PrimitiveCombination2) rand0).Rand0, (int) ((Quotation) ((PrimitiveCombination2) rand0).Rand1).Quoted, (int) rand1.Quoted) :
                new PrimitiveGeneralCarCdrSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "GeneralCarCdrSQ.EvalStep";
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

            // Compute answer
            if (Cons.GeneralCarCdr (out answer, ev0, this.rand1Value))
                throw new NotImplementedException ();
            return false;
        }
    }
}
