using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PrimitiveIsEq : PrimitiveCombination2
    {
        protected PrimitiveIsEq (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is LexicalVariable) ? PrimitiveIsEqL.Make (rator, (LexicalVariable) rand0, rand1) :
                (rand0 is PrimitiveCar) ? PrimitiveIsEqCar.Make (rator, (PrimitiveCar) rand0, rand1) :
                (rand0 is PrimitiveRecordRef) ? PrimitiveIsEqRecordRef.Make (rator, (PrimitiveRecordRef) rand0, rand1) :
                (rand0 is Quotation) ? PrimitiveIsEqQ.Make (rator, (Quotation) rand0, rand1) :
                (rand1 is LexicalVariable) ? PrimitiveIsEqSL.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveIsEqSQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveIsEq (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEq.EvalStep");
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

            if (ObjectModel.Eq (out answer, ( ev0), (ev1)))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqL : PrimitiveIsEq
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PrimitiveIsEqL (Primitive2 rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? PrimitiveIsEqA.Make (rator, (Argument) rand0, rand1)
                : (rand0 is LexicalVariable1) ? PrimitiveIsEqL1.Make (rator, (LexicalVariable1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveIsEqLL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveIsEqLQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsEqL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PrimitiveIsEqL.EvalStep";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqL.EvalStep.1";
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

            if (ObjectModel.Eq (out answer, ( ev0), (ev1)))
                throw new NotImplementedException();
            return false;
        }

    }

    class PrimitiveIsEqA : PrimitiveIsEqL
    {
        protected PrimitiveIsEqA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveIsEqA0.Make (rator, (Argument0) rand0, rand1)
                : (rand0 is Argument1) ? PrimitiveIsEqA1.Make (rator, (Argument1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveIsEqAL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveIsEqAQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsEqA (rator, rand0, rand1);
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
            if (ObjectModel.Eq (out answer, ( environment.ArgumentValue(this.rand0Offset)), (ev1)))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqA0 : PrimitiveIsEqA
    {
        protected PrimitiveIsEqA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is PrimitiveCar) ? PrimitiveIsEqA0Car.Make (rator, rand0, (PrimitiveCar) rand1) :
                (rand1 is LexicalVariable) ? PrimitiveIsEqA0L.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveIsEqA0Q.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveIsEqA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PrimitiveIsEqA0.EvalStep";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqA0.EvalStep.1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ObjectModel.Eq (out answer, environment.Argument0Value, ev1))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqA0Car : PrimitiveIsEqA0
    {
        protected PrimitiveIsEqA0Car (Primitive2 rator, Argument0 rand0, PrimitiveCar rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, PrimitiveCar rand1)
        {
            return
                (rand1 is PrimitiveCarL) ? PrimitiveIsEqA0CarL.Make (rator, rand0, (PrimitiveCarL) rand1) :
                new PrimitiveIsEqA0Car (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqA0CarL : PrimitiveIsEqA0Car
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsEqA0CarL (Primitive2 rator, Argument0 rand0, PrimitiveCarL rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.randName;
            this.rand1Depth = rand1.randDepth;
            this.rand1Offset = rand1.randOffset;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, PrimitiveCarL rand1)
        {
            return
                (rand1 is PrimitiveCarA) ? PrimitiveIsEqA0CarA.Make (rator, rand0, (PrimitiveCarA) rand1) :
                new PrimitiveIsEqA0CarL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqA0CarA : PrimitiveIsEqA0CarL
    {
        protected PrimitiveIsEqA0CarA (Primitive2 rator, Argument0 rand0, PrimitiveCarA rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, PrimitiveCarA rand1)
        {
            return
                new PrimitiveIsEqA0CarA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqA0L : PrimitiveIsEqA0
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsEqA0L (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsEqA0A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveIsEqA0L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveIsEqA0L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqA0L.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, environment.Argument0Value, ev1))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqA0A : PrimitiveIsEqA0L
    {
        protected PrimitiveIsEqA0A (Primitive2 rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsEqA0A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveIsEqA0A1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveIsEqA0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqA0A0 : PrimitiveIsEqA0A
    {
        protected PrimitiveIsEqA0A0 (Primitive2 rator, Argument0 rand0, Argument0 rand1)
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



    class PrimitiveIsEqA0A1 : PrimitiveIsEqA0A
    {
        protected PrimitiveIsEqA0A1 (Primitive2 rator, Argument0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            return
                new PrimitiveIsEqA0A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqA0A1.EvalStep");
#endif
            if (ObjectModel.Eq (out answer, ( environment.Argument0Value), (environment.Argument1Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqA0L1 : PrimitiveIsEqA0L
    {
        protected PrimitiveIsEqA0L1 (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveIsEqA0L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqA0L1.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, ( environment.Argument0Value), (ev1)))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqA0Q : PrimitiveIsEqA0
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value =  rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitiveIsEqA0Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqA0Q.EvalStep");
#endif
            if (ObjectModel.Eq (out answer, environment.Argument0Value, this.rand1Value))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqA1 : PrimitiveIsEqA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type>();
#endif
        protected PrimitiveIsEqA1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveIsEqA1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveIsEqA1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsEqA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveIsEqA1.EvalStep";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqA1.EvalStep.1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            ObjectModel.Eq (out answer, environment.Argument1Value, ev1);
            return false;
        }
    }

    class PrimitiveIsEqA1L : PrimitiveIsEqA1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsEqA1L (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsEqA1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveIsEqA1L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveIsEqA1L (rator, rand0, rand1);
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

            if (ObjectModel.Eq (out answer, ( environment.Argument1Value), (ev1)))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqA1A : PrimitiveIsEqA1L
    {
        protected PrimitiveIsEqA1A (Primitive2 rator, Argument1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsEqA1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented ()
                : new PrimitiveIsEqA1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif

            if (ObjectModel.Eq (out answer, ( environment.Argument1Value), (environment.ArgumentValue(this.rand1Offset))))
                throw new NotImplementedException();
            return false;
        }

    }



    class PrimitiveIsEqA1A0 : PrimitiveIsEqA1A
    {
        protected PrimitiveIsEqA1A0 (Primitive2 rator, Argument1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument0 rand1)
        {
            return
                 new PrimitiveIsEqA1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            if (ObjectModel.Eq (out answer, ( environment.Argument1Value), (environment.Argument0Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqA1A1 : PrimitiveIsEqA1A
    {
        protected PrimitiveIsEqA1A1 (Primitive2 rator, Argument1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            throw new NotImplementedException ();
            //new PrimitiveIsEqA1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqA1L1 : PrimitiveIsEqA1L
    {
        protected PrimitiveIsEqA1L1 (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveIsEqA1L1 (rator, rand0, rand1);
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
            object ev0 =  environment.Argument1Value;

            // Greater-than-fixnum?
            if (ObjectModel.Eq (out answer, ( ev0), (ev1)))
                throw new NotImplementedException();
            return false; throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqA1Q : PrimitiveIsEqA1
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqA1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value =  rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Quotation rand1)
        {
            return
                new PrimitiveIsEqA1Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            if (ObjectModel.Eq (out answer, ( environment.Argument1Value), (this.rand1Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqAL : PrimitiveIsEqA
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsEqAL (Primitive2 rator, Argument rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsEqAA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitiveIsEqAL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqAA : PrimitiveIsEqAL
    {
        protected PrimitiveIsEqAA (Primitive2 rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Unimplemented ()
                : (rand1 is Argument1) ? PrimitiveIsEqAA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveIsEqAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            if (ObjectModel.Eq (out answer, ( environment.ArgumentValue (this.rand0Offset)), (environment.ArgumentValue(this.rand1Offset))))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqAA0 : PrimitiveIsEqAA
    {
        protected PrimitiveIsEqAA0 (Primitive2 rator, Argument rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument0 rand1)
        {
            return new PrimitiveIsEqAA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqAA1 : PrimitiveIsEqAA
    {
        protected PrimitiveIsEqAA1 (Primitive2 rator, Argument rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument1 rand1)
        {
            return new PrimitiveIsEqAA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqAL1 : PrimitiveIsEqAL
    {
        protected PrimitiveIsEqAL1 (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
        {
            return new PrimitiveIsEqAL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqAQ : PrimitiveIsEqA
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqAQ (Primitive2 rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value =  rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Quotation rand1)
        {
            return
                new PrimitiveIsEqAQ (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqAQ.EvalStep");
#endif
            if (ObjectModel.Eq (out answer, ( environment.ArgumentValue(this.rand0Offset)), (this.rand1Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqL1 : PrimitiveIsEqL
    {
        protected PrimitiveIsEqL1 (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveIsEqL1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveIsEqL1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsEqL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PrimitiveIsEqL1.EvalStep";
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

            if (ObjectModel.Eq (out answer, ev0, ev1))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqL1L : PrimitiveIsEqL1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsEqL1L (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveIsEqL1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveIsEqL1L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveIsEqL1L (rator, rand0, rand1);
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


            if (ObjectModel.Eq (out answer, ( ev0), (ev1)))
                throw new NotImplementedException();
            return false;
        }

    }

    class PrimitiveIsEqL1A : PrimitiveIsEqL1L
    {
        protected PrimitiveIsEqL1A (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveIsEqL1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveIsEqL1A1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveIsEqL1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqL1A0 : PrimitiveIsEqL1A
    {
        protected PrimitiveIsEqL1A0 (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
        {
            return
                 new PrimitiveIsEqL1A0 (rator, rand0, rand1);
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

            if (ObjectModel.Eq (out answer, ( ev0), (environment.Argument0Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqL1A1 : PrimitiveIsEqL1A
    {
        protected PrimitiveIsEqL1A1 (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
        {
            return
                 new PrimitiveIsEqL1A1 (rator, rand0, rand1);
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

            if (ObjectModel.Eq (out answer, ( ev0), (environment.Argument1Value)))
                throw new NotImplementedException();
            return false;
        }

    }

    class PrimitiveIsEqL1L1 : PrimitiveIsEqL1L
    {
        protected PrimitiveIsEqL1L1 (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
        {
            if (rand0.Depth == rand1.Depth &&
                rand0.Offset == rand1.Offset)
                throw new NotImplementedException ();
            return
                 new PrimitiveIsEqL1L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqL1L1.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, ev0, ev1))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqL1Q : PrimitiveIsEqL1
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqL1Q (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value =  rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
        {
            return
                  new PrimitiveIsEqL1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqL1Q.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            if (ObjectModel.Eq (out answer, ev0, this.rand1Value))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqLL : PrimitiveIsEqL
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsEqLL (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveIsEqLA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveIsEqLL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveIsEqLL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqLL.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, ev0, ev1))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqLA : PrimitiveIsEqLL
    {
        protected PrimitiveIsEqLA (Primitive2 rator, LexicalVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveIsEqLA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveIsEqLA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveIsEqLA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            if (ObjectModel.Eq (out answer, ( ev0) , ( environment.ArgumentValue(this.rand1Offset))))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqLA0 : PrimitiveIsEqLA
    {
        protected PrimitiveIsEqLA0 (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
        {
            return
                 new PrimitiveIsEqLA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqLA0.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            if (ObjectModel.Eq (out answer, ( ev0), (environment.Argument0Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqLA1 : PrimitiveIsEqLA
    {
        protected PrimitiveIsEqLA1 (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
        {
            return
                 new PrimitiveIsEqLA1 (rator, rand0, rand1);
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

            if (ObjectModel.Eq (out answer, ( ev0), (environment.Argument1Value)))
                throw new NotImplementedException();
            return false;
        }

    }

    class PrimitiveIsEqLL1 : PrimitiveIsEqLL
    {
        protected PrimitiveIsEqLL1 (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveIsEqLL1 (rator, rand0, rand1);
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

            if (ObjectModel.Eq (out answer, ( ev0), (ev1)))
                throw new NotImplementedException();
            return false;
        }


    }

    class PrimitiveIsEqLQ : PrimitiveIsEqL
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqLQ (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value =  rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
        {
            return new PrimitiveIsEqLQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            if (ObjectModel.Eq (out answer, ( ev0), (this.rand1Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqCar : PrimitiveIsEq
    {
        public readonly SCode rand0Arg;

        protected PrimitiveIsEqCar (Primitive2 rator, PrimitiveCar rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Arg = rand0.Operand;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveCarL) ? PrimitiveIsEqCarL.Make (rator, (PrimitiveCarL) rand0, rand1) :
                //: (rand0 is PrimitiveCarQ) ? Unimplemented () // PrimitiveIsEqCarQ.Make (rator, (Quotation) rand0, rand1)
                (rand1 is LexicalVariable) ? PrimitiveIsEqCarSL.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveIsEqCarSQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveIsEqCar (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqCarL : PrimitiveIsEqCar
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PrimitiveIsEqCarL (Primitive2 rator, PrimitiveCarL rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.OperandName;
            this.rand0Depth = rand0.OperandDepth;
            this.rand0Offset = rand0.OperandOffset;
        }


        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveCarA) ? PrimitiveIsEqCarA.Make (rator, (PrimitiveCarA) rand0, rand1):
                (rand0 is PrimitiveCarL1) ? PrimitiveIsEqCarL1.Make (rator, (PrimitiveCarL1) rand0, rand1) :
                (rand1 is LexicalVariable) ? PrimitiveIsEqCarLL.Make (rator, rand0, (LexicalVariable) rand1):
                (rand1 is Quotation) ? PrimitiveIsEqCarLQ.Make (rator, rand0, (Quotation) rand1):
                new PrimitiveIsEqCarL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqCarA : PrimitiveIsEqCarL
    {
        protected PrimitiveIsEqCarA (Primitive2 rator, PrimitiveCarA rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveCarA0) ? PrimitiveIsEqCarA0.Make (rator, (PrimitiveCarA0) rand0, rand1) :
                (rand0 is PrimitiveCarA1) ? PrimitiveIsEqCarA1.Make (rator, (PrimitiveCarA1) rand0, rand1) :
                (rand1 is LexicalVariable) ? Unimplemented () :
                (rand1 is Quotation) ? Unimplemented () :
                new PrimitiveIsEqCarA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PrimitiveIsEqCarA.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqCarA.EvalStep";
#endif
            if (ObjectModel.Eq (out answer, ((Cons) environment.ArgumentValue (this.rand0Offset)).Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    class PrimitiveIsEqCarA0 : PrimitiveIsEqCarA
    {
        protected PrimitiveIsEqCarA0 (Primitive2 rator, PrimitiveCarA0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, SCode rand1)
        {
            return
                (rand1 is PrimitiveCar) ? PrimitiveIsEqCarA0Car.Make (rator, rand0, (PrimitiveCar) rand1) :
                (rand1 is LexicalVariable) ? PrimitiveIsEqCarA0L.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveIsEqCarA0Q.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveIsEqCarA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            noteCalls (this.rand1);
            SCode.location = "PrimitiveIsEqCarA0.EvalStep";
#endif
            object ev1;
            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }
#if DEBUG
                        SCode.location = "PrimitiveIsEqCarA0.EvalStep.1";
#endif
            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ev1);
            return false;
        }
    }

    class PrimitiveIsEqCarA0Car : PrimitiveIsEqCarA0
    {
        protected PrimitiveIsEqCarA0Car (Primitive2 rator, PrimitiveCarA0 rand0, PrimitiveCar rand1)
            : base (rator, rand0, rand1)
        {
        }


        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, PrimitiveCar rand1)
        {
            return
                (rand1 is PrimitiveCarL) ? PrimitiveIsEqCarA0CarL.Make (rator, rand0, (PrimitiveCarL) rand1) :
                new PrimitiveIsEqCarA0Car (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();

        }
    }

    class PrimitiveIsEqCarA0CarL : PrimitiveIsEqCarA0Car
    {
        protected PrimitiveIsEqCarA0CarL (Primitive2 rator, PrimitiveCarA0 rand0, PrimitiveCarL rand1)
            : base (rator, rand0, rand1)
        {
        }


        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, PrimitiveCarL rand1)
        {
            return
                (rand1 is PrimitiveCarA) ? PrimitiveIsEqCarA0CarA.Make (rator, rand0, (PrimitiveCarA) rand1) :
                new PrimitiveIsEqCarA0CarL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();

        }
    }

    class PrimitiveIsEqCarA0CarA : PrimitiveIsEqCarA0CarL
    {
        protected PrimitiveIsEqCarA0CarA (Primitive2 rator, PrimitiveCarA0 rand0, PrimitiveCarA rand1)
            : base (rator, rand0, rand1)
        {
        }


        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, PrimitiveCarA rand1)
        {
            return
                (rand1 is PrimitiveCarA1) ? PrimitiveIsEqCarA0CarA1.Make (rator, rand0, (PrimitiveCarA1) rand1) :
                new PrimitiveIsEqCarA0CarA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();

        }
    }

    class PrimitiveIsEqCarA0CarA1 : PrimitiveIsEqCarA0CarA
    {
        protected PrimitiveIsEqCarA0CarA1 (Primitive2 rator, PrimitiveCarA0 rand0, PrimitiveCarA1 rand1)
            : base (rator, rand0, rand1)
        {
        }


        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, PrimitiveCarA1 rand1)
        {
            return
                new PrimitiveIsEqCarA0CarA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("PrimitiveIsEqCarA0CarA1.EvalStep");
#endif
            answer = (((Cons) environment.Argument0Value).Car == ((Cons) environment.Argument1Value).Car) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsEqCarA0L : PrimitiveIsEqCarA0
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsEqCarA0L (Primitive2 rator, PrimitiveCarA0 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? Unimplemented () :
                (rand1 is LexicalVariable1) ? PrimitiveIsEqCarA0L1.Make (rator, rand0, (LexicalVariable1) rand1) :
                new PrimitiveIsEqCarA0L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqCarA0L.EvalStep");
#endif
            Object ev1;
            if (environment.FastLexicalRef(out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException();
            Cons temp = environment.Argument0Value as Cons;
            if (temp == null) throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, temp.Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    class PrimitiveIsEqCarA0L1 : PrimitiveIsEqCarA0L
    {


        protected PrimitiveIsEqCarA0L1 (Primitive2 rator, PrimitiveCarA0 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }


        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveIsEqCarA0L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqCarA0L1.EvalStep");
#endif
            Object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            Cons temp = environment.Argument0Value as Cons;
            if (temp == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, temp.Car, ev1);
            return false;
        }
    }

    class PrimitiveIsEqCarA0Q : PrimitiveIsEqCarA0
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqCarA0Q (Primitive2 rator, PrimitiveCarA0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }


        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, Quotation rand1)
        {
            return new PrimitiveIsEqCarA0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("PrimitiveIsEqCarA0Q.EvalStep");
#endif
            Cons temp = environment.Argument0Value as Cons;
            if (temp == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, temp.Car, this.rand1Value);
            return false;
        }
    }

    class PrimitiveIsEqCarA1 : PrimitiveIsEqCarA
    {
        protected PrimitiveIsEqCarA1 (Primitive2 rator, PrimitiveCarA1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }


        public static SCode Make (Primitive2 rator, PrimitiveCarA1 rand0, SCode rand1)
        {
            return
                 (rand1 is LexicalVariable) ? PrimitiveIsEqCarA1L.Make (rator, rand0, (LexicalVariable) rand1)
                 : (rand1 is Quotation) ? PrimitiveIsEqCarA1Q.Make (rator, rand0, (Quotation) rand1)
                 : new PrimitiveIsEqCarA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqCarA1L : PrimitiveIsEqCarA1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsEqCarA1L (Primitive2 rator, PrimitiveCarA1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }


        public static SCode Make (Primitive2 rator, PrimitiveCarA1 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? Unimplemented ()
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitiveIsEqCarA1L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            Object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            Cons temp = environment.Argument1Value as Cons;
            if (temp == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, temp.Car, ev1);
            return false;
        }
    }

    class PrimitiveIsEqCarA1Q : PrimitiveIsEqCarA1
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqCarA1Q (Primitive2 rator, PrimitiveCarA1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }


        public static SCode Make (Primitive2 rator, PrimitiveCarA1 rand0, Quotation rand1)
        {
            return new PrimitiveIsEqCarA1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            Cons temp = environment.Argument1Value as Cons;
            if (temp == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, temp.Car, this.rand1Value);
            return false;
        }
    }

    class PrimitiveIsEqCarL1 : PrimitiveIsEqCarL
    {

        protected PrimitiveIsEqCarL1 (Primitive2 rator, PrimitiveCarL1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }


        public static SCode Make (Primitive2 rator, PrimitiveCarL1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveIsEqCarL1L.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? Unimplemented() :
                new PrimitiveIsEqCarL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqCarL1L : PrimitiveIsEqCarL1
    {

        protected PrimitiveIsEqCarL1L (Primitive2 rator, PrimitiveCarL1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
        }


        public static SCode Make (Primitive2 rator, PrimitiveCarL1 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? Unimplemented() :
                (rand1 is LexicalVariable1) ? Unimplemented () :
                new PrimitiveIsEqCarL1L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }



    class PrimitiveIsEqCarLL : PrimitiveIsEqCarL
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsEqCarLL (Primitive2 rator, PrimitiveCarL rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? Unimplemented () :
                (rand1 is LexicalVariable1) ? Unimplemented () ://PrimitiveIsEqCarLL.Make (rator, rand0, (LexicalVariable) rand1)
                new PrimitiveIsEqCarLL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("PrimitiveIsEqCarLL.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, ((Cons)ev0).Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    class PrimitiveIsEqCarLQ : PrimitiveIsEqCarL
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqCarLQ (Primitive2 rator, PrimitiveCarL rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;

        }


        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, Quotation rand1)
        {
            return new PrimitiveIsEqCarLQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqCarSL : PrimitiveIsEqCar
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsEqCarSL (Primitive2 rator, PrimitiveCar rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, LexicalVariable rand1)
        {
            return 
                (rand1 is Argument) ? Unimplemented()
                : (rand1 is LexicalVariable1) ? Unimplemented()
                : new PrimitiveIsEqCarSL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            noteCalls (this.rand0);
            SCode.location = "PrimitiveIsEqCarSL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                    throw new NotImplementedException();

            object ev0;
            Control unev = this.rand0Arg;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
                        SCode.location = "PrimitiveIsEqCarSL.EvalStep.1";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException();
            }

            Cons ev0c = (Cons) ev0;
            if (ObjectModel.Eq (out answer, ev0c.Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    class PrimitiveIsEqCarSQ : PrimitiveIsEqCar
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqCarSQ (Primitive2 rator, PrimitiveCar rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, Quotation rand1)
        {
            return new PrimitiveIsEqCarSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqQ : PrimitiveIsEq
    {
        public readonly object rand0Value;

        protected PrimitiveIsEqQ (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveIsEqQL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveIsEqQQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsEqQ (rator, rand0, rand1);
        }


        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqQ.EvalStep");

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

            if (ObjectModel.Eq (out answer, this.rand0Value, (ev1)))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqQL : PrimitiveIsEqQ
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsEqQL (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsEqQA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveIsEqQL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveIsEqQL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqQA : PrimitiveIsEqQL
    {
        protected PrimitiveIsEqQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsEqQA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveIsEqQA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveIsEqQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqQA.EvalStep");
#endif
            if (ObjectModel.Eq (out answer, this.rand0Value , environment.ArgumentValue(this.rand1Offset)))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqQA0 : PrimitiveIsEqQA
    {
        protected PrimitiveIsEqQA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument0 rand1)
        {
            return
                 new PrimitiveIsEqQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqQA0.EvalStep");
#endif
            if (ObjectModel.Eq (out answer, this.rand0Value , (environment.Argument0Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqQA1 : PrimitiveIsEqQA
    {
        protected PrimitiveIsEqQA1 (Primitive2 rator, Quotation rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument1 rand1)
        {
            return
                 new PrimitiveIsEqQA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            if (ObjectModel.Eq (out answer, this.rand0Value , (environment.Argument1Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqQL1 : PrimitiveIsEqQL
    {
        protected PrimitiveIsEqQL1 (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveIsEqQL1 (rator, rand0, rand1);
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


            if (ObjectModel.Eq (out answer, this.rand0Value, (ev1)))
                throw new NotImplementedException();
            return false;
        }

    }

    class PrimitiveIsEqQQ : PrimitiveIsEqQ
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqQQ (Primitive2 rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value =  rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Quotation rand1)
        {
            return
                 new PrimitiveIsEqQQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // refetch because vector may be mutable
            if (ObjectModel.Eq (out answer, this.rand0Value , (this.rand1Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqSL : PrimitiveIsEq
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsEqSL (Primitive2 rator, SCode rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsEqSA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveIsEqSL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveIsEqSL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PrimitiveIsEqSL.EvalStep";
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
#if DEBUG
                        SCode.location = "PrimitiveIsEqSL.EvalStep.1";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ObjectModel.Eq (out answer, ev0, ev1))
                throw new NotImplementedException();
            return false;
        }
    }

    class PrimitiveIsEqSA : PrimitiveIsEqSL
    {
        protected PrimitiveIsEqSA (Primitive2 rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsEqSA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveIsEqSA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveIsEqSA (rator, rand0, rand1);
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

            if (ObjectModel.Eq (out answer, ( ev0), (environment.ArgumentValue(this.rand1Offset))))
                throw new NotImplementedException();
            return false;
        }

    }

    class PrimitiveIsEqSA0 : PrimitiveIsEqSA
    {
        protected PrimitiveIsEqSA0 (Primitive2 rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument0 rand1)
        {
            return

                new PrimitiveIsEqSA0 (rator, rand0, rand1);
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

            if (ObjectModel.Eq (out answer, ( ev0), (environment.Argument0Value)))
                throw new NotImplementedException();
            return false;
        }

    }

    class PrimitiveIsEqSA1 : PrimitiveIsEqSA
    {
        protected PrimitiveIsEqSA1 (Primitive2 rator, SCode rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument1 rand1)
        {
            return

                new PrimitiveIsEqSA1 (rator, rand0, rand1);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PrimitiveIsEqSA1.EvalStep";
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

            if (ObjectModel.Eq (out answer, ( ev0), (environment.Argument1Value)))
                throw new NotImplementedException();
            return false;
        }

    }

    class PrimitiveIsEqSL1 : PrimitiveIsEqSL
    {
        protected PrimitiveIsEqSL1 (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
        {
            return

                new PrimitiveIsEqSL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PrimitiveIsEqSL1.EvalStep";
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
#if DEBUG
            SCode.location = "PrimitiveIsEqSL1.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ObjectModel.Eq (out answer, ev0, ev1))
                throw new NotImplementedException();
            return false;
        }

    }

    class PrimitiveIsEqSQ : PrimitiveIsEq
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqSQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value =  rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveIsEqSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqSQ.EvalStep");
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

            if (ObjectModel.Eq (out answer, ev0, this.rand1Value))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqRecordRef : PrimitiveIsEq
    {
        public readonly SCode rand0rand0;
        public readonly SCode rand0rand1;

        protected PrimitiveIsEqRecordRef (Primitive2 rator, PrimitiveRecordRef rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0rand0 = rand0.Rand0;
            this.rand0rand1 = rand0.Rand1;
        }

        static public SCode Make (Primitive2 rator, PrimitiveRecordRef rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveRecordRefL) ? PrimitiveIsEqRecordRefL.Make (rator, (PrimitiveRecordRefL) rand0, rand1)
                : new PrimitiveIsEqRecordRef (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqRecordRef.EvalStep");
#endif
            object ev1;
            Control expr = this.rand1;
            Environment env = environment;
            while (expr.EvalStep (out ev1, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqRecordRefL1.EvalStep";
#endif
            // Eval argument1
            object ev0b;

            expr = this.rand0rand1;
            env = environment;
            while (expr.EvalStep (out ev0b, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqRecordRef.EvalStep";
#endif
            if (ev0b == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            object ev0a;
            expr = this.rand0rand0;
            env = environment;
            while (expr.EvalStep (out ev0a, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqRecordRef.EvalStep";
#endif
            if (ev0a == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object ev0 = ((Record) ev0a).Ref ((int) ev0b);
            if (ObjectModel.Eq (out answer, ev0, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    class PrimitiveIsEqRecordRefL : PrimitiveIsEqRecordRef
    {
        public readonly object rand0rand0Name;
        public readonly int rand0rand0Depth;
        public readonly int rand0rand0Offset;

        protected PrimitiveIsEqRecordRefL (Primitive2 rator, PrimitiveRecordRefL rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0rand0Name = rand0.rand0Name;
            this.rand0rand0Depth = rand0.rand0Depth;
            this.rand0rand0Offset = rand0.rand0Offset;
        }

        static public SCode Make (Primitive2 rator, PrimitiveRecordRefL rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveRecordRefA) ? PrimitiveIsEqRecordRefA.Make (rator, (PrimitiveRecordRefA) rand0, rand1)
                : (rand0 is PrimitiveRecordRefL1) ? PrimitiveIsEqRecordRefL1.Make (rator, (PrimitiveRecordRefL1) rand0, rand1)
                : new PrimitiveIsEqRecordRefL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqRecordRefL.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqRecordRefA : PrimitiveIsEqRecordRefL
    {
        protected PrimitiveIsEqRecordRefA (Primitive2 rator, PrimitiveRecordRefA rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        static public SCode Make (Primitive2 rator, PrimitiveRecordRefA rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveRecordRefA0) ? PrimitiveIsEqRecordRefA0.Make (rator, (PrimitiveRecordRefA0) rand0, rand1)
                : new PrimitiveIsEqRecordRefA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqRecordRefL1 : PrimitiveIsEqRecordRefL
    {
        protected PrimitiveIsEqRecordRefL1 (Primitive2 rator, PrimitiveRecordRefL1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        static public SCode Make (Primitive2 rator, PrimitiveRecordRefL1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? Unimplemented()
                : (rand1 is Quotation) ? Unimplemented()
                : new PrimitiveIsEqRecordRefL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqRecordRefL1.EvalStep");
#endif
            object ev1;
            Control expr = this.rand1;
            Environment env = environment;
            while (expr.EvalStep (out ev1, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqRecordRefL1.EvalStep.1";
#endif
            // Eval argument1
            object ev0b;

            expr = this.rand0rand1;
            env = environment;
            while (expr.EvalStep (out ev0b, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqRecordRefL1.EvalStep.2";
#endif
            if (ev0b == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            object ev0a;
            if (environment.FastLexicalRef1 (out ev0a, this.rand0rand0Name, this.rand0rand0Offset))
                throw new NotImplementedException ();

            object ev0 = ((Record) ev0a).Ref ((int) ev0b);
            if (ObjectModel.Eq (out answer, ev0, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }



    class PrimitiveIsEqRecordRefA0 : PrimitiveIsEqRecordRefA
    {
        protected PrimitiveIsEqRecordRefA0 (Primitive2 rator, PrimitiveRecordRefA0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        static public SCode Make (Primitive2 rator, PrimitiveRecordRefA0 rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveRecordRefA0Q) ? PrimitiveIsEqRecordRefA0Q.Make (rator, (PrimitiveRecordRefA0Q) rand0, rand1)
                : new PrimitiveIsEqRecordRefA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PrimitiveIsEqRecordRefA0Q : PrimitiveIsEqRecordRefA0
    {
        public readonly int rand0Index;

        protected PrimitiveIsEqRecordRefA0Q (Primitive2 rator, PrimitiveRecordRefA0Q rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Index = rand0.rand1Value;
        }

        static public SCode Make (Primitive2 rator, PrimitiveRecordRefA0Q rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? PrimitiveIsEqRecordRefA0QQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsEqRecordRefA0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqRecordRefA0Q.EvalStep");
#endif
            object ev1;
            Control expr = this.rand1;
            Environment env = environment;
            while (expr.EvalStep (out ev1, ref expr, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = ((Record) environment.Argument0Value).Ref (this.rand0Index);
            answer =
    ((ev0 == null) && (ev1 == null))
    || ((ev1 != null) &&
        ((ev0 == ev1)
         || ((ev0 is Int32 && ev1 is Int32) && ((int) ev0 == (int) ev1))
         || ((ev0 is char && ev1 is char) && ((char) ev0 == (char) ev1))
         || ((ev0 is bool && ev1 is bool) && ((bool) ev0 == (bool) ev1))))
         ? Constant.sharpT
         : Constant.sharpF;
            return false;
        }
    }

    class PrimitiveIsEqRecordRefA0QQ : PrimitiveIsEqRecordRefA0Q
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqRecordRefA0QQ (Primitive2 rator, PrimitiveRecordRefA0Q rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        static public SCode Make (Primitive2 rator, PrimitiveRecordRefA0Q rand0, Quotation rand1)
        {
            return
                new PrimitiveIsEqRecordRefA0QQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqRecordRefA0QQ.EvalStep");
#endif
            object ev1 = this.rand1Value;

            object ev0 = ((Record) environment.Argument0Value).Ref (this.rand0Index);
            answer =
    ((ev0 == null) && (ev1 == null))
    || ((ev1 != null) &&
        ((ev0 == ev1)
         || ((ev0 is Int32 && ev1 is Int32) && ((int) ev0 == (int) ev1))
         || ((ev0 is char && ev1 is char) && ((char) ev0 == (char) ev1))
         || ((ev0 is bool && ev1 is bool) && ((bool) ev0 == (bool) ev1))))
         ? Constant.sharpT
         : Constant.sharpF;
            return false;
        }
    }

}
