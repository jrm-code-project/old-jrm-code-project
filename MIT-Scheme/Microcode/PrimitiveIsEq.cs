using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PrimitiveIsEq : PrimitiveCombination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PrimitiveIsEq (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveCar) ? PrimitiveIsEqCar.Make (rator, (PrimitiveCar) rand0, rand1) :
                (rand0 is PrimitiveCaar) ? PrimitiveIsEqCaar.Make (rator, (PrimitiveCaar) rand0, rand1) :
                //(rand0 is PrimitiveRecordRef) ? PrimitiveIsEqRecordRef.Make (rator, (PrimitiveRecordRef) rand0, rand1) :
                (rand0 is Argument) ? PrimitiveIsEqA.Make (rator, (Argument) rand0, rand1) :
                (rand0 is StaticVariable) ? PrimitiveIsEqS.Make (rator, (StaticVariable) rand0, rand1) :
                (rand0 is Quotation) ? PrimitiveIsEqQ.Make (rator, (Quotation) rand0, rand1) :
                (rand1 is Argument) ? PrimitiveIsEqXA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? PrimitiveIsEqXQ.Make (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? PrimitiveIsEqXS.Make (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveIsEq (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);

            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveIsEq";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEq";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            // Eval argument0
            object ev0;

            unev = this.rand0;
            env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEq";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ObjectModel.Eq (out answer, ev0, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqCar : PrimitiveIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        public readonly Type rand0ArgType;
#endif
        public readonly SCode rand0Arg;

        protected PrimitiveIsEqCar (Primitive2 rator, PrimitiveCar rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Arg = rand0.Operand;
#if DEBUG
            this.rand0ArgType = rand0.Operand.GetType ();
#endif
        }

        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, SCode rand1)
        {

            return
                (rand0 is PrimitiveCarA) ? PrimitiveIsEqCarA.Make (rator, (PrimitiveCarA) rand0, rand1) :
                (rand0 is PrimitiveCarS) ? PrimitiveIsEqCarS.Make (rator, (PrimitiveCarS) rand0, rand1) :
                (rand1 is StaticVariable) ? PrimitiveIsEqCarXS.Make (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveIsEqCar (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0Arg);
            NoteCalls (this.rand1);

            rand0TypeHistogram.Note (this.rand0ArgType);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveIsEqCar";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqCar";
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

            unev = this.rand0Arg;
            env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqCar";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            Cons ev0Pair = ev0 as Cons;
            if (ev0Pair == null) throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, ev0Pair.Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqCarA : PrimitiveIsEqCar
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0ArgOffset;

        protected PrimitiveIsEqCarA (Primitive2 rator, PrimitiveCarA rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0ArgOffset = rand0.offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveCarA0) ? PrimitiveIsEqCarA0.Make (rator, (PrimitiveCarA0) rand0, rand1) :
                (rand1 is Quotation) ? PrimitiveIsEqCarAQ.Make (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new PrimitiveIsEqCarAS (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveIsEqCarA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);

            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveIsEqCarA";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqCarA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            Cons ev0Pair = environment.ArgumentValue (this.rand0ArgOffset) as Cons;
            if (ev0Pair == null) throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, ev0Pair.Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqCarA0 : PrimitiveIsEqCarA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PrimitiveIsEqCarA0 (Primitive2 rator, PrimitiveCarA0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, SCode rand1)
        {
            return
                (rand1 is PrimitiveCar) ? PrimitiveIsEqCarA0Car.Make (rator, rand0, (PrimitiveCar) rand1) :
                (rand1 is StaticVariable) ? PrimitiveIsEqCarA0S.Make (rator, rand0, (StaticVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveIsEqCarA0Q.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveIsEqCarA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);

            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveIsEqCarA0";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqCarA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            Cons ev0Pair = environment.Argument0Value as Cons;
            if (ev0Pair == null) throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, ev0Pair.Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqCarA0S : PrimitiveIsEqCarA0
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PrimitiveIsEqCarA0S (Primitive2 rator, PrimitiveCarA0 rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, StaticVariable rand1)
        {
            return
                new PrimitiveIsEqCarA0S (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqCarA0S");
#endif
            // Eval argument1
            object ev1;

            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            Cons ev0Pair = environment.Argument0Value as Cons;
            if (ev0Pair == null) throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, ev0Pair.Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqCarA0Q : PrimitiveIsEqCarA0
    {
        public readonly object rand1Value;
        protected PrimitiveIsEqCarA0Q (Primitive2 rator, PrimitiveCarA0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }


        static SCode RewriteEqNull (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            Debug.WriteLine ("(eq? ... null)");
            return PrimitiveCombination1.Make (Primitive.IsNull, rand0);
        }

        static SCode RewriteEqFalse (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            Debug.WriteLine ("(eq? ... #f)");
            return PrimitiveCombination1.Make (Primitive.Not, rand0);
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, Quotation rand1)
        {
#if DEBUG
            if (rand1.Quoted == null)
                Debugger.Break ();
#endif

            return
                               (rand1.Quoted == null &&
                Configuration.EnableCodeRewriting &&
                Configuration.EnableEqNullRewrite) ? RewriteEqNull (rator, rand0, rand1) :
                (rand1.Quoted is bool &&
                ((bool) rand1.Quoted) == false &&
                Configuration.EnableCodeRewriting &&
                Configuration.EnableEqFalseRewrite) ? RewriteEqFalse (rator, rand0, rand1) :
                new PrimitiveIsEqCarA0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqCarA0Q");
#endif
            if (ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, this.rand1Value))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqCarAQ : PrimitiveIsEqCarA
    {
        public readonly object rand1Value;
        protected PrimitiveIsEqCarAQ (Primitive2 rator, PrimitiveCarA rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        static SCode RewriteEqNull (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            Debug.WriteLine ("(eq? ... null)");
            return PrimitiveCombination1.Make (Primitive.IsNull, rand0);
        }

        static SCode RewriteEqFalse (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            Debug.WriteLine ("(eq? ... #f)");
            return PrimitiveCombination1.Make (Primitive.Not, rand0);
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA rand0, Quotation rand1)
        {
            return
               (rand1.Quoted == null &&
                Configuration.EnableCodeRewriting &&
                Configuration.EnableEqNullRewrite) ? RewriteEqNull (rator, rand0, rand1) :
                (rand1.Quoted is bool &&
                ((bool) rand1.Quoted) == false &&
                Configuration.EnableCodeRewriting &&
                Configuration.EnableEqFalseRewrite) ? RewriteEqFalse (rator, rand0, rand1) :
                new PrimitiveIsEqCarAQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqCarAQ");
#endif
            if (ObjectModel.Eq (out answer, ((Cons) environment.ArgumentValue (this.rand0ArgOffset)).Car, this.rand1Value))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqCarAS : PrimitiveIsEqCarA
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal PrimitiveIsEqCarAS (Primitive2 rator, PrimitiveCarA rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqCarAS");
#endif
            // Eval argument1
            object ev1;

            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            Cons ev0Pair = environment.ArgumentValue (this.rand0ArgOffset) as Cons;
            if (ev0Pair == null) throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, ev0Pair.Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqCarS : PrimitiveIsEqCar
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0ArgName;
        public readonly int rand0ArgOffset;

        protected PrimitiveIsEqCarS (Primitive2 rator, PrimitiveCarS rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0ArgOffset = rand0.offset;
            this.rand0ArgName = rand0.name;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarS rand0, SCode rand1)
        {
            return
                (rand1 is Quotation) ? PrimitiveIsEqCarSQ.Make (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? PrimitiveIsEqCarSS.Make (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveIsEqCarS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);

            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveIsEqCarS";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqCarA";
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
            if (environment.StaticValue (out ev0, this.rand0ArgName, this.rand0ArgOffset))
                throw new NotImplementedException ();
            Cons ev0Pair = ev0 as Cons;

            if (ev0Pair == null) throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, ev0Pair.Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqCarSQ : PrimitiveIsEqCarS
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqCarSQ (Primitive2 rator, PrimitiveCarS rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        static SCode RewriteEqNull (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            Debug.WriteLine ("(eq? ... null)");
            return PrimitiveCombination1.Make (Primitive.IsNull, rand0);
        }

        static SCode RewriteEqFalse (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            Debug.WriteLine ("(eq? ... #f)");
            return PrimitiveCombination1.Make (Primitive.Not, rand0);
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarS rand0, Quotation rand1)
        {
            return
                               (rand1.Quoted == null &&
                Configuration.EnableCodeRewriting &&
                Configuration.EnableEqNullRewrite) ? RewriteEqNull (rator, rand0, rand1) :
                (rand1.Quoted is bool &&
                ((bool) rand1.Quoted) == false &&
                Configuration.EnableCodeRewriting &&
                Configuration.EnableEqFalseRewrite) ? RewriteEqFalse (rator, rand0, rand1) :
                new PrimitiveIsEqCarSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PrimitiveIsEqCarS";
#endif
            // Eval argument1
            object ev1 = this.rand1Value;

            // Eval argument0
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0ArgName, this.rand0ArgOffset))
                throw new NotImplementedException ();
            Cons ev0Pair = ev0 as Cons;

            if (ev0Pair == null) throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, ev0Pair.Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqCarSS : PrimitiveIsEqCarS
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PrimitiveIsEqCarSS (Primitive2 rator, PrimitiveCarS rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarS rand0, StaticVariable rand1)
        {
            return
                new PrimitiveIsEqCarSS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PrimitiveIsEqCarSS";
#endif
            // Eval argument1
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0ArgName, this.rand0ArgOffset))
                throw new NotImplementedException ();
            Cons ev0Pair = ev0 as Cons;

            if (ev0Pair == null) throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, ev0Pair.Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqCarA0Car : PrimitiveIsEqCarA0
    {
#if DEBUG
        static Histogram<Type> rand1ArgTypeHistogram = new Histogram<Type> ();
        readonly Type rand1ArgType;
#endif
        public readonly SCode rand1Arg;
        protected PrimitiveIsEqCarA0Car (Primitive2 rator, PrimitiveCarA0 rand0, PrimitiveCar rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Arg = rand1.Operand;
#if DEBUG
            this.rand1ArgType = rand1.Operand.GetType ();
#endif
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, PrimitiveCar rand1)
        {
            return
                (rand1 is PrimitiveCarA) ? PrimitiveIsEqCarA0CarA.Make (rator, rand0, (PrimitiveCarA) rand1) :
                (rand1 is PrimitiveCarS) ? PrimitiveIsEqCarA0CarS.Make (rator, rand0, (PrimitiveCarS) rand1) :
                new PrimitiveIsEqCarA0Car (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1Arg);

            rand1ArgTypeHistogram.Note (this.rand1ArgType);
            SCode.location = "PrimitiveIsEqCarA0Car";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1Arg;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqCarA0Car";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            Cons ev1Pair = ev1 as Cons;
            if (ev1Pair == null) throw new NotImplementedException ();

            // Eval argument0
            Cons ev0Pair = environment.Argument0Value as Cons;
            if (ev0Pair == null) throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, ev0Pair.Car, ev1Pair.Car))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqCarA0CarA : PrimitiveIsEqCarA0Car
    {
        public readonly int rand1Offset;
        protected PrimitiveIsEqCarA0CarA (Primitive2 rator, PrimitiveCarA0 rand0, PrimitiveCarA rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, PrimitiveCarA rand1)
        {
            return
                new PrimitiveIsEqCarA0CarA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqCarA0CarA");
#endif
            Cons ev1Pair = environment.ArgumentValue (this.rand1Offset) as Cons;
            if (ev1Pair == null) throw new NotImplementedException ();

            // Eval argument0
            Cons ev0Pair = environment.Argument0Value as Cons;
            if (ev0Pair == null) throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, ev0Pair.Car, ev1Pair.Car))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqCarA0CarS : PrimitiveIsEqCarA0Car
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        protected PrimitiveIsEqCarA0CarS (Primitive2 rator, PrimitiveCarA0 rand0, PrimitiveCarS rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.name;
            this.rand1Offset = rand1.offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, PrimitiveCarS rand1)
        {
            return
                new PrimitiveIsEqCarA0CarS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqCarA0CarS");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            Cons ev1Pair = ev1 as Cons;
            if (ev1Pair == null) throw new NotImplementedException ();

            // Eval argument0
            Cons ev0Pair = environment.Argument0Value as Cons;
            if (ev0Pair == null) throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, ev0Pair.Car, ev1Pair.Car))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqCarXS : PrimitiveIsEqCar
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif

        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PrimitiveIsEqCarXS (Primitive2 rator, PrimitiveCar rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, StaticVariable rand1)
        {

            return
                new PrimitiveIsEqCarXS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0Arg);

            rand0TypeHistogram.Note (this.rand0ArgType);
            SCode.location = "PrimitiveIsEqCarXS";
#endif
            // Eval argument1
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset)) {
                throw new NotImplementedException ();
            }

            // Eval argument0
            object ev0;

            Control unev = this.rand0Arg;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqCarXS";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            Cons ev0Pair = ev0 as Cons;
            if (ev0Pair == null) throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, ev0Pair.Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqCaar : PrimitiveIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        public readonly Type rand0ArgType;
#endif
        public readonly SCode rand0Arg;

        protected PrimitiveIsEqCaar (Primitive2 rator, PrimitiveCaar rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Arg = rand0.Operand;
#if DEBUG
            this.rand0ArgType = rand0.Operand.GetType ();
#endif
        }

        public static SCode Make (Primitive2 rator, PrimitiveCaar rand0, SCode rand1)
        {

            return
                (rand0 is PrimitiveCaarA) ? PrimitiveIsEqCaarA.Make (rator, (PrimitiveCaarA) rand0, rand1) :
                new PrimitiveIsEqCaar (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0Arg);
            NoteCalls (this.rand1);

            rand0TypeHistogram.Note (this.rand0ArgType);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveIsEqCaar";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqCaar";
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

            unev = this.rand0Arg;
            env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqCaar";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            Cons ev0Pair = ev0 as Cons;
            if (ev0Pair == null) throw new NotImplementedException ();
            Cons ev0PairCar = ev0Pair.Car as Cons;

            if (ObjectModel.Eq (out answer, ev0PairCar.Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqCaarA : PrimitiveIsEqCaar
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0ArgOffset;

        protected PrimitiveIsEqCaarA (Primitive2 rator, PrimitiveCaarA rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0ArgOffset = rand0.offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCaarA rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveCaarA0) ? PrimitiveIsEqCaarA0.Make (rator, (PrimitiveCaarA0) rand0, rand1) :
                (rand1 is StaticVariable) ? new PrimitiveIsEqCaarAS (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveIsEqCaarA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);

            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveIsEqCaarA";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqCaarA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            Cons ev0Pair = environment.ArgumentValue (this.rand0ArgOffset) as Cons;
            if (ev0Pair == null) throw new NotImplementedException ();
            Cons ev0PairCar = ev0Pair.Car as Cons;

            if (ObjectModel.Eq (out answer, ev0PairCar.Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqCaarA0 : PrimitiveIsEqCaarA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PrimitiveIsEqCaarA0 (Primitive2 rator, PrimitiveCaarA0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCaarA0 rand0, SCode rand1)
        {
            return
                (rand1 is StaticVariable) ? PrimitiveIsEqCaarA0S.Make (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveIsEqCaarA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);

            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveIsEqCaarA0";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqCaarA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            Cons ev0Pair = environment.Argument0Value as Cons;
            if (ev0Pair == null) throw new NotImplementedException ();
            Cons ev0PairCar = ev0Pair.Car as Cons;
            if (ObjectModel.Eq (out answer, ev0PairCar.Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqCaarA0S : PrimitiveIsEqCaarA0
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PrimitiveIsEqCaarA0S (Primitive2 rator, PrimitiveCaarA0 rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCaarA0 rand0, StaticVariable rand1)
        {
            return
                new PrimitiveIsEqCaarA0S (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqCaarA0S");
#endif
            // Eval argument1
            object ev1;

            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            Cons ev0Pair = environment.Argument0Value as Cons;
            if (ev0Pair == null) throw new NotImplementedException ();
            Cons ev0PairCar = ev0Pair.Car as Cons;
            if (ObjectModel.Eq (out answer, ev0PairCar.Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqCaarAS : PrimitiveIsEqCaarA
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal PrimitiveIsEqCaarAS (Primitive2 rator, PrimitiveCaarA rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCaarA rand0, StaticVariable rand1)
        {
            return
                new PrimitiveIsEqCaarAS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqCaarAS");
#endif
            // Eval argument1
            object ev1;

            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            Cons ev0Pair = environment.ArgumentValue (this.rand0ArgOffset) as Cons;
            if (ev0Pair == null) throw new NotImplementedException ();
            Cons ev0PairCar = ev0Pair.Car as Cons;
            if (ObjectModel.Eq (out answer, ev0PairCar.Car, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqA : PrimitiveIsEq
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;
        protected PrimitiveIsEqA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveIsEqA0.Make (rator, (Argument0) rand0, rand1) :
                (rand1 is Argument) ? PrimitiveIsEqAA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? PrimitiveIsEqAQ.Make (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? PrimitiveIsEqAS.Make (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveIsEqA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveIsEqA";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            if (ObjectModel.Eq (out answer, environment.ArgumentValue (this.rand0Offset), ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqA0 : PrimitiveIsEqA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PrimitiveIsEqA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is PrimitiveCar) ? PrimitiveIsEqA0Car.Make (rator, rand0, (PrimitiveCar) rand1) :
                (rand1 is Argument) ? PrimitiveIsEqA0A.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? PrimitiveIsEqA0Q.Make (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? PrimitiveIsEqA0S.Make (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveIsEqA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveIsEqA0";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ObjectModel.Eq (out answer, environment.Argument0Value, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqA0Car : PrimitiveIsEqA0
    {
#if DEBUG
        static Histogram<Type> rand1ArgTypeHistogram = new Histogram<Type> ();
        readonly Type rand1ArgType;
#endif

        public readonly SCode rand1Arg;
        protected PrimitiveIsEqA0Car (Primitive2 rator, Argument0 rand0, PrimitiveCar rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Arg = rand1.Operand;
#if DEBUG
            this.rand1ArgType = rand1.Operand.GetType ();
#endif
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, PrimitiveCar rand1)
        {
            return
                (rand1 is PrimitiveCarA) ? PrimitiveIsEqA0CarA.Make (rator, rand0, (PrimitiveCarA) rand1) :
                new PrimitiveIsEqA0Car (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);

            rand1ArgTypeHistogram.Note (this.rand1ArgType);
            SCode.location = "PrimitiveIsEqCarA0Car";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1Arg;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqCarA0Car";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            Cons ev1Pair = ev1 as Cons;
            if (ev1Pair == null) throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, environment.Argument0Value, ev1Pair.Car))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqA0CarA : PrimitiveIsEqA0Car
    {
        public readonly int rand1Offset;
        protected PrimitiveIsEqA0CarA (Primitive2 rator, Argument0 rand0, PrimitiveCarA rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.offset;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, PrimitiveCarA rand1)
        {
            return
                new PrimitiveIsEqA0CarA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqA0CarA");
#endif
            // Eval argument1
            Cons ev1Pair = environment.ArgumentValue (this.rand1Offset) as Cons;
            if (ev1Pair == null) throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, environment.Argument0Value, ev1Pair))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqA0A : PrimitiveIsEqA0
    {
        public readonly int rand1Offset;

        protected PrimitiveIsEqA0A (Primitive2 rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Unimplemented () :
                new PrimitiveIsEqA0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqA0A");
#endif
            if (ObjectModel.Eq (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset)))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqA0Q : PrimitiveIsEqA0
    {
#if DEBUG
        static Histogram<object> objectHistogram = new Histogram<object> ();
#endif
        public readonly object rand1Value;
        protected PrimitiveIsEqA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        static SCode RewriteEqNull (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            Debug.WriteLine ("(eq? ... null)");
            return PrimitiveCombination1.Make (Primitive.IsNull, rand0);
        }

        static SCode RewriteEqFalse (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            Debug.WriteLine ("(eq? ... #f)");
            return PrimitiveCombination1.Make (Primitive.Not, rand0);
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                (rand1.Quoted == null &&
                Configuration.EnableCodeRewriting &&
                Configuration.EnableEqNullRewrite) ? RewriteEqNull (rator, rand0, rand1) :
                (rand1.Quoted is bool &&
                ((bool) rand1.Quoted) == false &&
                Configuration.EnableCodeRewriting &&
                Configuration.EnableEqFalseRewrite) ? RewriteEqFalse (rator, rand0, rand1) :
                new PrimitiveIsEqA0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqA0Q");
            objectHistogram.Note (this.rand1Value);
#endif
            if (ObjectModel.Eq (out answer, environment.Argument0Value, this.rand1Value))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqA0S : PrimitiveIsEqA0
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PrimitiveIsEqA0S (Primitive2 rator, Argument0 rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, StaticVariable rand1)
        {
            return
                new PrimitiveIsEqA0S (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqA0S");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            if (ObjectModel.Eq (out answer, environment.Argument0Value, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqA0A0 : PrimitiveIsEqA0A
    {
        protected PrimitiveIsEqA0A0 (Primitive2 rator, Argument0 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument0 rand1)
        {
            throw new NotImplementedException ();
            //new PrimitiveIsEqA1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqA0A0");
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveIsEqAA : PrimitiveIsEqA
    {
        public readonly int rand1Offset;
        protected PrimitiveIsEqAA (Primitive2 rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsEqAA0.Make (rator, rand0, (Argument0) rand1) :
                new PrimitiveIsEqAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqAA");
#endif
            if (ObjectModel.Eq (out answer, environment.ArgumentValue (this.rand0Offset), environment.ArgumentValue (this.rand1Offset)))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqAA0 : PrimitiveIsEqAA
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
            Warm ("PrimitiveIsEqAA0");
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqAQ : PrimitiveIsEqA
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqAQ (Primitive2 rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        static SCode RewriteEqNull (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            Debug.WriteLine ("(eq? ... null)");
            return PrimitiveCombination1.Make (Primitive.IsNull, rand0);
        }

        static SCode RewriteEqFalse (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            Debug.WriteLine ("(eq? ... #f)");
            return PrimitiveCombination1.Make (Primitive.Not, rand0);
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Quotation rand1)
        {
            return
                (rand1.Quoted == null &&
                Configuration.EnableCodeRewriting &&
                Configuration.EnableEqNullRewrite) ? RewriteEqNull (rator, rand0, rand1) :
                (rand1.Quoted is bool &&
                ((bool) rand1.Quoted) == false &&
                Configuration.EnableCodeRewriting &&
                Configuration.EnableEqFalseRewrite) ? RewriteEqFalse (rator, rand0, rand1) :
                new PrimitiveIsEqAQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqAQ");
#endif
            if (ObjectModel.Eq (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqAS : PrimitiveIsEqA
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PrimitiveIsEqAS (Primitive2 rator, Argument rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, StaticVariable rand1)
        {
            return
                new PrimitiveIsEqAS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqAS");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            if (ObjectModel.Eq (out answer, environment.ArgumentValue (this.rand0Offset), ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqS : PrimitiveIsEq
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected PrimitiveIsEqS (Primitive2 rator, StaticVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, StaticVariable rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsEqSA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? new PrimitiveIsEqSS (rator, rand0, (StaticVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveIsEqSQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveIsEqS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveIsEqS";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqS";
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
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            return ObjectModel.Eq (out answer, ev0, ev1);
        }
    }

    [Serializable]
    class PrimitiveIsEqSQ : PrimitiveIsEqS
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqSQ (Primitive2 rator, StaticVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        static SCode RewriteEqNull (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            Debug.WriteLine ("(eq? ... null)");
            return PrimitiveCombination1.Make (Primitive.IsNull, rand0);
        }

        static SCode RewriteEqFalse (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            Debug.WriteLine ("(eq? ... #f)");
            return PrimitiveCombination1.Make (Primitive.Not, rand0);
        }

        public static SCode Make (Primitive2 rator, StaticVariable rand0, Quotation rand1)
        {
            return
                               (rand1.Quoted == null &&
                Configuration.EnableCodeRewriting &&
                Configuration.EnableEqNullRewrite) ? RewriteEqNull (rator, rand0, rand1) :
                (rand1.Quoted is bool &&
                ((bool) rand1.Quoted) == false &&
                Configuration.EnableCodeRewriting &&
                Configuration.EnableEqFalseRewrite) ? RewriteEqFalse (rator, rand0, rand1) :
                new PrimitiveIsEqSQ (rator, rand0, rand1);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqSQ");
#endif

            // Eval argument0
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            return ObjectModel.Eq (out answer, ev0, this.rand1Value);
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqSS : PrimitiveIsEqS
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal PrimitiveIsEqSS (Primitive2 rator, StaticVariable rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
#if DEBUG
            if (rand0.Offset == rand1.Offset) Debugger.Break ();
#endif
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqSS");
#endif
            // Eval argument1
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            return ObjectModel.Eq (out answer, ev0, ev1);
        }
    }

    [Serializable]
    class PrimitiveIsEqQ : PrimitiveIsEq
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;

        protected PrimitiveIsEqQ (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        static SCode RewriteEqNull (Primitive2 rator, Quotation rand0, SCode rand1)
        {
                Debug.WriteLine ("(eq? null ...)");
                return PrimitiveCombination1.Make (Primitive.IsNull, rand1);
        }

        static SCode RewriteEqFalse (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            Debug.WriteLine ("(eq? #f ...)");
            return PrimitiveCombination1.Make (Primitive.Not, rand1);
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand0.Quoted == null &&
                Configuration.EnableCodeRewriting &&
                Configuration.EnableEqNullRewrite) ? RewriteEqNull (rator, rand0, rand1) :
                (rand0.Quoted is bool &&
                ((bool) rand0.Quoted) == false &&
                Configuration.EnableCodeRewriting &&
                Configuration.EnableEqFalseRewrite) ? RewriteEqFalse (rator, rand0, rand1) :
                (rand0.Quoted is char) ? PrimitiveIsCharEqQ.Make (rator, rand0, rand1) :
                (rand0.Quoted is int) ? PrimitiveIsIntEqQ.Make (rator, rand0, rand1) :
                (rand0.Quoted is Cons ||
                 rand0.Quoted is Symbol) ? PrimitiveIsObjectEqQ.Make (rator, rand0, rand1) :
                 (rand1 is Argument) ? PrimitiveIsEqQA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? PrimitiveIsEqQQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveIsEqQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveIsEqQ";
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

            if (ObjectModel.Eq (out answer, this.rand0Value, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqQA : PrimitiveIsEqQ
    {
        public readonly int rand1Offset;
        protected PrimitiveIsEqQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsEqQA0.Make (rator, rand0, (Argument0) rand1) :
                new PrimitiveIsEqQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqQA");
#endif
            if (ObjectModel.Eq (out answer, this.rand0Value, environment.ArgumentValue (this.rand1Offset)))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqQA0 : PrimitiveIsEqQA
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
            Warm ("PrimitiveIsEqQA0");
#endif
            if (ObjectModel.Eq (out answer, this.rand0Value, environment.Argument0Value))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqQQ : PrimitiveIsEqQ
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqQQ (Primitive2 rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Quotation rand1)
        {
            Debugger.Break ();
            return
                 new PrimitiveIsEqQQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqQQ");
#endif
            if (ObjectModel.Eq (out answer, this.rand0Value, (this.rand1Value)))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqQS : PrimitiveIsEqQ
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PrimitiveIsEqQS (Primitive2 rator, Quotation rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, StaticVariable rand1)
        {
            return
                new PrimitiveIsEqQS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqQS");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            if (ObjectModel.Eq (out answer, this.rand0Value, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqSA : PrimitiveIsEqS
    {
        public readonly int rand1Offset;
        protected PrimitiveIsEqSA (Primitive2 rator, StaticVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, StaticVariable rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsEqSA0.Make (rator, rand0, (Argument0) rand1) :
                 new PrimitiveIsEqSA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqSA");
#endif
            // Eval argument0
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            if (ObjectModel.Eq (out answer, ev0, environment.ArgumentValue (this.rand1Offset)))
                throw new NotImplementedException ();
            return false;
        }

    }

    [Serializable]
    sealed class PrimitiveIsEqSA0 : PrimitiveIsEqSA
    {
        protected PrimitiveIsEqSA0 (Primitive2 rator, StaticVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, StaticVariable rand0, Argument0 rand1)
        {
            return
                new PrimitiveIsEqSA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqSA0");
#endif
            // Eval argument0
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            if (ObjectModel.Eq (out answer, ev0, environment.Argument0Value))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsEqXA : PrimitiveIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;

        protected PrimitiveIsEqXA (Primitive2 rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsEqXA0.Make (rator, rand0, (Argument0) rand1) :
                new PrimitiveIsEqXA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveIsEqXA";
#endif
            // Eval argument1
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqXA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            return ObjectModel.Eq (out answer, ev0, ev1);
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqXA0 : PrimitiveIsEqXA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveIsEqXA0 (Primitive2 rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument0 rand1)
        {
            return
                new PrimitiveIsEqXA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveIsEqXA0";
#endif
            // Eval argument1
            object ev1 = environment.Argument0Value;

            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqXA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            return ObjectModel.Eq (out answer, ev0, ev1);
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqXS : PrimitiveIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PrimitiveIsEqXS (Primitive2 rator, SCode rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, StaticVariable rand1)
        {
            return
                //(rand0 is Argument0) ? PrimitiveIsEqA0.Make (rator, (Argument0) rand0, rand1) :
                //(rand0 is Argument1) ? PrimitiveIsEqA1.Make (rator, (Argument1) rand0, rand1) :
                //(rand1 is Quotation) ? PrimitiveIsEqAQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveIsEqXS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveIsEqXS";
#endif
            // Eval argument1
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsEqXS";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            return ObjectModel.Eq (out answer, ev0, ev1);
        }
    }

    [Serializable]
    class PrimitiveIsEqXQ : PrimitiveIsEq
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqXQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        static SCode RewriteEqNull (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            Debug.WriteLine ("(eq? ... null)");
            return PrimitiveCombination1.Make (Primitive.IsNull, rand0);
        }

        static SCode RewriteEqFalse (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            Debug.WriteLine ("(eq? ... #f)");
            return PrimitiveCombination1.Make (Primitive.Not, rand0);
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
               (rand1.Quoted == null &&
                Configuration.EnableCodeRewriting &&
                Configuration.EnableEqNullRewrite) ? RewriteEqNull (rator, rand0, rand1) :
                (rand1.Quoted is bool &&
                ((bool) rand1.Quoted) == false &&
                Configuration.EnableCodeRewriting &&
                Configuration.EnableEqFalseRewrite) ? RewriteEqFalse (rator, rand0, rand1) :
                (rand1.Quoted is char) ? PrimitiveIsEqXChar.Make (rator, rand0, rand1) :
                (rand1.Quoted is int) ? PrimitiveIsEqXFixnum.Make (rator, rand0, rand1) :
                (rand1.Quoted is Symbol ||
                 rand1.Quoted is Record) ? PrimitiveIsEqXObject.Make (rator, rand0, rand1) :
                new PrimitiveIsEqXQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqXQ");
            NoteCalls (this.rand0);
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

            return ObjectModel.Eq (out answer, ev0, this.rand1Value);
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqXChar : PrimitiveIsEqXQ
    {
        public readonly char rand1Value;

        protected PrimitiveIsEqXChar (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (char) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveIsEqXChar (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqXChar");
            NoteCalls (this.rand0);
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
            answer = ev0 is char && (((char) ev0) == this.rand1Value);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqXFixnum : PrimitiveIsEqXQ
    {
        public readonly int rand1Value;

        protected PrimitiveIsEqXFixnum (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = (int) rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveIsEqXFixnum (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsEqXFixnum");
            NoteCalls (this.rand0);
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
            answer = ev0 is int && (((int) ev0) == this.rand1Value);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsEqXObject : PrimitiveIsEqXQ
    {
        public readonly object rand1Value;

        protected PrimitiveIsEqXObject (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveIsEqXObject (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            SCode.location = "PrimitiveIsEqXObject";
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
            answer = ev0 == this.rand1Value;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsCharEqQ : PrimitiveIsEqQ
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly char rand0Value;

        protected PrimitiveIsCharEqQ (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = (char) rand0.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsCharEqQA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? PrimitiveIsCharEqQS.Make (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveIsCharEqQ (rator, rand0, rand1);
        }


        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveIsCharEqQ";
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
            //bool x = (ev1 is char);
            //bool y = (ev1 is Character);
            //if (x != y) Debugger.Break ();
            answer = ((ev1 is char) && (this.rand0Value == (char) ev1)) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsCharEqQA : PrimitiveIsCharEqQ
    {
        public readonly int rand1Offset;
        protected PrimitiveIsCharEqQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsCharEqQA0.Make (rator, rand0, (Argument0) rand1) :
               new PrimitiveIsCharEqQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEqQA");
#endif
            object temp = environment.ArgumentValue (this.rand1Offset);
            //bool x = (temp is char);
            //bool y = (temp is Character);
            //if (x != y) Debugger.Break ();
            answer = ((temp is char) && (this.rand0Value == (char) temp)) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsCharEqQA0 : PrimitiveIsCharEqQA
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
            Warm ("PrimitiveIsCharEqQA0");
#endif
            object temp = environment.Argument0Value;
            //bool x = (temp is char);
            //bool y = (temp is Character);
            //if (x != y) Debugger.Break ();
            answer = ((temp is char) && (this.rand0Value == (char) temp)) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsCharEqQS : PrimitiveIsCharEqQ
    {
        public readonly int rand1Offset;
        public readonly Symbol rand1Name;
        protected PrimitiveIsCharEqQS (Primitive2 rator, Quotation rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
            this.rand1Name = rand1.Name;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, StaticVariable rand1)
        {
            return
                new PrimitiveIsCharEqQS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharEqQS");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            //bool x = (ev1 is char);
            //bool y = (ev1 is Character);
            //if (x != y) Debugger.Break ();
            answer = ((ev1 is char) && (this.rand0Value == (char) ev1)) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsIntEqQ : PrimitiveIsEqQ
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Value;

        protected PrimitiveIsIntEqQ (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = (int) rand0.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsIntEqQA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? PrimitiveIsIntEqQS.Make (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveIsIntEqQ (rator, rand0, rand1);
        }


        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveIsIntEqQ";
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

            answer = (ev1 is int) && (this.rand0Value == (int) ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsIntEqQA : PrimitiveIsIntEqQ
    {
        public readonly int rand1Offset;
        protected PrimitiveIsIntEqQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsIntEqQA0.Make (rator, rand0, (Argument0) rand1) :
                new PrimitiveIsIntEqQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntEqQA");
#endif
            object temp = environment.ArgumentValue (this.rand1Offset);
            answer = ((temp is int) && this.rand0Value == (int) temp);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsIntEqQA0 : PrimitiveIsIntEqQA
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
            Warm ("PrimitiveIsIntEqQA0");
#endif
            object temp = environment.Argument0Value;
            answer = (temp is int && this.rand0Value == (int) temp);
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsIntEqQS : PrimitiveIsIntEqQ
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        protected PrimitiveIsIntEqQS (Primitive2 rator, Quotation rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, StaticVariable rand1)
        {
            return
                new PrimitiveIsIntEqQS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntEqQS");
#endif
            object that;
            if (environment.StaticValue (out that, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            answer = ((that is int) && (this.rand0Value == (int) that));
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectEqQ : PrimitiveIsEqQ
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;

        protected PrimitiveIsObjectEqQ (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsObjectEqQA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? PrimitiveIsEqQS.Make (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveIsObjectEqQ (rator, rand0, rand1);
        }


        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveIsObjectEqQ";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                //throw new NotImplementedException ();
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = (this.rand0Value == ev1);
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectEqQA : PrimitiveIsObjectEqQ
    {
        public readonly int rand1Offset;
        protected PrimitiveIsObjectEqQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsObjectEqQA0.Make (rator, rand0, (Argument0) rand1) :
                new PrimitiveIsObjectEqQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectEqQA");
#endif
            answer = (this.rand0Value == environment.ArgumentValue (this.rand1Offset));
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsObjectEqQA0 : PrimitiveIsObjectEqQA
    {
        protected PrimitiveIsObjectEqQA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
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
            Warm ("PrimitiveIsObjectEqQA0");
#endif
            answer = (this.rand0Value == environment.Argument0Value);
            return false;
        }
    }
}
