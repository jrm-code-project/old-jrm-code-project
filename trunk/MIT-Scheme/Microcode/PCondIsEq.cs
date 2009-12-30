using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PCondIsEq : PCond2
    {
#if DEBUG
            static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
            static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
            static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
            static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEq (PrimitiveIsEq predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }
        public static SCode Make (PrimitiveIsEq predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqA) ? PCondIsEqA.Make((PrimitiveIsEqA) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqCar) ? PCondIsEqCar.Make ((PrimitiveIsEqCar) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqCaar) ? PCondIsEqCaar.Make ((PrimitiveIsEqCaar) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqQ) ? PCondIsEqQ.Make ((PrimitiveIsEqQ) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqS) ? PCondIsEqS.Make ((PrimitiveIsEqS) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqXQ) ? PCondIsEqXQ.Make ((PrimitiveIsEqXQ) predicate, consequent, alternative) :
                new PCondIsEq (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEq";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }
#if DEBUG
            SCode.location = "PCondIsEq";
#endif

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
#if DEBUG
            SCode.location = "PCondIsEq";
#endif
            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }
    [Serializable]
    class PCondIsEqA : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;

        protected PCondIsEqA (PrimitiveIsEqA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Offset = predicate.rand0Offset;
        }

        public static SCode Make (PrimitiveIsEqA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqA0) ? PCondIsEqA0.Make ((PrimitiveIsEqA0) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqA1) ? PCondIsEqA1.Make ((PrimitiveIsEqA1) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqAQ) ? PCondIsEqAQ.Make ((PrimitiveIsEqAQ) predicate, consequent, alternative) :
                new PCondIsEqA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }
#if DEBUG
            SCode.location = "PCondIsEqA";
#endif
            ObjectModel.Eq (out answer, environment.ArgumentValue (this.rand0Offset), ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0 : PCondIsEqA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqA0 (PrimitiveIsEqA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqA0Car) ? PCondIsEqA0Car.Make ((PrimitiveIsEqA0Car) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqA0Q) ? PCondIsEqA0Q.Make ((PrimitiveIsEqA0Q) predicate, consequent, alternative) :
                (alternative is Quotation) ? new PCondIsEqA0XXQ (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }
#if DEBUG
            SCode.location = "PCondIsEqA0";
#endif
            ObjectModel.Eq (out answer, environment.Argument0Value, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0Q : PCondIsEqA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
        public readonly Type rand1Type;
#endif
        public readonly object rand1Value;

        protected PCondIsEqA0Q (PrimitiveIsEqA0Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
#if DEBUG
            this.rand1Type = predicate.rand1Value.GetType();
#endif
        }

        public static SCode Make (PrimitiveIsEqA0Q predicate, SCode consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCondIsEqA0QXA.Make (predicate, consequent, (Argument) alternative) :
                new PCondIsEqA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqA0Q";
#endif
            ObjectModel.Eq (out answer, environment.Argument0Value, this.rand1Value);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqA0Q";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqA0Q";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0QXA : PCondIsEqA0Q
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;

        protected PCondIsEqA0QXA (PrimitiveIsEqA0Q predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveIsEqA0Q predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqA0QXA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCondIsEqA0QXA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCondIsEqA0QXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqA0QXA";
#endif
            ObjectModel.Eq (out answer, environment.Argument0Value, this.rand1Value);

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqA0QXA";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0QXA0 : PCondIsEqA0QXA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;

        protected PCondIsEqA0QXA0 (PrimitiveIsEqA0Q predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqA0Q predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsEqA0QXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqA0QXA0";
#endif
            object ev0 = environment.Argument0Value;
            ObjectModel.Eq (out answer, ev0, this.rand1Value);

            if ((answer is bool) && (bool) answer == false) {
                answer = ev0;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqA0QXA";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0QXA1 : PCondIsEqA0QXA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqA0QXA1 (PrimitiveIsEqA0Q predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqA0Q predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCondIsEqA0QXA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqA0QXA1";
#endif
            ObjectModel.Eq (out answer, environment.Argument0Value, this.rand1Value);

            if ((answer is bool) && (bool) answer == false) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqA0QXA1";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    sealed class PCondIsEqA0XXQ : PCondIsEqA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        internal PCondIsEqA0XXQ (PrimitiveIsEqA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqA0XXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }
#if DEBUG
            SCode.location = "PCondIsEqA0XXQ";
#endif
            ObjectModel.Eq (out answer, environment.Argument0Value, ev1);

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqA1 : PCondIsEqA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type>();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
#endif

        protected PCondIsEqA1(PrimitiveIsEqA1 predicate, SCode consequent, SCode alternative)
            : base(predicate, consequent, alternative)
        {

        }

        public static SCode Make(PrimitiveIsEqA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqA1Q) ? PCondIsEqA1Q.Make ((PrimitiveIsEqA1Q) predicate, consequent, alternative) :
                new PCondIsEqA1(predicate, consequent, alternative);
        }

        public override bool EvalStep(out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            NoteCalls(this.rand1);
            rand1TypeHistogram.Note(this.rand1Type);
            SCode.location = "PCondIsEqA1";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep(out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqA1";
#endif
            if (ev1 == Interpreter.UnwindStack)
            {
                throw new NotImplementedException();
            }

            ObjectModel.Eq(out answer, environment.Argument1Value, ev1);

            if ((answer is bool) && (bool)answer == false)
            {
#if DEBUG
                SCode.location = "-";
                NoteCalls(this.alternative);
                alternativeTypeHistogram.Note(this.alternativeType);
                SCode.location = "PCondIsEqA1";
#endif
                expression = this.alternative;
                return true;
            }
            else
            {
#if DEBUG
                SCode.location = "-";
                NoteCalls(this.consequent);
                consequentTypeHistogram.Note(this.consequentType);
                SCode.location = "PCondIsEqA1";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqA1Q : PCondIsEqA1
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
        public readonly Type rand1Type;
#endif
        public readonly object rand1Value;

        protected PCondIsEqA1Q (PrimitiveIsEqA1Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
#if DEBUG
            this.rand1Type = predicate.rand1Value.GetType ();
#endif
        }

        public static SCode Make (PrimitiveIsEqA1Q predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondIsEqA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqA1Q";
#endif
            ObjectModel.Eq (out answer, environment.Argument1Value, this.rand1Value);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqA1Q";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqA1Q";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }
 
    [Serializable]
    class PCondIsEqA0Car : PCondIsEqA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqA0Car (PrimitiveIsEqA0Car predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveIsEqA0Car predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqA0CarA) ? PCondIsEqA0CarA.Make ((PrimitiveIsEqA0CarA) predicate, consequent, alternative) :
                new PCondIsEqA0Car (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

    }

    [Serializable]
    class PCondIsEqA0CarA : PCondIsEqA0Car
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;
        protected PCondIsEqA0CarA (PrimitiveIsEqA0CarA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsEqA0CarA predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondIsEqA0CarA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA0CarA.EvalStep");
#endif
            Cons ev1 = environment.ArgumentValue (this.rand1Offset) as Cons;
            if (ev1 == null) throw new NotImplementedException ();
            ObjectModel.Eq (out answer, environment.Argument0Value, ev1.Car);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqA0CarA";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqA0CarA";
#endif
                expression = this.consequent;
                return true;
            }
        }

    }

    [Serializable]
    class PCondIsEqAQ : PCondIsEqA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PCondIsEqAQ (PrimitiveIsEqAQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveIsEqAQ predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondIsEqAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqAQ");
#endif
            ObjectModel.Eq (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqCar : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
        public readonly Type rand0Type;
#endif
        protected readonly SCode rand0Arg;

        protected PCondIsEqCar (PrimitiveIsEqCar predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Arg = predicate.rand0Arg;
#if DEBUG
            this.rand0Type = predicate.rand0ArgType;
#endif
        }

        public static SCode Make (PrimitiveIsEqCar predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCarA) ? PCondIsEqCarA.Make ((PrimitiveIsEqCarA) predicate, consequent, alternative) :
                new PCondIsEqCar (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            NoteCalls (this.rand0Arg);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqCar";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqCar";
#endif
            if (ev1 == Interpreter.UnwindStack)
            {
                throw new NotImplementedException ();
            }

            unev = this.rand0Arg;
            env = environment;
            object ev0temp;
            while (unev.EvalStep (out ev0temp, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqCar";
#endif
            if (ev0temp == Interpreter.UnwindStack)
            {
                throw new NotImplementedException ();
            }

            Cons ev0 = ev0temp as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0.Car, ev1);

            if ((answer is bool) && (bool) answer == false)
            {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else
            {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqCarA : PCondIsEqCar
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly int rand0Offset;

        protected PCondIsEqCarA (PrimitiveIsEqCarA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Offset = predicate.rand0ArgOffset;
        }

        public static SCode Make (PrimitiveIsEqCarA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCarA0) ? PCondIsEqCarA0.Make ((PrimitiveIsEqCarA0) predicate, consequent, alternative) :
                new PCondIsEqCarA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqCarA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack)
            {
                throw new NotImplementedException ();
            }
#if DEBUG
            SCode.location = "PCondIsEqCarA";
#endif
            Cons ev0 = environment.ArgumentValue(this.rand0Offset) as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0.Car, ev1);

            if ((answer is bool) && (bool) answer == false)
            {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else
            {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqCarA0 : PCondIsEqCarA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqCarA0 (PrimitiveIsEqCarA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCarA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCarA0Car) ? PCondIsEqCarA0Car.Make ((PrimitiveIsEqCarA0Car) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqCarA0S) ? PCondIsEqCarA0S.Make ((PrimitiveIsEqCarA0S) predicate, consequent, alternative) :
                new PCondIsEqCarA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqCarA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack)
            {
                throw new NotImplementedException ();
            }
#if DEBUG
            SCode.location = "PCondIsEqCarA0";
#endif
            Cons ev0 = environment.Argument0Value as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0.Car, ev1);

            if ((answer is bool) && (bool) answer == false)
            {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else
            {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqCarA0Car : PCondIsEqCarA0
    {
#if DEBUG
        static Histogram<Type> rand1ArgTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
        protected Type rand1ArgType;
#endif
        public readonly SCode rand1Arg;
        protected PCondIsEqCarA0Car (PrimitiveIsEqCarA0Car predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Arg = predicate.rand1Arg;
#if DEBUG
            this.rand1ArgType = rand1Arg.GetType();
#endif
        }

        public static SCode Make (PrimitiveIsEqCarA0Car predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCarA0CarA) ? PCondIsEqCarA0CarA.Make ((PrimitiveIsEqCarA0CarA) predicate, consequent, alternative) :
                new PCondIsEqCarA0Car (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1Arg);
            rand1ArgTypeHistogram.Note (this.rand1ArgType);
            SCode.location = "PCondIsEqCarA0Car";
#endif
            Control unev = this.rand1Arg;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack)
            {
                throw new NotImplementedException ();
            }
#if DEBUG
            SCode.location = "PCondIsEqCarA0Car";
#endif
            Cons ev1Pair = ev1 as Cons;
            if (ev1Pair == null) throw new NotImplementedException ();
            Cons ev0 = environment.Argument0Value as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0.Car, ev1Pair.Car);

            if ((answer is bool) && (bool) answer == false)
            {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else
            {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqCarA0CarA : PCondIsEqCarA0Car
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;
        protected PCondIsEqCarA0CarA (PrimitiveIsEqCarA0CarA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsEqCarA0CarA predicate, SCode consequent, SCode alternative)
        {
            return
                //(predicate is PrimitiveIsEqCarA0CarA) ? PCondIsEqCarA0CarA ((PrimitiveIsEqCarA0CarA) predicate, consequent, alternative) :
                new PCondIsEqCarA0CarA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCarA0CarA");
#endif
            Cons ev1Pair = environment.ArgumentValue(this.rand1Offset) as Cons;
            if (ev1Pair == null) throw new NotImplementedException ();
            Cons ev0 = environment.Argument0Value as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0.Car, ev1Pair.Car);

            if ((answer is bool) && (bool) answer == false)
            {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else
            {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqCarA0S : PCondIsEqCarA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PCondIsEqCarA0S (PrimitiveIsEqCarA0S predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsEqCarA0S predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqCarA0SA.Make (predicate, (Argument) consequent, alternative) :
                new PCondIsEqCarA0S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCarA0S");
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Cons ev0 = environment.Argument0Value as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0.Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqCarA0S";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqCarA0S";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqCarA0SA : PCondIsEqCarA0S
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCondIsEqCarA0SA (PrimitiveIsEqCarA0S predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsEqCarA0S predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqCarA0SA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCondIsEqCarA0SA1.Make (predicate, (Argument1) consequent, alternative) :
                new PCondIsEqCarA0SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCarA0SA");
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Cons ev0 = environment.Argument0Value as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0.Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqCarA0SA";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqCarA0SA0 : PCondIsEqCarA0SA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqCarA0SA0 (PrimitiveIsEqCarA0S predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCarA0S predicate, Argument0 consequent, SCode alternative)
        {
            return
                new PCondIsEqCarA0SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCarA0SA0");
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Cons ev0 = environment.Argument0Value as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0.Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqCarA0SA0";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = ev0;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqCarA0SA1 : PCondIsEqCarA0SA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqCarA0SA1 (PrimitiveIsEqCarA0S predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCarA0S predicate, Argument1 consequent, SCode alternative)
        {
            return
                new PCondIsEqCarA0SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCarA0SA1");
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Cons ev0 = environment.Argument0Value as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0.Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqCarA0SA1";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqCaar : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly SCode rand0Arg;

        protected PCondIsEqCaar (PrimitiveIsEqCaar predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Arg = predicate.rand0Arg;
#if DEBUG
            this.rand0Type = predicate.rand0ArgType;
#endif
        }

        public static SCode Make (PrimitiveIsEqCaar predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCaarA) ? PCondIsEqCaarA.Make ((PrimitiveIsEqCaarA) predicate, consequent, alternative) :
                new PCondIsEqCaar (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            NoteCalls (this.rand0Arg);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqCaar";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqCaar";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0Arg;
            env = environment;
            object ev0temp;
            while (unev.EvalStep (out ev0temp, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqCaar";
#endif
            if (ev0temp == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            Cons ev0 = ev0temp as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ((Cons)(ev0.Car)).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqCaarA : PCondIsEqCaar
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly int rand0Offset;

        protected PCondIsEqCaarA (PrimitiveIsEqCaarA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Offset = predicate.rand0ArgOffset;
        }

        public static SCode Make (PrimitiveIsEqCaarA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCaarA0) ? PCondIsEqCaarA0.Make ((PrimitiveIsEqCaarA0) predicate, consequent, alternative) :
                new PCondIsEqCaarA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqCaarA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }
#if DEBUG
            SCode.location = "PCondIsEqCaarA";
#endif
            Cons ev0 = environment.ArgumentValue (this.rand0Offset) as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ((Cons)(ev0.Car)).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqCaarA0 : PCondIsEqCaarA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqCaarA0 (PrimitiveIsEqCaarA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCaarA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCaarA0S) ? PCondIsEqCaarA0S.Make ((PrimitiveIsEqCaarA0S) predicate, consequent, alternative) :
                new PCondIsEqCaarA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqCaarA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }
#if DEBUG
            SCode.location = "PCondIsEqCaarA0";
#endif
            Cons ev0 = environment.Argument0Value as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ((Cons)(ev0.Car)).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqCaarA0S : PCondIsEqCaarA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PCondIsEqCaarA0S (PrimitiveIsEqCaarA0S predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            rand1Name = predicate.rand1Name;
            rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsEqCaarA0S predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is PrimitiveCarA0) ? PCondIsEqCaarA0SCarA0.Make (predicate, (PrimitiveCarA0) consequent, alternative) :
                new PCondIsEqCaarA0S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCaarA0S");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Cons ev0 = environment.Argument0Value as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ((Cons) (ev0.Car)).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqCaarA0SCarA0 : PCondIsEqCaarA0S
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqCaarA0SCarA0 (PrimitiveIsEqCaarA0S predicate, PrimitiveCarA0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCaarA0S predicate, PrimitiveCarA0 consequent, SCode alternative)
        {
            return
                new PCondIsEqCaarA0SCarA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCaarA0SCarA0");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Cons ev0 = environment.Argument0Value as Cons;
            if (ev0 == null) throw new NotImplementedException ();
            Cons ev0Car = ev0.Car as Cons;

            ObjectModel.Eq (out answer, ev0Car.Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqCaarA0SCarA0";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = ev0Car;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQ : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;

        protected PCondIsEqQ (PrimitiveIsEqQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Value = predicate.rand0Value;
        }

        public static SCode Make (PrimitiveIsEqQ predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondIsEqQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, this.rand0Value, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqQ";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqS : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected PCondIsEqS (PrimitiveIsEqS predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Name = predicate.rand0Name;
            this.rand0Offset = predicate.rand0Offset;
        }

        public static SCode Make (PrimitiveIsEqS predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqSS) ? PCondIsEqSS.Make ((PrimitiveIsEqSS) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqSQ) ? PCondIsEqSQ.Make ((PrimitiveIsEqSQ) predicate, consequent, alternative) :
                (consequent is Argument) ? PCondIsEqSXA.Make (predicate, (Argument) consequent, alternative) :
                (alternative is Quotation) ? new PCondIsEqSXXQ (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqS";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqS";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqS";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqSS : PCondIsEqS
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PCondIsEqSS (PrimitiveIsEqSS predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsEqSS predicate, SCode consequent, SCode alternative)
        {
            return
                (alternative is Quotation) ? new PCondIsEqSSXQ (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqSS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqSS";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqSS";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    sealed class PCondIsEqSSXQ : PCondIsEqSS
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        internal PCondIsEqSSXQ (PrimitiveIsEqSS predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted ;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqSSXQ");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqSS";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqSQ : PCondIsEqS
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PCondIsEqSQ (PrimitiveIsEqSQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveIsEqSQ predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondIsEqSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqSQ");
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, this.rand1Value);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqSQ";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqSQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqSXA : PCondIsEqS
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCondIsEqSXA (PrimitiveIsEqS predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsEqS predicate, Argument consequent, SCode alternative)
        {
            return
                new PCondIsEqSXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqSXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqSXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqSXA";
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXQ : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PCondIsEqXQ (PrimitiveIsEqXQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveIsEqXQ predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondIsEqXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqXQ";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, this.rand1Value);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqXQ";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqXQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }


    [Serializable]
    sealed class PCondIsEqSXXQ : PCondIsEqS
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        internal PCondIsEqSXXQ (PrimitiveIsEqS predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqSXXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqSXXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqSXXQ";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

#if NIL
    class PCondIsEqA0A : PCondIsEqA0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqA0A (PrimitiveIsEqA0A predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqA0A predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqA0A1) ? PCondIsEqA0A1.Make ((PrimitiveIsEqA0A1) predicate, consequent, alternative) :
                new PCondIsEqA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA0A.EvalStep");
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            ObjectModel.Eq (out answer, environment.Argument0Value, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqA0A1 : PCondIsEqA0A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqA0A1 (PrimitiveIsEqA0A1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqA0A1 predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondIsEqA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA0A1.EvalStep");
#endif
            ObjectModel.Eq (out answer, environment.Argument0Value, environment.Argument1Value);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }


    class PCondIsEqA0Q : PCondIsEqA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PCondIsEqA0Q (PrimitiveIsEqA0Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;

        }

        public static SCode Make (PrimitiveIsEqA0Q predicate, SCode consequent, SCode alternative)
        {
            return
                 new PCondIsEqA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA0Q.EvalStep");
#endif
            ObjectModel.Eq (out answer, environment.Argument0Value, this.rand1Value);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqA1 : PCondIsEqA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqA1 (PrimitiveIsEqA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveIsEqA1 predicate, SCode consequent, SCode alternative)
        {
            return
                 new PCondIsEqA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA1.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, environment.Argument1Value, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }


    class PCondIsEqCarL : PCondIsEqCar
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PCondIsEqCarL (PrimitiveIsEqCarL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Name = predicate.rand0Name;
            this.rand0Depth = predicate.rand0Depth;
            this.rand0Offset = predicate.rand0Offset;
        }


        public static SCode Make (PrimitiveIsEqCarL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCarA) ? PCondIsEqCarA.Make ((PrimitiveIsEqCarA) predicate, consequent, alternative)
                : new PCondIsEqCarL (predicate, consequent, alternative);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqCarL.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqCarL.EvalStep.1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }


            object ev0a;
            if (environment.FastLexicalRef (out ev0a, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            Cons ev0 = ev0a as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0.Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqCarA : PCondIsEqCarL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqCarA (PrimitiveIsEqCarA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsEqCarA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCarA0) ? PCondIsEqCarA0.Make ((PrimitiveIsEqCarA0) predicate, consequent, alternative)
                : new PCondIsEqCarA (predicate, consequent, alternative);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCondIsEqCarA.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ((Cons) environment.ArgumentValue(this.rand0Offset)).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqCarA0 : PCondIsEqCarA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqCarA0 (PrimitiveIsEqCarA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCarA0 predicate, SCode consequent, SCode alternative)
        {
            return 
                (predicate is PrimitiveIsEqCarA0Car) ? PCondIsEqCarA0Car.Make ((PrimitiveIsEqCarA0Car) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqCarA0L) ? PCondIsEqCarA0L.Make ((PrimitiveIsEqCarA0L) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqCarA0Q) ? PCondIsEqCarA0Q.Make ((PrimitiveIsEqCarA0Q) predicate, consequent, alternative) :
                //(consequent is LexicalVariable) ? Unimplemented() :
                //(consequent is Quotation) ? Unimplemented() :
                //(alternative is LexicalVariable) ? Unimplemented():
                (alternative is Quotation) ? PCondIsEqCarA0SSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqCarA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCondIsEqCarA0.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ((Cons)environment.Argument0Value).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqCarA0Car : PCondIsEqCarA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqCarA0Car (PrimitiveIsEqCarA0Car predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCarA0Car predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCarA0CarL) ? PCondIsEqCarA0CarL.Make ((PrimitiveIsEqCarA0CarL) predicate, consequent, alternative) :
                new PCondIsEqCarA0Car (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();

        }
    }

    class PCondIsEqCarA0CarL : PCondIsEqCarA0Car
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqCarA0CarL (PrimitiveIsEqCarA0CarL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCarA0CarL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCarA0CarA) ? PCondIsEqCarA0CarA.Make ((PrimitiveIsEqCarA0CarA) predicate, consequent, alternative) :
                new PCondIsEqCarA0CarL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();

        }
    }

    class PCondIsEqCarA0CarA : PCondIsEqCarA0CarL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqCarA0CarA (PrimitiveIsEqCarA0CarA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCarA0CarA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCarA0CarA1) ? PCondIsEqCarA0CarA1.Make ((PrimitiveIsEqCarA0CarA1) predicate, consequent, alternative) :
                new PCondIsEqCarA0CarA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();

        }
    }

    class PCondIsEqCarA0CarA1 : PCondIsEqCarA0CarA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqCarA0CarA1 (PrimitiveIsEqCarA0CarA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCarA0CarA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (alternative is Quotation) ? PCondIsEqCarA0CarA1SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqCarA0CarA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("PCondIsEqCarA0CarA1");
#endif
            Cons ev1 = environment.Argument1Value as Cons;
            Cons ev0 = environment.Argument0Value as Cons;
            ObjectModel.Eq (out answer, ev0.Car, ev1.Car);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqCarA0CarA1.EvalStep.1";
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqCarA0CarA1.EvalStep.1";
#endif
                expression = this.consequent;
                return true;
            }
            

        }
    }

    class PCondIsEqCarA0CarA1SQ : PCondIsEqCarA0CarA1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondIsEqCarA0CarA1SQ (PrimitiveIsEqCarA0CarA1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsEqCarA0CarA1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqCarA0CarA1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCarA0CarA1SQ");
#endif
            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ((Cons) environment.Argument1Value).Car);

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqCarA0CarA1.EvalStep.1";
#endif
                expression = this.consequent;
                return true;
            }
        }
    }



    sealed class PCondIsEqCarA0SSQ : PCondIsEqCarA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        PCondIsEqCarA0SSQ (PrimitiveIsEqCarA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsEqCarA0 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqCarA0SSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqCarA0SSQ.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqCarA0L : PCondIsEqCarA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsEqCarA0L (PrimitiveIsEqCarA0L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }


        public static SCode Make (PrimitiveIsEqCarA0L predicate, SCode consequent, SCode alternative)
        {
            return 
                //(predicate is PrimitiveIsEqCarA0A) ? Unimplemented():
                (predicate is PrimitiveIsEqCarA0L1) ? PCondIsEqCarA0L1.Make ((PrimitiveIsEqCarA0L1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCondIsEqCarA0LL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented():
                (alternative is LexicalVariable) ? Unimplemented():
                (alternative is Quotation) ? Unimplemented():
                new PCondIsEqCarA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCarA0L.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqCarA0L1 : PCondIsEqCarA0L
    {


        protected PCondIsEqCarA0L1 (PrimitiveIsEqCarA0L1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsEqCarA0L1 predicate, SCode consequent, SCode alternative)
        {
            return
                 new PCondIsEqCarA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();

        }
    }

    class PCondIsEqCarA0LL : PCondIsEqCarA0L
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsEqCarA0LL (PrimitiveIsEqCarA0L predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }


        public static SCode Make (PrimitiveIsEqCarA0L predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqCarA0LA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqCarA0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCarA0LL.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCondIsEqCarA0LA : PCondIsEqCarA0LL
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqCarA0LA (PrimitiveIsEqCarA0L predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsEqCarA0L predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqCarA0LA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqCarA0LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqCarA0LA.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCondIsEqCarA0LA0 : PCondIsEqCarA0LA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqCarA0LA0 (PrimitiveIsEqCarA0L predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCarA0L predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Combination1LCdrL1) ? PCondIsEqCarA0LA0Fragment0.Make (predicate, consequent, (Combination1LCdrL1) alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqCarA0LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCarA0LA0.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;
            ObjectModel.Eq (out answer, ((Cons) ev0).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = ev0;
                return false;
            }
        }
    }

    sealed class PCondIsEqCarA0LA0Fragment0 : PCondIsEqCarA0LA0
    {
        public readonly object altRand0Name;
        public readonly int altRand0Offset;
        public readonly object altRatorName;
        public readonly int altRatorDepth;
        public readonly int altRatorOffset;
        PCondIsEqCarA0LA0Fragment0 (PrimitiveIsEqCarA0L predicate, Argument0 consequent, Combination1LCdrL1 alternative)
            : base (predicate, consequent, alternative)
        {
            this.altRand0Name = alternative.rand0Name;
            this.altRand0Offset = alternative.rand0Offset;
            this.altRatorName = alternative.ratorName;
            this.altRatorDepth = alternative.ratorDepth;
            this.altRatorOffset = alternative.ratorOffset;
        }

        public static SCode Make (PrimitiveIsEqCarA0L predicate, Argument0 consequent, Combination1LCdrL1 alternative)
        {
            return
                new PCondIsEqCarA0LA0Fragment0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCarA0LA0Fragment0.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;
            object ev0Car = ((Cons) ev0).Car;
            //             ObjectModel.Eq (out answer, ((Cons) ev0).Car, ev1);
            if (((ev0Car == null) && (ev1 == null))
                || ((ev1 != null) &&
                    ((ev0Car == ev1)
                     || ((ev0Car is Int32 && ev1 is Int32) && ((int) ev0Car == (int) ev1))
                     || ((ev0Car is char && ev1 is char) && ((char) ev0Car == (char) ev1))
                     || ((ev0Car is bool && ev1 is bool) && ((bool) ev0Car == (bool) ev1))))) {
                answer = ev0;
                return false;
            }
            else {
                object evarg;
                if (environment.FastLexicalRef1 (out evarg, this.altRand0Name, this.altRand0Offset))
                    throw new NotImplementedException ();

                object evop;
                if (environment.FastLexicalRef (out evop, this.altRatorName, this.altRatorDepth, this.altRatorOffset))
                    throw new NotImplementedException ();

                return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) evarg).Cdr);
            }
        }
    }

    class PCondIsEqCarA0Q : PCondIsEqCarA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PCondIsEqCarA0Q (PrimitiveIsEqCarA0Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }


        public static SCode Make (PrimitiveIsEqCarA0Q predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqCarA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCarA1 : PCondIsEqCarA
    {
        protected PCondIsEqCarA1 (PrimitiveIsEqCarA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsEqCarA1 predicate, SCode consequent, SCode alternative)
        {
            return Unimplemented();
                 //(rand1 is LexicalVariable) ? PCondIsEqCarA1L.Make (rator, rand0, (LexicalVariable) rand1)
                 //: (rand1 is Quotation) ? PCondIsEqCarA1Q.Make (rator, rand0, (Quotation) rand1)
                 //: new PCondIsEqCarA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCarA1L : PCondIsEqCarA1
    {
        //public readonly object randName;
        //public readonly int randDepth;
        //public readonly int randOffset;

        protected PCondIsEqCarA1L (PrimitiveIsEqCarA1L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            //this.randName = rand1.Name;
            //this.randDepth = rand1.Depth;
            //this.randOffset = rand1.Offset;
        }


        public static SCode Make (PrimitiveIsEqCarA1L predicate, SCode consequent, SCode alternative)
        {
            return Unimplemented();
                //(rand1 is Argument) ? Unimplemented ()
                //: (rand1 is LexicalVariable1) ? Unimplemented ()
                //: new PCondIsEqCarA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCarA1Q : PCondIsEqCarA1
    {
        //public readonly object rand1Value;

        protected PCondIsEqCarA1Q (PrimitiveIsEqCarA1Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }


        public static SCode Make (PrimitiveIsEqCarA1Q predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqCarA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();

        }
    }

    class PCondIsEqCarLL : PCondIsEqCarL
    {
        //public readonly object randName;
        //public readonly int randDepth;
        //public readonly int randOffset;

        protected PCondIsEqCarLL (PrimitiveIsEqCarLL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }


        public static SCode Make (PrimitiveIsEqCarLL predicate, SCode consequent, SCode alternative)
        {
            return Unimplemented();

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCarLQ : PCondIsEqCarL
    {
       // public readonly object rand1Value;

        protected PCondIsEqCarLQ (PrimitiveIsEqCarLQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {


        }


        public static SCode Make (PrimitiveIsEqCarLQ predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqCarLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCarSL : PCondIsEqCar
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsEqCarSL (PrimitiveIsEqCarSL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsEqCarSL predicate, SCode consequent, SCode alternative)
        {
            return Unimplemented ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0Arg;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            Cons ev0c = ev0 as Cons;
            ObjectModel.Eq (out answer, ev0c.Car, ev1);
            return false;
        }
    }

    class PCondIsEqCarSQ : PCondIsEqCar
    {
        public readonly object rand1Value;

        protected PCondIsEqCarSQ (PrimitiveIsEqCarSQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveIsEqCarSQ predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqCarSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PCondIsEqCaar : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly SCode rand0Arg;

        protected PCondIsEqCaar (PrimitiveIsEqCaar predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Arg = predicate.rand0Arg;
        }

        public static SCode Make (PrimitiveIsEqCaar predicate, SCode consequent, SCode alternative)
        {
            return 
                (predicate is PrimitiveIsEqCaarL) ? PCondIsEqCaarL.Make ((PrimitiveIsEqCaarL) predicate, consequent, alternative)
                //(predicate is PrimitiveIsEqCaarQ) ? Unimplemented()
                //: (predicate is PrimitiveIsEqCaarSL) ? 
                : (predicate is PrimitiveIsEqCaarSQ) ? PCondIsEqCaarSQ.Make ((PrimitiveIsEqCaarSQ) predicate, consequent, alternative)
                //: (consequent is LexicalVariable) ? Unimplemented()
                //: (consequent is Quotation) ? Unimplemented()
                //: (alternative is LexicalVariable) ? Unimplemented()
                //: (alternative is Quotation) ? Unimplemented()
                : new PCondIsEqCaar (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ("PCondIsEqCaar.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0Arg;
            env = environment;
            object ev0temp;
            while (unev.EvalStep (out ev0temp, ref unev, ref env)) { };
            if (ev0temp == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            Cons ev0 = ev0temp as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0.Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqCaarL : PCondIsEqCaar
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PCondIsEqCaarL (PrimitiveIsEqCaarL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Name = predicate.rand0Name;
            this.rand0Depth = predicate.rand0Depth;
            this.rand0Offset = predicate.rand0Offset;
        }


        public static SCode Make (PrimitiveIsEqCaarL predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCaarA) ? PCondIsEqCaarA.Make ((PrimitiveIsEqCaarA) predicate, consequent, alternative)
                : new PCondIsEqCaarL (predicate, consequent, alternative);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqCaarL.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqCaarL.EvalStep.1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }


            object ev0a;
            if (environment.FastLexicalRef (out ev0a, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            Cons ev0 = ev0a as Cons;
            if (ev0 == null) throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0.Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqCaarA : PCondIsEqCaarL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqCaarA (PrimitiveIsEqCaarA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsEqCaarA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqCaarA0) ? PCondIsEqCaarA0.Make ((PrimitiveIsEqCaarA0) predicate, consequent, alternative)
                : new PCondIsEqCaarA (predicate, consequent, alternative);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCondIsEqCaarA.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ((Cons) environment.ArgumentValue(this.rand0Offset)).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqCaarA0 : PCondIsEqCaarA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqCaarA0 (PrimitiveIsEqCaarA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCaarA0 predicate, SCode consequent, SCode alternative)
        {
            return 
                (predicate is PrimitiveIsEqCaarA0L) ? PCondIsEqCaarA0L.Make ((PrimitiveIsEqCaarA0L) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqCaarA0Q) ? PCondIsEqCaarA0Q.Make ((PrimitiveIsEqCaarA0Q) predicate, consequent, alternative) :
                //(consequent is LexicalVariable) ? Unimplemented() :
                //(consequent is Quotation) ? Unimplemented() :
                //(alternative is LexicalVariable) ? Unimplemented():
                (alternative is Quotation) ? PCondIsEqCaarA0SSQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqCaarA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PCondIsEqCaarA0.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ((Cons)environment.Argument0Value).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }



    sealed class PCondIsEqCaarA0SSQ : PCondIsEqCaarA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        PCondIsEqCaarA0SSQ (PrimitiveIsEqCaarA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsEqCaarA0 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqCaarA0SSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqCaarA0SSQ.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqCaarA0L : PCondIsEqCaarA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsEqCaarA0L (PrimitiveIsEqCaarA0L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }


        public static SCode Make (PrimitiveIsEqCaarA0L predicate, SCode consequent, SCode alternative)
        {
            return 
                //(predicate is PrimitiveIsEqCaarA0A) ? Unimplemented():
                (predicate is PrimitiveIsEqCaarA0L1) ? PCondIsEqCaarA0L1.Make ((PrimitiveIsEqCaarA0L1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCondIsEqCaarA0LL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented():
                (alternative is LexicalVariable) ? Unimplemented():
                (alternative is Quotation) ? Unimplemented():
                new PCondIsEqCaarA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ("PCondIsEqCaarA0L.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqCaarA0L1 : PCondIsEqCaarA0L
    {


        protected PCondIsEqCaarA0L1 (PrimitiveIsEqCaarA0L1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsEqCaarA0L1 predicate, SCode consequent, SCode alternative)
        {
            return
                 new PCondIsEqCaarA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();

        }
    }

    class PCondIsEqCaarA0LL : PCondIsEqCaarA0L
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsEqCaarA0LL (PrimitiveIsEqCaarA0L predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }


        public static SCode Make (PrimitiveIsEqCaarA0L predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqCaarA0LA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqCaarA0LL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ("PCondIsEqCaarA0LL.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCondIsEqCaarA0LA : PCondIsEqCaarA0LL
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqCaarA0LA (PrimitiveIsEqCaarA0L predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsEqCaarA0L predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqCaarA0LA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqCaarA0LA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqCaarA0LA.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ((Cons) environment.Argument0Value).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    class PCondIsEqCaarA0LA0 : PCondIsEqCaarA0LA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqCaarA0LA0 (PrimitiveIsEqCaarA0L predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqCaarA0L predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Combination1LCdrL1) ? PCondIsEqCaarA0LA0Fragment0.Make (predicate, consequent, (Combination1LCdrL1) alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqCaarA0LA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ("PCondIsEqCaarA0LA0.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;
            ObjectModel.Eq (out answer, ((Cons) ev0).Car, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = ev0;
                return false;
            }
        }
    }

    sealed class PCondIsEqCaarA0LA0Fragment0 : PCondIsEqCaarA0LA0
    {
        public readonly object altRand0Name;
        public readonly int altRand0Offset;
        public readonly object altRatorName;
        public readonly int altRatorDepth;
        public readonly int altRatorOffset;
        PCondIsEqCaarA0LA0Fragment0 (PrimitiveIsEqCaarA0L predicate, Argument0 consequent, Combination1LCdrL1 alternative)
            : base (predicate, consequent, alternative)
        {
            this.altRand0Name = alternative.rand0Name;
            this.altRand0Offset = alternative.rand0Offset;
            this.altRatorName = alternative.ratorName;
            this.altRatorDepth = alternative.ratorDepth;
            this.altRatorOffset = alternative.ratorOffset;
        }

        public static SCode Make (PrimitiveIsEqCaarA0L predicate, Argument0 consequent, Combination1LCdrL1 alternative)
        {
            return
                new PCondIsEqCaarA0LA0Fragment0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ("PCondIsEqCaarA0LA0Fragment0.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;
            object ev0Car = ((Cons) ev0).Car;
            //             ObjectModel.Eq (out answer, ((Cons) ev0).Car, ev1);
            if (((ev0Car == null) && (ev1 == null))
                || ((ev1 != null) &&
                    ((ev0Car == ev1)
                     || ((ev0Car is Int32 && ev1 is Int32) && ((int) ev0Car == (int) ev1))
                     || ((ev0Car is char && ev1 is char) && ((char) ev0Car == (char) ev1))
                     || ((ev0Car is bool && ev1 is bool) && ((bool) ev0Car == (bool) ev1))))) {
                answer = ev0;
                return false;
            }
            else {
                object evarg;
                if (environment.FastLexicalRef1 (out evarg, this.altRand0Name, this.altRand0Offset))
                    throw new NotImplementedException ();

                object evop;
                if (environment.FastLexicalRef (out evop, this.altRatorName, this.altRatorDepth, this.altRatorOffset))
                    throw new NotImplementedException ();

                return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) evarg).Cdr);
            }
        }
    }

    class PCondIsEqCaarA0Q : PCondIsEqCaarA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PCondIsEqCaarA0Q (PrimitiveIsEqCaarA0Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }


        public static SCode Make (PrimitiveIsEqCaarA0Q predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqCaarA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCaarA1 : PCondIsEqCaarA
    {
        protected PCondIsEqCaarA1 (PrimitiveIsEqCaarA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsEqCaarA1 predicate, SCode consequent, SCode alternative)
        {
            return Unimplemented();
                 //(rand1 is LexicalVariable) ? PCondIsEqCaarA1L.Make (rator, rand0, (LexicalVariable) rand1)
                 //: (rand1 is Quotation) ? PCondIsEqCaarA1Q.Make (rator, rand0, (Quotation) rand1)
                 //: new PCondIsEqCaarA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCaarA1L : PCondIsEqCaarA1
    {
        //public readonly object randName;
        //public readonly int randDepth;
        //public readonly int randOffset;

        protected PCondIsEqCaarA1L (PrimitiveIsEqCaarA1L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            //this.randName = rand1.Name;
            //this.randDepth = rand1.Depth;
            //this.randOffset = rand1.Offset;
        }


        public static SCode Make (PrimitiveIsEqCaarA1L predicate, SCode consequent, SCode alternative)
        {
            return Unimplemented();
                //(rand1 is Argument) ? Unimplemented ()
                //: (rand1 is LexicalVariable1) ? Unimplemented ()
                //: new PCondIsEqCaarA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCaarA1Q : PCondIsEqCaarA1
    {
        //public readonly object rand1Value;

        protected PCondIsEqCaarA1Q (PrimitiveIsEqCaarA1Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }


        public static SCode Make (PrimitiveIsEqCaarA1Q predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqCaarA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();

        }
    }

    class PCondIsEqCaarLL : PCondIsEqCaarL
    {
        //public readonly object randName;
        //public readonly int randDepth;
        //public readonly int randOffset;

        protected PCondIsEqCaarLL (PrimitiveIsEqCaarLL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }


        public static SCode Make (PrimitiveIsEqCaarLL predicate, SCode consequent, SCode alternative)
        {
            return Unimplemented();

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCaarLQ : PCondIsEqCaarL
    {
       // public readonly object rand1Value;

        protected PCondIsEqCaarLQ (PrimitiveIsEqCaarLQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {


        }


        public static SCode Make (PrimitiveIsEqCaarLQ predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqCaarLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCaarSL : PCondIsEqCaar
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsEqCaarSL (PrimitiveIsEqCaarSL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsEqCaarSL predicate, SCode consequent, SCode alternative)
        {
            return Unimplemented ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
      throw new NotImplementedException();
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Control unev = this.rand0Arg;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            Cons ev0c = ev0 as Cons;
            ObjectModel.Eq (out answer, ev0c.Car, ev1);
            return false;
        }
    }

    class PCondIsEqCaarSQ : PCondIsEqCaar
    {
        public readonly object rand1Value;

        protected PCondIsEqCaarSQ (PrimitiveIsEqCaarSQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveIsEqCaarSQ predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqCaarSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }


    class PCondIsEqQ : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;

        protected PCondIsEqQ (PrimitiveIsEqQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Value = predicate.rand0Value;
        }

        public static SCode Make (PrimitiveIsEqQ predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqQ.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSL : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsEqSL (PrimitiveIsEqSL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsEqSL predicate, SCode consequent, SCode alternative)
        {
            return 
                (predicate is PrimitiveIsEqSA) ? PCondIsEqSA.Make ((PrimitiveIsEqSA) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqSL1) ? PCondIsEqSL1.Make ((PrimitiveIsEqSL1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? PCondIsEqSLL.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqSLQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? PCondIsEqSLSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                (alternative is Quotation) ? Unimplemented():
                new PCondIsEqSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSA : PCondIsEqSL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA (PrimitiveIsEqSA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqSA0) ? PCondIsEqSA0.Make ((PrimitiveIsEqSA0) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqSA1) ? PCondIsEqSA1.Make ((PrimitiveIsEqSA1) predicate, consequent, alternative) :
                (consequent is LexicalVariable) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented() :
                (alternative is LexicalVariable) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSA0 : PCondIsEqSA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA0 (PrimitiveIsEqSA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsEqSA0L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqSA0Q.Make (predicate, (Quotation) consequent, alternative) :
                //(alternative is LexicalVariable) ? Unimplemented () :
                //(alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSA0L : PCondIsEqSA0
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA0L (PrimitiveIsEqSA0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqSA0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSA0A : PCondIsEqSA0L
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA0A (PrimitiveIsEqSA0 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA0 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqSA0A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSA0A0 : PCondIsEqSA0A
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA0A0 (PrimitiveIsEqSA0 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA0 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqSA0A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqSA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCondIsEqSA0A0Q : PCondIsEqSA0A0
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif

        PCondIsEqSA0A0Q (PrimitiveIsEqSA0 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA0 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCondIsEqSA0A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);

#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);

#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSA0Q : PCondIsEqSA0
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA0Q (PrimitiveIsEqSA0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA0 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSA1 : PCondIsEqSA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA1 (PrimitiveIsEqSA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsEqSA1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqSA1Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqSA1SQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSA1L : PCondIsEqSA1
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA1L (PrimitiveIsEqSA1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqSA1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented() :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSA1A : PCondIsEqSA1L
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA1A (PrimitiveIsEqSA1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqSA1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSA1A0 : PCondIsEqSA1A
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA1A0 (PrimitiveIsEqSA1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqSA1A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqSA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCondIsEqSA1A0Q : PCondIsEqSA1A0
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif

        PCondIsEqSA1A0Q (PrimitiveIsEqSA1 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA1 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCondIsEqSA1A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);

#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);

#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSA1Q : PCondIsEqSA1
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA1Q (PrimitiveIsEqSA1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented():
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCondIsEqSA1SQ : PCondIsEqSA1
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        PCondIsEqSA1SQ (PrimitiveIsEqSA1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSA1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqSA1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG

#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }



    class PCondIsEqSL1 : PCondIsEqSL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSL1 (PrimitiveIsEqSL1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? PCondIsEqSL1L.Make (predicate, (LexicalVariable) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented() :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSL1L : PCondIsEqSL1
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected PCondIsEqSL1L (PrimitiveIsEqSL1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsEqSL1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqSL1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqSL1LQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqSL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
 
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSL1A : PCondIsEqSL1L
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSL1A (PrimitiveIsEqSL1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqSL1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented() :
                new PCondIsEqSL1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSL1A0 : PCondIsEqSL1A
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSL1A0 (PrimitiveIsEqSL1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL1 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqSL1A0Q.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsEqSL1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCondIsEqSL1A0Q : PCondIsEqSL1A0
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        PCondIsEqSL1A0Q (PrimitiveIsEqSL1 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsEqSL1 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCondIsEqSL1A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqSL1A0Q.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object temp;
            ObjectModel.Eq (out temp, ev0, ev1);

            answer = ((temp is bool) && (bool) temp == false) ? this.alternativeValue : environment.Argument0Value;
            return false;
        }
    }

    sealed class PCondIsEqSL1LQ : PCondIsEqSL1L
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        PCondIsEqSL1LQ (PrimitiveIsEqSL1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsEqSL1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return
                new PCondIsEqSL1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqSL1LQ.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
                answer = this.alternativeValue;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
            }
            return false;
        }
    }

    class PCondIsEqSLL : PCondIsEqSL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif


        protected PCondIsEqSLL (PrimitiveIsEqSL predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqSLA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is LexicalVariable1) ? Unimplemented() :
                (alternative is LexicalVariable) ? Unimplemented() :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSLA : PCondIsEqSLL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif


        protected PCondIsEqSLA (PrimitiveIsEqSL predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqSLA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? Unimplemented () :
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSLA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSLA0 : PCondIsEqSLA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif


        protected PCondIsEqSLA0 (PrimitiveIsEqSL predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                new PCondIsEqSLA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqSLA0.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object temp;
            ObjectModel.Eq (out temp, ev0, ev1);

            if ((temp is bool) && (bool) temp == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }



    class PCondIsEqSLQ : PCondIsEqSL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSLQ (PrimitiveIsEqSL predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL predicate, Quotation consequent, SCode alternative)
        {
            return 
                (alternative is LexicalVariable) ? Unimplemented():
                (alternative is Quotation) ? Unimplemented():
                new PCondIsEqSLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSLSL : PCondIsEqSL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected PCondIsEqSLSL (PrimitiveIsEqSL predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name ;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveIsEqSL predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? PCondIsEqSLSA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is LexicalVariable1) ? Unimplemented () :
                new PCondIsEqSLSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    class PCondIsEqSLSA : PCondIsEqSLSL
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSLSA (PrimitiveIsEqSL predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqSLSA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? Unimplemented () :
                new PCondIsEqSLSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    sealed class PCondIsEqSLSA0 : PCondIsEqSLSA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        PCondIsEqSLSA0 (PrimitiveIsEqSL predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqSL predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsEqSLSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("PCondIsEqSL.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }



    class PCondIsEqSQ : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PCondIsEqSQ (PrimitiveIsEqSQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveIsEqSQ predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqSQ.EvalStep");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }


            ObjectModel.Eq (out answer, ev0, ev1);

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }
#endif

}
