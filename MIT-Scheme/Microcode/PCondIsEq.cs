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
}
