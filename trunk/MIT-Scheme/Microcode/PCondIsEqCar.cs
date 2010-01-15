using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PCondIsEqCar : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
        protected readonly Type rand0InnerType;
#endif
        public readonly SCode rand0Inner;

        protected PCondIsEqCar (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Inner = ((PrimitiveCar) predicate.Operand0).Operand;
#if DEBUG
            this.rand0InnerType = this.rand0Inner.GetType();
#endif
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (((PrimitiveCar) predicate.Operand0).Operand is Argument) ? PCondIsEqCarA.Make (predicate, consequent, alternative) :
                new PCondIsEqCar (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0Inner);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0InnerType);
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
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0Inner;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqCar";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqCar";
#endif
            if (!ObjectModel.SchemeEq (((Cons)ev0).Car, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqCar";

#endif
                expression = this.consequent;

            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqCar";

#endif
                expression = this.alternative;
            }
            answer = null; // happy compiler
            return true;
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
        public readonly int rand0Offset;
        protected PCondIsEqCarA (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Offset = ((Argument) ((PrimitiveCar) predicate.Operand0).Operand).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                 (((PrimitiveCar) predicate.Operand0).Operand is Argument0) ? PCondIsEqCarA0.Make (predicate, consequent, alternative) :
                //(predicate.Operand0 is Argument0) ? PCondIsEqA0.Make (predicate, consequent, alternative) :
                //(predicate.Operand1 is Argument) ? PCondIsEqAA.Make (predicate, consequent, alternative) :
                //(predicate.Operand1 is Quotation) ? PCondIsEqAQ.Make (predicate, consequent, alternative) :
                //(predicate.Operand1 is StaticVariable) ? PCondIsEqAS.Make (predicate, consequent, alternative) :
                //(consequent is Argument) ? PCondIsEqAXA.Make (predicate, (Argument) consequent, alternative) :
                //(consequent is Quotation) ? PCondIsEqAXQ.Make (predicate, (Quotation) consequent, alternative) :
                //(consequent is StaticVariable) ? Unimplemented () :
                //(alternative is Argument) ? PCondIsEqAXXA.Make (predicate, consequent, (Argument) alternative) :
                //(alternative is Quotation) ? PCondIsEqAXXQ.Make (predicate, consequent, (Quotation) alternative) :
                //(alternative is StaticVariable) ? Unimplemented () :
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
#if DEBUG
            SCode.location = "PCondIsEqCarA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqCarA";
#endif
            if (!ObjectModel.SchemeEq (((Cons)ev0).Car, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null; // happy compiler
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
        protected PCondIsEqCarA0 (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                //(predicate.Operand1 is Argument) ? PCondIsEqA0A.Make (predicate, consequent, alternative) :
                //(predicate.Operand1 is Quotation) ? PCondIsEqA0Q.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is StaticVariable) ? PCondIsEqCarA0S.Make (predicate, consequent, alternative) :
                //(consequent is Argument) ? PCondIsEqA0XA.Make (predicate, (Argument) consequent, alternative) :
                //(consequent is Quotation) ? PCondIsEqA0XQ.Make (predicate, (Quotation) consequent, alternative) :
                //(consequent is StaticVariable) ? Unimplemented () :
                //(alternative is Argument) ? PCondIsEqA0XXA.Make (predicate, consequent, (Argument) alternative) :
                //(alternative is Quotation) ? PCondIsEqA0XXQ.Make (predicate, consequent, (Quotation) alternative) :
                //(alternative is StaticVariable) ? PCondIsEqA0XXS.Make (predicate, consequent, (StaticVariable) alternative) :
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
#if DEBUG
            SCode.location = "PCondIsEqCarA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqCarA0";
#endif
            if (!ObjectModel.SchemeEq (((Cons)ev0).Car, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null; // happy compiler
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
        protected PCondIsEqCarA0S (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                //(consequent is Argument) ? PCondIsEqA0SA.Make (predicate, (Argument) consequent, alternative) :
                //(consequent is Quotation) ? PCondIsEqA0SQ.Make (predicate, (Quotation) consequent, alternative) :
                //(consequent is StaticVariable) ? PCondIsEqA0SS.Make (predicate, (StaticVariable) consequent, alternative) :
                //(alternative is Argument) ? PCondIsEqA0SXA.Make (predicate, consequent, (Argument) alternative) :
                //(alternative is Quotation) ? PCondIsEqA0SXQ.Make (predicate, consequent, (Quotation) alternative) :
                //(alternative is StaticVariable) ? PCondIsEqA0SXS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondIsEqCarA0S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqCarA0S";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqCarA0S";
#endif
            if (!ObjectModel.SchemeEq (((Cons)ev0).Car, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }


}
