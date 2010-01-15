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

        protected PCondIsEq (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand0 is Argument) ? PCondIsEqA.Make (predicate, consequent, alternative) :
                (predicate.Operand0 is PrimitiveCar) ? PCondIsEqCar.Make (predicate, consequent, alternative) :
                (predicate.Operand0 is Quotation) ? PCondIsEqQ.Make (predicate, consequent, alternative) :
                (predicate.Operand0 is StaticVariable) ? PCondIsEqS.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is Argument) ? PCondIsEqXA.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is Quotation) ? PCondIsEqXQ.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is StaticVariable) ? PCondIsEqXS.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? PCondIsEqXXA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqXXQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? PCondIsEqXXXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqXXXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
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
#if DEBUG
            SCode.location = "PCondIsEq";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEq";
#endif
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
            if (ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
            }
            answer = null; // happy compiler
            return true;
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
        protected PCondIsEqA (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Offset = ((Argument) predicate.Operand0).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand0 is Argument0) ? PCondIsEqA0.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is Argument) ? PCondIsEqAA.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is Quotation) ? PCondIsEqAQ.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is StaticVariable) ? PCondIsEqAS.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? PCondIsEqAXA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqAXQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? PCondIsEqAXXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqAXXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
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
#if DEBUG
            SCode.location = "PCondIsEqA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
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
    class PCondIsEqA0 : PCondIsEqA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqA0 (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument) ? PCondIsEqA0A.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is Quotation) ? PCondIsEqA0Q.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is StaticVariable) ? PCondIsEqA0S.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? PCondIsEqA0XA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqA0XQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? PCondIsEqA0XXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqA0XXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCondIsEqA0XXS.Make (predicate, consequent, (StaticVariable) alternative) :
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
#if DEBUG
            SCode.location = "PCondIsEqA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
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
    class PCondIsEqA0A : PCondIsEqA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;
        protected PCondIsEqA0A (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? Unimplemented () :
                (consequent is Argument) ? PCondIsEqA0AA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqA0AQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqA0AXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0A";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0A";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
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
    class PCondIsEqA0AA : PCondIsEqA0A
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCondIsEqA0AA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqA0AA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? PCondIsEqA0AAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqA0AA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0AA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0AA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0AA0 : PCondIsEqA0AA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqA0AA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCondIsEqA0AA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqA0AA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0AA0";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0AA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0AA0A : PCondIsEqA0AA0
    {
        public readonly int alternativeOffset;
        protected PCondIsEqA0AA0A (PrimitiveCombination2 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented () :
                new PCondIsEqA0AA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0AA0A";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0AA0A";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0AAA : PCondIsEqA0AA
    {
        public readonly int alternativeOffset;
        protected PCondIsEqA0AAA (PrimitiveCombination2 predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqA0AAA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqA0AAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0AAA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0AAA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0AAA0 : PCondIsEqA0AAA
    {
        protected PCondIsEqA0AAA0 (PrimitiveCombination2 predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, Argument0 alternative)
        {
            return
                new PCondIsEqA0AAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0AAA0";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0AAA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0AQ : PCondIsEqA0A
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqA0AQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqA0AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0AQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0AQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0AXQ : PCondIsEqA0A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqA0AXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqA0AXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0AXQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0AXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
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
    class PCondIsEqA0S : PCondIsEqA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        protected PCondIsEqA0S (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqA0SA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqA0SQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCondIsEqA0SS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCondIsEqA0SXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqA0SXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCondIsEqA0SXS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondIsEqA0S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0S";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0S";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
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
    class PCondIsEqA0SA : PCondIsEqA0S
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCondIsEqA0SA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqA0SA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqA0SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0SA";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0SA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0SA0 : PCondIsEqA0SA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqA0SA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? PCondIsEqA0SA0S.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondIsEqA0SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0SA0";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0SA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0SA0S : PCondIsEqA0SA0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected PCondIsEqA0SA0S (PrimitiveCombination2 predicate, Argument0 consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, StaticVariable alternative)
        {
            return
                new PCondIsEqA0SA0S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0SA0S";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0SA0S";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0SQ : PCondIsEqA0S
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqA0SQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqA0SQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqA0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0SQ";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0SQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0SQQ : PCondIsEqA0SQ
    {
        public readonly object alternativeValue;
        protected PCondIsEqA0SQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCondIsEqA0SQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0SQQ";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0SQQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0SS : PCondIsEqA0S
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;
        protected PCondIsEqA0SS (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqA0SS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0SS";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0SS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0SXA : PCondIsEqA0S
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsEqA0SXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqA0SXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqA0SXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0SXA";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0SXA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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
    class PCondIsEqA0SXA0 : PCondIsEqA0SXA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqA0SXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsEqA0SXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0SXA0";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0SXA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.Argument0Value;
                return false;
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
    class PCondIsEqA0SXQ : PCondIsEqA0S
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqA0SXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqA0SXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0SXQ";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0SXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
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
    class PCondIsEqA0SXS : PCondIsEqA0S
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCondIsEqA0SXS (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCondIsEqA0SXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0SXS";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0SXS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
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
    class PCondIsEqA0XA : PCondIsEqA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCondIsEqA0XA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqA0XA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqA0XA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqA0XA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqA0XA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0XA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0XA0 : PCondIsEqA0XA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqA0XA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqA0XA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqA0XA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqA0XA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0XA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0XQ : PCondIsEqA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqA0XQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCondIsEqA0XQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqA0XQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqA0XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqA0XQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqA0XQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0XQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0XQA : PCondIsEqA0XQ
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsEqA0XQA (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqA0XQA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqA0XQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqA0XQA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqA0XQA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0XQA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0XQA0 : PCondIsEqA0XQA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqA0XQA0 (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCondIsEqA0XQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqA0XQA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqA0XQA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0XQA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0XQQ : PCondIsEqA0XQ
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondIsEqA0XQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCondIsEqA0XQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqA0XQQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqA0XQQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0XQQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0Q : PCondIsEqA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;
        protected PCondIsEqA0Q (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = ((Quotation) predicate.Operand1).Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqA0QA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqA0QQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCondIsEqA0QS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCondIsEqA0QXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqA0QXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCondIsEqA0QXS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondIsEqA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0Q";
#endif
            object ev0 = environment.Argument0Value;

            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqA0Q";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
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
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0QA : PCondIsEqA0Q
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCondIsEqA0QA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqA0QA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqA0QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0QA";
#endif
            object ev0 = environment.Argument0Value;

            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqA0QA";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0QA0 : PCondIsEqA0QA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqA0QA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCondIsEqA0QA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqA0QA0Q.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqA0QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0QA";
#endif
            object ev0 = environment.Argument0Value;

            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqA0QA0";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0QA0A : PCondIsEqA0QA0
    {
        public readonly int alternativeOffset;
        protected PCondIsEqA0QA0A (PrimitiveCombination2 predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented () :
                new PCondIsEqA0QA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0QAA";
#endif
            object ev0 = environment.Argument0Value;

            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0QA0Q : PCondIsEqA0QA0
    {
        public readonly object alternativeValue;
        protected PCondIsEqA0QA0Q (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCondIsEqA0QA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0QAQ";
#endif
            object ev0 = environment.Argument0Value;

            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0QQ : PCondIsEqA0Q
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqA0QQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCondIsEqA0QQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqA0QQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqA0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0QQ";
#endif
            object ev0 = environment.Argument0Value;


            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqA0QQ";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0QQA : PCondIsEqA0QQ
    {
        public readonly int alternativeOffset;
        protected PCondIsEqA0QQA (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqA0QQA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqA0QQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0QQA";
#endif
            object ev0 = environment.Argument0Value;


            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0QQA0 : PCondIsEqA0QQA
    {
        protected PCondIsEqA0QQA0 (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCondIsEqA0QQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0QQA0";
#endif
            object ev0 = environment.Argument0Value;


            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0QQQ : PCondIsEqA0QQ
    {
        public readonly object alternativeValue;
        protected PCondIsEqA0QQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCondIsEqA0QQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0QQQ";
#endif
            object ev0 = environment.Argument0Value;


            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0QS : PCondIsEqA0Q
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;
        protected PCondIsEqA0QS (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? PCondIsEqA0QSS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondIsEqA0QS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0QS";
#endif
            object ev0 = environment.Argument0Value;


            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqA0QS";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0QSS : PCondIsEqA0QS
    {
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCondIsEqA0QSS (PrimitiveCombination2 predicate, StaticVariable consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, StaticVariable alternative)
        {
            return
                new PCondIsEqA0QSS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0QSS";
#endif
            object ev0 = environment.Argument0Value;

            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0QXA : PCondIsEqA0Q
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsEqA0QXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqA0QXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqA0QXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0QXA";
#endif
            object ev0 = environment.Argument0Value;


            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqA0QXQ";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0QXA0 : PCondIsEqA0QXA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqA0QXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsEqA0QXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0QXA0";
#endif
            object ev0 = environment.Argument0Value;

            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqA0QXA0";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0QXQ : PCondIsEqA0Q
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqA0QXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqA0QXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0QXQ";
#endif
            object ev0 = environment.Argument0Value;


            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqA0QXQ";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0QXS : PCondIsEqA0Q
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCondIsEqA0QXS (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCondIsEqA0QXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqA0QXS";
#endif
            object ev0 = environment.Argument0Value;


            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqA0QXQ";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0XXA : PCondIsEqA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        internal PCondIsEqA0XXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqA0XXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqA0XXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqA0XXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqA0XXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0XXA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqA0XXA";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqA0XXA0 : PCondIsEqA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        internal PCondIsEqA0XXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsEqA0XXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqA0XXA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqA0XXA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0XXA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqA0XXA0";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
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
        internal PCondIsEqA0XXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqA0XXQ (predicate, consequent, alternative);
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
#if DEBUG
            SCode.location = "PCondIsEqA0XXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0XXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqA0XXQ";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    sealed class PCondIsEqA0XXS : PCondIsEqA0
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        internal PCondIsEqA0XXS (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCondIsEqA0XXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqA0XXS";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqA0XXS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = "PCondIsEqA0XXS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqA0XXS";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqAA : PCondIsEqA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;
        protected PCondIsEqAA (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? PCondIsEqAA0.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? PCondIsEqAAA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqAAQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? PCondIsEqAAXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqAAXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqAA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqAA";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqAA";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqAA0 : PCondIsEqAA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqAA0 (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? Unimplemented () :
                (consequent is Quotation) ? PCondIsEqAA0Q.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqAA0XQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAA0";
#endif
            object ev1 = environment.Argument0Value;
            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqAA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqAA0";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqAA0";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    class PCondIsEqAA0Q : PCondIsEqAA0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqAA0Q (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqAA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAA0Q";
#endif
            object ev1 = environment.Argument0Value;
            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqAA0Q";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqAA0Q";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqAA0XQ : PCondIsEqAA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqAA0XQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqAA0XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAA0XQ";
#endif
            object ev1 = environment.Argument0Value;
            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqAA0XQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqAA0";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    class PCondIsEqAAA : PCondIsEqAA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCondIsEqAAA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? Unimplemented () :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqAAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAAA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqAAA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqAAA";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    class PCondIsEqAAQ : PCondIsEqAA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqAAQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqAAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAAQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqAAQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqAAQ";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqAAXA : PCondIsEqAA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsEqAAXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented () :
                new PCondIsEqAAXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAAXA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqAAXA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqAAXQ";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqAAXQ : PCondIsEqAA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqAAXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqAAXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAAXQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqAAXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqAAXQ";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
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
        protected PCondIsEqAQ (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = ((Quotation) predicate.Operand1).Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqAQA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqAQQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? PCondIsEqAQXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqAQXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAQ";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);


            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqAQ";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqAQ";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqAQA : PCondIsEqAQ
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCondIsEqAQA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqAQA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqAQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAQA";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqAQ";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqAQA0 : PCondIsEqAQA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqAQA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqAQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAQA0";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqAQA0";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqAQQ : PCondIsEqAQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqAQQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCondIsEqAQQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqAQQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqAQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAQQ";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqAQQ";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqAQQA : PCondIsEqAQQ
    {

        public readonly int alternativeOffset;
        protected PCondIsEqAQQA (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqAQQA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqAQQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAQQA";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);


            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqAQQA0 : PCondIsEqAQQA
    {

        protected PCondIsEqAQQA0 (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCondIsEqAQQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAQQA0";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);


            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqAQQQ : PCondIsEqAQQ
    {
#if DEBUG
        [NonSerialized]
#endif
        public readonly object alternativeValue;
        protected PCondIsEqAQQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCondIsEqAQQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAQQQ";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqAQXA : PCondIsEqAQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsEqAQXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqAQXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqAQXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAQXA";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqAQXA";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqAQXA0 : PCondIsEqAQXA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqAQXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsEqAQXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAQXA0";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqAQXA0";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqAQXQ : PCondIsEqAQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqAQXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqAQXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAQXQ";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);


            if (!ObjectModel.SchemeEq (ev0, this.rand1Value)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqAQXQ";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqAS : PCondIsEqA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        protected PCondIsEqAS (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqASA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqASQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? PCondIsEqASXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqASXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqAS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqAS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqAS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqAS";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqAS";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqASA : PCondIsEqAS
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCondIsEqASA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqASA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqASA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqASA";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqASA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqAS";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqASA0 : PCondIsEqASA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqASA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqASA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqASA0";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqASA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqASA0";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqASQ : PCondIsEqAS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqASQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCondIsEqASQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqASQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqASQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqASQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqASQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqASQ";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqASQA : PCondIsEqASQ
    {

        public readonly int alternativeOffset;
        protected PCondIsEqASQA (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqASQA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqASQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqASQA";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqASQA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqASQA0 : PCondIsEqASQA
    {

        protected PCondIsEqASQA0 (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCondIsEqASQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqASQA0";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqASQA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqASQQ : PCondIsEqASQ
    {
#if DEBUG
        [NonSerialized]
#endif
        public readonly object alternativeValue;
        protected PCondIsEqASQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCondIsEqASQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqASQQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqASQQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqASXA : PCondIsEqAS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsEqASXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqASXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqASXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqASXA";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqASXA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqASXA";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqASXA0 : PCondIsEqASXA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqASXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsEqASXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqASXA0";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqASXA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqASXA0";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqASXQ : PCondIsEqAS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqASXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqASXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqASXQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqASXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqASXQ";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqAXA : PCondIsEqA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCondIsEqAXA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqAXA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqAXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqAXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqAXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqAXA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqAXA0 : PCondIsEqAXA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqAXA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqAXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqAXA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqAXA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqAXA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqAXQ : PCondIsEqA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqAXQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCondIsEqAXQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqAXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqAXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqAXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqAXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqAXQA : PCondIsEqAXQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsEqAXQA (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqAXQA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqAXQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqAXQA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqAXQA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqAXQA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqAXQA0 : PCondIsEqAXQA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqAXQA0 (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCondIsEqAXQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqAXQA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqAXQA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqAXQA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqAXXA : PCondIsEqA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsEqAXXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented () :
                new PCondIsEqAXXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqAXXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqAXXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqAXXA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;

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
    class PCondIsEqAXXQ : PCondIsEqA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqAXXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqAXXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqAXXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqAXXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "PCondIsEqAXXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;

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
    class PCondIsEqQ : PCondIsEq
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;
        protected PCondIsEqQ (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Value = ((Quotation) predicate.Operand0).Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument) ? PCondIsEqQA.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is Quotation) ? Unimplemented () :
                (predicate.Operand1 is StaticVariable) ? PCondIsEqQS.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? PCondIsEqQXA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqQXQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCondIsEqQXS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCondIsEqQXXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqQXXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
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

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
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
    class PCondIsEqQA : PCondIsEqQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;
        protected PCondIsEqQA (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? PCondIsEqQA0.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? Unimplemented () :
                (consequent is Quotation) ? PCondIsEqQAQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCondIsEqQAS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCondIsEqQAXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqQAXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
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
    class PCondIsEqQA0 : PCondIsEqQA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqQA0 (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqQA0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqQA0Q.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCondIsEqQA0S.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCondIsEqQA0XA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqQA0XQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQA0";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
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
    class PCondIsEqQA0A : PCondIsEqQA0
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCondIsEqQA0A (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqQA0A0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqQA0AQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqQA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQA0A";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQA0A";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQA0A0 : PCondIsEqQA0A
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqQA0A0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqQA0A0Q.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqQA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQA0A0";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQA0A0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQA0A0Q : PCondIsEqQA0A0
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondIsEqQA0A0Q (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCondIsEqQA0A0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQA0A0Q";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQA0A0Q";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQA0AQ : PCondIsEqQA0A
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondIsEqQA0AQ (PrimitiveCombination2 predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, Quotation alternative)
        {
            return
                new PCondIsEqQA0AQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQA0AQ";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQA0AQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQA0Q : PCondIsEqQA0
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqQA0Q (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCondIsEqQA0QA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqQA0QQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqQA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQA0Q";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQA0Q";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQA0QA : PCondIsEqQA0Q
    {

        public readonly int alternativeOffset;
        protected PCondIsEqQA0QA (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqQA0QA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqQA0QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQA0QA";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQA0QA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQA0QA0 : PCondIsEqQA0QA
    {

        protected PCondIsEqQA0QA0 (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new PCondIsEqQA0QA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQA0QA0";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQA0QA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQA0QQ : PCondIsEqQA0Q
    {

        public readonly object alternativeValue;
        protected PCondIsEqQA0QQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCondIsEqQA0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQA0QQ";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQA0QQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQA0S : PCondIsEqQA0
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;

        protected PCondIsEqQA0S (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () : //PCondIsEqQA0AQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqQA0S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQA0S";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQA0S";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQA0XA : PCondIsEqQA0
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsEqQA0XA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented () :
                new PCondIsEqQA0XA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQA0XA";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQA0XA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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
    class PCondIsEqQA0XQ : PCondIsEqQA0
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqQA0XQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqQA0XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQA0XQ";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQA0XQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
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
    class PCondIsEqQAQ : PCondIsEqQA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqQAQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCondIsEqQAQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqQAQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqQAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQAQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQAQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQAQA : PCondIsEqQAQ
    {

        public readonly int alternativeOffset;
        protected PCondIsEqQAQA (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented () : //PCondIsEqQAQA0.Make (predicate, consequent, alternative) :
                new PCondIsEqQAQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQAQA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQAQA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQAQQ : PCondIsEqQAQ
    {

        public readonly object alternativeValue;
        protected PCondIsEqQAQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCondIsEqQAQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQAQQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQAQQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQAS : PCondIsEqQA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;

        protected PCondIsEqQAS (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqQAS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQAS";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQAS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQAXA : PCondIsEqQA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsEqQAXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented () :
                new PCondIsEqQAXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQAXA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQAXA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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
    class PCondIsEqQAXQ : PCondIsEqQA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqQAXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqQAXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQAXQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQAXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
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
    class PCondIsEqQS : PCondIsEqQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        protected PCondIsEqQS (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? Unimplemented () :
                (consequent is Quotation) ? PCondIsEqQSQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCondIsEqQSS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqQSXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqQS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
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
    class PCondIsEqQSQ : PCondIsEqQS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqQSQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqQSQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqQSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQSQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQSQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQSQQ : PCondIsEqQSQ
    {

        public readonly object alternativeValue;
        protected PCondIsEqQSQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCondIsEqQSQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQSQQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQSQQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQSS : PCondIsEqQS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;

        protected PCondIsEqQSS (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () : //PCondIsEqQSQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqQSS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQSS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQSS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQSXQ : PCondIsEqQS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqQSXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqQSXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqQSXQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQSXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
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
    class PCondIsEqQXA : PCondIsEqQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCondIsEqQXA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqQXA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqQXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqQXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqQXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQXA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQXA0 : PCondIsEqQXA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqQXA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqQXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqQXA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqQXA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQXA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQXQ : PCondIsEqQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqQXQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? PCondIsEqQXQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqQXQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqQXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqQXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqQXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQXQA : PCondIsEqQXQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsEqQXQA (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented () :
                new PCondIsEqQXQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqQXQA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqQXQA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQXQA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQXQQ : PCondIsEqQXQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqQXQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCondIsEqQXQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqQXQQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqQXQQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQXQQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQXS : PCondIsEqQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;
        protected PCondIsEqQXS (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? PCondIsEqQXSS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondIsEqQXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqQXS";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqQXS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQXS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQXSS : PCondIsEqQXS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCondIsEqQXSS (PrimitiveCombination2 predicate, StaticVariable consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, StaticVariable alternative)
        {
            return
                new PCondIsEqQXSS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqQXSS";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqQXSS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQXSS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqQXXA : PCondIsEqQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsEqQXXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqQXXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqQXXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqQXXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqQXXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQXXA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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
    class PCondIsEqQXXA0 : PCondIsEqQXXA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqQXXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsEqQXXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqQXXA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqQXXA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQXXA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.Argument0Value;
                return false;
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
    class PCondIsEqQXXQ : PCondIsEqQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqQXXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqQXXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqQXXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqQXXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;

#if DEBUG
            SCode.location = "PCondIsEqQXXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
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
    class PCondIsEqS : PCondIsEq
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;
        protected PCondIsEqS (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Name = ((StaticVariable) predicate.Operand0).Name;
            this.rand0Offset = ((StaticVariable) predicate.Operand0).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument) ? PCondIsEqSA.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is Quotation) ? PCondIsEqSQ.Make (predicate, consequent, alternative) :
                (predicate.Operand1 is StaticVariable) ? PCondIsEqSS.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? PCondIsEqSXA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqSXQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCondIsEqSXS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCondIsEqSXXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqSXXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCondIsEqSXXS.Make (predicate, consequent, (StaticVariable) alternative) :
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

#if DEBUG
            SCode.location = "PCondIsEqS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
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
    class PCondIsEqSA : PCondIsEqS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;
        protected PCondIsEqSA (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? PCondIsEqSA0.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? Unimplemented () :
                (consequent is Quotation) ? Unimplemented () :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqSAXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
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
    class PCondIsEqSA0 : PCondIsEqSA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSA0 (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqSA0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqSA0Q.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCondIsEqSA0S.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCondIsEqSA0XA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqSA0XQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSA0";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
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
    class PCondIsEqSA0A : PCondIsEqSA0
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCondIsEqSA0A (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqSA0A0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqSA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSA0A";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSA0A";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSA0A0 : PCondIsEqSA0A
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqSA0A0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqSA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSA0A0";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSA0A0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSA0Q : PCondIsEqSA0
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqSA0Q (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqSA0QQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqSA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSA0Q";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSA0Q";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSA0QQ : PCondIsEqSA0Q
    {

        public readonly object alternativeValue;
        protected PCondIsEqSA0QQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCondIsEqSA0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSA0QQ";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSA0QQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSA0S : PCondIsEqSA0
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;
        protected PCondIsEqSA0S (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqSA0S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSA0S";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSA0S";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSA0XA : PCondIsEqSA0
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsEqSA0XA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqSA0XA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqSA0XA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSA0XA";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSA0XA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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
    class PCondIsEqSA0XA0 : PCondIsEqSA0XA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqSA0XA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsEqSA0XA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSA0XA0";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSA0XA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.Argument0Value;
                return false;
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
    class PCondIsEqSA0XQ : PCondIsEqSA0
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqSA0XQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqSA0XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSA0XQ";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSA0XQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
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
    class PCondIsEqSAXQ : PCondIsEqSA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqSAXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqSAXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSAXQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSAXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
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
    class PCondIsEqSQ : PCondIsEqS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;
        protected PCondIsEqSQ (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = ((Quotation) predicate.Operand1).Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqSQA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqSQQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCondIsEqSQS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCondIsEqSQXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqSQXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCondIsEqSQXS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondIsEqSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSQ";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
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
    class PCondIsEqSQA : PCondIsEqSQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCondIsEqSQA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqSQA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () : //PCondIsEqSQQS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondIsEqSQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSQA";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSQA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSQA0 : PCondIsEqSQA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqSQA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () : //PCondIsEqSQQS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondIsEqSQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSQA0";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSQA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSQQ : PCondIsEqSQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqSQQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqSQQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCondIsEqSQQS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondIsEqSQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSQQ";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSQQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSQQQ : PCondIsEqSQQ
    {

        public readonly object alternativeValue;

        protected PCondIsEqSQQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCondIsEqSQQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSQQQ";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSQQQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSQQS : PCondIsEqSQQ
    {

        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCondIsEqSQQS (PrimitiveCombination2 predicate, Quotation consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, StaticVariable alternative)
        {
            return
                new PCondIsEqSQQS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSQQS";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSQQS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSQS : PCondIsEqSQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;
        protected PCondIsEqSQS (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqSQSQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () : //PCondIsEqSQQS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondIsEqSQS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSQS";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSQS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSQSQ : PCondIsEqSQS
    {

        public readonly object alternativeValue;
        protected PCondIsEqSQSQ (PrimitiveCombination2 predicate, StaticVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, Quotation alternative)
        {
            return
                new PCondIsEqSQSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSQSQ";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSQSQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSQXA : PCondIsEqSQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsEqSQXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqSQXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqSQXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSQXA";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSQXA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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
    class PCondIsEqSQXA0 : PCondIsEqSQXA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqSQXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsEqSQXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSQXA0";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSQXA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.Argument0Value;
                return false;
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
    class PCondIsEqSQXQ : PCondIsEqSQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondIsEqSQXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqSQXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSQXQ";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSQXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
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
    class PCondIsEqSQXS : PCondIsEqSQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCondIsEqSQXS (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCondIsEqSQXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSQXS";
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSQXS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
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
    class PCondIsEqSS : PCondIsEqS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        protected PCondIsEqSS (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqSSA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented () :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqSSXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCondIsEqSSXS.Make (predicate, consequent, (StaticVariable) alternative) :
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

#if DEBUG
            SCode.location = "PCondIsEqSA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
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
    class PCondIsEqSSA : PCondIsEqSS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCondIsEqSSA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqSSA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqSSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSSA";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSSA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSSA0 : PCondIsEqSSA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqSSA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqSSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSSA0";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSSA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSSXQ : PCondIsEqSS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondIsEqSSXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqSSXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSSXQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSSXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
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
    class PCondIsEqSSXS : PCondIsEqSS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected PCondIsEqSSXS (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCondIsEqSSXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsEqSSXS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSSXS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
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
    class PCondIsEqSXA : PCondIsEqS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        public readonly int consequentOffset;
        protected PCondIsEqSXA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqSXA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
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

#if DEBUG
            SCode.location = "PCondIsEqSXA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSXA0 : PCondIsEqSXA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSXA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqSXA0Q.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCondIsEqSXA0S.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondIsEqSXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqSXA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqSXA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSXA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSXA0Q : PCondIsEqSXA0
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqSXA0Q (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCondIsEqSXA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqSXA0Q";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqSXA0Q";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSXA0Q";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSXA0S : PCondIsEqSXA0
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCondIsEqSXA0S (PrimitiveCombination2 predicate, Argument0 consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, StaticVariable alternative)
        {
            return
                new PCondIsEqSXA0S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqSXA0S";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqSXA0S";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSXA0S";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSXQ : PCondIsEqS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object consequentValue;
        protected PCondIsEqSXQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqSXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqSXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqSXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSXS : PCondIsEqS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;
        protected PCondIsEqSXS (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqSXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqSXS";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqSXS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSXS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqSXXA : PCondIsEqS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsEqSXXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqSXXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqSXXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqSXXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqSXXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSXXA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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
    class PCondIsEqSXXA0 : PCondIsEqSXXA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqSXXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsEqSXXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqSXXA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqSXXA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSXXA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.Argument0Value;
                return false;
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
    class PCondIsEqSXXQ : PCondIsEqS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqSXXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqSXXQ (predicate, consequent, alternative);
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

#if DEBUG
            SCode.location = "PCondIsEqSXXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
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
    class PCondIsEqSXXS : PCondIsEqS
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCondIsEqSXXS (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCondIsEqSXXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqSXXS";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqSXXS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "PCondIsEqSXXS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
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
    class PCondIsEqXA : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;

        protected PCondIsEqXA (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? PCondIsEqXA0.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? PCondIsEqXAA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqXAQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqXAXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqXA";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqXA";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqXA0 : PCondIsEqXA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqXA0 (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? Unimplemented () : //PCondIsEqXAA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? Unimplemented () : //PCondIsEqXAQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqXA0XQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXA0";
#endif
            object ev1 = environment.Argument0Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqXA0";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqXA0";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqXA0XQ : PCondIsEqXA0
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqXA0XQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqXA0XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXA0XQ";
#endif
            object ev1 = environment.Argument0Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXA0XQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXA0XQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqXA0";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqXAA : PCondIsEqXA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCondIsEqXAA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqXAA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqXAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXAA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXAA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXAA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqXAA";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXAA0 : PCondIsEqXAA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqXAA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqXAA0Q.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqXAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXAA0";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXAA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXAA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqXAA0";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXAA0Q : PCondIsEqXAA0
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqXAA0Q (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCondIsEqXAA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXAA0Q";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXAA0Q";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXAA0Q";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXAQ : PCondIsEqXA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondIsEqXAQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqXAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXAQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXAQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXAQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqXAQ";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXAXQ : PCondIsEqXA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqXAXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqXAXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXAXQ";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXAXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXAXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqXAXQ";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqXQ : PCondIsEq
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PCondIsEqXQ (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = ((Quotation) predicate.Operand1).Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqXQA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqXQQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? PCondIsEqXQXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqXQXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXQ";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEq";
#endif
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
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
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
    class PCondIsEqXQA : PCondIsEqXQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCondIsEqXQA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqXQA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqXQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXQA";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXQA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXQA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXQA0 : PCondIsEqXQA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqXQA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqXQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXQA0";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXQA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXQA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXQQ : PCondIsEqXQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondIsEqXQQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqXQQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCondIsEqXQQS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondIsEqXQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXQQ";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXQQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXQQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXQQQ : PCondIsEqXQQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondIsEqXQQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCondIsEqXQQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXQQQ";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXQQQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXQQQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXQQS : PCondIsEqXQQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected PCondIsEqXQQS (PrimitiveCombination2 predicate, Quotation consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, Quotation consequent, StaticVariable alternative)
        {
            return
                new PCondIsEqXQQS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXQQS";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXQQS";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXQQS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXQXA : PCondIsEqXQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;

        protected PCondIsEqXQXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsEqXQXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqXQXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXQXA";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXQXA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXQXA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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
    class PCondIsEqXQXA0 : PCondIsEqXQXA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqXQXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsEqXQXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXQXA0";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXQXA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXQXA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.Argument0Value;
                return false;
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
    class PCondIsEqXQXQ : PCondIsEqXQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondIsEqXQXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqXQXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXQXQ";
#endif

            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXQXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXQXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
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
    class PCondIsEqXS : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PCondIsEqXS (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsEqXSA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsEqXSQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCondIsEqXSS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCondIsEqXSXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsEqXSXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXS";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqXS";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqXS";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqXSA : PCondIsEqXS
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCondIsEqXSA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqXSA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqXSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXSA";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXSA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXSA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqXSA";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXSA0 : PCondIsEqXSA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqXSA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqXSA0Q.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqXSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXSA0";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXSA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXSA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqXSA0";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXSA0Q : PCondIsEqXSA0
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqXSA0Q (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCondIsEqXSA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXSA0Q";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXSA0Q";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXSA0Q";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXSQ : PCondIsEqXS
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondIsEqXSQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqXSQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqXSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXSQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXSQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXSQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqXSQ";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXSQQ : PCondIsEqXSQ
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondIsEqXSQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCondIsEqXSQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXSQQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXSQQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXSQQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXSS : PCondIsEqXS
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;

        public readonly int consequentOffset;
        protected PCondIsEqXSS (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqXSSQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqXSS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXSS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXSS";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXSS";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsEqXSS";
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXSSQ : PCondIsEqXSS
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqXSSQ (PrimitiveCombination2 predicate, StaticVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, StaticVariable consequent, Quotation alternative)
        {
            return
                new PCondIsEqXSSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXSSQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXSSQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXSSQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXSXA : PCondIsEqXS
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;

        protected PCondIsEqXSXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument) ? PCondIsEqXSXA0.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsEqXSXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXSXA";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXSXA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXSXA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqXSXQ";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqXSXA0 : PCondIsEqXSXA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqXSXA0 (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsEqXSXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXSXA0";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXSXA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXSXA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqXSXA0";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqXSXQ : PCondIsEqXS
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondIsEqXSXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqXSXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PCondIsEqXSXQ";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXSXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXSXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsEqXSXQ";
#endif
                expression = this.consequent;
                answer = null; // happy compiler
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsEqXXA : PCondIsEq
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCondIsEqXXA (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsEqXXA0.Make (predicate, (Argument0) consequent, alternative) :
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? Unimplemented () :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqXXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqXXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXXA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXXA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXXA0 : PCondIsEqXXA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsEqXXA0 (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqXXA0Q.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqXXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqXXA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXXA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXXA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXXA0";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXXA0Q : PCondIsEqXXA0
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqXXA0Q (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new PCondIsEqXXA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqXXA0Q";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXXA0Q";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXXA0Q";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXXA0Q";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXXQ : PCondIsEq
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsEqXXQ (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? Unimplemented () :
                (alternative is Quotation) ? PCondIsEqXXQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented () :
                new PCondIsEqXXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqXXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null; // happy compiler
                return true;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXXQQ : PCondIsEqXXQ
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqXXQQ (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new PCondIsEqXXQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqXXQQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXXQQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXXQQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXXQQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsEqXXXA : PCondIsEq
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsEqXXXA (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented () :
                new PCondIsEqXXXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqXXXA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXXXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXXXA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXXXA";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
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
    class PCondIsEqXXXQ : PCondIsEq
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsEqXXXQ (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsEqXXXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PCondIsEqXXXQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXXXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsEqXXXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "PCondIsEqXXXQ";
#endif
            if (!ObjectModel.SchemeEq (ev0, ev1)) {
                answer = this.alternativeValue;
                return false;
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
