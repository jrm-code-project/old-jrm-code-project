using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
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
                (predicate is PrimitiveIsEqL) ? PCondIsEqL.Make ((PrimitiveIsEqL) predicate, consequent, alternative)
                : (predicate is PrimitiveIsEqCar) ? PCondIsEqCar.Make ((PrimitiveIsEqCar) predicate, consequent, alternative)
                : (predicate is PrimitiveIsEqQ) ? PCond2IsEqQ.Make ((PrimitiveIsEqQ) predicate, consequent, alternative)
                : (predicate is PrimitiveIsEqSL) ? PCond2IsEqSL.Make ((PrimitiveIsEqSL) predicate, consequent, alternative)
                : (predicate is PrimitiveIsEqSQ) ? PCond2IsEqSQ.Make ((PrimitiveIsEqSQ) predicate, consequent, alternative)
                : new PCondIsEq (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEq.EvalStep");
            noteCalls (this.rand0);
            noteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

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

    class PCondIsEqL : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PCondIsEqL (PrimitiveIsEqL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Name = predicate.rand0Name;
            this.rand0Depth = predicate.rand0Depth;
            this.rand0Offset = predicate.rand0Offset;
        }

        public static SCode Make (PrimitiveIsEqL predicate, SCode consequent, SCode alternative)
        {
            return 
                (predicate is PrimitiveIsEqA) ? PCondIsEqA.Make ((PrimitiveIsEqA) predicate, consequent, alternative)
                : (predicate is PrimitiveIsEqLQ) ? PCondIsEqLQ.Make ((PrimitiveIsEqLQ) predicate, consequent, alternative)
                : new PCondIsEqL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqL.EvalStep");
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

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

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

    class PCondIsEqA : PCondIsEqL
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsEqA (PrimitiveIsEqA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (PrimitiveIsEqA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqA0) ? PCondIsEqA0.Make ((PrimitiveIsEqA0) predicate, consequent, alternative)
                : (predicate is PrimitiveIsEqA1) ? PCondIsEqA1.Make ((PrimitiveIsEqA1) predicate, consequent, alternative)
                : new PCondIsEqA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA.EvalStep");
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

            ObjectModel.Eq (out answer, environment.ArgumentValue (this.rand0Offset), ev1);

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
                (predicate is PrimitiveIsEqA0L) ? PCondIsEqA0L.Make ((PrimitiveIsEqA0L) predicate, consequent, alternative) :
                (predicate is PrimitiveIsEqA0Q) ? PCondIsEqA0Q.Make ((PrimitiveIsEqA0Q) predicate, consequent, alternative) :
                new PCondIsEqA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA0.EvalStep");
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

    class PCondIsEqA0L : PCondIsEqA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCondIsEqA0L (PrimitiveIsEqA0L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsEqA0L predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqA0A) ? PCondIsEqA0A.Make ((PrimitiveIsEqA0A) predicate, consequent, alternative) :
                new PCondIsEqA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqA0L.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

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

    class PCondIsEqLQ : PCondIsEqL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PCondIsEqLQ (PrimitiveIsEqLQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveIsEqLQ predicate, SCode consequent, SCode alternative)
        {
            return new PCondIsEqLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqLQ.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, this.rand1Value);

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

    class PCondIsEqCar : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly SCode rand0Arg;

        protected PCondIsEqCar (PrimitiveIsEqCar predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Arg = predicate.rand0Arg;
        }

        public static SCode Make (PrimitiveIsEqCar predicate, SCode consequent, SCode alternative)
        {
            return 
                (predicate is PrimitiveIsEqCarL) ? PCondIsEqCarL.Make ((PrimitiveIsEqCarL) predicate, consequent, alternative)
                //(predicate is PrimitiveIsEqCarQ) ? Unimplemented()
                //: (predicate is PrimitiveIsEqCarSL) ? 
                : (predicate is PrimitiveIsEqCarSQ) ? PCondIsEqCarSQ.Make ((PrimitiveIsEqCarSQ) predicate, consequent, alternative)
                //: (consequent is LexicalVariable) ? Unimplemented()
                //: (consequent is Quotation) ? Unimplemented()
                //: (alternative is LexicalVariable) ? Unimplemented()
                //: (alternative is Quotation) ? Unimplemented()
                : new PCondIsEqCar (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCar.EvalStep");
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
            Warm ();
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

            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCarA0 : PCondIsEqCarA
    {
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

        protected PCondIsEqCarA0 (PrimitiveIsEqCarA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }


        public static SCode Make (PrimitiveIsEqCarA0 predicate, SCode consequent, SCode alternative)
        {
            return 
                (predicate is PrimitiveIsEqCarA0L) ? PCondIsEqCarA0L.Make ((PrimitiveIsEqCarA0L) predicate, consequent, alternative)
                : new PCondIsEqCarA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsEqCarA0.EvalStep");
            noteCalls (this.rand1);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            ObjectModel.Eq (out answer, ((Cons)environment.ArgumentValue (this.rand0Offset)).Car, ev1);

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
            return new PCondIsEqCarA0L (predicate, consequent, alternative);

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

    class PCondIsEqCarA0Q : PCondIsEqCarA0
    {
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
                 //: (rand1 is Quotation) ? PCond2IsEqCarA1Q.Make (rator, rand0, (Quotation) rand1)
                 //: new PCondIsEqCarA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCondIsEqCarA1L : PCondIsEqCarA1
    {
        //public readonly object rand1Name;
        //public readonly int rand1Depth;
        //public readonly int rand1Offset;

        protected PCondIsEqCarA1L (PrimitiveIsEqCarA1L predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            //this.rand1Name = rand1.Name;
            //this.rand1Depth = rand1.Depth;
            //this.rand1Offset = rand1.Offset;
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

    class PCond2IsEqCarA1Q : PCondIsEqCarA1
    {
        //public readonly object rand1Value;

        protected PCond2IsEqCarA1Q (PrimitiveIsEqCarA1Q predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }


        public static SCode Make (PrimitiveIsEqCarA1Q predicate, SCode consequent, SCode alternative)
        {
            return new PCond2IsEqCarA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();

        }
    }

    class PCond2IsEqCarLL : PCondIsEqCarL
    {
        //public readonly object rand1Name;
        //public readonly int rand1Depth;
        //public readonly int rand1Offset;

        protected PCond2IsEqCarLL (PrimitiveIsEqCarLL predicate, SCode consequent, SCode alternative)
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

    class PCond2IsEqCarLQ : PCondIsEqCarL
    {
       // public readonly object rand1Value;

        protected PCond2IsEqCarLQ (PrimitiveIsEqCarLQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {


        }


        public static SCode Make (PrimitiveIsEqCarLQ predicate, SCode consequent, SCode alternative)
        {
            return new PCond2IsEqCarLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class PCond2IsEqCarSL : PCondIsEqCar
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCond2IsEqCarSL (PrimitiveIsEqCarSL predicate, SCode consequent, SCode alternative)
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


    class PCond2IsEqQ : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;

        protected PCond2IsEqQ (PrimitiveIsEqQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand0Value = predicate.rand0Value;
        }

        public static SCode Make (PrimitiveIsEqQ predicate, SCode consequent, SCode alternative)
        {
            return new PCond2IsEqQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2IsEqQ.EvalStep");
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

    class PCond2IsEqSL : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PCond2IsEqSL (PrimitiveIsEqSL predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Name = predicate.rand1Name;
            this.rand1Depth = predicate.rand1Depth;
            this.rand1Offset = predicate.rand1Offset;
        }

        public static SCode Make (PrimitiveIsEqSL predicate, SCode consequent, SCode alternative)
        {
            return new PCond2IsEqSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCond2IsEqSL.EvalStep");
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

    class PCond2IsEqSQ : PCondIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PCond2IsEqSQ (PrimitiveIsEqSQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveIsEqSQ predicate, SCode consequent, SCode alternative)
        {
            return new PCond2IsEqSQ (predicate, consequent, alternative);
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


}
