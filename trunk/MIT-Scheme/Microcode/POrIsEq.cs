using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class POrIsEq : POr2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected POrIsEq (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand0 is Argument) ? POrIsEqA.Make (predicate, alternative) :
                (predicate.Operand0 is Quotation) ? POrIsEqQ.Make (predicate, alternative) :
                (predicate.Operand0 is StaticVariable) ? POrIsEqS.Make (predicate, alternative) :
                (predicate.Operand1 is Argument) ? POrIsEqXA.Make (predicate, alternative) :
                (predicate.Operand1 is Quotation) ? POrIsEqXQ.Make (predicate, alternative) :
                (predicate.Operand1 is StaticVariable) ? POrIsEqXS.Make (predicate, alternative) :
                new POrIsEq (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "POrIsEq.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrIsEq.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrIsEq.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEq.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqA : POrIsEq
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;

        protected POrIsEqA (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand0Offset = ((Argument) predicate.Operand0).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand0 is Argument0) ? POrIsEqA0.Make (predicate, alternative) :
                (predicate.Operand1 is Argument) ? POrIsEqAA.Make (predicate, alternative) :
                (predicate.Operand1 is Quotation) ? POrIsEqAQ.Make (predicate, alternative) :
                (predicate.Operand1 is StaticVariable) ? POrIsEqAS.Make (predicate, alternative) :
                new POrIsEqA (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "POrIsEqA.EvalStep";
#endif

            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrIsEqA.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            object ev0 = environment.ArgumentValue (this.rand0Offset);


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqA.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqA0 : POrIsEqA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected POrIsEqA0 (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument) ? POrIsEqA0A.Make (predicate, alternative) :
                (predicate.Operand1 is Quotation) ? POrIsEqA0Q.Make (predicate, alternative) :
                (predicate.Operand1 is StaticVariable) ? POrIsEqA0S.Make (predicate, alternative) :
                new POrIsEqA0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "POrIsEqA0.EvalStep";
#endif

            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrIsEqA0.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object ev0 = environment.Argument0Value;


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqA0.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqA0A : POrIsEqA0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;
        protected POrIsEqA0A (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? POrIsEqA0A0.Make (predicate, alternative) :
                new POrIsEqA0A (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "POrIsEqA0A.EvalStep";
#endif

            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = environment.Argument0Value;


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqA1.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqA0A0 : POrIsEqA0A
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected POrIsEqA0A0 (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
#if DEBUG
            Debugger.Break();
#endif
            return
                new POrIsEqA0A0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "POrIsEqA0A0.EvalStep";
#endif

            object ev0 = environment.Argument0Value;


            if (! ObjectModel.SchemeEq (ev0, ev0)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqA0A0.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqA0Q : POrIsEqA0
    {
#if DEBUG
        static Histogram<object> objectHistogram = new Histogram<object> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected POrIsEqA0Q (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Value = ((Quotation) predicate.Operand1).Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (((Quotation) predicate.Operand1).Quoted is int) ?  POrIsEqA0Int.Make (predicate, alternative) :
                new POrIsEqA0Q (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "POrIsEqA0Q.EvalStep";
            objectHistogram.Note (this.rand1Value);
#endif

            object ev1 = this.rand1Value;
            object ev0 = environment.Argument0Value;


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqA0Q.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqA0Int : POrIsEqA0Q
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Value;

        protected POrIsEqA0Int (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Value = (int) ((Quotation) predicate.Operand1).Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POrIsEqA0Int (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("POrIsEqA0Int.EvalStep");
#endif
            object ev0 = environment.Argument0Value;
            if (ev0 is int &&
                (int) ev0 == this.rand1Value) {
                answer = Constant.sharpT;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqA0Int.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
        }
    }


    [Serializable]
    class POrIsEqA0S : POrIsEqA0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected POrIsEqA0S (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POrIsEqA0S (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "POrIsEqA0S.EvalStep";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqA0S.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqAA : POrIsEqA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;
        protected POrIsEqAA (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? POrIsEqAA0.Make (predicate, alternative) :
                new POrIsEqAA (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "POrIsEqAA.EvalStep";
#endif

            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = environment.ArgumentValue (this.rand0Offset);


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqAA.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqAA0 : POrIsEqAA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected POrIsEqAA0 (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POrIsEqAA0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "POrIsEqAA0.EvalStep";
#endif
            object ev1 = environment.Argument0Value;
            object ev0 = environment.ArgumentValue (this.rand1Offset);


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqAA0.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqAQ : POrIsEqA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected POrIsEqAQ (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Value = ((Quotation) predicate.Operand1).Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POrIsEqAQ (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "POrIsEqAQ.EvalStep";
#endif

            object ev1 = this.rand1Value;
            object ev0 = environment.ArgumentValue (this.rand0Offset);


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqAQ.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqAS : POrIsEqA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected POrIsEqAS (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POrIsEqAS (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "POrIsEqAS.EvalStep";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.ArgumentValue (this.rand0Offset);


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqAS.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqSA : POrIsEqS
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;
        protected POrIsEqSA (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? POrIsEqSA0.Make (predicate, alternative) :
                new POrIsEqSA (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "POrIsEqSA.EvalStep";
#endif

            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqSA.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqSA0 : POrIsEqSA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected POrIsEqSA0 (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POrIsEqSA0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "POrIsEqSA0.EvalStep";
#endif
            object ev1 = environment.Argument0Value;
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqSA0.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqSQ : POrIsEqS
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected POrIsEqSQ (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Value = ((Quotation) predicate.Operand1).Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POrIsEqSQ (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "POrIsEqSQ.EvalStep";
#endif

            object ev1 = this.rand1Value;
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqSQ.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqSS : POrIsEqS
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected POrIsEqSS (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POrIsEqSS (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "POrIsEqSS.EvalStep";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqSS.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqQ : POrIsEq
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;

        protected POrIsEqQ (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand0Value = ((Quotation) predicate.Operand0).Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument) ? POrIsEqQA.Make (predicate, alternative) :
                (predicate.Operand1 is Quotation) ? Unimplemented () :
                (predicate.Operand1 is StaticVariable) ? POrIsEqQS.Make (predicate, alternative) :
                new POrIsEqQ (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "POrIsEqQ.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrIsEqQ.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            object ev0 = this.rand0Value;

            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqQ.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqQA : POrIsEqQ
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;

        protected POrIsEqQA (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? POrIsEqQA0.Make (predicate, alternative) :
                new POrIsEqQA (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "POrIsEqQA.EvalStep";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = this.rand0Value;

            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqQA.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqQA0 : POrIsEqQA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected POrIsEqQA0 (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POrIsEqQA0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "POrIsEqQA0.EvalStep";
#endif
            object ev1 = environment.Argument0Value;
            object ev0 = this.rand0Value;

            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqQA0.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqQS : POrIsEqQ
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected POrIsEqQS (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POrIsEqQS (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "POrIsEqQS.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = this.rand0Value;

            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqQS.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqS : POrIsEq
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected POrIsEqS (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand0Name = ((StaticVariable) predicate.Operand0).Name;
            this.rand0Offset = ((StaticVariable) predicate.Operand0).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand1 is Quotation) ? POrIsEqSQ.Make (predicate, alternative) :
                new POrIsEqS (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "POrIsEqS.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrIsEqS.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqS.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqXA : POrIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;

        protected POrIsEqXA (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? POrIsEqXA0.Make (predicate, alternative) :
                new POrIsEqXA (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "POrIsEqXA.EvalStep";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrXA.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqXA.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqXA0 : POrIsEqXA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected POrIsEqXA0 (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POrIsEqXA0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "POrIsEqXA0.EvalStep";
#endif
            object ev1 = environment.Argument0Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrXA0.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqXA0.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqXQ : POrIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected POrIsEqXQ (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Value = ((Quotation) predicate.Operand1).Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POrIsEqXQ (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "POrIsEqXQ.EvalStep";
#endif
            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrXQ.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqXQ.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }

    [Serializable]
    class POrIsEqXS : POrIsEq
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected POrIsEqXS (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POrIsEqXS (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "POrIsEqXS.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrXS.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }


            if (! ObjectModel.SchemeEq (ev0, ev1)) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqXS.EvalStep";
#endif
                expression = this.alternative;
                answer = null; //happy compiler
                return true;
            }
            else {
                answer = Constant.sharpT;
                return false;
                }
        }
    }
}
