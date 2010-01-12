using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable] 
    class POrIsEq : POr2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type>();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type>();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
#endif

        protected POrIsEq(PrimitiveIsEq predicate, SCode alternative)
            : base(predicate, alternative)
        {
        }

        public static SCode Make(PrimitiveIsEq predicate, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqA) ? POrIsEqA.Make ((PrimitiveIsEqA) predicate, alternative) :
                (predicate is PrimitiveIsEqQ) ? POrIsEqQ.Make ((PrimitiveIsEqQ) predicate, alternative) :
                (predicate is PrimitiveIsEqS) ? POrIsEqS.Make ((PrimitiveIsEqS) predicate, alternative) :
                 new POrIsEq (predicate, alternative);
        }

        public override bool EvalStep(out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            NoteCalls(this.rand0);
            rand0TypeHistogram.Note(this.rand0Type);
            NoteCalls(this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "POrIsEq";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep(out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrIsEq";
#endif
            if (ev1 == Interpreter.UnwindStack)
            {
                throw new NotImplementedException();
                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep(out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrIsEq";
#endif
            if (ev1 == Interpreter.UnwindStack)
            {
                throw new NotImplementedException();
                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            ObjectModel.Eq (out answer, ev0, ev1);
            if ((answer is Boolean) && ((bool) answer) == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls(this.alternative);
                alternativeTypeHistogram.Note(this.alternativeType);
                SCode.location = "POrIsEq";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            } 
            else {
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
        protected POrIsEqA (PrimitiveIsEqA predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand0Offset = predicate.rand0Offset;
        }

        public static SCode Make (PrimitiveIsEqA predicate, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqA0) ? POrIsEqA0.Make ((PrimitiveIsEqA0) predicate, alternative) :
                 new POrIsEqA (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "POrIsEqA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrIsEqA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            ObjectModel.Eq (out answer, ev0, ev1);
            if ((answer is Boolean) && ((bool) answer) == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqA";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
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
        protected POrIsEqA0 (PrimitiveIsEqA0 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveIsEqA0 predicate, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqA0Q) ? POrIsEqA0Q.Make ((PrimitiveIsEqA0Q) predicate, alternative) :
                 new POrIsEqA0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "POrIsEqA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrIsEqA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object ev0 = environment.Argument0Value;

            ObjectModel.Eq (out answer, ev0, ev1);
            if ((answer is Boolean) && ((bool) answer) == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqA0";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                return false;
            }
        }
    }

    [Serializable]
    class POrIsEqA0Q : POrIsEqA0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;
        protected POrIsEqA0Q (PrimitiveIsEqA0Q predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveIsEqA0Q predicate, SCode alternative)
        {
            return
                 new POrIsEqA0Q (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("POrIsEqA0Q");
#endif


            object ev0 = environment.Argument0Value;

            ObjectModel.Eq (out answer, ev0, this.rand1Value);
            if ((answer is Boolean) && ((bool) answer) == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqA0Q";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
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
        protected POrIsEqQ (PrimitiveIsEqQ predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand0Value = predicate.rand0Value;
        }

        public static SCode Make (PrimitiveIsEqQ predicate, SCode alternative)
        {
            return
                 new POrIsEqQ (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "POrIsEqQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrIsEqQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object ev0 = this.rand0Value;

            ObjectModel.Eq (out answer, ev0, ev1);
            if ((answer is Boolean) && ((bool) answer) == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqQ";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
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

        protected POrIsEqS (PrimitiveIsEqS predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand0Name = predicate.rand0Name;
            this.rand0Offset = predicate.rand0Offset;
        }

        public static SCode Make (PrimitiveIsEqS predicate, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEqSQ) ? POrIsEqSQ.Make ((PrimitiveIsEqSQ) predicate, alternative) :
                 new POrIsEqS (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "POrIsEqS";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrIsEqS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, ev1);
            if ((answer is Boolean) && ((bool) answer) == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqS";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                return false;
            }
        }
    }

    class POrIsEqSQ : POrIsEqS
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected POrIsEqSQ (PrimitiveIsEqSQ predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Value = predicate.rand1Value;
        }

        public static SCode Make (PrimitiveIsEqSQ predicate, SCode alternative)
        {
            return
                 new POrIsEqSQ (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "POrIsEqSQ";
#endif

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            ObjectModel.Eq (out answer, ev0, this.rand1Value);
            if ((answer is Boolean) && ((bool) answer) == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrIsEqSQ";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                return false;
            }
        }
    }

}
