using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class Combination3 : Combination
    {
#if DEBUG
        static Histogram<Type> c0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> c1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
        protected Type c0Type;
        protected Type c1Type;
        protected Type c2Type;
        protected Type c3Type;
#endif
        protected Combination3 (SCode rator, SCode rand0, SCode rand1, SCode rand2)
            : base (new SCode [] { rator, rand0, rand1, rand2 })
        {
#if DEBUG
            this.c0Type = rator.GetType ();
            this.c1Type = rand0.GetType ();
            this.c2Type = rand1.GetType ();
            this.c3Type = rand2.GetType ();
#endif
        }

        public static SCode Make (SCode rator, SCode rand0, SCode rand1, SCode rand2)
        {
            return 
                //(rand2 is Argument) ? Combination3SSSA.Make (rator, rand0, rand1, (Argument) rand2)
                //(rand2 is Quotation) ? Combination3SSSQ.Make (rator, rand0, rand1, (Quotation) rand2)
                new Combination3 (rator, rand0, rand1, rand2);
        }

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialResult rator = this.components[0].PartialEval (environment);
            PartialResult rand0 = this.components[1].PartialEval (environment);
            PartialResult rand1 = this.components[2].PartialEval (environment);
            PartialResult rand2 = this.components [3].PartialEval (environment);
            return new PartialResult (rator.Residual == this.components[0] &&
                rand0.Residual == this.components[1] &&
                rand1.Residual == this.components[2] &&
                rand2.Residual == this.components[3] ? this : Combination3.Make (rator.Residual, rand0.Residual, rand1.Residual, rand2.Residual));
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            this.components[0].CollectFreeVariables (freeVariableSet);
            this.components[1].CollectFreeVariables (freeVariableSet);
            this.components[2].CollectFreeVariables (freeVariableSet);
            this.components [3].CollectFreeVariables (freeVariableSet);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.components [0]);
            NoteCalls (this.components [1]);
            NoteCalls (this.components [2]);
            NoteCalls (this.components [3]);
            c0TypeHistogram.Note (this.c0Type);
            c1TypeHistogram.Note (this.c1Type);
            c2TypeHistogram.Note (this.c2Type);
            c3TypeHistogram.Note (this.c3Type);
            SCode.location = "Combination3";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.components [3];
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev1;
            env = environment;
            unev = this.components [2];
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.components [1];
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination3Frame2 (this, environment, ev1, ev2));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop;
            env = environment;
            unev = this.components [0];
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination3Frame3 (this, environment, ev0, ev1, ev2));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
        }
    }

    class Combination3Frame0 : SubproblemContinuation<Combination3>, ISystemVector
    {
        public Combination3Frame0 (Combination3 combination, Environment environment)
            : base (combination, environment)
        {
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { throw new NotImplementedException (); }
        }

        public object SystemVectorRef (int index)
        {
            throw new NotImplementedException ();
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev2)
        {
            object ev1;
            Environment env = environment;
            Control unevop = this.expression.Components [2];
            while (unevop.EvalStep (out ev1, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this.expression, environment, ev2));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0;
            env = environment;
            unevop = this.expression.Components [1];
            while (unevop.EvalStep (out ev0, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object evop;
            env = environment;
            unevop = this.expression.Operator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
        }
    }

    class Combination3Frame1 : SubproblemContinuation<Combination3>, ISystemVector
    {
        readonly object ev2;
        public Combination3Frame1 (Combination3 combination, Environment environment, object ev2)
            : base (combination, environment)
        {
            this.ev2 = ev2;
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { throw new NotImplementedException (); }
        }

        public object SystemVectorRef (int index)
        {
            throw new NotImplementedException ();
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev1)
        {
            object ev0;
            Environment env = environment;
            Control unevop = this.expression.Components [1];
            while (unevop.EvalStep (out ev0, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object evop;
            env = environment;
            unevop = this.expression.Operator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, this.ev2);
        }
    }

    class Combination3Frame2 : SubproblemContinuation<Combination3>, ISystemVector
    {
        readonly object ev1;
        readonly object ev2;
        public Combination3Frame2 (Combination3 combination, Environment environment, object ev1, object ev2)
            : base (combination, environment)
        {
            this.ev1 = ev1;
            this.ev2 = ev2;
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { throw new NotImplementedException (); }
        }

        public object SystemVectorRef (int index)
        {
            throw new NotImplementedException ();
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            object evop;
            Environment env = environment;
            Control unevop = this.expression.Operator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, value, this.ev1, this.ev2);
        }
    }

    [Serializable]
    class Combination3Frame3 : SubproblemContinuation<Combination3>, ISystemVector
    {
        readonly object ev0;
        readonly object ev1;
        readonly object ev2;

        public Combination3Frame3 (Combination3 combination3, Environment environment, object ev0, object ev1, object ev2)
            : base (combination3, environment)
        {
            this.ev0 = ev0;
            this.ev1 = ev1;
            this.ev2 = ev2;
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { throw new NotImplementedException (); }
        }

        public object SystemVectorRef (int index)
        {
            throw new NotImplementedException ();
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            return Interpreter.Call (out answer, ref expression, ref environment, value, this.ev0, this.ev1, this.ev2);
        }
    }

#if NIL

    [Serializable]
    class Combination3SSSA : Combination3
    {
#if DEBUG
        static Histogram<Type> c0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> c1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
#endif
        int rand2Offset;
        protected Combination3SSSA (SCode rator, SCode rand0, SCode rand1, Argument rand2)
            : base (rator, rand0, rand1, rand2)
        {
            this.rand2Offset = rand2.Offset;
        }

        public static SCode Make (SCode rator, SCode rand0, SCode rand1, Argument rand2)
        {
            return (rator is LexicalVariable1) ? Combination3L1SSA.Make ((LexicalVariable1) rator, rand0, rand1, rand2)
                : new Combination3SSSA (rator, rand0, rand1, rand2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination3SSSA.EvalStep");
            noteCalls (this.components [0]);
            noteCalls (this.components [1]);
            noteCalls (this.components [2]);
            c0TypeHistogram.Note (this.c0Type);
            c1TypeHistogram.Note (this.c1Type);
            c2TypeHistogram.Note (this.c2Type);
#endif
            object ev2 = environment.ArgumentValue (this.rand2Offset);

            object ev1;
            Environment env = environment;
            Control unev = this.components [2];
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object ev0;
            env = environment;
            unev = this.components [1];
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination3Frame2 (this, environment, ev1, ev2));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object evop;
            env = environment;
            unev = this.components [0];
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination3Frame3 (this, environment, ev0, ev1, ev2));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
        }

    }

    [Serializable]
    class Combination3L1SSA : Combination3
    {
#if DEBUG
        static Histogram<Type> c1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
#endif
        public object ratorName;
        public int ratorOffset;
        int rand2Offset;
        protected Combination3L1SSA (LexicalVariable1 rator, SCode rand0, SCode rand1, Argument rand2)
            : base (rator, rand0, rand1, rand2)
        {
            this.ratorName = rator.Name;
            this.ratorOffset = rator.Offset;
            this.rand2Offset = rand2.Offset;
        }

        public static SCode Make (LexicalVariable1 rator, SCode rand0, SCode rand1, Argument rand2)
        {
            return new Combination3L1SSA (rator, rand0, rand1, rand2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.components [1]);
            noteCalls (this.components [2]);
            c1TypeHistogram.Note (this.c1Type);
            c2TypeHistogram.Note (this.c2Type);
            SCode.location = "Combination3L1SSA.EvalStep";
#endif
            object ev2 = environment.ArgumentValue (this.rand2Offset);

            object ev1;
            Environment env = environment;
            Control unev = this.components [2];
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination3L1SSA.EvalStep.1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object ev0;
            env = environment;
            unev = this.components [1];
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
                        SCode.location = "Combination3L1SSA.EvalStep.2";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination3Frame2 (this, environment, ev1, ev2));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object evop;
            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();


            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
        }

    }

    [Serializable]
    class Combination3SSSQ : Combination3
    {
#if DEBUG
        static Histogram<Type> c0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> c1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand2Value;

        protected Combination3SSSQ (SCode rator, SCode rand0, SCode rand1, Quotation rand2)
            : base (rator, rand0, rand1, rand2)
        {
            this.rand2Value = rand2.Quoted;
        }

        public static SCode Make (SCode rator, SCode rand0, SCode rand1, Quotation rand2)
        {
            return new Combination3SSSQ (rator, rand0, rand1, rand2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination3SSSQ.EvalStep");
            noteCalls (this.components [0]);
            noteCalls (this.components [1]);
            noteCalls (this.components [2]);
            c0TypeHistogram.Note (this.c0Type);
            c1TypeHistogram.Note (this.c1Type);
            c2TypeHistogram.Note (this.c2Type);
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.components [2];
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object ev0;
            env = environment;
            unev = this.components [1];
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination3Frame2 (this, environment, ev1, ev2));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object evop;
            env = environment;
            unev = this.components [0];
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination3Frame3 (this, environment, ev0, ev1, ev2));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, this.rand2Value);
        }

    }
#endif

}
