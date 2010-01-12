using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Serialization;
using System.Security.Permissions;

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
        protected Combination3 (SCode [] components)
            : base (components)
        {
#if DEBUG
            this.c0Type = components [0].GetType ();
            this.c1Type = components [1].GetType ();
            this.c2Type = components [2].GetType ();
            this.c3Type = components [3].GetType ();
#endif
        }

        static SCode [] makeSCodeArray (SCode rator, SCode rand0, SCode rand1, SCode rand2)
        {
            return new SCode [] { rator, rand0, rand1, rand2 };
        }
        protected Combination3 (SCode rator, SCode rand0, SCode rand1, SCode rand2)
            : this (makeSCodeArray (rator, rand0, rand1, rand2))
        {
        }

        public static SCode Make (SCode rator, SCode rand0, SCode rand1, SCode rand2)
        {
            return
                (rator is StaticVariable) ? Combination3S.Make ((StaticVariable) rator, rand0, rand1, rand2) :
                new Combination3 (rator, rand0, rand1, rand2);
        }

        public static SCode Make (object rator, object rand0, object rand1, object rand2)
        {
            return
                Combination3.Make (EnsureSCode (rator),
                                   EnsureSCode (rand0),
                                   EnsureSCode (rand1),
                                   EnsureSCode (rand2));

        }

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialResult rator = this.components [0].PartialEval (environment);
            PartialResult rand0 = this.components [1].PartialEval (environment);
            PartialResult rand1 = this.components [2].PartialEval (environment);
            PartialResult rand2 = this.components [3].PartialEval (environment);
            return new PartialResult (rator.Residual == this.components [0] &&
                rand0.Residual == this.components [1] &&
                rand1.Residual == this.components [2] &&
                rand2.Residual == this.components [3] ? this : Combination3.Make (rator.Residual, rand0.Residual, rand1.Residual, rand2.Residual));
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            this.components [0].CollectFreeVariables (freeVariableSet);
            this.components [1].CollectFreeVariables (freeVariableSet);
            this.components [2].CollectFreeVariables (freeVariableSet);
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

    [Serializable]
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

    [Serializable]
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

    [Serializable]
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

    [Serializable]
    class Combination3S : Combination3
    {
#if DEBUG
        static Histogram<Type> c1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol ratorName;
        public readonly int ratorOffset;

        protected Combination3S (StaticVariable rator, SCode rand0, SCode rand1, SCode rand2)
            : base (rator, rand0, rand1, rand2)
        {
            this.ratorName = rator.Name;
            this.ratorOffset = rator.Offset;
        }

        public static SCode Make (StaticVariable rator, SCode rand0, SCode rand1, SCode rand2)
        {
            return
                new Combination3S (rator, rand0, rand1, rand2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-"); ;
            NoteCalls (this.components [1]);
            NoteCalls (this.components [2]);
            NoteCalls (this.components [3]);
            c1TypeHistogram.Note (this.c1Type);
            c2TypeHistogram.Note (this.c2Type);
            c3TypeHistogram.Note (this.c3Type);
            SCode.location = "Combination3S";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.components [3];
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination3S";
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
            SCode.location = "Combination3S";
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
            SCode.location = "Combination3S";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination3Frame2 (this, environment, ev1, ev2));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
        }
    }

}