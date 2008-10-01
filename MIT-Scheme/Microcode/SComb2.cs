using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Serialization;

namespace Microcode
{
    [Serializable]
    class Combination2 : SCode, ISystemHunk3
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<string> histogram = new Histogram<String> ();
        protected readonly string histogramKey;

        protected readonly Type ratorType;
        protected readonly Type rand0Type;
        protected readonly Type rand1Type;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand1;

        protected Combination2 (SCode rator, SCode rand0, SCode rand1)
            : base (TC.COMBINATION_2)
        {
            this.rator = rator;
            this.rand0 = rand0;
            this.rand1 = rand1;
#if DEBUG
            this.histogramKey = rator.GetType().ToString () + " " + rand0.GetType ().Name.ToString () + " " + rand1.GetType ().Name.ToString ();
            ratorType = rator.GetType ();
            rand0Type = rand0.GetType ();
            rand1Type = rand1.GetType ();
#endif
        }

        public static SCode Make (object rator, object rand0, object rand1)
        {
            return new Combination2 (EnsureSCode(rator), EnsureSCode(rand0), EnsureSCode(rand1));
        }

        public static SCode Make (Hunk3 init)
        {
            return Make (init.Cxr0, init.Cxr1, init.Cxr2);
        }

        public SCode Operand0
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand0;
            }
        }

        public SCode Operand1
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand1;
            }
        }

        public SCode Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.rator;
            }
        }

        [SchemePrimitive ("COMBINATION2?", 1, true)]
        public static bool IsCombination2 (out object answer, object arg)
        {
            answer = arg is Combination2;
            return false;
        }

        #region ISystemHunk3 Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr0
        {
            get
            {
                return UnwrapQuoted (rator);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr1
        {
            get
            {
                return UnwrapQuoted (rand0);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr2
        {
            get
            {
                return UnwrapQuoted (rand1);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optRator = this.rator.Bind (ctenv);
            SCode optRand0 = this.rand0.Bind (ctenv);
            SCode optRand1 = this.rand1.Bind (ctenv);
            return 
                (optRand0 is Argument) ? Combination2A1.Make (optRator, (Argument)optRand0, optRand1)
                : (optRator is LexicalVariable) ? Combination2L0.Make ((LexicalVariable) optRator, optRand0, optRand1)
                : (optRator == this.rator && optRand0 == this.rand0 && optRand1 == this.rand1) ? this
                : new Combination2 (optRator, optRand0, optRand1);
        }


        public override bool CallsTheEnvironment ()
        {
            return this.rator.CallsTheEnvironment ()
                || this.rand0.CallsTheEnvironment ()
                || this.rand1.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rator);
            noteCalls (this.rand0);
            noteCalls (this.rand1);
            histogram.Note (this.histogramKey);
            ratorTypeHistogram.Note (this.ratorType);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            // expression = (SCode) evop;
            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }

        public override bool MutatesAny (object [] formals)
        {
            return this.rator.MutatesAny (formals)
                || this.rand0.MutatesAny (formals)
                || this.rand1.MutatesAny (formals);
        }

        public override bool UsesAny (object [] formals)
        {
            return this.rator.UsesAny (formals)
                || this.rand0.UsesAny (formals)
                || this.rand1.UsesAny (formals);
        }
    }

    [Serializable]
    class Combination2Frame0 : SubproblemContinuation<Combination2>, ISystemVector
    {

        public Combination2Frame0 (Combination2 combination2, Environment environment)
            : base (combination2, environment)
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            object ev0 = null;
            Environment env = environment;
            Control unev0 = this.expression.Operand0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object evop = null;
            env = environment;
            Control unevop = this.expression.Operator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, value);
        }
    }

    [Serializable]
    class Combination2Frame1 : SubproblemContinuation<Combination2>, ISystemVector
    {
        readonly object ev1;

        public Combination2Frame1 (Combination2 combination2, Environment environment, object ev1)
            : base (combination2, environment)
        {
            this.ev1 = ev1;
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
            object evop = null;
            Environment env = environment;
            Control unevop = this.expression.Operator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, value, this.ev1);
        }
    }

    [Serializable]
    class Combination2Frame2 : SubproblemContinuation<Combination2>, ISystemVector
    {
        readonly object ev0;
        readonly object ev1;

        public Combination2Frame2 (Combination2 combination2, Environment environment, object ev0, object ev1)
            : base (combination2, environment)
        {
            this.ev0 = ev0;
            this.ev1 = ev1;
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
            object evop = null;
            Environment env = environment;
            Control unevop = this.expression.Operator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, value, this.ev0, this.ev1);
        }
    }

    [Serializable]
    class Combination2L0 : Combination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected string ratorName;
        protected int ratorDepth;
        protected int ratorOffset;

        protected Combination2L0 (LexicalVariable rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.ratorName = rator.name;
            this.ratorDepth = rator.Depth;
            this.ratorOffset = rator.Offset;
        }

        public static SCode Make (LexicalVariable rator, SCode rand0, SCode rand1)
        {
            return new Combination2L0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
            noteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
#endif

            object ev1 = null;
            Environment env = environment;
            Control unev1 = this.rand1;
            while (unev1.EvalStep (out ev1, ref unev1, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0 = null;
            env = environment;
            Control unev0 = this.rand0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop = null;
            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
                throw new NotImplementedException ();

            // expression = (SCode) evop;
            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2A1 : Combination2
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<string> histogram = new Histogram<String> ();
#endif
        protected int a1offset;
        
        Combination2A1 (SCode rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.a1offset = rand0.Offset;
        }

        public static SCode Make (SCode rator, Argument rand0, SCode rand1)
        {
            return new Combination2A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rator);
            noteCalls (this.rand1);
            histogram.Note (this.histogramKey);
            ratorTypeHistogram.Note (this.ratorType);
            rand1TypeHistogram.Note (this.rand1Type);
#endif

            object ev1 = null;
            Environment env = environment;
            Control unev1 = this.rand1;
            while (unev1.EvalStep (out ev1, ref unev1, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object evop = null;
            env = environment;
            Control unevop = this.rator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            // expression = (SCode) evop;
            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue(a1offset), ev1);
        }
    }
}