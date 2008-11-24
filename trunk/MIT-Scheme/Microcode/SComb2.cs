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

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Type ratorType;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Type rand0Type;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
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

        public static SCode Make (SCode rator, SCode rand0, SCode rand1)
        {            
            return
                (Configuration.EnableSuperOperators && rator is Lambda) ? Let2.Make ((Lambda) rator, rand0, rand1)
                : (Configuration.EnableSuperOperators && rator is LexicalVariable) ? Combination2L.Make ((LexicalVariable) rator, rand0, rand1)
                : (Configuration.EnableSuperOperators && rand0 is LexicalVariable) ? Combination2SL.Make (rator, (LexicalVariable) rand0, rand1)
                : (Configuration.EnableSuperOperators && rand0 is Quotation) ? Combination2SQ.Make (rator, (Quotation) rand0, rand1)
                : (Configuration.EnableSuperOperators && rand1 is LexicalVariable) ? Combination2SSL.Make (rator, rand0, (LexicalVariable) rand1)
                 : (Configuration.EnableSuperOperators && rand1 is LexicalVariable) ? Combination2SSQ.Make (rator, rand0, (Quotation) rand1)
                 : new Combination2 (rator, rand0, rand1);
        }

        public static SCode Make (object rator, object rand0, object rand1)
        {
            SCode srator = EnsureSCode (rator);
            SCode srand0 = EnsureSCode (rand0);
            SCode srand1 = EnsureSCode (rand1);
            return Make (srator, srand0, srand1);
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

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode optRator = this.rator.Bind (ctenv);
            SCode optRand0 = this.rand0.Bind (ctenv);
            SCode optRand1 = this.rand1.Bind (ctenv);
            return  (optRator == this.rator && optRand0 == this.rand0 && optRand1 == this.rand1) ? this
                : Combination2.Make (optRator, optRand0, optRand1);
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
            Warm ("Combination2.EvalStep");
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

        public override SCode Substitute (object name, object newObject)
        {
            throw new NotImplementedException ();
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
    class Let2 : Combination2
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected object [] formals;
        public SCode body;

        protected Let2 (Lambda rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            formals = rator.Formals;
            body = rator.Body;
        }

        public static SCode Make (Lambda rator, SCode arg0, SCode arg1)
        {
            return (rator is SimpleLambda) ? SimpleLet2.Make ((SimpleLambda) rator, arg0, arg1)
                : (arg0 is LexicalVariable) ? Let2L.Make (rator, (LexicalVariable) arg0, arg1)
                : (arg0 is Quotation) ? Let2Q.Make (rator, (Quotation) arg0, arg1)
                : new Let2 (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Let2.EvalStep");
            noteCalls (this.rator);
            noteCalls (this.rand0);
            noteCalls (this.rand1);
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object ev0;
            env = environment;
            unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            // expression = (SCode) evop;
            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);

        }

        public object [] Formals
        {
            get
            {
                return this.formals;
            }
        }
        public SCode Body
        {
            get
            {
                return this.body;
            }
        }

    }

    [Serializable]
    class SimpleLet2 : Let2
    {
        protected SimpleLet2 (SimpleLambda rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (SimpleLambda rator, SCode arg0, SCode arg1)
        {
            return
                (arg0 is LexicalVariable) ? SimpleLet2L.Make (rator, (LexicalVariable) arg0, arg1)
                : (arg0 is Quotation) ? SimpleLet2Q.Make (rator, (Quotation) arg0, arg1)
                : new SimpleLet2 (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet2.EvalStep");
            noteCalls (this.rator);
            noteCalls (this.rand0);
            noteCalls (this.rand1);
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object ev0;
            env = environment;
            unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            //object evop;
            //env = environment;
            //unev = this.rator;
            //while (unev.EvalStep (out evop, ref unev, ref env)) { };
            //if (evop == Interpreter.UnwindStack) {
            //    throw new NotImplementedException ();
            //    //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
            //    //environment = env;
            //    //answer = Interpreter.UnwindStack;
            //    //return false;
            //}

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment2 (cl, ev0, ev1);
            answer = null;
            return true;
        }

    }

    [Serializable]
    class SimpleLet2L : SimpleLet2
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected SimpleLet2L (SimpleLambda rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (SimpleLambda rator, LexicalVariable arg0, SCode arg1)
        {
            return
                (arg1 is LexicalVariable) ? SimpleLet2LL.Make (rator, (LexicalVariable) arg0, (LexicalVariable) arg1)
                : (arg1 is Quotation) ? SimpleLet2LQ.Make (rator, (LexicalVariable) arg0, (Quotation) arg1)
                : new SimpleLet2L (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet2L.EvalStep");
            noteCalls (this.rator);
            noteCalls (this.rand1);
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            //object evop;
            //env = environment;
            //unev = this.rator;
            //while (unev.EvalStep (out evop, ref unev, ref env)) { };
            //if (evop == Interpreter.UnwindStack) {
            //    throw new NotImplementedException ();
            //    //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
            //    //environment = env;
            //    //answer = Interpreter.UnwindStack;
            //    //return false;
            //}

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment2 (cl, ev0, ev1);
            answer = null;
            return true;
            
            // return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }

    }

    [Serializable]
    class SimpleLet2LL : SimpleLet2L
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected SimpleLet2LL (SimpleLambda rator, LexicalVariable rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (SimpleLambda rator, LexicalVariable arg0, LexicalVariable arg1)
        {
            return new SimpleLet2LL (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet2LL.EvalStep");
            noteCalls (this.rator);
#endif

            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            //object evop;
            //env = environment;
            //unev = this.rator;
            //while (unev.EvalStep (out evop, ref unev, ref env)) { };
            //if (evop == Interpreter.UnwindStack) {
            //    throw new NotImplementedException ();
            //    //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
            //    //environment = env;
            //    //answer = Interpreter.UnwindStack;
            //    //return false;
            //}

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment2 (cl, ev0, ev1);
            answer = null;
            return true;
            // return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }

    }

    [Serializable]
    class SimpleLet2LQ : SimpleLet2L
    {
        public readonly object rand1Value;

        protected SimpleLet2LQ (SimpleLambda rator, LexicalVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (SimpleLambda rator, LexicalVariable arg0, Quotation arg1)
        {
            return new SimpleLet2LQ (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            //object evop;
            //env = environment;
            //unev = this.rator;
            //while (unev.EvalStep (out evop, ref unev, ref env)) { };
            //if (evop == Interpreter.UnwindStack) {
            //    throw new NotImplementedException ();
            //    //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
            //    //environment = env;
            //    //answer = Interpreter.UnwindStack;
            //    //return false;
            //}

            throw new NotImplementedException ();
            // return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }

    }

    [Serializable]
    class SimpleLet2Q : SimpleLet2
    {
        public readonly object rand0Value;

        protected SimpleLet2Q (SimpleLambda rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        public static SCode Make (SimpleLambda rator, Quotation arg0, SCode arg1)
        {
            return
                (arg1 is LexicalVariable) ? SimpleLet2QL.Make (rator,  arg0, (LexicalVariable) arg1)
                : (arg1 is Quotation) ? SimpleLet2QQ.Make (rator,  arg0, (Quotation) arg1)
                : new SimpleLet2Q (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rator);
            noteCalls (this.rand1);
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object ev0 = this.rand0Value;


            //object evop;
            //env = environment;
            //unev = this.rator;
            //while (unev.EvalStep (out evop, ref unev, ref env)) { };
            //if (evop == Interpreter.UnwindStack) {
            //    throw new NotImplementedException ();
            //    //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
            //    //environment = env;
            //    //answer = Interpreter.UnwindStack;
            //    //return false;
            //}

            throw new NotImplementedException ();
            // return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }

    }

    [Serializable]
    class SimpleLet2QL : SimpleLet2Q
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected SimpleLet2QL (SimpleLambda rator, Quotation rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (SimpleLambda rator, Quotation arg0, LexicalVariable arg1)
        {
            return new SimpleLet2QL (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet2QL.EvalStep");
            noteCalls (this.rator);
#endif

            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = this.rand0Value;


            //object evop;
            //env = environment;
            //unev = this.rator;
            //while (unev.EvalStep (out evop, ref unev, ref env)) { };
            //if (evop == Interpreter.UnwindStack) {
            //    throw new NotImplementedException ();
            //    //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
            //    //environment = env;
            //    //answer = Interpreter.UnwindStack;
            //    //return false;
            //}

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment2 (cl, ev0, ev1);
            answer = null;
            return true;
            // return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }

    }

    [Serializable]
    class SimpleLet2QQ : SimpleLet2Q
    {
        public readonly object rand1Value;

        protected SimpleLet2QQ (SimpleLambda rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (SimpleLambda rator, Quotation arg0, Quotation arg1)
        {
            return new SimpleLet2QQ (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet2QQ.EvalStep");
            noteCalls (this.rator);

#endif

            object ev1 = this.rand1Value;


            object ev0 = this.rand0Value;


            //object evop;
            //env = environment;
            //unev = this.rator;
            //while (unev.EvalStep (out evop, ref unev, ref env)) { };
            //if (evop == Interpreter.UnwindStack) {
            //    throw new NotImplementedException ();
            //    //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
            //    //environment = env;
            //    //answer = Interpreter.UnwindStack;
            //    //return false;
            //}

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment2 (cl, ev0, ev1);
            answer = null;
            return true;

            // return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }

    }

    [Serializable]
    class Let2L : Let2
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected Let2L (Lambda rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Lambda rator, LexicalVariable arg0, SCode arg1)
        {
            return
                (arg1 is LexicalVariable) ? Let2LL.Make (rator, arg0, (LexicalVariable) arg1)
                : (arg1 is Quotation) ? Let2LQ.Make (rator, arg0, (Quotation) arg1)
                : new Let2L (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class Let2LL : Let2L
    {

        protected Let2LL (Lambda rator, LexicalVariable rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Lambda rator, LexicalVariable arg0, LexicalVariable arg1)
        {
            return new Let2LL (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class Let2LQ : Let2L
    {
        public readonly object rand1Value;

        protected Let2LQ (Lambda rator, LexicalVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Lambda rator, LexicalVariable arg0, Quotation arg1)
        {
            return new Let2LQ (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Let2LQ.EvalStep");
            noteCalls (this.rator);
#endif

            object ev1 = this.rand1Value;

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }


    [Serializable]
    class Let2Q : Let2
    {
        public readonly object rand0Value;

        protected Let2Q (Lambda rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        public static SCode Make (Lambda rator, Quotation arg0, SCode arg1)
        {
            return
                (arg1 is LexicalVariable) ? Let2QL.Make (rator, arg0, (LexicalVariable) arg1)
                : (arg1 is Quotation) ? Let2QQ.Make (rator, arg0, (Quotation) arg1)
                : new Let2Q (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    [Serializable]
    class Let2QL : Let2Q
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected Let2QL (Lambda rator, Quotation rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Lambda rator, Quotation arg0, LexicalVariable arg1)
        {
            return new Let2QL (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }


    }

    [Serializable]
    class Let2QQ : Let2Q
    {
        public readonly object rand1Value;

        protected Let2QQ (Lambda rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Lambda rator, Quotation arg0, Quotation arg1)
        {
            return new Let2QQ (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Let2QQ.EvalStep");
            noteCalls (this.rator);
#endif

            object ev1 = this.rand1Value;

            object ev0 = this.rand0Value;


            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }
            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }


    }


    [Serializable]
    class Combination2L : Combination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object ratorName;
        public readonly int ratorDepth;
        public readonly int ratorOffset;

        protected Combination2L (LexicalVariable rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.ratorName = rator.Name;
            this.ratorDepth = rator.Depth;
            this.ratorOffset = rator.Offset;
        }

        public static SCode Make (LexicalVariable rator, SCode rand0, SCode rand1)
        {
            return 
                (rator is LexicalVariable1) ? Combination2L1.Make ((LexicalVariable1) rator, rand0, rand1)
                : (rand0 is LexicalVariable) ? Combination2LL.Make (rator, (LexicalVariable) rand0, rand1)
                : (rand0 is PrimitiveCarA0) ? Combination2LCarA0.Make (rator, (PrimitiveCarA0) rand0, rand1)
                : (rand0 is PrimitiveCdrA0) ? Combination2LCdrA0.Make (rator, (PrimitiveCdrA0) rand0, rand1)
                : (rand0 is Quotation) ? Combination2LQ.Make (rator, (Quotation) rand0, rand1)
                : (rand1 is LexicalVariable) ? Combination2LSL.Make (rator,  rand0, (LexicalVariable)rand1)
                : (rand1 is Quotation) ? Combination2LSQ.Make (rator, rand0, (Quotation) rand1)
                : new Combination2L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2L.EvalStep");
            noteCalls (this.rand0);
            noteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            env = environment;
            unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object evop;
            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2L1 : Combination2L
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected Combination2L1 (LexicalVariable1 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (LexicalVariable1 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is LexicalVariable) ? Combination2L1L.Make (rator, (LexicalVariable) rand0, rand1)
                //: (rand0 is PrimitiveCarA0) ? Combination2LCarA0.Make (rator, (PrimitiveCarA0) rand0, rand1)
                //: (rand0 is PrimitiveCdrA0) ? Combination2LCdrA0.Make (rator, (PrimitiveCdrA0) rand0, rand1)
                //: (rand0 is Quotation) ? Combination2LQ.Make (rator, (Quotation) rand0, rand1)
                //: (rand1 is LexicalVariable) ? Combination2LSL.Make (rator, rand0, (LexicalVariable) rand1)
                //: (rand1 is Quotation) ? Combination2LSQ.Make (rator, rand0, (Quotation) rand1)
                : new Combination2L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            noteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2L1.EvalStep";
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2L1.EvalStep.1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            env = environment;
            unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2L1.EvalStep.2";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object evop;
            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2L1L : Combination2L1
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object rand0Name;
        protected readonly int rand0Depth;
        protected readonly int rand0Offset;

        protected Combination2L1L (LexicalVariable1 rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (LexicalVariable1 rator, LexicalVariable rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? Combination2L1A.Make (rator, (Argument) rand0, rand1) :
                new Combination2L1L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2L1L.EvalStep";
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
                        SCode.location = "Combination2L1L.EvalStep.1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2L1A : Combination2L1L
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected Combination2L1A (LexicalVariable1 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (LexicalVariable1 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? Combination2L1A0.Make (rator, (Argument0) rand0, rand1) :
                (rand0 is Argument1) ? Combination2L1A1.Make (rator, (Argument1) rand0, rand1) :
                new Combination2L1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2L1A.EvalStep";
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2L1A.EvalStep.1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            object evop;
            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    class Combination2L1A0 : Combination2L1A
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected Combination2L1A0 (LexicalVariable1 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (LexicalVariable1 rator, Argument0 rand0, SCode rand1)
        {
            return
               
                new Combination2L1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2L1A0.EvalStep";
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2L1A0.EvalStep.1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument0Value;

            object evop;
            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    class Combination2L1A1 : Combination2L1A
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected Combination2L1A1 (LexicalVariable1 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (LexicalVariable1 rator, Argument1 rand0, SCode rand1)
        {
            return

                new Combination2L1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2L1A0.EvalStep";
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2L1A1.EvalStep.1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = environment.Argument1Value;

            object evop;
            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }



    [Serializable]
    class Combination2LL : Combination2L
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type>();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type>();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type>();
#endif
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected Combination2LL (LexicalVariable rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (LexicalVariable rator, LexicalVariable rand0, SCode rand1)
        {
            return (rand1 is LexicalVariable) ? Combination2LLL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? Combination2LLQ.Make (rator, rand0, (Quotation) rand1)
                : new Combination2LL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            ratorTypeHistogram.Note (this.ratorType);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            Warm ("Combination2LL.EvalStep");
            noteCalls (this.rand1);
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
                throw new NotImplementedException ();
            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1); 
        }
    }

    [Serializable]
    class Combination2LLL : Combination2LL
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        Combination2LLL (LexicalVariable rator, LexicalVariable rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (LexicalVariable rator, LexicalVariable rand0, LexicalVariable rand1)
        {
            return new Combination2LLL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2LLL.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2LLQ : Combination2LL
    {
        public readonly object rand1Value;

        Combination2LLQ (LexicalVariable rator, LexicalVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (LexicalVariable rator, LexicalVariable rand0, Quotation rand1)
        {
            return new Combination2LLQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2LLQ.EvalStep");
#endif
            object ev1 = this.rand1Value;

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    class Combination2LCarA0 : Combination2L
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2LCarA0 (LexicalVariable rator, PrimitiveCarA0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (LexicalVariable rator, PrimitiveCarA0 rand0, SCode rand1)
        {
            return
                 new Combination2LCarA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2LCarA0.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = ((Cons) environment.Argument0Value).Car;

            object evop;
            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    class Combination2LCdrA0 : Combination2L
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2LCdrA0 (LexicalVariable rator, PrimitiveCdrA0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (LexicalVariable rator, PrimitiveCdrA0 rand0, SCode rand1)
        {
            return
                 new Combination2LCdrA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2LCdrA0.EvalStep");
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = ((Cons) environment.Argument0Value).Cdr;

            object evop;
            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }


    [Serializable]
    class Combination2LQ : Combination2L
    {
        public readonly object rand0Value;

        protected Combination2LQ (LexicalVariable rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        public static SCode Make (LexicalVariable rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? Combination2LQL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? Combination2LQQ.Make (rator, rand0, (Quotation) rand1)
                : new Combination2LQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2LQ.EvalStep");
            noteCalls (this.rand1);
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object ev0 = this.rand0Value;

            object evop;
            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2LQL : Combination2LQ
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        Combination2LQL (LexicalVariable rator, Quotation rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (LexicalVariable rator, Quotation rand0, LexicalVariable rand1)
        {
            return new Combination2LQL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2LQL.EvalStep");
#endif

            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = this.rand0Value;

            object evop;
            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2LQQ : Combination2LQ
    {
        public readonly object rand1Value;

        Combination2LQQ (LexicalVariable rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (LexicalVariable rator, Quotation rand0, Quotation rand1)
        {
            return new Combination2LQQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2LQQ.EvalStep");
#endif

            object ev1 = this.rand1Value;

            object ev0 = this.rand0Value;

            object evop;
            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1); 
        }
    }


    [Serializable]
    class Combination2LSL : Combination2L
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        Combination2LSL (LexicalVariable rator, SCode rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (LexicalVariable rator, SCode rand0, LexicalVariable rand1)
        {
            return new Combination2LSL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("Combination2LSL.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object evop;
            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }


    [Serializable]
    class Combination2LSQ : Combination2L
    {
        public readonly object rand1Value;

        Combination2LSQ (LexicalVariable rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (LexicalVariable rator, SCode rand0, Quotation rand1)
        {
            return new Combination2LSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2LSQ.EvalStep");
            noteCalls (this.rand0);

#endif

            object ev1 = this.rand1Value;

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object evop;
            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
                throw new NotImplementedException ();

            // expression = (SCode) evop;
            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }


    [Serializable]
    class Combination2SL : Combination2
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected Combination2SL (SCode rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (SCode rator, LexicalVariable rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? Combination2SLL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? Combination2SLQ.Make (rator, rand0, (Quotation) rand1)
                : new Combination2SL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2SL.EvalStep");
            noteCalls (this.rator);
            noteCalls (this.rand1);
            ratorTypeHistogram.Note (this.ratorType);
            rand1TypeHistogram.Note (this.rand1Type);
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2SQ : Combination2
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;

        protected Combination2SQ (SCode rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        public static SCode Make (SCode rator, Quotation rand0, SCode rand1)
        {
            return (rand1 is LexicalVariable) ? Combination2SQL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? Combination2SQQ.Make (rator, rand0, (Quotation) rand1)
                : new Combination2SQ (rator, rand0, rand1);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2SQ.EvalStep");
            noteCalls (this.rator);
            noteCalls (this.rand1);
            ratorTypeHistogram.Note (this.ratorType);
            rand1TypeHistogram.Note (this.rand1Type);
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0 = this.rand0Value;


            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2SQL : Combination2SQ
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected Combination2SQL (SCode rator, Quotation rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (SCode rator, Quotation rand0, LexicalVariable rand1)
        {
            return new Combination2SQL (rator, rand0, rand1);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2SQL.EvalStep");
            noteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);

#endif

            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = this.rand0Value;


            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2SQQ : Combination2SQ
    {
        public readonly object rand1Value;

        Combination2SQQ (SCode rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (SCode rator, Quotation rand0, Quotation rand1)
        {
            return new Combination2SQQ (rator, rand0, rand1);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2SQQ.EvalStep");
            noteCalls (this.rator);
#endif

            object ev1 = this.rand1Value;

            object ev0 = this.rand0Value;


            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }


    [Serializable]
    class Combination2SSL : Combination2
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif

        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        Combination2SSL (SCode rator, SCode rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (SCode rator, SCode rand0, LexicalVariable rand1)
        {
            return new Combination2SSL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2SSL.EvalStep");
            noteCalls (this.rator);
            noteCalls (this.rand0);
            ratorTypeHistogram.Note (this.ratorType);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2SSQ : Combination2
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif

        public readonly object rand1Value;

        Combination2SSQ (SCode rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (SCode rator, SCode rand0, Quotation rand1)
        {
            return new Combination2SSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rator);
            noteCalls (this.rand0);
            ratorTypeHistogram.Note (this.ratorType);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1 = this.rand1Value;


            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }


    // ---------------



    [Serializable]
    class Combination2SLL : Combination2SL
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        Combination2SLL (SCode rator, LexicalVariable rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {

            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (SCode rator, LexicalVariable rand0, LexicalVariable rand1)
        {
            return new Combination2SLL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2SLL.EvalStep";
#endif

            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2SLL.EvalStep";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);

        }
    }

//    [Serializable]
//    class Combination2LSL : Combination2
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type>();
//#endif
//        public readonly object ratorName;
//        public readonly int ratorDepth;
//        public readonly int ratorOffset;

//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        Combination2LSL (LexicalVariable rator, SCode rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.ratorName = rator.Name;
//            this.ratorDepth = rator.Depth;
//            this.ratorOffset = rator.Offset;

//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (LexicalVariable rator, SCode rand0, LexicalVariable rand1)
//        {
//            return new Combination2LSL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//#endif

//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();


//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//            }

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

    [Serializable]
    class Combination2LLS : Combination2
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type>();
#endif
        public readonly object ratorName;
        public readonly int ratorDepth;
        public readonly int ratorOffset;

        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        Combination2LLS (LexicalVariable rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.ratorName = rator.Name;
            this.ratorDepth = rator.Depth;
            this.ratorOffset = rator.Offset;

            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (LexicalVariable rator, LexicalVariable rand0, SCode rand1)
        {
            return new Combination2LLS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2SLQ : Combination2SL
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object rand1Value;

        Combination2SLQ (SCode rator, LexicalVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (SCode rator, LexicalVariable rand0, Quotation rand1)
        {
            return new Combination2SLQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2SLQ.EvalStep");
            noteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
#endif

            object ev1 = this.rand1Value;
 
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

//    [Serializable]
//    class Combination2LSS : Combination2
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected object ratorName;
//        protected int ratorDepth;
//        protected int ratorOffset;

//        protected Combination2LSS (LexicalVariable rator, SCode rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.ratorName = rator.Name;
//            this.ratorDepth = rator.Depth;
//            this.ratorOffset = rator.Offset;
//        }

//        public static SCode Make (LexicalVariable rator, SCode rand0, SCode rand1)
//        {
//            return new Combination2LSS (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand0);
//            noteCalls (this.rand1);
//            rand0TypeHistogram.Note (this.rand0Type);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2LSSFrame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object ev0;
//            env = environment;
//            unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2LSSFrame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            // expression = (SCode) evop;
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    [Serializable]
//    class Combination2L1SS : Combination2LSS
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2L1SS (LexicalVariable1 rator, SCode rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, SCode rand0, SCode rand1)
//        {
//            return 
//                (rand0 is Argument) ? Combination2L1AS.Make (rator, (Argument) rand0, rand1)
//                : new Combination2L1SS (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand0);
//            noteCalls (this.rand1);
//            rand0TypeHistogram.Note (this.rand0Type);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object ev0;
//            env = environment;
//            unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object evop = null;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            // expression = (SCode) evop;
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    [Serializable]
//    class Combination2L1AS : Combination2LSS
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        readonly int argOffset;

//        protected Combination2L1AS (LexicalVariable1 rator, Argument rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.argOffset = rand0.Offset;
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument rand0, SCode rand1)
//        {
//            return
//                (rand0 is Argument0) ? Combination2L1A0S.Make (rator, (Argument0) rand0, rand1)
//                : (rand0 is Argument1) ? Combination2L1A1S.Make (rator, (Argument1) rand0, rand1)
//                : new Combination2L1AS (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand0);
//            noteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object evop = null;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            // expression = (SCode) evop;
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue(argOffset), ev1);
//        }
//    }

//        [Serializable]
//    class Combination2L1A0S : Combination2LSS
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2L1A0S (LexicalVariable1 rator, Argument0 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument0 rand0, SCode rand1)
//        {
//            return new Combination2L1A0S (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object evop = null;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            // expression = (SCode) evop;
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, ev1);
//        }
//    }

//        [Serializable]
//    class Combination2L1A1S : Combination2LSS
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2L1A1S (LexicalVariable1 rator, Argument1 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument1 rand0, SCode rand1)
//        {
//            return new Combination2L1A1S (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object evop = null;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            // expression = (SCode) evop;
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value, ev1);
//        }
//    }

[Serializable]
class Combination2AAA : Combination2
{
        protected int a1offset;
        
        Combination2AAA (Argument rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.a1offset = rand0.Offset;
        }

        public static SCode Make (Argument rator, Argument rand0, Argument rand1)
        {
            return new Combination2AAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
throw new NotImplementedException();
    }
}

[Serializable]
class Combination2AAS : Combination2
{
#if DEBUG
    static Histogram<Type> rand1TypeHistogram = new Histogram<Type>();
#endif
    public int ratorOffset;
    public int rand0Offset;

    Combination2AAS (Argument rator, Argument rand0, SCode rand1)
        : base (rator, rand0, rand1)
    {
        this.ratorOffset = rator.Offset;
        this.rand0Offset = rand0.Offset;
    }

    public static SCode Make (Argument rator, Argument rand0, SCode rand1)
    {
        return new Combination2AAS (rator, rand0, rand1);
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
        noteCalls (this.rand1);
        rand1TypeHistogram.Note (this.rand1Type);
#endif

        object ev1;
        Environment env = environment;
        Control unev = this.rand1;
        while (unev.EvalStep (out ev1, ref unev, ref env)) { };
        if (ev1 == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        object ev0 = environment.ArgumentValue (this.rand0Offset);

        object evop = environment.ArgumentValue (this.ratorOffset);

        return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
    }


}

[Serializable]
class Combination2ASA : Combination2
{
#if DEBUG
    static Histogram<Type> rand0TypeHistogram = new Histogram<Type>();
#endif
    public int ratorOffset;
    public int rand1Offset;

    Combination2ASA (Argument rator, SCode rand0, Argument rand1)
        : base (rator, rand0, rand1)
    {
        this.ratorOffset = rator.Offset;
        this.rand1Offset = rand1.Offset;
    }

    public static SCode Make (Argument rator, SCode rand0, Argument rand1)
    {
        return new Combination2ASA (rator, rand0, rand1);
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
        noteCalls (this.rand0);
        rand0TypeHistogram.Note (this.rand0Type);
#endif

        object ev1 = environment.ArgumentValue (this.rand1Offset);

        object ev0;
        Environment env = environment;
        Control unev = this.rand0;
        while (unev.EvalStep (out ev0, ref unev, ref env)) { };
        if (ev0 == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        object evop = environment.ArgumentValue (this.ratorOffset);

        return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
    }
}

[Serializable]
class Combination2SAA: Combination2
{
#if DEBUG
    static Histogram<Type> procedureTypeHistogram = new Histogram<Type>();
#endif
    public int arg0Offset;
    public int arg1Offset;

    Combination2SAA (SCode rator, Argument rand0, Argument rand1)
        : base (rator, rand0, rand1)
    {
        this.arg0Offset = rand0.Offset;
        this.arg1Offset = rand1.Offset;
    }

    public static SCode Make (SCode rator, Argument rand0, Argument rand1)
    {
        return new Combination2SAA (rator, rand0, rand1);
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
        noteCalls (this.rator);
        procedureTypeHistogram.Note (this.ratorType);
#endif

        object ev1 = environment.ArgumentValue (this.arg1Offset);

        object ev0 = environment.ArgumentValue (this.arg0Offset);

        object evop;
        Environment env = environment;
        Control unev = this.rator;
        while (unev.EvalStep (out evop, ref unev, ref env)) { };
        if (evop == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        // expression = (SCode) evop;
        return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
    }


}

class Combination2SSA : Combination2
{
#if DEBUG
    static Histogram<Type> ratorTypeHistogram = new Histogram<Type>();
    static Histogram<Type> rand0TypeHistogram = new Histogram<Type>();
#endif

    public int arg1Offset;
    Combination2SSA (SCode rator, SCode rand0, Argument rand1)
        : base (rator, rand0, rand1)
    {
        this.arg1Offset = rand1.Offset;
    }

    public static SCode Make (SCode rator, SCode rand0, Argument rand1)
    {
        return new Combination2SSA (rator, rand0, rand1);
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
        noteCalls (this.rator);
        noteCalls (this.rand0);
        ratorTypeHistogram.Note (this.ratorType);
        rand0TypeHistogram.Note (this.rand0Type);
#endif

        object ev1 = environment.ArgumentValue (this.arg1Offset);

        object ev0;
        Environment env = environment;
        Control unev = this.rand0;
        while (unev.EvalStep (out ev0, ref unev, ref env)) { };
        if (ev0 == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        object evop;
        env = environment;
        unev = this.rator;
        while (unev.EvalStep (out evop, ref unev, ref env)) { };
        if (evop == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        // expression = (SCode) evop;
        return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
    }


}

[Serializable]
class Combination2SAS : Combination2
{
#if DEBUG
    static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
    static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
    static Histogram<string> histogram = new Histogram<String> ();
#endif
    protected int a1offset;

    Combination2SAS (SCode rator, Argument rand0, SCode rand1)
        : base (rator, rand0, rand1)
    {
        this.a1offset = rand0.Offset;
    }

    public static SCode Make (SCode rator, Argument rand0, SCode rand1)
    {
        return (rator is LexicalVariable1) ? Combination2L1AS.Make ((LexicalVariable1) rator, rand0, rand1)
            : new Combination2SAS (rator, rand0, rand1);
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
        return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue (a1offset), ev1);
    }
}

class Combination2ASS : Combination2
{
#if DEBUG
    static Histogram<Type> rand0TypeHistogram = new Histogram<Type>();
    static Histogram<Type> rand1TypeHistogram = new Histogram<Type>();
#endif
    public int pOffset;
    Combination2ASS (Argument rator, SCode rand0, SCode rand1)
        : base (rator, rand0, rand1)
    {
        this.pOffset = rator.Offset;
    }

    public static SCode Make (Argument rator, SCode rand0, SCode rand1)
    {
        return new Combination2ASS (rator, rand0, rand1);
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

        object ev1;
        Environment env = environment;
        Control unev = this.rand1;
        while (unev.EvalStep (out ev1, ref unev, ref env)) { };
        if (ev1 == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        object ev0;
        env = environment;
        unev = this.rand0;
        while (unev.EvalStep (out ev0, ref unev, ref env)) { };
        if (ev0 == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        object evop = environment.ArgumentValue (this.pOffset);
        return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
    }


}


[Serializable]
class Combination2L1AS : Combination2
{
#if DEBUG
    static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
    static Histogram<string> histogram = new Histogram<String> ();
#endif
    protected object rName;
    protected int rOffset;
    protected int a1offset;

    Combination2L1AS (LexicalVariable1 rator, Argument rand0, SCode rand1)
        : base (rator, rand0, rand1)
    {
        this.rName = rator.Name;
        this.rOffset = rator.Offset;
        this.a1offset = rand0.Offset;
    }

    public static SCode Make (LexicalVariable1 rator, Argument rand0, SCode rand1)
    {
        return  new Combination2L1AS (rator, rand0, rand1);
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
        noteCalls (this.rand1);
        histogram.Note (this.histogramKey);
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

        object evop;
        if (environment.FastLexicalRef1 (out evop, this.rName, this.rOffset))
            throw new NotImplementedException ();

        return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue (a1offset), ev1);
    }
}


[Serializable]
class Combination2L1L1S : Combination2
{
#if DEBUG
    static Histogram<Type> rand1TypeHistogram = new Histogram<Type>();
#endif
    public object ratorName;
    public int ratorOffset;
    public object rand0Name;
    public int rand0Offset;

    Combination2L1L1S (LexicalVariable1 rator, LexicalVariable1 rand0, SCode rand1)
        : base (rator, rand0, rand1)
    {
        this.ratorName = rator.Name;
        this.ratorOffset = rator.Offset;
        this.rand0Name = rand0.Name;
        this.rand0Offset = rand0.Offset;
    }

    public static SCode Make (LexicalVariable1 rator, LexicalVariable1 rand0, SCode rand1)
    {
        return new Combination2L1L1S (rator, rand0, rand1);
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
        noteCalls (this.rand1);
        rand1TypeHistogram.Note (this.rand1Type);
#endif

        object ev1;
        Environment env = environment;
        Control unev = this.rand1;
        while (unev.EvalStep (out ev1, ref unev, ref env)) { };
        if (ev1 == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        object ev0; if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset)) throw new NotImplementedException();

        object evop; if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset)) throw new NotImplementedException ();

        return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
    }


}

[Serializable]
class Combination2L1SL1 : Combination2
{
#if DEBUG
    static Histogram<Type> rand0TypeHistogram = new Histogram<Type>();
#endif
    public object ratorName;
    public int ratorOffset;
    public object rand1Name;
    public int rand1Offset;

    Combination2L1SL1 (LexicalVariable1 rator, SCode rand0, LexicalVariable1 rand1)
        : base (rator, rand0, rand1)
    {
        this.ratorName = rator.Name;
        this.ratorOffset = rator.Offset;
        this.rand1Name = rand1.Name;
        this.rand1Offset = rand1.Offset;
    }

    public static SCode Make (LexicalVariable1 rator, SCode rand0, LexicalVariable1 rand1)
    {
        return new Combination2L1SL1 (rator, rand0, rand1);
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
        noteCalls (this.rand0);
        rand0TypeHistogram.Note (this.rand0Type);
#endif

        object ev1; if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset)) throw new NotImplementedException ();

        object ev0;
        Environment env = environment;
        Control unev = this.rand0;
        while (unev.EvalStep (out ev0, ref unev, ref env)) { };
        if (ev0 == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        object evop; if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset)) throw new NotImplementedException();

        return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
    }
}

[Serializable]
class Combination2SL1L1: Combination2
{
#if DEBUG
    static Histogram<Type> procedureTypeHistogram = new Histogram<Type>();
#endif
    public object arg0Name;
    public int arg0Offset;
    public object arg1Name;
    public int arg1Offset;

    Combination2SL1L1 (SCode rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
        : base (rator, rand0, rand1)
    {
	this.arg0Name = rand0.Name;
        this.arg0Offset = rand0.Offset;
	this.arg1Name = rand1.Name;
        this.arg1Offset = rand1.Offset;
    }

    public static SCode Make (SCode rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
    {
        return new Combination2SL1L1 (rator, rand0, rand1);
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
        noteCalls (this.rator);
        procedureTypeHistogram.Note (this.ratorType);
#endif

        object ev1; if (environment.FastLexicalRef1 (out ev1, this.arg1Name, this.arg1Offset)) throw new NotImplementedException();

        object ev0; if (environment.FastLexicalRef1 (out ev0, this.arg0Name, this.arg0Offset)) throw new NotImplementedException ();

        object evop;
        Environment env = environment;
        Control unev = this.rator;
        while (unev.EvalStep (out evop, ref unev, ref env)) { };
        if (evop == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        // expression = (SCode) evop;
        return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
    }


}

class Combination2SSL1 : Combination2
{
#if DEBUG
    static Histogram<Type> ratorTypeHistogram = new Histogram<Type>();
    static Histogram<Type> rand0TypeHistogram = new Histogram<Type>();
#endif

    public object arg1Name;
    public int arg1Offset;

    Combination2SSL1 (SCode rator, SCode rand0, LexicalVariable1 rand1)
        : base (rator, rand0, rand1)
    {
	this.arg1Name = rand1.Name;
        this.arg1Offset = rand1.Offset;
    }

    public static SCode Make (SCode rator, SCode rand0, LexicalVariable1 rand1)
    {
        return new Combination2SSL1 (rator, rand0, rand1);
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
        noteCalls (this.rator);
        noteCalls (this.rand0);
        ratorTypeHistogram.Note (this.ratorType);
        rand0TypeHistogram.Note (this.rand0Type);
#endif

        object ev1; if (environment.FastLexicalRef1 (out ev1, this.arg1Name, this.arg1Offset)) throw new NotImplementedException ();

        object ev0;
        Environment env = environment;
        Control unev = this.rand0;
        while (unev.EvalStep (out ev0, ref unev, ref env)) { };
        if (ev0 == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        object evop;
        env = environment;
        unev = this.rator;
        while (unev.EvalStep (out evop, ref unev, ref env)) { };
        if (evop == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        // expression = (SCode) evop;
        return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
    }


}

[Serializable]
class Combination2SL1S : Combination2
{
#if DEBUG
    static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
    static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
    static Histogram<string> histogram = new Histogram<String> ();
#endif
    object a0Name;
    protected int a1offset;

    Combination2SL1S (SCode rator, LexicalVariable1 rand0, SCode rand1)
        : base (rator, rand0, rand1)
    {
        this.a0Name = rand0.Name;
        this.a1offset = rand0.Offset;
    }

    public static SCode Make (SCode rator, LexicalVariable1 rand0, SCode rand1)
    {
        return new Combination2SL1S (rator, rand0, rand1);
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

        object evrand;
        if (environment.FastLexicalRef1 (out evrand, this.a0Name, a1offset)) throw new NotImplementedException();
        return Interpreter.Call (out answer, ref expression, ref environment, evop, evrand, ev1);
    }
}

class Combination2L1SS : Combination2
{
#if DEBUG
    static Histogram<Type> rand0TypeHistogram = new Histogram<Type>();
    static Histogram<Type> rand1TypeHistogram = new Histogram<Type>();
#endif
    public object pName;
    public int pOffset;

    Combination2L1SS (LexicalVariable1 rator, SCode rand0, SCode rand1)
        : base (rator, rand0, rand1)
    {
        this.pName = rator.Name;
        this.pOffset = rator.Offset;
    }

    public static SCode Make (LexicalVariable1 rator, SCode rand0, SCode rand1)
    {
        return new Combination2L1SS (rator, rand0, rand1);
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

        object ev1;
        Environment env = environment;
        Control unev = this.rand1;
        while (unev.EvalStep (out ev1, ref unev, ref env)) { };
        if (ev1 == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        object ev0;
        env = environment;
        unev = this.rand0;
        while (unev.EvalStep (out ev0, ref unev, ref env)) { };
        if (ev0 == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        object evop; 
        if (environment.FastLexicalRef1 (out evop, this.pName, this.pOffset)) 
            throw new NotImplementedException();
        return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
    }
}

    [Serializable]
    class Letrec2 : Combination2
    {
        protected Letrec2 (LambdaBase rator)
            : base (rator, Quotation.Unassigned, Quotation.Unassigned)
        {
        }

        public static SCode Make (LambdaBase rator)
        {
            return new Letrec2 (rator);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif

            object evop = ((Lambda) this.rator).Close (environment);
            return Interpreter.Call (out answer, ref expression, ref environment, evop, ReferenceTrap.Unassigned, ReferenceTrap.Unassigned);
        }

    }
}
