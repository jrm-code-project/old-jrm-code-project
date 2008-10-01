using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Serialization;

namespace Microcode
{
    [Serializable]
    class Combination1 : SCode, ISystemPair
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type>();
        protected Type ratorType;
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
        Type randType;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand;

        protected Combination1 (SCode rator, SCode rand)
            : base (TC.COMBINATION_1)
        {
            if (rator == null)
                throw new ArgumentNullException ("rator");
            if (rand == null)
                throw new ArgumentNullException ("rand");
            this.rator = rator;
            this.rand = rand;
#if DEBUG
            this.ratorType = rator.GetType();
            this.randType = rand.GetType();
#endif
        }

        public static SCode Make (object rator, object arg0)
        {
            SCode srator = EnsureSCode (rator);
            SCode srand = EnsureSCode (arg0);
            if (Configuration.EnableSuperOperators)
                return (srator is Lambda) ? Let1.Make ((Lambda) srator, srand)
                    : new Combination1 (srator, srand);
            return new Combination1 (srator, srand);
        }

        public SCode Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.rator;
            }
        }

        public SCode Operand
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand;
            }
        }

        [SchemePrimitive ("COMBINATION1?", 1, true)]
        public static bool IsCombination1 (out object answer, object arg)
        {
            answer = arg is Combination1;
            return false;
        }

        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            [DebuggerStepThrough]
            get
            {

                return UnwrapQuoted (this.rator);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCdr
        {
            [DebuggerStepThrough]
            get
            {

                return UnwrapQuoted (this.rand);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optimizedRator = this.rator.Bind (ctenv);
            SCode optimizedRand = this.rand.Bind (ctenv);
            if (Configuration.EnableSuperOperators)
                return (optimizedRator is Lambda) ? Let1.Make ((Lambda) optimizedRator, optimizedRand)
                    : (optimizedRator is LexicalVariable) ? Combination1LS.Make ((LexicalVariable) optimizedRator, optimizedRand)
                    : (optimizedRand is Argument) ? Combination1SA.Make (optimizedRator, (Argument) optimizedRand)
                    : (optimizedRator == this.rator && optimizedRand == this.rand) ? this
                    : new Combination1 (optimizedRator, optimizedRand);
            else
                return (optimizedRator == this.rator && optimizedRand == this.rand)
                    ? this
                    : new Combination1 (optimizedRator, optimizedRand);
        }


        public override bool CallsTheEnvironment ()
        {
            return this.rand.CallsTheEnvironment ()
                || this.rator.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rator);
            noteCalls (this.rand);
            ratorTypeHistogram.Note (this.ratorType);
            randTypeHistogram.Note (this.randType);
#endif

            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object evop = null;
            Control unevop = this.rator;
            env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }

        public override bool MutatesAny (object [] formals)
        {
            return this.rator.MutatesAny (formals)
                || this.rand.MutatesAny (formals);
        }


        public override bool UsesAny (object [] formals)
        {
            return this.rator.UsesAny (formals)
                || this.rand.UsesAny (formals);
        }
    }

    [Serializable]
    sealed class Combination1Frame0 : SubproblemContinuation<Combination1>, ISystemVector
    {

        public Combination1Frame0 (Combination1 combination1, Environment environment)
            : base (combination1, environment)
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
            object evop = null;
            Control unevop = this.expression.Operator;
            Environment env = this.environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, value);
        }
    }

    [Serializable]
    sealed class Combination1Frame1 : SubproblemContinuation<Combination1>, ISystemVector
    {
        readonly object evarg;

        public Combination1Frame1 (Combination1 combination1, Environment environment, object evarg)
            : base (combination1, environment)
        {
            this.evarg = evarg;
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
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class Combination1SA : Combination1
    {
        public readonly int roffset;

        Combination1SA (SCode rator, Argument rand)
            : base (rator, rand)
        {
            this.roffset = rand.Offset;
        }

        public static SCode Make (SCode rator, Argument arg0)
        {
            return new Combination1SA (rator, arg0);
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ("Shouldn't be necessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rator);
#endif

            object evop = null;
            Control unev = this.rator;
            Environment env = environment;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1SAFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                // return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue(this.roffset));
        }
    }


    [Serializable]
    class Combination1LS : Combination1
    {
#if DEBUG
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
        Type randType;
#endif
        public readonly string name;
        public readonly int depth;
        public readonly int offset;

        Combination1LS (LexicalVariable rator, SCode rand)
            : base (rator, rand)
        {
#if DEBUG
            this.randType = rand.GetType ();
#endif
            this.name = rator.name;
            this.depth = rator.Depth;
            this.offset = rator.Offset;
        }

        public static SCode Make (LexicalVariable rator, SCode arg0)
        {
            return new Combination1LS (rator, arg0);
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ("Shouldn't be necessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
#endif

            object evarg = null;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1LSFrame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object evop = null;
            if (environment.FastLexicalRef (out evop, this.name, this.depth, this.offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    [Serializable]
    sealed class Combination1LSFrame0 : SubproblemContinuation<Combination1LS>, ISystemVector
    {

        public Combination1LSFrame0 (Combination1LS combination1, Environment environment)
            : base (combination1, environment)
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object evarg)
        {
            object evop = null;
            if (environment.FastLexicalRef (out evop, this.expression.name, this.expression.depth, this.expression.offset))
                throw new NotImplementedException ();
            //Control unevop = this.rator;
            //env = environment;
            //while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            //if (evop == Interpreter.UnwindStack) {
            //    ((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
            //    answer = Interpreter.UnwindStack;
            //    environment = env;
            //    return false;
            //}

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }


    [Serializable]
    class Let1 : Combination1
    {
#if DEBUG
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
        Type randType;
#endif
        string [] formals;
        SCode body;

        Let1 (Lambda rator, SCode rand)
            : base (rator, rand)
        {
            formals = rator.Formals;
            body = rator.Body;
#if DEBUG
            randType = rand.GetType ();
#endif
        }

        public static SCode Make (Lambda rator, SCode arg0)
        {
            if (arg0 is Quotation
                && ((Quotation) arg0).Quoted == ReferenceTrap.Unassigned
                && rator.Body is Sequence2
                && ((Sequence2) rator.Body).First is Assignment
                && ((Assignment) ((Sequence2) rator.Body).First).Name == rator.Formals [0])
                return Letrec1.Make (rator);
            return new Let1 (rator, arg0);
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optimizedRator = this.rator.Bind (ctenv);
            SCode optimizedRand = this.rand.Bind (ctenv);
            return 
                (optimizedRator is SimpleLambda) ? SimpleLet1.Make ((SimpleLambda) optimizedRator, optimizedRand)
                : optimizedRator == this.rator && optimizedRand == this.rand ? this
                : Let1.Make ((Lambda)optimizedRator, optimizedRand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rator);
            noteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
#endif

            object evarg = null;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object evop = null;
            Control unevop = this.rator;
            env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    class Letrec1 : Combination1
    {
#if DEBUG
                static Histogram<Type> ratorTypeHistogram = new Histogram<Type>();
#endif
        string name;
        SCode value;
        SCode body;

        Letrec1 (Lambda rator)
            : base (rator, Quotation.Make (ReferenceTrap.Unassigned))
        {
            this.name = rator.Formals [0];
            this.value = ((Assignment) ((Sequence2) rator.Body).First).Value;
            this.body = ((Sequence2) rator.Body).Second;
        }

        public static SCode Make (Lambda rator)
        {
            return new Letrec1 (rator);
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optimizedRator = this.rator.Bind (ctenv);
            SCode optimizedRand = this.rand.Bind (ctenv);
            return optimizedRator == this.rator && optimizedRand == this.rand
                ? this
                : Letrec1.Make ((Lambda) optimizedRator, optimizedRand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
#endif
            object evop = null;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, ReferenceTrap.Unassigned));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ReferenceTrap.Unassigned);
        }
    }

    [Serializable]
    class SimpleLet1 : Combination1
    {
#if DEBUG
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
        Type randType;
#endif
        string [] formals;
        SCode body;

        SimpleLet1 (SimpleLambda rator, SCode rand)
            : base (rator, rand)
        {
            formals = rator.Formals;
            body = rator.Body;
#if DEBUG
            randType = rand.GetType ();
#endif
        }

        public static SCode Make (SimpleLambda rator, SCode arg0)
        {
            return new SimpleLet1 (rator, arg0);
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ("not needed");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.body);
            noteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
#endif

            object evarg = null;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg);
            answer = null;
            return true;
        }

    }
}