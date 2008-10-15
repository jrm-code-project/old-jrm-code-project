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
        protected Type randType;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly SCode rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly SCode rand;

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

        static SCode Make (SCode rator, SCode rand)
        {
            Lambda lrator = rator as Lambda;
            return 
                (Configuration.EnableSuperOperators && 
                 rand == Quotation.Unassigned &&
                 lrator != null &&
                 lrator.IsLetrecLambda()) ? Letrec1.Make (lrator)
                : (Configuration.EnableSuperOperators && rator is Argument && rand is Argument) ? Combination1AA.Make ((Argument) rator, (Argument) rand)
                : (Configuration.EnableSuperOperators && rator is Argument) ? Combination1AS.Make ((Argument) rator, rand)
                : (Configuration.EnableSuperOperators && rand is Argument) ? Combination1SA.Make (rator, (Argument) rand)
                : new Combination1 (rator, rand);
        }

        public static SCode Make (object rator, object arg0)
        {
            SCode srator = EnsureSCode (rator);
            SCode srand = EnsureSCode (arg0);
            return Combination1.Make (srator, srand);
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

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode optimizedRator = this.rator.Bind (ctenv);
            SCode optimizedRand = this.rand.Bind (ctenv);
            return  (optimizedRator == this.rator && optimizedRand == this.rand) 
                ? this
                : Combination1.Make (optimizedRator, optimizedRand);
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

            object evop;
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

        public override SCode Alpha (object from, object to)
        {
            throw new NotImplementedException ();
        }

        public override bool IsLetrecBody (object [] formals, object [] remainingFormals)
        {
            throw new NotImplementedException ();
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            return Interpreter.Call (out answer, ref expression, ref environment, value, this.evarg);
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

     }

    [Serializable]
    class Combination1SA : Combination1
    {
        public readonly int roffset;

        protected Combination1SA (SCode rator, Argument rand)
            : base (rator, rand)
        {
            this.roffset = rand.Offset;
        }

        public static SCode Make (SCode rator, Argument arg0)
        {
            return (arg0 is Argument0) ? Combination1SA0.Make (rator, (Argument0) arg0)
                : (arg0 is Argument1) ? Combination1SA1.Make (rator, (Argument1) arg0)
                : new Combination1SA (rator, arg0);
        }

        public override SCode Bind (LexicalMap ctenv)
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue (this.roffset));
        }
    }

    [Serializable]
    class Combination1SA0 : Combination1SA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        Combination1SA0 (SCode rator, Argument0 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SCode rator, Argument0 arg0)
        {
            return new Combination1SA0 (rator, arg0);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ("Shouldn't be necessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value);
        }
    }

    [Serializable]
    class Combination1SA1 : Combination1SA
    {
        Combination1SA1 (SCode rator, Argument1 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SCode rator, Argument1 arg0)
        {
            return new Combination1SA1 (rator, arg0);
        }

        public override SCode Bind (LexicalMap ctenv)
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value);
        }
    }

    [Serializable]
    class Combination1AS : Combination1
    {
        public readonly int poffset;

        protected Combination1AS (Argument rator, SCode rand)
            : base (rator, rand)
        {
            this.poffset = rator.Offset;
        }

        public static SCode Make (Argument rator, SCode arg0)
        {
            return new Combination1AS (rator, arg0);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ("Shouldn't be necessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand);
#endif

            object ev;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out ev, ref unev, ref env)) { };
            if (ev == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1SAFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                // return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.poffset), ev);
        }
    }

    [Serializable]
    class Combination1AA : Combination1
    {
        public readonly int poffset;
        public readonly int aoffset;

        protected Combination1AA (Argument rator, Argument rand)
            : base (rator, rand)
        {
            this.poffset = rator.Offset;
            this.aoffset = rand.Offset;
        }

        public static SCode Make (Argument rator, Argument arg0)
        {
            return new Combination1AA (rator, arg0);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ("Shouldn't be necessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue(this.poffset), environment.ArgumentValue (this.aoffset));
        }
    }


//    [Serializable]
//    class Combination1LS : Combination1
//    {
//#if DEBUG
//        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object varname;
//        public readonly int depth;
//        public readonly int offset;

//        protected Combination1LS (LexicalVariable rator, SCode rand)
//            : base (rator, rand)
//        {
//#if DEBUG
//            this.randType = rand.GetType ();
//#endif
//            this.varname = rator.Name;
//            this.depth = rator.Depth;
//            this.offset = rator.Offset;
//        }

//        public static SCode Make (LexicalVariable rator, SCode arg0)
//        {
//            return (rator is LexicalVariable1) ? Combination1L1S.Make ((LexicalVariable1) rator, arg0)
//                : new Combination1LS (rator, arg0); 
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand);
//            randTypeHistogram.Note (this.randType);
//#endif

//            object evarg = null;
//            Control unev = this.rand;
//            Environment env = environment;
//            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
//            if (evarg == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination1LSFrame0 (this, environment));
//                answer = Interpreter.UnwindStack;
//                environment = env;
//                return false;
//            }

//            object evop = null;
//            if (environment.FastLexicalRef (out evop, this.varname, this.depth, this.offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
//        }
//    }

//    [Serializable]
//    sealed class Combination1LSFrame0 : SubproblemContinuation<Combination1LS>, ISystemVector
//    {

//        public Combination1LSFrame0 (Combination1LS combination1, Environment environment)
//            : base (combination1, environment)
//        {
//        }

//        #region ISystemVector Members

//        public int SystemVectorSize
//        {
//            get { throw new NotImplementedException (); }
//        }

//        public object SystemVectorRef (int index)
//        {
//            throw new NotImplementedException ();
//        }

//        public object SystemVectorSet (int index, object newValue)
//        {
//            throw new NotImplementedException ();
//        }

//        #endregion

//        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object evarg)
//        {
//            object evop = null;
//            if (environment.FastLexicalRef (out evop, this.expression.varname, this.expression.depth, this.expression.offset))
//                throw new NotImplementedException ();
//            //Control unevop = this.rator;
//            //env = environment;
//            //while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
//            //if (evop == Interpreter.UnwindStack) {
//            //    ((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
//            //    answer = Interpreter.UnwindStack;
//            //    environment = env;
//            //    return false;
//            //}

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
//        }
//    }

//    [Serializable]
//    class Combination1L1S : Combination1LS
//    {
//#if DEBUG
//        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination1L1S (LexicalVariable1 rator, SCode rand)
//            : base (rator, rand)
//        {
//#if DEBUG
//            this.randType = rand.GetType ();
//#endif
//        }

//        public static SCode Make (LexicalVariable1 rator, SCode arg0)
//        {
//            return new Combination1L1S( rator, arg0);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand);
//            randTypeHistogram.Note (this.randType);
//#endif

//            object evarg = null;
//            Control unev = this.rand;
//            Environment env = environment;
//            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
//            if (evarg == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination1L1SFrame0 (this, environment));
//                answer = Interpreter.UnwindStack;
//                environment = env;
//                return false;
//            }

//            object evop = null;
//            if (environment.FastLexicalRef1 (out evop, this.varname, this.offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
//        }
//    }

//    [Serializable]
//    sealed class Combination1L1SFrame0 : SubproblemContinuation<Combination1L1S>, ISystemVector
//    {

//        public Combination1L1SFrame0 (Combination1L1S combination1, Environment environment)
//            : base (combination1, environment)
//        {
//        }

//        #region ISystemVector Members

//        public int SystemVectorSize
//        {
//            get { throw new NotImplementedException (); }
//        }

//        public object SystemVectorRef (int index)
//        {
//            throw new NotImplementedException ();
//        }

//        public object SystemVectorSet (int index, object newValue)
//        {
//            throw new NotImplementedException ();
//        }

//        #endregion

//        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object evarg)
//        {
//            object evop = null;
//            if (environment.FastLexicalRef1 (out evop, this.expression.varname, this.expression.offset))
//                throw new NotImplementedException ();
//            //Control unevop = this.rator;
//            //env = environment;
//            //while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
//            //if (evop == Interpreter.UnwindStack) {
//            //    ((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
//            //    answer = Interpreter.UnwindStack;
//            //    environment = env;
//            //    return false;
//            //}

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
//        }
//    }

//    [Serializable]
//    class Let1 : Combination1
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();

//        protected Type bodyType;
//#endif
//        protected object [] formals;
//        public SCode body;

//        protected Let1 (Lambda rator, SCode rand)
//            : base (rator, rand)
//        {
//            formals = rator.Formals;
//            body = rator.Body;
//#if DEBUG
//            this.bodyType = body.GetType ();
//#endif
//        }

//        public static SCode Make (Lambda rator, SCode arg0)
//        {
//            return (rator is SimpleLambda) ? SimpleLet1.Make ((SimpleLambda) rator, arg0)
//                : (arg0 is Quotation) ? Let1SQ.Make (rator, (Quotation) arg0)
//                : new Let1 (rator, arg0);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Shouldn't be needed");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rator);
//            noteCalls (this.rand);
//            ratorTypeHistogram.Note (this.ratorType);
//            randTypeHistogram.Note (this.randType);
//#endif

//            object evarg = null;
//            Control unev = this.rand;
//            Environment env = environment;
//            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
//            if (evarg == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination1Frame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            object evop = null;
//            Control unevop = this.rator;
//            env = environment;
//            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
//            if (evop == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
//        }
//    }

//    [Serializable]
//    class Combination1TLVA0 : Combination1
//    {
//        readonly ValueCell cell;

//        Combination1TLVA0 (TopLevelVariable rator, Argument0 rand)
//            : base (rator, rand)
//        {
//            this.cell = rator.cell;
//        }

//        public static SCode Make (TopLevelVariable rator, Argument0 arg0)
//        {
//            return new Combination1TLVA0 (rator, arg0);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            object evop;
//            if (this.cell.GetValue (out evop))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value);
//        }
//    }

    class Letrec1 : Combination1
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif

        Letrec1 (Lambda rator)
            : base (rator, Quotation.Unassigned)
        {
        }

        public static SCode Make (Lambda rator)
        {
            return new Letrec1 (rator);
        }

        public override SCode Bind (LexicalMap ctenv)
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
#endif
            object evop = ((Lambda) this.rator).Close (environment);
            return Interpreter.Call (out answer, ref expression, ref environment, evop, ReferenceTrap.Unassigned);
        }
    }

//    [Serializable]
//    class SimpleLet1 : Let1
//    {
//#if DEBUG
//        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
//#endif

//        protected SimpleLet1 (SimpleLambda rator, SCode rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (SimpleLambda rator, SCode arg0)
//        {
//            return new SimpleLet1 (rator, arg0);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("not needed");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.body);
//            bodyTypeHistogram.Note (this.bodyType);
//            noteCalls (this.rand);
//            randTypeHistogram.Note (this.randType);
//#endif

//            object evarg = null;
//            Control unev = this.rand;
//            Environment env = environment;
//            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
//            if (evarg == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new SimpleLet1Frame0 (this, environment));
//                answer = Interpreter.UnwindStack;
//                environment = env;
//                return false;
//            }

//            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

//            expression = this.body;
//            environment = new SmallEnvironment1 (cl, evarg);
//            answer = null;
//            return true;
//        }

//    }

//    [Serializable]
//    sealed class SimpleLet1Frame0 : SubproblemContinuation<SimpleLet1>, ISystemVector
//    {
//        public SimpleLet1Frame0 (SimpleLet1 combination1, Environment environment)
//            : base (combination1, environment)
//        {
//        }

//        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
//        {
//            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.expression.rator, environment);

//            expression = this.expression.body;
//            environment = new SmallEnvironment1 (cl, value);
//            answer = null;
//            return true;
//        }

//        #region ISystemVector Members

//        public int SystemVectorSize
//        {
//            get { throw new NotImplementedException (); }
//        }

//        public object SystemVectorRef (int index)
//        {
//            throw new NotImplementedException ();
//        }

//        public object SystemVectorSet (int index, object newValue)
//        {
//            throw new NotImplementedException ();
//        }

//        #endregion

//    }

//    class SimpleLet1A0 : SimpleLet1
//    {
//        SimpleLet1A0 (SimpleLambda rator, Argument0 rand)
//            : base (rator, rand)
//        {
//        }

//        static public SCode Make (SimpleLambda rator, Argument0 rand)
//        {
//            object [] newFormals = new object [rator.Formals.Length - 1];
//            Array.Copy (rator.Formals, newFormals, rator.Formals.Length - 1);
//            SCode newBody = rator.Body.Alpha (rator.Formals [0], rand.Name);
//            SCode newComb = Combination0.Make (SimpleLambda.Make (rator.Name, newFormals, newBody));
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class Let1SQ : Let1
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif

//        protected object quoted;

//        protected Let1SQ (Lambda rator, Quotation rand)
//            : base (rator, rand)
//        {
//            this.quoted = rand.Quoted;
//        }

//        public static SCode Make (Lambda rator, Quotation arg0)
//        {
//            return  new Let1SQ (rator, arg0);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Shouldn't be needed");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif

//            object evop = null;
//            Control unevop = this.rator;
//            Environment env = environment;
//            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
//            if (evop == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.quoted);
//        }
//    }

}