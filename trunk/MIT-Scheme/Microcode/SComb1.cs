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
            return 
                (Configuration.EnableSuperOperators && rator is Lambda) ? Let1.Make ((Lambda) rator, rand)  
                : (Configuration.EnableSuperOperators && rator is LexicalVariable) ? Combination1L.Make ((LexicalVariable) rator, rand)
                : (Configuration.EnableSuperOperators && rator is TopLevelVariable) ? Combination1T.Make ((TopLevelVariable) rator, rand)
                : (Configuration.EnableSuperOperators && rand is LexicalVariable) ? Combination1SL.Make (rator, (LexicalVariable) rand)
                : (Configuration.EnableSuperOperators && rand is Quotation) ? Combination1SQ.Make (rator, (Quotation) rand)
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
            SCode optimizedRand = this.rand.Bind (ctenv);
            if (optimizedRand is LexicalVariable
                && ((LexicalVariable) optimizedRand).Binder is SimpleLambda
                && this.rator is SimpleLambda) {
                return ((SimpleLambda) this.rator).Body.Bind (ctenv.AddSubstitution (((SimpleLambda) this.rator).Formals [0], (BoundVariable) optimizedRand));
            }

            SCode optimizedRator = this.rator.Bind (ctenv);
            
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
            Warm ("-");
            noteCalls (this.rator);
            noteCalls (this.rand);
            ratorTypeHistogram.Note (this.ratorType);
            randTypeHistogram.Note (this.randType);
            SCode.location = "Combination1.EvalStep";
#endif

            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
#if DEBUG
                        SCode.location = "Combination1.EvalStep.1";
#endif
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
#if DEBUG
            SCode.location = "Combination1.EvalStep.2";
#endif
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
#if DEBUG
            SCode.location = "Combination1.EvalStep.3";
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }

        public override bool MutatesAny (object [] formals)
        {
            return this.rator.MutatesAny (formals)
                || this.rand.MutatesAny (formals);
        }

        public override SCode Substitute (object name, object newObject)
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
    class Combination1L : Combination1
    {
#if DEBUG
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object varname;
        public readonly int depth;
        public readonly int offset;

        protected Combination1L (LexicalVariable rator, SCode rand)
            : base (rator, rand)
        {
            this.varname = rator.Name;
            this.depth = rator.Depth;
            this.offset = rator.Offset;
        }

        public static SCode Make (LexicalVariable rator, SCode rand0)
        {
            return 
                (rator is Argument) ? Combination1A.Make ((Argument) rator, rand0)
                : (rator is LexicalVariable1) ? Combination1L1.Make ((LexicalVariable1) rator, rand0)
                : (rand0 is LexicalVariable) ? Combination1LL.Make (rator, (LexicalVariable) rand0)
                : (rand0 is PrimitiveCombination1) ? Combination1LPComb1.Make (rator, (PrimitiveCombination1) rand0)
                //: (rand0 is Quotation) ? Unimplemented()
                : new Combination1L (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1L.EvalStep");
            noteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
#endif

            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1LFrame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object evop = null;
            if (environment.FastLexicalRef (out evop, this.varname, this.depth, this.offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    [Serializable]
    sealed class Combination1LFrame0 : SubproblemContinuation<Combination1L>, ISystemVector
    {

        public Combination1LFrame0 (Combination1L combination1, Environment environment)
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
            if (environment.FastLexicalRef (out evop, this.expression.varname, this.expression.depth, this.expression.offset))
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

    class Combination1A : Combination1L
    {
#if DEBUG
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif

        protected Combination1A (Argument rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (Argument rator, SCode arg0)
        {
            return 
                (rator is Argument0) ? Combination1A0.Make ((Argument0) rator, arg0)
                : (rator is Argument1) ? Combination1A1.Make ((Argument1) rator, arg0)
                : new Combination1A (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1A.EvalStep");
            noteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
#endif

            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
            if (evarg == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1LFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object evop = environment.ArgumentValue (this.offset);
            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    class Combination1A0 : Combination1A
    {
#if DEBUG
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif

        protected Combination1A0 (Argument0 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (Argument0 rator, SCode arg0)
        {
            return new Combination1A0 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1A0.EvalStep");
            noteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
#endif

            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1LFrame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object evop = environment.Argument0Value;
            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    class Combination1A1 : Combination1A
    {
#if DEBUG
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif

        protected Combination1A1 (Argument1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (Argument1 rator, SCode arg0)
        {
            return new Combination1A1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1A1.EvalStep");
            noteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
#endif

            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
            if (evarg == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1LFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object evop = environment.Argument1Value;
            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    class Combination1L1 : Combination1L
    {
#if DEBUG
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif

        protected Combination1L1 (LexicalVariable1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (LexicalVariable1 rator, SCode arg0)
        {
            return
                (arg0 is PrimitiveCdrA0) ? Combination1L1CdrA0.Make (rator, (PrimitiveCdrA0) arg0)
                : new Combination1L1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1L1.EvalStep");
            noteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
#endif

            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1L1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object evop;
            if (environment.FastLexicalRef1 (out evop, this.varname, this.offset))
                throw new NotImplementedException ();
            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    [Serializable]
    sealed class Combination1L1Frame0 : SubproblemContinuation<Combination1L1>, ISystemVector
    {

        public Combination1L1Frame0 (Combination1L1 combination1, Environment environment)
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
            if (environment.FastLexicalRef1 (out evop, this.expression.varname, this.expression.offset))
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
    class Combination1LL : Combination1L
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected Combination1LL (LexicalVariable rator, LexicalVariable rand)
            : base (rator, rand)
        {
            this.rand0Name = rand.Name;
            this.rand0Depth = rand.Depth;
            this.rand0Offset = rand.Offset;
        }

        public static SCode Make (LexicalVariable rator, LexicalVariable rand0)
        {
            return
                (rand0 is Argument) ? Combination1LA.Make (rator, (Argument) rand0)
                //: (rand0 is LexicalVariable1) ? Unimplemented()
                : new Combination1LL (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1LL.EvalStep");
#endif

            object evarg;
            if (environment.FastLexicalRef (out evarg, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            object evop = null;
            if (environment.FastLexicalRef (out evop, this.varname, this.depth, this.offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    [Serializable]
    class Combination1LA : Combination1LL
    {
        protected Combination1LA (LexicalVariable rator, Argument rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (LexicalVariable rator, Argument rand0)
        {
            return
                (rand0 is Argument0) ? Combination1LA0.Make (rator, (Argument0) rand0)
                : (rand0 is Argument1) ? Combination1LA1.Make (rator, (Argument1) rand0)
                : new Combination1LA (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object evop = null;
            if (environment.FastLexicalRef (out evop, this.varname, this.depth, this.offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue(this.rand0Offset));
        }
    }

        [Serializable]
    class Combination1LA0 : Combination1LA
    {
        protected Combination1LA0 (LexicalVariable rator, Argument rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (LexicalVariable rator, Argument0 rand0)
        {
            return
                 new Combination1LA0 (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1LA0.EvalStep");
#endif
            object evop = null;
            if (environment.FastLexicalRef (out evop, this.varname, this.depth, this.offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value);
        }
    }

    [Serializable]
    class Combination1LA1 : Combination1LA
    {
        protected Combination1LA1 (LexicalVariable rator, Argument1 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (LexicalVariable rator, Argument1 rand0)
        {
            return
                 new Combination1LA1 (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object evop = null;
            if (environment.FastLexicalRef (out evop, this.varname, this.depth, this.offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value);
        }
    }

    [Serializable]
    class Combination1LPComb1 : Combination1L
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Primitive1 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        protected PrimitiveMethod1 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode arg0;

        protected Combination1LPComb1 (LexicalVariable rator, PrimitiveCombination1 rand)
            : base (rator, rand)
        {
            this.procedure = rand.Operator;
            this.method = procedure.Method;
            this.arg0 = rand.Operand;
        }

        public static SCode Make (LexicalVariable rator, PrimitiveCombination1 rand0)
        {
            return 
                (rand0 is PrimitiveCar) ? Combination1LCar.Make (rator, (PrimitiveCar) rand0)
                : (rand0 is PrimitiveCdr) ? Combination1LCdr.Make (rator, (PrimitiveCdr) rand0)
                : new Combination1LPComb1 (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1LPComb1.EvalStep");
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            Debug.WriteLineIf (Primitive.Noisy, this.procedure.ToString ());
#endif
            object evarg;

            if (this.method (out evarg, ev0)) {
                TailCallInterpreter tci = evarg as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    expression = tci.Expression;
                    environment = tci.Environment;
                    return true;
                }
                else
                    throw new NotImplementedException ();
            }

            object evop;
            if (environment.FastLexicalRef (out evop, this.varname, this.depth, this.offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    [Serializable]
    class Combination1LCar : Combination1LPComb1
    {

        protected Combination1LCar (LexicalVariable rator, PrimitiveCar rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (LexicalVariable rator, PrimitiveCar rand0)
        {
            return
                (rand0 is PrimitiveCarL) ? Combination1LCarL.Make (rator, (PrimitiveCarL) rand0)
                : new Combination1LCar (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            Cons evarg = ev0 as Cons;
            if (evarg == null) throw new NotImplementedException ();

            object evop;
            if (environment.FastLexicalRef (out evop, this.varname, this.depth, this.offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg.Car);
        }
    }

    [Serializable]
    class Combination1LCarL : Combination1LCar
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected Combination1LCarL (LexicalVariable rator, PrimitiveCarL rand)
            : base (rator, rand)
        {
            this.rand0Name = ((LexicalVariable) rand.Operand).Name;
            this.rand0Depth = ((LexicalVariable) rand.Operand).Depth;
            this.rand0Offset = ((LexicalVariable) rand.Operand).Offset;
        }

        public static SCode Make (LexicalVariable rator, PrimitiveCarL rand0)
        {
            return
                (rand0 is PrimitiveCarA) ? Combination1LCarA.Make (rator, (PrimitiveCarA) rand0)
                : new Combination1LCarL (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1LCarL.EvalStep");
#endif

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            Cons evarg = ev0 as Cons;
            if (evarg == null) throw new NotImplementedException ();

            object evop;
            if (environment.FastLexicalRef (out evop, this.varname, this.depth, this.offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg.Car);
        }
    }

    [Serializable]
    class Combination1LCarA : Combination1LCarL
    {

        protected Combination1LCarA (LexicalVariable rator, PrimitiveCarA rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (LexicalVariable rator, PrimitiveCarA rand0)
        {
            return

                new Combination1LCarA (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1LCarA.EvalStep");
#endif
            Cons evarg = environment.ArgumentValue (this.rand0Offset) as Cons;
            if (evarg == null) throw new NotImplementedException ();

            object evop;
            if (environment.FastLexicalRef (out evop, this.varname, this.depth, this.offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg.Car);
        }
    }

    [Serializable]
    class Combination1LCdr : Combination1LPComb1
    {

        protected Combination1LCdr (LexicalVariable rator, PrimitiveCdr rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (LexicalVariable rator, PrimitiveCdr rand0)
        {
            return
                (rand0 is PrimitiveCdrL) ? Combination1LCdrL.Make (rator, (PrimitiveCdrL) rand0)
                : new Combination1LCdr (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1LCdr.EvalStep");
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            Cons evarg = ev0 as Cons;
            if (evarg == null) throw new NotImplementedException ();

            object evop;
            if (environment.FastLexicalRef (out evop, this.varname, this.depth, this.offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg.Cdr);
        }
    }

    [Serializable]
    class Combination1LCdrL : Combination1LCdr
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected Combination1LCdrL (LexicalVariable rator, PrimitiveCdrL rand)
            : base (rator, rand)
        {
            this.rand0Name = rand.OperandName;
            this.rand0Depth = rand.OperandDepth;
            this.rand0Offset = rand.OperandOffset;
        }

        public static SCode Make (LexicalVariable rator, PrimitiveCdrL rand0)
        {
            return
                (rand0 is PrimitiveCdrA) ? Combination1LCdrA.Make (rator, (PrimitiveCdrA) rand0)
                : new Combination1LCdrL (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1LCdrL.EvalStep");
#endif

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            Cons evarg = ev0 as Cons;
            if (evarg == null) throw new NotImplementedException ();

            object evop;
            if (environment.FastLexicalRef (out evop, this.varname, this.depth, this.offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg.Cdr);
        }
    }

    [Serializable]
    class Combination1LCdrA : Combination1LCdrL
    {

        protected Combination1LCdrA (LexicalVariable rator, PrimitiveCdrA rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (LexicalVariable rator, PrimitiveCdrA rand0)
        {
            return

                new Combination1LCdrA (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1LCdrA.EvalStep");
#endif

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            Cons evarg = ev0 as Cons;
            if (evarg == null) throw new NotImplementedException ();

            object evop;
            if (environment.FastLexicalRef (out evop, this.varname, this.depth, this.offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg.Cdr);
        }
    }

    [Serializable]
    class Combination1SQ : Combination1
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object randValue;

        protected Combination1SQ (SCode rator, Quotation rand)
            : base (rator, rand)
        {
            this.randValue = rand.Quoted;
        }

        public static SCode Make (SCode rator, Quotation rand)
        {
            return new Combination1SQ (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1SQ.EvalStep");
            noteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
#endif

            object evrand = this.randValue;

            object evop;
            Control unev = this.rator;
            Environment env = environment;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }
            return Interpreter.Call (out answer, ref expression, ref environment, evop, evrand);
        }
    }

    [Serializable]
    class Combination1SL : Combination1
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object randName;
        public readonly int randDepth;
        public readonly int randOffset;

        protected Combination1SL (SCode rator, LexicalVariable rand)
            : base (rator, rand)
        {
            this.randName = rand.Name;
            this.randDepth = rand.Depth;
            this.randOffset = rand.Offset;
        }

        public static SCode Make (SCode rator, LexicalVariable rand)
        {
            return 
                (rand is Argument) ? Combination1SA.Make (rator, (Argument) rand)
                : (rand is LexicalVariable1) ? Combination1SL1.Make (rator, (LexicalVariable1) rand)
                : new Combination1SL (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1SL.EvalStep");
            noteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
#endif

            object evrand;
            if (environment.FastLexicalRef (out evrand, this.randName, this.randDepth, this.randOffset))
                throw new NotImplementedException ();

            object evop;
            Control unev = this.rator;
            Environment env = environment;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }
            return Interpreter.Call (out answer, ref expression, ref environment, evop, evrand);
        }
    }

    [Serializable]
    class Combination1SA : Combination1SL
    {

        protected Combination1SA (SCode rator, Argument rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SCode rator, Argument arg0)
        {
            return (arg0 is Argument0) ? Combination1SA0.Make (rator, (Argument0) arg0)
                : (arg0 is Argument1) ? Combination1SA1.Make (rator, (Argument1) arg0)
                : new Combination1SA (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1SA.EvalStep");
            noteCalls (this.rator);
#endif

            object evop;
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue (this.randOffset));
        }
    }

    [Serializable]
    class Combination1SA0 : Combination1SA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        protected Combination1SA0 (SCode rator, Argument0 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SCode rator, Argument0 arg0)
        {
            return 
                (rator is TopLevelVariable) ? Combination1TA0.Make ((TopLevelVariable) rator, arg0)
                : new Combination1SA0 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination1SA0.EvalStep";
#endif

            object evop;
            Control unev = this.rator;
            Environment env = environment;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
#if DEBUG
             SCode.location = "Combination1SA0.EvalStep.1";
#endif
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
    class Combination1T : Combination1
    {
#if DEBUG
        static Histogram<object> ratorNameHistogram = new Histogram<object> ();
#endif
        public readonly TopLevelVariable ratorVar;
        protected Combination1T (TopLevelVariable rator, SCode rand)
            : base (rator, rand)
        {
            this.ratorVar = rator;
        }

        public static SCode Make (TopLevelVariable rator, SCode arg0)
        {
            return
                (arg0 is LexicalVariable) ? Combination1TL.Make (rator, (LexicalVariable) arg0)
                : new Combination1T (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1T.EvalStep");
#endif
            object evarg;
            Control expr = this.rand;
            Environment env = environment;
            while (expr.EvalStep (out evarg, ref expr, ref env)) { };
            if (evarg == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object evop;
            if (this.ratorVar.cell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }



    [Serializable]
    class Combination1TL : Combination1T
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected Combination1TL (TopLevelVariable rator, LexicalVariable rand)
            : base (rator, rand)
        {
            this.rand0Name = rand.Name;
            this.rand0Depth = rand.Depth;
            this.rand0Offset = rand.Offset;
        }

        public static SCode Make (TopLevelVariable rator, LexicalVariable arg0)
        {
            return
                (arg0 is Argument) ? Combination1TA.Make (rator, (Argument) arg0)
                : new Combination1TL (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1TL.EvalStep");
#endif
            object evarg;
            if (environment.FastLexicalRef (out evarg, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (this.ratorVar.cell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }



    [Serializable]
    class Combination1TA : Combination1TL
    {

        protected Combination1TA (TopLevelVariable rator, Argument rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (TopLevelVariable rator, Argument arg0)
        {
            return
                (arg0 is Argument0) ? Combination1TA0.Make (rator, (Argument0) arg0)
                : (arg0 is Argument1) ? Combination1TA1.Make (rator, (Argument1) arg0)
                : new Combination1TA (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1TA.EvalStep");
#endif
            object evop;
            if (this.ratorVar.cell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue(this.rand0Offset));
        }
    }



    [Serializable]
    sealed class Combination1TA0 : Combination1TA
    {

        Combination1TA0 (TopLevelVariable rator, Argument0 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (TopLevelVariable rator, Argument0 arg0)
        {
            return
                 new Combination1TA0 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1TA0.EvalStep");
#endif
            object evop;
            if (this.ratorVar.cell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value);
        }
    }

    [Serializable]
    sealed class Combination1TA1 : Combination1TA
    {

        Combination1TA1 (TopLevelVariable rator, Argument1 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (TopLevelVariable rator, Argument1 arg0)
        {
            return
                 new Combination1TA1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1TA1.EvalStep");
#endif
            object evop;
            if (this.ratorVar.cell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value);
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

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1SA1.EvalStep");
            noteCalls (this.rator);
#endif

            object evop;
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
    class Combination1SL1 : Combination1SL
    {

        protected Combination1SL1 (SCode rator, LexicalVariable1 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SCode rator, LexicalVariable1 arg0)
        {
            return  new Combination1SL1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1SL1.EvalStep");
            noteCalls (this.rator);
#endif
            object evarg;
            if (environment.FastLexicalRef1 (out evarg, this.randName, this.randOffset))
                throw new NotImplementedException ();

            object evop;
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }


    // --------

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


    [Serializable]
    class Combination1LCdrL1 : Combination1L
    {
        public readonly object argName;
        public readonly int argOffset;

        protected Combination1LCdrL1 (LexicalVariable rator, PrimitiveCdrL1 rand)
            : base (rator, rand)
        {
            this.argName = rand.OperandName;
            this.argOffset = rand.OperandOffset;
        }

        public static SCode Make (LexicalVariable rator, PrimitiveCdrL1 arg0)
        {
            return new Combination1LCdrL1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object evarg;
            if (environment.FastLexicalRef1 (out evarg, this.argName, this.argOffset))
                throw new NotImplementedException ();
            Cons evarg1 = evarg as Cons;
            if (evarg1 == null)
                throw new NotImplementedException ();

            object evop;
            if (environment.FastLexicalRef (out evop, this.varname, this.depth, this.offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg1.Cdr);
        }
    }


    [Serializable]
    class Combination1L1S : Combination1
    {
#if DEBUG
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif
        public object varname;
        public int offset;
        protected Combination1L1S (LexicalVariable1 rator, SCode rand)
            : base (rator, rand)
        {
            this.varname = rator.Name;
            this.offset = rator.Offset;
#if DEBUG
            this.randType = rand.GetType ();
#endif
        }

        public static SCode Make (LexicalVariable1 rator, SCode arg0)
        {
            return 
                (arg0 is PrimitiveCdrA0) ? Combination1L1CdrA0.Make (rator, (PrimitiveCdrA0) arg0)
                : new Combination1L1S (rator, arg0);
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
                ((UnwinderState) env).AddFrame (new Combination1LV1SFrame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object evop = null;
            if (environment.FastLexicalRef1 (out evop, this.varname, this.offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    [Serializable]
    sealed class Combination1LV1SFrame0 : SubproblemContinuation<Combination1L1S>, ISystemVector
    {

        public Combination1LV1SFrame0 (Combination1L1S combination1, Environment environment)
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
            if (environment.FastLexicalRef1 (out evop, this.expression.varname, this.expression.offset))
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
    class Combination1L1CdrA0 : Combination1L1
    {
        protected Combination1L1CdrA0 (LexicalVariable1 rator, PrimitiveCdrA0 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (LexicalVariable1 rator, PrimitiveCdrA0 arg0)
        {
            return new Combination1L1CdrA0 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1L1CdrA0.EvalStep");
#endif
            object evop;
            if (environment.FastLexicalRef1 (out evop, this.varname, this.offset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ((Cons) (environment.Argument0Value)).Cdr);
        }
    }


    [Serializable]
    class Combination1SLV1 : Combination1
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        public object varname;
        public int offset;
        protected Combination1SLV1 (SCode rator, LexicalVariable1 rand)
            : base (rator, rand)
        {
            this.varname = rand.Name;
            this.offset = rand.Offset;
        }

        public static SCode Make (SCode rator, LexicalVariable1 arg0)
        {
            return new Combination1SLV1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
#endif

            object evarg;
            if (environment.FastLexicalRef1 (out evarg, this.varname, this.offset))
                throw new NotImplementedException ();

            object evop;
            Control unev = this.rator;
            Environment env = environment;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evarg == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1LV1SFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    [Serializable]
    class Combination1LV1LV1 : Combination1
    {
        public object ratorName;
        public int ratorOffset;
        public object randName;
        public int randOffset;
        protected Combination1LV1LV1 (LexicalVariable1 rator, LexicalVariable1 rand)
            : base (rator, rand)
        {
            this.ratorName = rator.Name;
            this.ratorOffset = rator.Offset;
            this.randName = rand.Name;
            this.randOffset = rand.Offset;
        }

        public static SCode Make (LexicalVariable1 rator, LexicalVariable1 arg0)
        {
            return new Combination1LV1LV1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();

#endif
            object evrand;
            if (environment.FastLexicalRef1 (out evrand, this.randName, this.randOffset))
                throw new NotImplementedException ();

            object evop = null;
            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evrand);
        }
    }

    [Serializable]
    class Let1 : Combination1
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();

        protected Type bodyType;
#endif
        protected object [] formals;
        public SCode body;

        protected Let1 (Lambda rator, SCode rand)
            : base (rator, rand)
        {
            formals = rator.Formals;
            body = rator.Body;
#if DEBUG
            this.bodyType = body.GetType ();
#endif
        }

        public static SCode Make (Lambda rator, SCode arg0)
        {
            return (rator is SimpleLambda) ? SimpleLet1.Make ((SimpleLambda) rator, arg0)
                : (arg0 is LexicalVariable) ? Let1L.Make (rator, (LexicalVariable) arg0)
                : (arg0 is Quotation) ? Let1Q.Make (rator, (Quotation) arg0)
                : new Let1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Let1.EvalStep");
            noteCalls (this.rator);
            noteCalls (this.rand);
            ratorTypeHistogram.Note (this.ratorType);
            randTypeHistogram.Note (this.randType);
#endif

            object evarg = null;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
            if (evarg == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object evop = null;
            Control unevop = this.rator;
            env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
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
    class Let1L : Let1
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object argumentName;
        public readonly int argumentDepth;
        public readonly int argumentOffset;

        protected Let1L (Lambda rator, LexicalVariable rand)
            : base (rator, rand)
        {
            this.argumentName = rand.Name;
            this.argumentDepth = rand.Depth;
            this.argumentOffset = rand.Offset;
        }

        public static SCode Make (Lambda rator, LexicalVariable arg0)
        {
            return new Let1L (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rator);
            noteCalls (this.rand);
            ratorTypeHistogram.Note (this.ratorType);
#endif

            object evarg;
            if (environment.FastLexicalRef (out evarg, this.argumentName, this.argumentDepth, this.argumentOffset))
                throw new NotImplementedException ();

            object evop;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }

    }

    [Serializable]
    class Let1Q : Let1
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object argumentValue;

        protected Let1Q (Lambda rator, Quotation rand)
            : base (rator, rand)
        {
            this.argumentValue = rand.Quoted;
        }

        public static SCode Make (Lambda rator, Quotation arg0)
        {
            return 
                (arg0.Quoted == ReferenceTrap.Unassigned
                && rator.Body is Sequence2
                && (((Sequence2) rator.Body).First is Assignment)
                && ((Assignment) ((Sequence2) rator.Body).First).Name == rator.Formals[0]
                && ((Assignment) ((Sequence2) rator.Body).First).Value is Lambda
                && (((Sequence2) rator.Body).Second is Variable)
                && ((Variable) ((Sequence2) rator.Body).Second).Name == rator.Formals[0]) ? Letrec1.Make (rator)
                : new Let1Q (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet1Q.EvalStep");
            noteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
#endif

            object evarg = this.argumentValue;

            object evop;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    [Serializable]
    class SimpleLet1 : Let1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif

        protected SimpleLet1 (SimpleLambda rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, SCode arg0)
        {
            return 
                (arg0 is LexicalVariable) ? SimpleLet1L.Make (rator, (LexicalVariable) arg0)
                : (arg0 is PrimitiveCar) ? SimpleLet1Car.Make (rator, (PrimitiveCar) arg0)
                : (arg0 is Quotation) ? SimpleLet1Q.Make (rator, (Quotation) arg0)
                : new SimpleLet1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            noteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
            SCode.location = "SimpleLet1.EvalStep";
#endif

            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "SimpleLet1.EvalStep.1";
#endif
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new SimpleLet1Frame0 (this, environment));
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

    [Serializable]
    sealed class SimpleLet1Frame0 : SubproblemContinuation<SimpleLet1>, ISystemVector
    {
        public SimpleLet1Frame0 (SimpleLet1 combination1, Environment environment)
            : base (combination1, environment)
        {
        }

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.expression.rator, environment);

            expression = this.expression.body;
            environment = new SmallEnvironment1 (cl, value);
            answer = null;
            return true;
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
    class SimpleLet1L : SimpleLet1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object argName;
        public readonly int argDepth;
        public readonly int argOffset;

        protected SimpleLet1L (SimpleLambda rator, LexicalVariable rand)
            : base (rator, rand)
        {
            this.argName = rand.Name;
            this.argDepth = rand.Depth;
            this.argOffset = rand.Offset;
        }

        public static SCode Make (SimpleLambda rator, LexicalVariable arg0)
        {
            if (arg0.Binder is SimpleLambda)
                throw new NotImplementedException ();
            return new SimpleLet1L (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1L.EvalStep";
#endif

            object evarg;
            if (environment.FastLexicalRef (out evarg, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg);
            answer = null;
            return true;
        }

    }
 
    [Serializable]
    class SimpleLet1Car : SimpleLet1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif

        protected SimpleLet1Car (SimpleLambda rator, PrimitiveCar rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCar arg0)
        {
            return
                (arg0 is PrimitiveCarL) ? SimpleLet1CarL.Make (rator, (PrimitiveCarL) arg0)
                : new SimpleLet1Car (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class SimpleLet1CarL : SimpleLet1Car
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected SimpleLet1CarL (SimpleLambda rator, PrimitiveCarL rand)
            : base (rator, rand)
        {
            this.rand0Name = rand.OperandName;
            this.rand0Depth = rand.OperandDepth;
            this.rand0Offset = rand.OperandOffset;
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCarL arg0)
        {
            return
                (arg0 is PrimitiveCarA) ? SimpleLet1CarA.Make (rator, (PrimitiveCarA) arg0)
                : (arg0 is PrimitiveCarL1) ? SimpleLet1CarL1.Make (rator, (PrimitiveCarL1) arg0)
                : new SimpleLet1CarL (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet1CarL.EvalStep");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
#endif
            object temp;
            if (environment.FastLexicalRef (out temp, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            Cons evarg = temp as Cons;
            if (evarg == null) throw new NotImplementedException ();


            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg.Car);
            answer = null;
            return true;

        }
    }

    [Serializable]
    class SimpleLet1CarA : SimpleLet1CarL
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif

        protected SimpleLet1CarA (SimpleLambda rator, PrimitiveCarA rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCarA arg0)
        {
            return
                (arg0 is PrimitiveCarA0) ? SimpleLet1CarA0.Make (rator, (PrimitiveCarA0) arg0)
                : (arg0 is PrimitiveCarA1) ? SimpleLet1CarA1.Make (rator, (PrimitiveCarA1) arg0)
                : new SimpleLet1CarA (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1CarA.EvalStep";
#endif

            Cons evarg = environment.ArgumentValue(this.rand0Offset) as Cons;
            if (evarg == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg.Car);
            answer = null;
            return true;
        }
    }

    [Serializable]
    class SimpleLet1CarA0 : SimpleLet1CarA
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif

        protected SimpleLet1CarA0 (SimpleLambda rator, PrimitiveCarA0 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCarA0 arg0)
        {
            return
                (rator.Body is Conditional) ? SimpleLet1CarA0Cond.Make (rator, (Conditional) rator.Body, arg0)
                //(rator.Body is Sequence2) ? SimpleLet1CarA0Z2.Make ( rator, arg0)
                : new SimpleLet1CarA0 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet1CarA0.EvalStep");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
#endif

            Cons evarg = environment.Argument0Value as Cons;
            if (evarg == null) throw new NotImplementedException ();


            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg.Car);
            answer = null;
            return true;
        }

    }

    [Serializable]
    class SimpleLet1CarA0Cond : SimpleLet1CarA0
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

        public readonly Type predicateType;
        public readonly Type consequentType;
        public readonly Type alternativeType;

#endif
        public readonly SCode predicate;
        public readonly SCode consequent;
        public readonly SCode alternative;

        protected SimpleLet1CarA0Cond (SimpleLambda rator, SCode predicate, SCode consequent, SCode alternative, PrimitiveCarA0 rand)
            : base (rator, rand)
        {
            this.predicate = predicate;
            this.consequent = consequent;
            this.alternative = alternative;
#if DEBUG
            this.predicateType = predicate.GetType();
            this.consequentType = consequent.GetType();
            this.alternativeType = alternative.GetType();
#endif
        }

        public static SCode Make (SimpleLambda rator, Conditional body, PrimitiveCarA0 arg0)
        {
            return 
                (body.Predicate is PrimitiveIsPairA0) ? SimpleLet1CarA0PairA0.Make (rator, (PrimitiveIsPairA0) body.Predicate, body.Consequent, body.Alternative, arg0)
                : new SimpleLet1CarA0Cond (rator, body.Predicate, body.Consequent, body.Alternative, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet1CarA0Cond.EvalStep");
#endif

            Cons evarg = environment.Argument0Value as Cons;
            if (evarg == null) throw new NotImplementedException ();


            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            environment = new SmallEnvironment1 (cl, evarg.Car);

#if DEBUG
            noteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
            if (ev == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;

            }

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #t");
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }


        }

    }

    [Serializable]
    class SimpleLet1CarA0PairA0 : SimpleLet1CarA0Cond
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

#endif

        protected SimpleLet1CarA0PairA0 (SimpleLambda rator, PrimitiveIsPairA0 predicate, SCode consequent, SCode alternative, PrimitiveCarA0 rand)
            : base (rator, predicate, consequent, alternative, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, PrimitiveIsPairA0 predicate, SCode consequent, SCode alternative, PrimitiveCarA0 rand)
        {
            return
                (consequent is Conditional) ? SimpleLet1CarA0PairA0Cond.Make (rator, predicate, (Conditional) consequent, alternative, rand)
                : new SimpleLet1CarA0PairA0 (rator, predicate, consequent, alternative, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif

            Cons evarg = environment.Argument0Value as Cons;
            if (evarg == null) throw new NotImplementedException ();


            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            environment = new SmallEnvironment1 (cl, evarg.Car);

            if (! (evarg.Car is Cons)) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #t");
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }


        }

    }

    [Serializable]
    class SimpleLet1CarA0PairA0Cond : SimpleLet1CarA0PairA0
    {
#if DEBUG
        static Histogram<Type> predicate1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequent1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternative1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

        public readonly Type predicate1Type;
        public readonly Type consequent1Type;
        public readonly Type alternative1Type;
#endif
        public readonly SCode predicate1;
        public readonly SCode consequent1;
        public readonly SCode alternative1;

        protected SimpleLet1CarA0PairA0Cond (SimpleLambda rator, PrimitiveIsPairA0 predicate, Conditional consequent, SCode alternative, PrimitiveCarA0 rand)
            : base (rator, predicate, consequent, alternative, rand)
        {
            this.predicate1 = consequent.Predicate;
            this.consequent1 = consequent.Consequent;
            this.alternative1 = consequent.Alternative;
#if DEBUG
            this.predicate1Type = this.predicate1.GetType();
            this.consequent1Type = this.consequent1.GetType();
            this.alternative1Type = this.alternative1.GetType();
#endif
        }

        public static SCode Make (SimpleLambda rator, PrimitiveIsPairA0 predicate, Conditional consequent, SCode alternative, PrimitiveCarA0 rand)
        {
            return
                (consequent.Alternative is Combination1LCdrL) ? SLet1CA0PA0CondComb1LCdrL.Make (rator, predicate, consequent, consequent.Predicate, consequent.Consequent, (Combination1LCdrL) consequent.Alternative, alternative, rand)
                 : new SimpleLet1CarA0PairA0Cond (rator, predicate, consequent, alternative, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            Cons evarg = environment.Argument0Value as Cons;
            if (evarg == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);
            environment = new SmallEnvironment1 (cl, evarg.Car);

            if (!(evarg.Car is Cons)) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.predicate1);
                predicate1TypeHistogram.Note (this.predicate1Type);
#endif
                object ev;
                Control unev = this.predicate1;
                Environment env = environment;

                while (unev.EvalStep (out ev, ref unev, ref env)) { };
                if (ev == Interpreter.UnwindStack) {
                    throw new NotImplementedException ();
                    //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                    //environment = env;
                    //answer = Interpreter.UnwindStack;
                    //return false;
                }

                if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                    noteCalls (this.alternative1);
                    alternative1TypeHistogram.Note (this.alternative1Type);
                    Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                    expression = this.alternative1;
                    answer = null;
                    return true;
                }
                else {
#if DEBUG
                    noteCalls (this.consequent1);
                    consequent1TypeHistogram.Note (this.consequent1Type);
                    Debug.WriteLineIf (Primitive.Noisy, "    => #t");
#endif
                    expression = this.consequent1;
                    answer = null;
                    return true;
                }
            }
        }
    }

    class SLet1CA0PA0CondComb1LCdrL : SimpleLet1CarA0PairA0Cond
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> predicate1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequent1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternative1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object altRatorName;
        public readonly int altRatorDepth;
        public readonly int altRatorOffset;
        public readonly object altRandName;
        public readonly int altRandDepth;
        public readonly int altRandOffset;

        protected SLet1CA0PA0CondComb1LCdrL (SimpleLambda rator, PrimitiveIsPairA0 predicate, Conditional consequent,
            SCode consPredicate, SCode consConsequent, Combination1LCdrL consAlternative,
            SCode alternative, PrimitiveCarA0 rand)
            : base (rator, predicate, consequent, alternative, rand)
        {
            this.altRatorName = consAlternative.varname;
            this.altRatorDepth = consAlternative.depth;
            this.altRatorOffset = consAlternative.offset;
            this.altRandName = consAlternative.rand0Name;
            this.altRandDepth = consAlternative.rand0Depth;
            this.altRandOffset = consAlternative.rand0Offset;
        }

        static public SCode Make (SimpleLambda rator, PrimitiveIsPairA0 predicate, Conditional consequent, 
            SCode consPredicate, SCode consConsequent, Combination1LCdrL consAlternative,
            SCode alternative, PrimitiveCarA0 rand)
        {
            return
                (consPredicate is PrimitiveIsEqCarA0L) ? SComb1Fragment3.Make (rator, predicate, consequent, (PrimitiveIsEqCarA0L) consPredicate, consConsequent, consAlternative, alternative, rand)
                : new SLet1CA0PA0CondComb1LCdrL (rator, predicate, consequent, consPredicate, consConsequent, consAlternative, alternative, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SLet1CA0PA0CondComb1LCdrL.EvalStep");
#endif
            Cons evarg = environment.Argument0Value as Cons;
            if (evarg == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);
            environment = new SmallEnvironment1 (cl, evarg.Car);

            if (!(evarg.Car is Cons)) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.predicate1);
                predicate1TypeHistogram.Note (this.predicate1Type);
#endif
                object ev;
                Control unev = this.predicate1;
                Environment env = environment;

                while (unev.EvalStep (out ev, ref unev, ref env)) { };
                if (ev == Interpreter.UnwindStack) {
                    throw new NotImplementedException ();
                    //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                    //environment = env;
                    //answer = Interpreter.UnwindStack;
                    //return false;
                }

                if ((ev is bool) && (bool) ev == false) {

                    object alt1Evrandtemp;
                    if (environment.FastLexicalRef (out alt1Evrandtemp, this.altRandName, this.altRandDepth, this.altRandOffset))
                        throw new NotImplementedException ();

                    object alt1Evrand = ((Cons) alt1Evrandtemp).Cdr;

                    object evop;
                    if (environment.FastLexicalRef (out evop, this.altRatorName, this.altRatorDepth,this.altRatorOffset)) 
                        throw new NotImplementedException();
                    return Interpreter.Call (out answer, ref expression, ref environment, evop, alt1Evrand);
//#if DEBUG
//                    noteCalls (this.alternative1);
//                    alternative1TypeHistogram.Note (this.alternative1Type);
//                    Debug.WriteLineIf (Primitive.Noisy, "    => #f");
//#endif
//                    expression = this.alternative1;
//                    answer = null;
//                    return true;
                }
                else {
#if DEBUG
                    noteCalls (this.consequent1);
                    consequent1TypeHistogram.Note (this.consequent1Type);
                    Debug.WriteLineIf (Primitive.Noisy, "    => #t");
#endif
                    expression = this.consequent1;
                    answer = null;
                    return true;
                }
            }
        }

    }

    class SComb1Fragment3 : SLet1CA0PA0CondComb1LCdrL
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> predicate1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequent1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternative1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object pred1Rand1Name;
        public readonly int pred1Rand1Depth;
        public readonly int pred1Rand1Offset;

        protected SComb1Fragment3 (SimpleLambda rator, PrimitiveIsPairA0 predicate, Conditional consequent,
            PrimitiveIsEqCarA0L consPredicate, SCode consConsequent, Combination1LCdrL consAlternative,
            SCode alternative, PrimitiveCarA0 rand)
            : base (rator, predicate, consequent, consPredicate, consConsequent, consAlternative, alternative, rand)
        {
            this.pred1Rand1Name = consPredicate.rand1Name;
            this.pred1Rand1Depth = consPredicate.rand1Depth;
            this.pred1Rand1Offset = consPredicate.rand1Offset;

        }

        static public SCode Make (SimpleLambda rator, PrimitiveIsPairA0 predicate, Conditional consequent,
            PrimitiveIsEqCarA0L consPredicate, SCode consConsequent, Combination1LCdrL consAlternative,
            SCode alternative, PrimitiveCarA0 rand)
        {
            return
                 new SComb1Fragment3 (rator, predicate, consequent, consPredicate, consConsequent, consAlternative, alternative, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SComb1Fragment3.EvalStep");
#endif
            Cons evarg = environment.Argument0Value as Cons;
            if (evarg == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);
            environment = new SmallEnvironment1 (cl, evarg.Car);

            if (!(evarg.Car is Cons)) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                object ev;
                object pred1Arg1;
                if (environment.FastLexicalRef (out pred1Arg1, this.pred1Rand1Name, this.pred1Rand1Depth, this.pred1Rand1Offset))
                    throw new NotImplementedException ();
                object pred1Arg0 = ((Cons) evarg.Car).Car;

                ObjectModel.Eq (out ev, pred1Arg0, pred1Arg1);
                //Control unev = this.predicate1;
                //Environment env = environment;

                //while (unev.EvalStep (out ev, ref unev, ref env)) { };
                //if (ev == Interpreter.UnwindStack) {
                //    throw new NotImplementedException ();
                //    //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                //    //environment = env;
                //    //answer = Interpreter.UnwindStack;
                //    //return false;
                //}

                if ((ev is bool) && (bool) ev == false) {

                    object alt1Evrandtemp;
                    if (environment.FastLexicalRef (out alt1Evrandtemp, this.altRandName, this.altRandDepth, this.altRandOffset))
                        throw new NotImplementedException ();

                    object alt1Evrand = ((Cons) alt1Evrandtemp).Cdr;

                    object evop;
                    if (environment.FastLexicalRef (out evop, this.altRatorName, this.altRatorDepth, this.altRatorOffset))
                        throw new NotImplementedException ();
                    return Interpreter.Call (out answer, ref expression, ref environment, evop, alt1Evrand);
                    //#if DEBUG
                    //                    noteCalls (this.alternative1);
                    //                    alternative1TypeHistogram.Note (this.alternative1Type);
                    //                    Debug.WriteLineIf (Primitive.Noisy, "    => #f");
                    //#endif
                    //                    expression = this.alternative1;
                    //                    answer = null;
                    //                    return true;
                }
                else {
#if DEBUG
                    noteCalls (this.consequent1);
                    consequent1TypeHistogram.Note (this.consequent1Type);
                    Debug.WriteLineIf (Primitive.Noisy, "    => #t");
#endif
                    expression = this.consequent1;
                    answer = null;
                    return true;
                }
            }
        }

    }

    [Serializable]
    class SimpleLet1CarA0Z2 : SimpleLet1CarA0
    {
        public readonly SCode first;
        public readonly SCode second;

        protected SimpleLet1CarA0Z2 (SimpleLambda rator, PrimitiveCarA0 rand)
            : base (rator, rand)
        {
            this.first = ((Sequence2) rator.Body).First;
            this.second = ((Sequence2) rator.Body).Second;
        }

        public static new SCode Make (SimpleLambda rator, PrimitiveCarA0 arg0)
        {
            return new SimpleLet1CarA0Z2 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.first);
            noteCalls (this.second);
#endif
            Cons evarg = environment.Argument0Value as Cons;
            if (evarg == null) throw new NotImplementedException ();


            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);
            environment = new SmallEnvironment1 (cl, evarg.Car);


            Control unev = this.first;
            Environment env = environment;
            while (unev.EvalStep (out answer, ref unev, ref env)) { };
            if (answer == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Sequence2Frame0 ((Sequence2) this.body, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            expression = this.second;
            return true;
        }

    }

    [Serializable]
    class SimpleLet1CarA1 : SimpleLet1CarA
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif

        protected SimpleLet1CarA1 (SimpleLambda rator, PrimitiveCarA1 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCarA1 arg0)
        {
            return new SimpleLet1CarA1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet1CarA1.EvalStep");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
#endif

            Cons evarg = environment.Argument1Value as Cons;
            if (evarg == null) throw new NotImplementedException ();


            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg.Car);
            answer = null;
            return true;
        }

    }

    [Serializable]
    class SimpleLet1CarL1 : SimpleLet1CarL
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif

        protected SimpleLet1CarL1 (SimpleLambda rator, PrimitiveCarL1 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCarL1 arg0)
        {
            return new SimpleLet1CarL1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
#endif
            object temp;
            if (environment.FastLexicalRef1 (out temp, this.rand0Name, this.rand0Offset))
               throw new NotImplementedException ();
            Cons evarg = temp as Cons;
            if (evarg == null) throw new NotImplementedException ();


            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg.Car);
            answer = null;
            return true;
        }

    }

    [Serializable]
    class SimpleLet1Q : SimpleLet1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object arg0Value;

        protected SimpleLet1Q (SimpleLambda rator, Quotation rand)
            : base (rator, rand)
        {
            this.arg0Value = rand.Quoted;
        }

        public static SCode Make (SimpleLambda rator, Quotation arg0)
        {
            return new SimpleLet1Q (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet1Q");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
#endif

            object evarg = this.arg0Value;

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg);
            answer = null;
            return true;
        }

    }


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

    class Letrec1 : Let1
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Lambda recursiveProc;

        Letrec1 (Lambda rator)
            : base (rator, Quotation.Unassigned)
        {
            this.recursiveProc = (Lambda) ((Assignment) ((Sequence2) rator.Body).First).Value;
        }

        public static SCode Make (Lambda rator)
        {
            return new Letrec1 (rator);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Letrec1.EvalStep");
#endif
            answer = new SmallEnvironment1 (((Lambda) this.rator).Close (environment), recursiveProc).Argument0Value;
            return false;
        }
    }




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