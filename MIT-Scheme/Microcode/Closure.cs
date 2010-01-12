using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    [Serializable]
    public abstract class ClosureBase<LType> : SchemeObject, ISystemPair where LType : LambdaBase
    {
#if DEBUG
        static Histogram<ClosureBase<LType>> hotClosures = new Histogram<ClosureBase<LType>>();
#endif
        static long closureCount;

        protected long callCount;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly LType closureLambda;

        //[DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected SCode lambdaBody;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Environment closureEnvironment;

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        //protected readonly ValueCell [] staticBindings;
        protected readonly object[] staticBindings;

        protected ClosureBase (LType lambda, Environment environment)
            : base ()
        {
            this.closureLambda = lambda;
            this.lambdaBody = lambda.Body;
            this.closureEnvironment = environment;
            this.staticBindings = environment.GetValueCells (lambda.StaticMapping);
        }

        protected ClosureBase (LType lambda, Environment environment, 
            //ValueCell [] staticBindings
            object [] staticBindings)
            : base ()
        {
            this.closureLambda = lambda;
            this.lambdaBody = lambda.Body;
            this.closureEnvironment = environment;
            this.staticBindings = staticBindings;
        }

        internal LType Lambda
        {
            get { return this.closureLambda; }
        }

        /// <summary>
        /// Returns the array of symbols bound when applying this closure.
        /// </summary>
        public Symbol [] BoundVariables
        {
            [DebuggerStepThrough]
            get
            {
                return this.closureLambda.Formals;
            }
        }

        /// <summary>
        /// Returns the collection of variables used by this closure.
        /// The bound variables are not in the collection.
        /// </summary>
        public ICollection<Symbol> FreeVariables
        {
            [DebuggerStepThrough]
            get
            {
                return this.closureLambda.FreeVariables;
            }
        }

        public int FormalOffset (object name)
        {
            return this.closureLambda.LexicalOffset (name);
        }

        public int StaticOffset (object name)
        {
            return this.closureLambda.StaticOffset (name, this.closureEnvironment);
        }

        public StaticMapping StaticNames (ICollection<Symbol> freeVariables)
        {
            throw new NotImplementedException ();
            //StaticMapping [] answer = this.Environment.GetStaticMapping (freeVariables);
            //StaticMapping.ValidateStaticMapping (answer);
            //return answer;
        }

        public bool StaticValue (out object value, object name, int staticOffset)
        {
            //return this.staticBindings [staticOffset].GetValue (out value);
            object cell = this.staticBindings[staticOffset];
            ValueCell vcell = cell as ValueCell;
            if (vcell == null) {
                value = cell;
                return false;
            }
            else {
                return vcell.GetValue(out value);
            }
        }

        internal bool SetStaticValue (out object oldValue, object name, object newValue, int staticOffset)
        {
            object cell = this.staticBindings[staticOffset];
            ValueCell vcell = cell as ValueCell;
            if (vcell != null)
                return vcell.Assign(out oldValue, newValue);
            else
                throw new NotImplementedException();
                // return this.staticBindings [staticOffset].Assign (out oldValue, newValue);
        }

        //public ValueCell StaticCell (int staticOffset)
        //{
        //    return this.staticBindings [staticOffset];
        //}

        //public ValueCell [] StaticCells { [DebuggerStepThrough] get { return this.staticBindings; } }
        public object[] StaticCells { [DebuggerStepThrough] get { return this.staticBindings; } }

        public Symbol Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.closureLambda.Name;
            }
        }


        public Environment Environment
        {
            [DebuggerStepThrough]
            get { return this.closureEnvironment; }
        }

        //public static Symbol [] closureHistory = new Symbol [15];
        //public static Symbol isPort = Symbol.Make ("port?");
        public static Symbol readChar = Symbol.Make ("read-char");
        public static Symbol peekChar = Symbol.Make ("peek-char");
        public static Symbol readUnquoted = Symbol.Make ("read-unquoted");

        public static bool printName = false;
        static Histogram<string> callPath = new Histogram<String> ();
        protected void BumpCallCount() 
        {
            closureCount += 1;
            //if (printName) {
            //    Debug.WriteLine (this.closureLambda.Name);
            //    if (this.closureLambda.Name == readUnquoted) {
            //        Debug.WriteLine ("");
            //        Debugger.Break ();
            //    }
            //}
#if DEBUG
            //for (int i = closureHistory.Length-1; i > 0; --i)
            //    closureHistory [i] = closureHistory [i-1];
            //closureHistory[0] = this.closureLambda.Name;
            //if (this.closureLambda.Name == isPort) {
            //    string path = "";
            //    for (int i = 0; i < closureHistory.Length; i++)
            //        path = path +  closureHistory [i].ToString () + " ";
            //    callPath.Note (path);
            //}
            //if (this.closureLambda.Name == Symbol.Make ("read"))
            //    Debugger.Break ();
            if ((callCount % 1000) == 999 &&
                this.StaticCells.Length > 0) {
                hotClosures.Note (this);
            }
#endif
        }

        protected abstract void XXOptimize ();


        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            [DebuggerStepThrough]
            get
            {
                return this.closureLambda;
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
                if (this.closureEnvironment is GlobalEnvironment)
                    return Constant.sharpF;
                else
                    return this.closureEnvironment;
            }

            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

    }

    [Serializable]
    abstract class ExtendedClosure<LType> : ClosureBase<LType> where LType:ExtendedLambda
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.EXTENDED_PROCEDURE; } }

        protected ExtendedClosure (LType lambda, Environment environment)
            : base (lambda, environment)
        {
        }


        public override string ToString ()
        {
            return "#<EXTENDED-PROCEDURE " + this.Name + ">";
        }
    }

    [Serializable]
    class StandardExtendedClosure : ExtendedClosure<StandardExtendedLambda>, IApplicable
    {
        public StandardExtendedClosure (StandardExtendedLambda lambda, Environment environment)
            : base (lambda, environment)
        {
        }

        #region IApplicable Members

        public bool Call (out object answer, ref Control expression, ref Environment environment)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
#if DEBUG
            SCode.location = "StaticExtendedClosure.Call1";
#endif
            return Apply (out answer, ref expression, ref environment, new object [] { arg0 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
#if DEBUG
            SCode.location = "StaticExtendedClosure.Call2";
#endif
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2, arg3 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2, arg3, arg4 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2, arg3, arg4, arg5 });
        }

        public bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
#if DEBUG
            SCode.location = "-";
            this.BumpCallCount ();
            SCode.location = "StandardExtendedClosure.Apply";
            SCode.NoteCalls (this, this.closureLambda.Body);
#endif
            int nargs = args.Length;
            int nparams = this.closureLambda.Formals.Length; // param 0 is self
            int formals = (int) this.closureLambda.required;
            int parms = (int) this.closureLambda.optional + formals;
            bool rest_flag = this.closureLambda.rest;
            int auxes = nparams - (parms + (rest_flag ? 1 : 0));

            if (nargs < formals) {
                throw new NotImplementedException ("Too few arguments.");
            }
            else if (!rest_flag & (nargs > parms)) {
                throw new NotImplementedException ("Too many arguments.");
            }
            int size = parms + (rest_flag ? 1 : 0) + auxes;

            int argptr = 0;
            int frameptr = 0;
            object [] framevector = new object [size];

            if (nargs <= parms) {
                int i;
                for (i = (nargs); --i >= 0; ) {
                    framevector [frameptr++] = args [argptr++];
                }
                for (i = (parms - nargs); --i >= 0; )
                    framevector [frameptr++] = Constant.DefaultObject;
                if (rest_flag)
                    framevector [frameptr++] = null;
                for (i = auxes; --i >= 0; )
                    framevector [frameptr++] = Constant.DefaultObject;
            }
            else {
                // theRestMarker flag must be true
                int i;
                int listloc;
                for (i = (parms); --i >= 0; )
                    framevector [frameptr++] = args [argptr++];
                listloc = frameptr++;
                framevector [listloc] = null;
                for (i = auxes; --i >= 0; ) {
                    throw new NotImplementedException ();
                    //framevector [frameptr++] = Constant.Unassigned;
                }
                argptr = args.Length;
                for (i = (nargs - parms); --i >= 0; )
                    framevector [listloc] = new Cons (args [--argptr], framevector [listloc]);
            }
            expression = this.closureLambda.Body;
            environment = new StandardEnvironment<StandardExtendedLambda, StandardExtendedClosure> (this, framevector);
            answer = null;
            return true;
        }

        #endregion

        protected override void XXOptimize ()
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    abstract class Closure<LType> : ClosureBase<LType> where LType:LambdaBase
    {
        public readonly int arity;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.PROCEDURE; } }

        protected Closure (LType lambda, Environment environment)
            : base (lambda, environment)
        {
            this.arity = lambda.Formals.Length;
        }

        protected Closure (LType lambda, Environment environment, object [] staticBindings)
            : base (lambda, environment, staticBindings)
        {
            this.arity = lambda.Formals.Length;
        }

        // Used in subclasses when they want to invoke the closure
        // on zero arguments.
        static protected object [] noArguments =  new object [] { };

        public override string ToString ()
        {
            return "#<PROCEDURE " + this.Name + ">";
        }
    }

    [Serializable]
    sealed class StandardClosure : Closure<StandardLambda>, IApplicable
    {
        internal StandardClosure (StandardLambda lambda, Environment environment)
            : base (lambda, environment)
        {
        }

        #region IApplicable Members

        public bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
#if DEBUG
            this.BumpCallCount ();
            SCode.location = "StandardClosure.Apply";
            SCode.NoteCalls (this, this.closureLambda.Body);
#endif
            if (args.Length != this.arity)
                throw new NotImplementedException ();

            expression = this.closureLambda.Body;
            environment = new StandardEnvironment<StandardLambda, StandardClosure> (this, args);

            answer = null; // keep the compiler happy
            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment)
        {
            return this.Apply (out answer, ref expression, ref environment, noArguments);
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
#if DEBUG
            SCode.location = "StandardClosure.Call1";
#endif
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
#if DEBUG
            SCode.location = "StandardClosure.Call2";
#endif
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2, arg3 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            throw new NotImplementedException ();
        }

        #endregion

        protected override void XXOptimize ()
        {
            throw new NotImplementedException ();
        }
    }

    abstract class StaticClosureBase<LType> : Closure<LType> where LType : StaticLambdaBase
    {
        protected StaticClosureBase (LType lambda, Environment environment, object [] staticBindings)
            : base (lambda, environment, staticBindings)
        { }
    }

    sealed class StaticClosure : StaticClosureBase<StaticLambda>, IApplicable
    {
        internal StaticClosure (StaticLambda lambda, Environment environment, object [] staticBindings)
            : base (lambda, environment, staticBindings)
        {
        }

        #region IApplicable Members

        public bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
#if DEBUG
            this.BumpCallCount ();
            SCode.location = "StaticClosure.Apply";
            SCode.NoteCalls (this, this.closureLambda.Body);
#endif
            if (args.Length != this.arity)
                throw new NotImplementedException ();
            expression = this.closureLambda.Body;
            environment = new StaticEnvironment (this, args);
            answer = null; // keep the compiler happy
            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment)
        {
            return this.Apply (out answer, ref expression, ref environment, noArguments);
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
#if DEBUG
            SCode.location = "StaticClosure.Call1";
#endif
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
#if DEBUG
            SCode.location = "StaticClosure.Call2";
#endif
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
#if DEBUG
            SCode.location = "StaticClosure.Call3";
#endif
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
#if DEBUG
            SCode.location = "StaticClosure.Call4";
#endif
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2, arg3 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            throw new NotImplementedException ();
        }

        #endregion


        protected override void XXOptimize ()
        {
            throw new NotImplementedException ();
        }
    }

    sealed class SimpleClosure : StaticClosureBase<SimpleLambda>, IApplicable
    {
        internal SimpleClosure (SimpleLambda lambda, Environment environment, object [] staticBindings)
            : base (lambda, environment, staticBindings)
        {
        }

        #region IApplicable Members

        public bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
            switch (args.Length) {
                case 0: return this.Call (out answer, ref expression, ref environment);
                case 1: return this.Call (out answer, ref expression, ref environment, args [0]);
                case 2: return this.Call (out answer, ref expression, ref environment, args [0], args [1]);
                case 3: return this.Call (out answer, ref expression, ref environment, args [0], args [1], args [2]);
                default:
#if DEBUG
                    this.BumpCallCount ();
                    SCode.location = "SimpleClosure.Apply";
                    SCode.NoteCalls (this, this.lambdaBody);
#endif
                    if (args.Length != this.arity)
                        return Error.WrongNumberOfArguments (out answer, ref expression, ref environment);
                    expression = this.lambdaBody;
                    environment = new SimpleEnvironment (this, args);
                    answer = null; // keep the compiler happy
                    return true;
            }
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            this.BumpCallCount ();
            SCode.location = "SimpleClosure.Call0";
            SCode.NoteCalls (this, this.lambdaBody);
#endif 
            if (this.arity != 0)
                return Error.WrongNumberOfArguments (out answer, ref expression, ref environment);

            if (callCount++ == Configuration.OptimizeThreshold && this.StaticCells.Length != 0) this.XXOptimize ();

            expression = this.lambdaBody;
            environment = new SmallEnvironment0 (this);
            answer = null; // keep the compiler happy
            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
#if DEBUG
            SCode.location = "-";
            this.BumpCallCount ();
            SCode.location = "SimpleClosure.Call1";
            SCode.NoteCalls (this, this.lambdaBody);
#endif
            if (this.arity != 1)
                return Error.WrongNumberOfArguments (out answer, ref expression, ref environment);
            //if (callCount++ == Configuration.OptimizeThreshold && this.StaticCells.Length != 0) this.XXOptimize ();

            expression = this.lambdaBody;
            environment = new SmallEnvironment1 (this, arg0);

            answer = null; // keep the compiler happy
            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
#if DEBUG
            SCode.location = "-";
            this.BumpCallCount ();
            SCode.NoteCalls (this, this.lambdaBody);
            SCode.location = "SimpleClosure.Call2";
#endif
            if (this.arity != 2)
                return Error.WrongNumberOfArguments (out answer, ref expression, ref environment);
            //if (callCount++ == Configuration.OptimizeThreshold  && this.StaticCells.Length != 0) this.XXOptimize ();

            expression = this.lambdaBody;
            environment = new SmallEnvironment2 (this, arg0, arg1);

            answer = null; // keep the compiler happy
            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
#if DEBUG
            this.BumpCallCount ();
            SCode.location = "SimpleClosure.Call3";
            SCode.NoteCalls (this, this.lambdaBody);
#endif
            if (this.arity != 3)
                return Error.WrongNumberOfArguments (out answer, ref expression, ref environment);
            if (callCount++ == Configuration.OptimizeThreshold && this.StaticCells.Length != 0) this.XXOptimize ();

            expression = this.lambdaBody; 
            environment = new SmallEnvironment3 (this, arg0, arg1, arg2);
            answer = null; // keep the compiler happy
            return true;            
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2, arg3 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            throw new NotImplementedException ();
        }

        #endregion

        protected override void XXOptimize ()
                    {
                        return;
            //Symbol [] names = this.closureLambda.StaticMapping.Names;
            //object [] values = this.staticBindings;
            //Debugger.Break ();
            //IDictionary<Symbol, object> namesToValues = new Dictionary<Symbol, object> ();
            //for (int i = 0; i < names.Length; i++) {
            //    Symbol name = names [i];
            //    object value = values [i];
            //    ValueCell cell = value as ValueCell;
            //    if (cell == null) {
            //        namesToValues.Add (name, value);
            //    }
            //}
            //if (namesToValues.Count > 0) {
            //    PartialEnvironment env = new PartialValueEnvironment (namesToValues);
            //    Debugger.Break ();
            //    PartialResult res = this.closureLambda.PartialEval (env);
            //    this.lambdaBody = res.Residual;
            //}
        }
    }

    sealed class ConstantClosure : StaticClosureBase<ConstantLambda>, IApplicable
    {
        public readonly object constantValue;
        internal ConstantClosure (ConstantLambda lambda, Environment environment, object [] staticBindings)
            : base (lambda, environment, staticBindings)
        {
            this.constantValue = lambda.constantValue;
        }

        #region IApplicable Members

        public bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
            if (this.arity != args.Length)
                return Error.WrongNumberOfArguments (out answer, ref expression, ref environment);
            answer = this.constantValue;
            return false;

//            switch (args.Length) {
//                case 0: return this.Call (out answer, ref expression, ref environment);
//                case 1: return this.Call (out answer, ref expression, ref environment, args [0]);
//                case 2: return this.Call (out answer, ref expression, ref environment, args [0], args [1]);
//                case 3: return this.Call (out answer, ref expression, ref environment, args [0], args [1], args [2]);
//                default:
//#if DEBUG
//                    this.BumpCallCount ();
//                    SCode.location = "SimpleClosure.Apply";
//                    SCode.NoteCalls (this, this.lambdaBody);
//#endif
//                    if (args.Length != this.arity)
//                        return Error.WrongNumberOfArguments (out answer, ref expression, ref environment);
//                    expression = this.lambdaBody;
//                    environment = new SimpleEnvironment (this, args);
//                    answer = null; // keep the compiler happy
//                    return true;
//            }
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
//            this.BumpCallCount ();
            SCode.location = "SimpleClosure.Call0";
//            SCode.NoteCalls (this, this.lambdaBody);
#endif
            if (this.arity != 0)
                return Error.WrongNumberOfArguments (out answer, ref expression, ref environment);
            answer = this.constantValue;
            return false;

//            if (callCount++ == Configuration.OptimizeThreshold && this.StaticCells.Length != 0) this.XXOptimize ();

//            expression = this.lambdaBody;
//            environment = new SmallEnvironment0 (this);
//            answer = null; // keep the compiler happy
//            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
#if DEBUG
//            SCode.location = "-";
//            this.BumpCallCount ();
            SCode.location = "SimpleClosure.Call1";
//            SCode.NoteCalls (this, this.lambdaBody);
#endif
            if (this.arity != 1)
                return Error.WrongNumberOfArguments (out answer, ref expression, ref environment);
            answer = this.constantValue;
            return false;
//            //if (callCount++ == Configuration.OptimizeThreshold && this.StaticCells.Length != 0) this.XXOptimize ();

//            expression = this.lambdaBody;
//            environment = new SmallEnvironment1 (this, arg0);

//            answer = null; // keep the compiler happy
//            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
#if DEBUG
//            SCode.location = "-";
//            this.BumpCallCount ();
            SCode.location = "SimpleClosure.Call2";
//            SCode.NoteCalls (this, this.lambdaBody);
#endif
            if (this.arity != 2)
                return Error.WrongNumberOfArguments (out answer, ref expression, ref environment);
            answer = this.constantValue;
            return false;
//            if (this.arity != 2)
//                return Error.WrongNumberOfArguments (out answer, ref expression, ref environment);
//            if (callCount++ == Configuration.OptimizeThreshold && this.StaticCells.Length != 0) this.XXOptimize ();

//            expression = this.lambdaBody;
//            environment = new SmallEnvironment2 (this, arg0, arg1);

//            answer = null; // keep the compiler happy
//            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
#if DEBUG
//            this.BumpCallCount ();
            SCode.location = "SimpleClosure.Call3";
//            SCode.NoteCalls (this, this.lambdaBody);
#endif
            if (this.arity != 3)
                return Error.WrongNumberOfArguments (out answer, ref expression, ref environment);
            answer = this.constantValue;
            return false;
//            if (this.arity != 3)
//                return Error.WrongNumberOfArguments (out answer, ref expression, ref environment);
//            if (callCount++ == Configuration.OptimizeThreshold && this.StaticCells.Length != 0) this.XXOptimize ();

//            expression = this.lambdaBody;
//            environment = new SmallEnvironment3 (this, arg0, arg1, arg2);
//            answer = null; // keep the compiler happy
//            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            if (this.arity != 4)
                return Error.WrongNumberOfArguments (out answer, ref expression, ref environment);
            answer = this.constantValue;
            return false;
            //return this.Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2, arg3 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            throw new NotImplementedException ();
        }

        #endregion

        protected override void XXOptimize ()
        {
            return;
            //Symbol [] names = this.closureLambda.StaticMapping.Names;
            //object [] values = this.staticBindings;
            //Debugger.Break ();
            //IDictionary<Symbol, object> namesToValues = new Dictionary<Symbol, object> ();
            //for (int i = 0; i < names.Length; i++) {
            //    Symbol name = names [i];
            //    object value = values [i];
            //    ValueCell cell = value as ValueCell;
            //    if (cell == null) {
            //        namesToValues.Add (name, value);
            //    }
            //}
            //if (namesToValues.Count > 0) {
            //    PartialEnvironment env = new PartialValueEnvironment (namesToValues);
            //    Debugger.Break ();
            //    PartialResult res = this.closureLambda.PartialEval (env);
            //    this.lambdaBody = res.Residual;
            //}
        }
    }


}
