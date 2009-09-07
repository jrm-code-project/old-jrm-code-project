using System;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    // Recapitulate the lambda hierarchy

    [Serializable]
    abstract public class ClosureBase : SchemeObject, IApplicable, ISystemPair
    {
#if DEBUG
        static Histogram<ClosureBase> hotClosures = new Histogram<ClosureBase>();
        protected long callCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected Environment closureEnvironment;

        protected ClosureBase (Environment environment)
        {
            this.closureEnvironment = environment;
        }

        public abstract LambdaBase Lambda { get; }

        public int FormalOffset (object name)
        {
            return this.Lambda.LexicalOffset (name);
        }

        public Symbol Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.Lambda.Name;
            }
        }

        public string LongName
        {
            get
            {
                Symbol name = this.Lambda.Name;
                if ((name == LambdaBase.Unnamed
                    || name == LambdaBase.InternalLambda
                    || name == LambdaBase.Let)
                    && this.Environment.closure != null)
                    return this.Environment.closure.LongName + " " + name;
                else
                    return name.ToString();
            }
        }

        public Environment Environment
        {
            [DebuggerStepThrough]
            get { return this.closureEnvironment; }
        }

#if DEBUG
        protected void BumpCallCount() 
        {
            callCount+= 1;
            if ((callCount % 500) == 499)
                hotClosures.Note (this);   
        }
#endif

        public abstract bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args);
        public abstract bool Call (out object answer, ref Control expression, ref Environment environment);
        public abstract bool Call (out object answer, ref Control expression, ref Environment environment, object arg0);
        public abstract bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1);
        public abstract bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2);
        public abstract bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3);
        public abstract bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4);
        public abstract bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5);

        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            [DebuggerStepThrough]
            get
            {
                return this.Lambda;
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
    abstract class ExtendedClosure : ClosureBase
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.EXTENDED_PROCEDURE; } }

        protected ExtendedClosure (Environment environment)
            : base (environment)
        {
        }

        [SchemePrimitive ("EXTENDED-PROCEDURE?", 1, true)]
        public static bool IsExtendedProcedure (out object answer, object arg)
        {
            answer = arg is ExtendedClosure;
            return false;
        }

        public override string ToString ()
        {
            return "#<EXTENDED-PROCEDURE " + this.Name + ">";
        }
    }

    public class PartialClosure : ClosureBase
    {
        LambdaBase closureLambda;
        Boolean needsEnvironment;

        public PartialClosure (LambdaBase lambda, Environment environment)
            : base (environment)
        {
            this.closureLambda = lambda;
            this.needsEnvironment = lambda.CallsTheEnvironment();
        }

        public override LambdaBase Lambda
        {
            get { return this.closureLambda; }
        }

        public override bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
            throw new NotImplementedException ();
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
            throw new NotImplementedException ();
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
            throw new NotImplementedException ();
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
            throw new NotImplementedException ();
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            throw new NotImplementedException ();
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            throw new NotImplementedException ();
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            throw new NotImplementedException ();
        }

        public override TC TypeCode
        {
            get { throw new NotImplementedException (); }
        }

        public Boolean FirstClassEnvironment
        {
            get { return this.needsEnvironment; }
        }
    }


    [Serializable]
    sealed class StandardExtendedClosure : ExtendedClosure
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly StandardExtendedLambda lambda;

        public StandardExtendedClosure (StandardExtendedLambda lambda, Environment environment)
            : base (environment)
        {
            this.lambda = lambda;
        }

        public override LambdaBase Lambda
        {
            [DebuggerStepThrough]
            get { return this.lambda; }
        }


        #region IApplicable Members

        public override bool Call (out object answer, ref Control expression, ref Environment environment)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2 });

        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2, arg3 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2, arg3, arg4 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2, arg3, arg4, arg5 });
        }

        public override bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
#if DEBUG
            SCode.location = "StandardExtendedClosure.Apply";
            this.BumpCallCount ();
#endif
            //object [] rands = closureEnvironment.FrameVector;
            int nargs = args.Length;
            int nparams = this.lambda.Formals.Length; // param 0 is self
            int formals = (int) this.lambda.required;
            int parms = (int) this.lambda.optional + formals;
            bool rest_flag = this.lambda.rest;
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
            expression = this.lambda.Body;
            environment = new StandardEnvironment (this, framevector);
            answer = null;
            return true;
        }

        #endregion
    }

    [Serializable]
    sealed class StaticExtendedClosure : ExtendedClosure
    {
                [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly StaticExtendedLambda lambda;

        public StaticExtendedClosure (StaticExtendedLambda lambda, Environment environment)
            : base (environment)
        {
            this.lambda = lambda;
        }

        public override LambdaBase Lambda
        {
            [DebuggerStepThrough]
            get { return this.lambda; }
        }


        #region IApplicable Members

        public override bool Call (out object answer, ref Control expression, ref Environment environment)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2 });

        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2, arg3 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2, arg3, arg4 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2, arg3, arg4, arg5 });
        }


        public override bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
#if DEBUG            
            SCode.location = "StaticExtendedClosure.Apply";
            this.BumpCallCount ();
#endif
            //object [] rands = closureEnvironment.FrameVector;
            int nargs = args.Length;
            int nparams = this.lambda.Formals.Length; // param 0 is self
            int formals = (int) this.lambda.required;
            int parms = (int) this.lambda.optional + formals;
            bool rest_flag = this.lambda.rest;
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
            expression = this.lambda.Body;
            environment = new StaticEnvironment (this, framevector);
            answer = null;
            return true;
        }

        #endregion

    }

    [Serializable]
    abstract class Closure : ClosureBase
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.PROCEDURE; } }

        protected Closure (Environment environment)
            //: base (TC.PROCEDURE, closureEnvironment)
            : base (environment)
        {
        }

        [SchemePrimitive ("PROCEDURE?", 1, true)]
        public static bool IsRecord (out object answer, object arg0)
        {
            answer = arg0 is Closure;
            return false;
        }

        public override string ToString ()
        {
            return "#<PROCEDURE " + this.Name + ">";
        }
    }

    [Serializable]
    sealed class StandardClosure : Closure
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly StandardLambda lambda;

        public readonly int argumentCount;

        internal StandardClosure (StandardLambda lambda, Environment environment)
            : base (environment)
        {
            this.lambda = lambda;
            this.argumentCount = lambda.Formals.Length;
        }

        public override LambdaBase Lambda
        {
            [DebuggerStepThrough]
            get { return this.lambda; }
        }

        #region IApplicable Members

        public override bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
#if DEBUG
            this.BumpCallCount ();
            SCode.location = "StandardClosure.Apply";
#endif
            if (args.Length != this.argumentCount)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new StandardEnvironment (this, args);
            answer = null; // keep the compiler happy
            return true;
        }

        static object [] noArguments =  new object [] { };
        public override bool Call (out object answer, ref Control expression, ref Environment environment)
        {
            return this.Apply (out answer, ref expression, ref environment, noArguments);
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2, arg3 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            throw new NotImplementedException ();
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            throw new NotImplementedException ();
        }

        #endregion
    }

    [Serializable]
    abstract class StaticClosureBase<LType> : Closure where LType:LambdaBase
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly LType lambda;

        public readonly int argumentCount;

        protected StaticClosureBase (Environment environment, LType lambda)
            : base (environment)
        {
            this.lambda = lambda;
            this.argumentCount = lambda.Formals.Length;
        }

        public override LambdaBase Lambda
        {
            [DebuggerStepThrough]
            get { return this.lambda; }
        }
    }

    [Serializable]
    sealed class StaticClosure : StaticClosureBase<StaticLambda>
    {
        internal StaticClosure (StaticLambda lambda, Environment environment)
            : base (environment, lambda)
        {
        }

        #region IApplicable Members

        public override bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
           
#if DEBUG
            this.BumpCallCount();
            SCode.location = "StaticClosure.Apply";
#endif
            if (this.argumentCount != args.Length)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new StaticEnvironment (this, args);
            answer = null; // happy compiler
            return true;
        }

        static object [] noArguments = new object [] { };
        public override bool Call (out object answer, ref Control expression, ref Environment environment)
        {
            return this.Apply (out answer, ref expression, ref environment, noArguments);
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2, arg3 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            throw new NotImplementedException ();
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            throw new NotImplementedException ();
        }

        #endregion
    }

    [Serializable]
    sealed class NonClosure : StaticClosureBase<StaticLambda>
    {
        internal NonClosure (StaticLambda lambda)
            : base (null, lambda)
        {
            if (lambda.Body.CallsTheEnvironment ())
                throw new NotImplementedException ();
        }

        #region IApplicable Members

        public override bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
#if DEBUG
            this.BumpCallCount ();
            SCode.location = "NonClosure.Apply";
#endif
            if (this.argumentCount != args.Length)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new StaticEnvironment (this, args);
            answer = null; // happy compiler
            return true;
        }

        static object [] noArguments = new object [] { };
        public override bool Call (out object answer, ref Control expression, ref Environment environment)
        {
            return this.Apply (out answer, ref expression, ref environment, noArguments);
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            return this.Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2, arg3 });
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            throw new NotImplementedException ();
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            throw new NotImplementedException ();
        }

        #endregion
    }


    [Serializable]
    sealed class SimpleClosure : StaticClosureBase<SimpleLambda>
    {
        SCode body;
        internal SimpleClosure (SimpleLambda lambda, Environment environment)
            : base (environment, lambda)
        {
            //if (lambda.Formals.Length == 0) throw new NotImplementedException ();
            this.body = lambda.Body;
        }

        #region IApplicable Members

        public override bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {

            switch (args.Length) {
                case 0: return this.Call (out answer, ref expression, ref environment);
                case 1: return this.Call (out answer, ref expression, ref environment, args [0]);
                case 2: return this.Call (out answer, ref expression, ref environment, args [0], args [1]);
                case 3: return this.Call (out answer, ref expression, ref environment, args [0], args [1], args [2]);
                case 4: return this.Call (out answer, ref expression, ref environment, args [0], args [1], args [2], args[3]);
                default:
#if DEBUG
                    this.BumpCallCount ();
#endif
                    if (args.Length != this.argumentCount)
                        throw new NotImplementedException ();
                    expression = this.body;
                    environment = (Environment) new SimpleEnvironment (this, args);
                    answer = null; // keep the compiler happy
                    return true;
            }
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            this.BumpCallCount ();
            SCode.location = "SimpleClosure.Call.0";
#endif
            if (this.argumentCount != 0)
                throw new NotImplementedException ("Wrong number of arguments.  Got 0, wanted " + this.argumentCount);
            expression = this.body;
#if DEBUG
            SCode.location = "new SmallEnvironment0";
#endif
            environment = new SmallEnvironment0 (this);
#if DEBUG
            SCode.location = "SimpleClosure.Call.0";
#endif
            answer = null; // keep the compiler happy
            return true;
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {        
#if DEBUG
            this.BumpCallCount ();
            SCode.location = "SimpleClosure.Call.1";
#endif
            if (this.argumentCount != 1)
                throw new NotImplementedException ("Wrong number of arguments.  Got 1, wanted " + this.argumentCount);
            expression = this.body;
#if DEBUG
            SCode.location = "new SmallEnvironment1";
#endif
            environment = new SmallEnvironment1 (this, arg0);
#if DEBUG
            SCode.location = "SimpleClosure.Call.1";
#endif
            answer = null; // keep the compiler happy
            return true;
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
#if DEBUG 
            SCode.location = "-";
            this.BumpCallCount ();
            SCode.location = "SimpleClosure.Call.2";
#endif
            if (this.argumentCount != 2)
                throw new NotImplementedException ();
            expression = this.body;
#if DEBUG
            SCode.location = "new SmallEnvironment2";
#endif
            environment = new SmallEnvironment2 (this, arg0, arg1);
#if DEBUG
            SCode.location = "SimpleClosure.Call.2";
#endif
            answer = null; // keep the compiler happy
            return true;
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
#if DEBUG
            this.BumpCallCount ();
#endif
            if (this.argumentCount != 3)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new SmallEnvironment3 (this, arg0, arg1, arg2);
            answer = null; // keep the compiler happy
            return true;
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
#if DEBUG
            this.BumpCallCount ();
#endif
            if (this.argumentCount != 4)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new SmallEnvironment4 (this, arg0, arg1, arg2, arg3);
            answer = null; // keep the compiler happy
            return true;
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            throw new NotImplementedException ("SimpleClosure called with 5 arguments");
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            throw new NotImplementedException ("SimpleClosure called with 6 arguments");
        }

        #endregion
    }

    [Serializable]
    sealed class SimpleNonClosure : StaticClosureBase<SimpleLambda>
    {
        SCode body;
        internal SimpleNonClosure (SimpleLambda lambda)
            : base (null, lambda)
        {
            //if (lambda.Formals.Length == 0) throw new NotImplementedException ();
            this.body = lambda.Body;
        }

        #region IApplicable Members

        public override bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {

            switch (args.Length) {
                case 0: return this.Call (out answer, ref expression, ref environment);
                case 1: return this.Call (out answer, ref expression, ref environment, args [0]);
                case 2: return this.Call (out answer, ref expression, ref environment, args [0], args [1]);
                case 3: return this.Call (out answer, ref expression, ref environment, args [0], args [1], args [2]);
                case 4: return this.Call (out answer, ref expression, ref environment, args [0], args [1], args [2], args [3]);
                default:
#if DEBUG
                    this.BumpCallCount ();
#endif
                    if (args.Length != this.argumentCount)
                        throw new NotImplementedException ();
                    expression = this.body;
                    environment = (Environment) new SimpleEnvironment (this, args);
                    answer = null; // keep the compiler happy
                    return true;
            }
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            this.BumpCallCount ();
            SCode.location = "SimpleNonClosure.Call.0";
#endif
            if (this.argumentCount != 0)
                throw new NotImplementedException ("Wrong number of arguments.  Got 0, wanted " + this.argumentCount);
            expression = this.body;
#if DEBUG
            SCode.location = "new SmallEnvironment0";
#endif
            environment = new SmallEnvironment0 (this);
#if DEBUG
            SCode.location = "SimpleNonClosure.Call.0";
#endif
            answer = null; // keep the compiler happy
            return true;
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
#if DEBUG
            this.BumpCallCount ();
            SCode.location = "SimpleNonClosure.Call.1";
#endif
            if (this.argumentCount != 1)
                throw new NotImplementedException ("Wrong number of arguments.  Got 1, wanted " + this.argumentCount);
            expression = this.body;
#if DEBUG
            SCode.location = "new SmallEnvironment1";
#endif
            environment = new SmallEnvironment1 (this, arg0);
#if DEBUG
            SCode.location = "SimpleNonClosure.Call.1";
#endif
            answer = null; // keep the compiler happy
            return true;
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
#if DEBUG
            SCode.location = "-";
            this.BumpCallCount ();
            SCode.location = "SimpleNonClosure.Call.2";
#endif
            if (this.argumentCount != 2)
                throw new NotImplementedException ();
            expression = this.body;
#if DEBUG
            SCode.location = "new SmallEnvironment2";
#endif
            environment = new SmallEnvironment2 (this, arg0, arg1);
#if DEBUG
            SCode.location = "SimpleNonClosure.Call.2";
#endif
            answer = null; // keep the compiler happy
            return true;
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
#if DEBUG
            this.BumpCallCount ();
#endif
            if (this.argumentCount != 3)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new SmallEnvironment3 (this, arg0, arg1, arg2);
            answer = null; // keep the compiler happy
            return true;
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
#if DEBUG
            this.BumpCallCount ();
#endif
            if (this.argumentCount != 4)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new SmallEnvironment4 (this, arg0, arg1, arg2, arg3);
            answer = null; // keep the compiler happy
            return true;
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            throw new NotImplementedException ("SimpleNonClosure called with 5 arguments");
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            throw new NotImplementedException ("SimpleNonClosure called with 6 arguments");
        }

        #endregion
    }
}
