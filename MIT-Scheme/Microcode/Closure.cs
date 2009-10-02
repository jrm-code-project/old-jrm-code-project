using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    // Recapitulate the lambda hierarchy (to some extent)
    [Serializable]
    public abstract class ClosureBase : SchemeObject, IApplicable, ISystemPair
    {
#if DEBUG
        static Histogram<ClosureBase> hotClosures = new Histogram<ClosureBase>();
        static long closureCount;
        static long leafClosureCallCount;
        static long staticClosureCallCount;
#endif
        protected long callCount;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Environment closureEnvironment;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly ValueCell [] staticBindings;

        protected ClosureBase (Environment environment, ValueCell [] staticBindings)
        {
            this.closureEnvironment = environment;
            this.staticBindings = staticBindings;
        }

        internal abstract LambdaBase Lambda { get; }

        public int FormalOffset (object name)
        {
            return this.Lambda.LexicalOffset (name);
        }

        public int StaticOffset (object name)
        {
            return this.Lambda.StaticOffset (name, this.closureEnvironment);
        }

        public bool StaticValue (out object value, object name, int staticOffset)
        {
            return this.staticBindings [staticOffset].GetValue (out value);
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
                if (name == LambdaBase.InternalLambda ||
                    name == LambdaBase.Let || 
                    name == LambdaBase.Unnamed) {
                    LexicalEnvironment lenv = this.Environment as LexicalEnvironment;
                    if (lenv != null &&
                        lenv.Closure != null)
                        return lenv.Closure.LongName + " " + name;
                    else
                        return name.ToString ();
                }
                else
                    return name.ToString ();
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
            closureCount += 1;
            callCount+= 1;
            if ((callCount % 500) == 499)
                hotClosures.Note (this);
            if (this.Lambda.internalLambdaCount == 0)
                leafClosureCallCount += 1;
            if (!this.Lambda.CallsTheEnvironment())
                staticClosureCallCount += 1;
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

        protected ExtendedClosure (Environment environment, ValueCell [] staticBindings)
            : base (environment, staticBindings)
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

    [Serializable]
    sealed class StandardExtendedClosure : ExtendedClosure
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly StandardExtendedLambda lambda;

        public StandardExtendedClosure (StandardExtendedLambda lambda, Environment environment)
            : base (environment, environment.GetValueCells (lambda.GetStaticMapping (environment)))
        {
            this.lambda = lambda;
        }

        internal override LambdaBase Lambda
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

    abstract class PartialClosureBase<LType> : ClosureBase where LType : LambdaBase
    {
        LType closureLambda;

        public PartialClosureBase (LType lambda, Environment environment)
            : base (environment, null)
        {
            this.closureLambda = lambda;
        }

        internal override LambdaBase Lambda
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
    }

    class PartialClosure : PartialClosureBase<LambdaBase>
    {
        public PartialClosure (LambdaBase lambda, Environment environment)
            : base (lambda, environment)
        {
        }

        public override TC TypeCode
        {
            get { throw new NotImplementedException (); }
        }
    }

    [Serializable]
    abstract class Closure : ClosureBase
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.PROCEDURE; } }

        protected Closure (Environment environment, ValueCell [] staticBindings)
            : base (environment, staticBindings)
        {
        }

        [SchemePrimitive ("PROCEDURE?", 1, true)]
        public static bool IsProcedure (out object answer, object arg0)
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
            : base (environment, environment.GetValueCells (lambda.GetStaticMapping(environment)))
        {
            this.lambda = lambda;
            this.argumentCount = lambda.Formals.Length;
        }

        internal override LambdaBase Lambda
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
}
