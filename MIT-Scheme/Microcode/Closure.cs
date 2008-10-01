using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace Microcode
{
    // Recapitulate the lambda hierarchy

    abstract class ClosureBase : SchemeObject, IApplicable, ISystemPair
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Environment environment;

        protected ClosureBase (TC typeCode, Environment environment)
            : base (typeCode)
        {
            this.environment = environment;
        }

        public abstract LambdaBase Lambda { get; }

        public int FormalOffset (string name)
        {
            return this.Lambda.LexicalOffset (name);
        }


        public string Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.Lambda.Name;
            }
        }

        public Environment Environment
        {
            [DebuggerStepThrough]
            get { return this.environment; }
        }

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
                return this.environment;
            }

            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion
    }

    [Serializable]
    sealed class ExtendedClosure : ClosureBase
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly ExtendedLambda lambda;

        public ExtendedClosure (ExtendedLambda lambda, Environment environment)
            : base (TC.EXTENDED_PROCEDURE, environment)
        {
            this.lambda = lambda;
        }

        public override LambdaBase Lambda
        {
            [DebuggerStepThrough]
            get { return this.lambda; }
        }

        public override string ToString ()
        {
            return "#<EXTENDED-PROCEDURE " + this.Name + ">";
        }

        [SchemePrimitive ("EXTENDED-PROCEDURE?", 1, true)]
        public static bool IsExtendedProcedure (out object answer, object arg)
        {
            answer = arg is ExtendedClosure;
            return false;
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
            //object [] rands = environment.FrameVector;
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
            environment = this.lambda.CallsTheEnvironment () || !Configuration.EnableLexicalAddressing
                ? (Environment) new StandardEnvironment (this, framevector)
                : (Environment) new StaticEnvironment (this, framevector);
            answer = null;
            return true;
        }

        #endregion
    }

    abstract class Closure : ClosureBase
    {
        protected Closure (Environment environment)
            : base (TC.PROCEDURE, environment)
        {
        }
    }

    [Serializable]
    sealed class StandardClosure : Closure
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly StandardLambda lambda;

        internal StandardClosure (StandardLambda lambda, Environment environment)
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

        public override bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
            if (args.Length != this.lambda.Formals.Length)
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
    abstract class StaticClosureBase : Closure
    {
        protected StaticClosureBase (Environment environment)
            : base (environment)
        { }
    }

    [Serializable]
    sealed class StaticClosure : Closure
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly StaticLambda lambda;

        internal StaticClosure (StaticLambda lambda, Environment environment)
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

        public override bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
            if (this.lambda.Formals.Length != args.Length)
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
    sealed class SimpleClosure : StaticClosureBase
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SimpleLambda lambda;

        internal SimpleClosure (SimpleLambda lambda, Environment environment)
            : base (environment)
        {
            if (lambda.Formals.Length == 0) throw new NotImplementedException ();
            this.lambda = lambda;
        }

        public override LambdaBase Lambda
        {
            [DebuggerStepThrough]
            get { return this.lambda; }
        }

        #region IApplicable Members

        public override bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
            switch (args.Length) {
                case 1: return this.Call (out answer, ref expression, ref environment, args [0]);
                case 2: return this.Call (out answer, ref expression, ref environment, args [0], args [1]);
                case 3: return this.Call (out answer, ref expression, ref environment, args [0], args [1], args [2]);
                default:
                    if (args.Length != this.lambda.Formals.Length)
                        throw new NotImplementedException ();
                    expression = this.lambda.Body;
                    environment = (Environment) new SimpleEnvironment (this, args);
                    answer = null; // keep the compiler happy
                    return true;
            }
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
            if (this.lambda.Formals.Length != 1)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new SmallEnvironment1 (this, arg0);
            answer = null; // keep the compiler happy
            return true;
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
            if (this.lambda.Formals.Length != 2)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new SmallEnvironment2 (this, arg0, arg1);
            answer = null; // keep the compiler happy
            return true;
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
            if (this.lambda.Formals.Length != 3)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new SmallEnvironment3 (this, arg0, arg1, arg2);
            answer = null; // keep the compiler happy
            return true;
        }

        public override bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            if (this.lambda.Formals.Length != 4)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new SmallEnvironment4 (this, arg0, arg1, arg2, arg3);
            answer = null; // keep the compiler happy
            return true;
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






    //[Serializable]
    //sealed class SimpleClosure : SchemeObject, IClosure
    //{
    //    static public readonly string internalLambda = String.Intern ("#[internal-lambda]");
    //    static public readonly string unnamed = String.Intern ("#[unnamed-procedure]");
    //    static public readonly string let = String.Intern ("#[let-procedure]");

    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly SimpleLambda lambda;

    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly Environment environment;

    //    public SimpleClosure (SimpleLambda lambda, Environment environment)
    //        : base (TC.PROCEDURE)
    //    {
    //        if (environment == null)
    //            throw new ArgumentNullException ("environment");
    //        this.lambda = lambda;
    //        this.environment = environment;
    //    }

    //    public string Name
    //    {
    //        [DebuggerStepThrough]
    //        get { return this.lambda.Name; }
    //    }

    //    public override string ToString ()
    //    {
    //        if (this.lambda.Name == unnamed
    //            || this.lambda.Name == internalLambda
    //            || this.lambda.Name == let) {
    //            StringBuilder sb = new StringBuilder ();
    //            sb.Append ("#<PROCEDURE " + this.lambda.Name + " (");
    //            string [] formals = this.lambda.Formals;
    //            if (formals.Length > 0) {
    //                sb.Append (formals [0]);
    //                for (int i = 1; i < formals.Length; i++) {
    //                    sb.Append (" ");
    //                    sb.Append (formals [i]);
    //                }
    //            }
    //            sb.Append (")>");
    //            return sb.ToString ();
    //        }
    //        else
    //            return "#<PROCEDURE " + this.lambda.Name + ">";
    //    }


    //    #region IClosure Members

    //    public int FormalOffset (string name)
    //    {
    //        return this.lambda.LexicalOffset (name);
    //    }

    //    public Environment Environment
    //    {
    //        [DebuggerStepThrough]
    //        get { return this.environment; }
    //    }

    //    public ILambda Lambda
    //    {
    //        [DebuggerStepThrough]
    //        get { return this.lambda; }
    //    }

    //    #endregion

    //    #region IApplicable Members

    //    public bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
    //    {
    //        if (args.Length != this.lambda.Formals.Length)
    //            throw new NotImplementedException ();
    //        expression = this.lambda.Body;
    //        environment = new SimpleEnvironment (this, args);
    //        answer = null; // keep the compiler happy
    //        return true;
    //    }

    //    public bool Call (out object answer, ref Control expression, ref Environment environment)
    //    {
    //        if (this.lambda.Formals.Length != 0)
    //            throw new NotImplementedException ("this.lambda.formals.length != 0, it is " + this.lambda.Formals.Length.ToString ());
    //        expression = this.lambda.Body;
    //        environment = new SimpleEnvironment (this, new object [] {});
    //        answer = null; // keep the compiler happy
    //        return true;
    //    }

    //    public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
    //    {
    //        if (this.lambda.Formals.Length != 1)
    //            throw new NotImplementedException ("this.lambda.formals.length != 1, it is " + this.lambda.Formals.Length.ToString());
    //        expression = this.lambda.Body;
    //        environment = new SmallEnvironment (this, arg0);
    //        answer = null; // keep the compiler happy
    //        return true;
    //    }

    //    public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
    //    {
    //        if (this.lambda.Formals.Length != 2)
    //            throw new NotImplementedException ("this.lambda.formals.length != 2");
    //        expression = this.lambda.Body;
    //        environment = new SmallEnvironment (this, arg0, arg1);
    //        answer = null; // keep the compiler happy
    //        return true;
    //    }

    //    public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
    //    {
    //        if (this.lambda.Formals.Length != 3)
    //            throw new NotImplementedException ("this.lambda.formals.length != 3");
    //        expression = this.lambda.Body;
    //        environment = new SimpleEnvironment (this, new object [] {arg0, arg1, arg2});
    //        answer = null; // keep the compiler happy
    //        return true;
    //    }

    //    public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
    //    {
    //        throw new NotImplementedException ("call with 4 arguments");
    //    }

    //    public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
    //    {
    //        throw new NotImplementedException ("call with 5 arguments");
    //    }

    //    public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
    //    {
    //        throw new NotImplementedException ("call with 6 arguments");
    //    }

    //    #endregion

    //    #region ISystemPair Members

    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    public object SystemPairCar
    //    {
    //        [DebuggerStepThrough]
    //        get
    //        {
    //            return this.lambda;
    //        }

    //        set
    //        {
    //            throw new NotImplementedException ();
    //        }
    //    }

    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    public object SystemPairCdr
    //    {
    //        [DebuggerStepThrough]
    //        get
    //        {
    //            return this.environment;
    //        }

    //        set
    //        {
    //            throw new NotImplementedException ();
    //        }
    //    }

    //    #endregion
    //}

}
