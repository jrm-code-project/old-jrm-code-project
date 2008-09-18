using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace Microcode
{
    public interface IClosure : IApplicable, ISystemPair
    {
        int FormalOffset (string name);

        Environment Environment { get; }
        ILambda Lambda { get; }
    }

    [Serializable]
    sealed class Closure : SchemeObject, IClosure
    {
#if DEBUG
        [NonSerialized]
        static long applicationCount = 0;
        [NonSerialized]
        static public bool Noisy = false;
        [NonSerialized]
        static long [] histogram = new long [700];

#endif

        static public readonly string internalLambda = String.Intern ("#[internal-lambda]");
        static public readonly string unnamed = String.Intern ("#[unnamed-procedure]");
        static public readonly string let = String.Intern ("#[let-procedure]");

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Lambda lambda;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Environment environment;

        public Closure (Lambda lambda, Environment environment)
            : base (TC.PROCEDURE)
        {
            if (environment == null)
                throw new ArgumentNullException ("environment");
            this.lambda = lambda;
            this.environment = environment;
        }

        public static SchemeObject Make (object lambda, Environment environment)
        {
            SimpleLambda slambda = lambda as SimpleLambda;
            if (slambda != null) return new SimpleClosure (slambda, environment);
            Lambda l = lambda as Lambda;
            if (l != null) return new Closure (l, environment);
            TrivialLambda tlambda = lambda as TrivialLambda;
            if (tlambda != null) return new TrivialClosure (tlambda, environment);
            throw new NotImplementedException ();
        }

        public string Name
        {
            [DebuggerStepThrough]
            get { return this.lambda.Name; }
        }

        public override string ToString ()
        {
            if (this.lambda.Name == unnamed
                || this.lambda.Name == internalLambda
                || this.lambda.Name == let)
            {
                StringBuilder sb = new StringBuilder ();      
                sb.Append ("#<PROCEDURE " + this.lambda.Name + " (");        
                string [] formals = this.lambda.Formals;
                if (formals.Length > 0)
                {                
                    sb.Append (formals [0]);
                    for (int i = 1; i < formals.Length; i++)
                    {
                        sb.Append (" ");
                        sb.Append (formals [i]);
                    }
                }
                sb.Append (")>");
                return sb.ToString ();
            }
            else
                return "#<PROCEDURE " + this.lambda.Name + ">";
        }

        [SchemePrimitive ("PROCEDURE?", 1, true)]
        public static bool IsProcedure (out object answer, object arg)
        {
            answer = arg is Closure || arg is SimpleClosure || arg is TrivialClosure;
            return false;
        }

        #region IClosure Members

        public int FormalOffset (string name)
        {
            return this.lambda.LexicalOffset (name);
        }

        public Environment Environment
        {
            [DebuggerStepThrough]   
            get { return this.environment; }
        }

        public ILambda Lambda
        {
            [DebuggerStepThrough]
            get { return this.lambda; }
        }

        #endregion

        #region IApplicable Members

        public bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
#if DEBUG
            Closure.applicationCount += 1;
            Closure.histogram [args.Length] += 1;
#endif
            if (args.Length != this.lambda.Formals.Length)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new StandardEnvironment (this, args);
            answer = null; // keep the compiler happy
            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
#if DEBUG
            Closure.applicationCount += 1;
            Closure.histogram [1] += 1;
#endif
            if (this.lambda.Formals.Length != 1)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new StandardEnvironment (this, new object [] { arg0 });
            answer = null; // keep the compiler happy
            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
#if DEBUG
            Closure.applicationCount += 1;
            Closure.histogram [2] += 1;
#endif
            if (this.lambda.Formals.Length != 2)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new StandardEnvironment (this, new object [] { arg0, arg1 });
            answer = null; // keep the compiler happy
            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            throw new NotImplementedException ();
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

        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]  
        public object SystemPairCar
        {
            [DebuggerStepThrough]
            get
            {
                return this.lambda;
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

        #region IApplicable Members


 

        #endregion
    }

    [Serializable]
    sealed class ExtendedClosure : SchemeObject, IClosure
    {
#if DEBUG
        static long applicationCount = 0;
        static public bool Noisy = false;
#endif
        static public readonly string internalLambda = String.Intern ("#[internal-lambda]");
        static public readonly string unnamed = String.Intern ("#[unnamed-procedure]");
        static public readonly string let = String.Intern ("#[let-procedure]");

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly ExtendedLambda lambda;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Environment environment;

        public ExtendedClosure (ExtendedLambda lambda, Environment environment)
            : base (TC.EXTENDED_PROCEDURE)
        {
            this.lambda = lambda;
            if (environment == null)
                throw new ArgumentNullException ("environment");
            this.environment = environment;
        }

        public string Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.lambda.Name;
            }
        }

        public override string ToString ()
        {
            return "#<PROCEDURE " + this.lambda.Name + ">";
        }

        [SchemePrimitive ("EXTENDED-PROCEDURE?", 1, true)]
        public static bool IsExtendedProcedure (out object answer, object arg)
        {
            answer = arg is ExtendedClosure;
            return false;
        }

        #region IClosure Members

        public int FormalOffset (string name)
        {
            return this.lambda.LexicalOffset (name);
        }

        public Environment Environment
        {
            [DebuggerStepThrough]
            get { return this.environment; }
        }

        public ILambda Lambda
        {
            [DebuggerStepThrough]
            get { return this.lambda; }
        }

        #endregion

        #region IApplicable Members

        public bool Call (out object answer, ref Control expression, ref Environment environment)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
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
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            throw new NotImplementedException ();
        }


        public bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
#if DEBUG
            ExtendedClosure.applicationCount += 1;
#endif
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
                for (i = (nargs); --i >= 0; ){
                    framevector [frameptr++] = args [argptr++];
                }
                for (i = (parms - nargs); --i >= 0; )
                    framevector [frameptr++] = Constant.DefaultObject;
                if (rest_flag)
                    framevector [frameptr++] = null;
                for (i = auxes; --i >= 0; ) {
                    throw new NotImplementedException ();
                    //framevector [frameptr++] = Constant.Unassigned;
                }
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
#if DEBUG
            if (Noisy && this.lambda.Name != unnamed && this.lambda.Name != let && this.lambda.Name != internalLambda) Debug.WriteLine (this.lambda.Name);
#endif
            expression = this.lambda.Body;
            environment = new StandardEnvironment (this, framevector);
            answer = null;
            return true;
        }

        #endregion

        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            [DebuggerStepThrough]
            get
            {
                return this.lambda;
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


        #region IApplicable Members



        #endregion
    }

    [Serializable]
    sealed class SimpleClosure : SchemeObject, IClosure
    {
#if DEBUG
        [NonSerialized]
        static long applicationCount = 0;
        [NonSerialized]
        static public bool Noisy = false;
        [NonSerialized]
        static long [] histogram = new long [700];

#endif

        static public readonly string internalLambda = String.Intern ("#[internal-lambda]");
        static public readonly string unnamed = String.Intern ("#[unnamed-procedure]");
        static public readonly string let = String.Intern ("#[let-procedure]");

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SimpleLambda lambda;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Environment environment;

        public SimpleClosure (SimpleLambda lambda, Environment environment)
            : base (TC.PROCEDURE)
        {
            if (environment == null)
                throw new ArgumentNullException ("environment");
            this.lambda = lambda;
            this.environment = environment;
        }

        public string Name
        {
            [DebuggerStepThrough]
            get { return this.lambda.Name; }
        }

        public override string ToString ()
        {
            if (this.lambda.Name == unnamed
                || this.lambda.Name == internalLambda
                || this.lambda.Name == let) {
                StringBuilder sb = new StringBuilder ();
                sb.Append ("#<PROCEDURE " + this.lambda.Name + " (");
                string [] formals = this.lambda.Formals;
                if (formals.Length > 0) {
                    sb.Append (formals [0]);
                    for (int i = 1; i < formals.Length; i++) {
                        sb.Append (" ");
                        sb.Append (formals [i]);
                    }
                }
                sb.Append (")>");
                return sb.ToString ();
            }
            else
                return "#<PROCEDURE " + this.lambda.Name + ">";
        }


        #region IClosure Members

        public int FormalOffset (string name)
        {
            return this.lambda.LexicalOffset (name);
        }

        public Environment Environment
        {
            [DebuggerStepThrough]
            get { return this.environment; }
        }

        public ILambda Lambda
        {
            [DebuggerStepThrough]
            get { return this.lambda; }
        }

        #endregion

        #region IApplicable Members

        public bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
#if DEBUG
            SimpleClosure.applicationCount += 1;
            SimpleClosure.histogram [args.Length] += 1;
#endif
            if (args.Length != this.lambda.Formals.Length)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new SimpleEnvironment (this, args);
            answer = null; // keep the compiler happy
            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
#if DEBUG
            SimpleClosure.applicationCount += 1;
            SimpleClosure.histogram [1] += 1;
#endif
            if (this.lambda.Formals.Length != 1)
                throw new NotImplementedException ("this.lambda.formals.length != 1, it is " + this.lambda.Formals.Length.ToString());
            expression = this.lambda.Body;
            environment = new SmallEnvironment (this, arg0);
            answer = null; // keep the compiler happy
            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
#if DEBUG
            SimpleClosure.applicationCount += 1;
            SimpleClosure.histogram [2] += 1;
#endif
            if (this.lambda.Formals.Length != 2)
                throw new NotImplementedException ("this.lambda.formals.length != 2");
            expression = this.lambda.Body;
            environment = new SmallEnvironment (this, arg0, arg1);
            answer = null; // keep the compiler happy
            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            throw new NotImplementedException ("call with 4 arguments");
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            throw new NotImplementedException ("call with 5 arguments");
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            throw new NotImplementedException ("call with 6 arguments");
        }

        #endregion

        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            [DebuggerStepThrough]
            get
            {
                return this.lambda;
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
    sealed class TrivialClosure : SchemeObject, IClosure
    {
#if DEBUG
        [NonSerialized]
        static long applicationCount = 0;
        [NonSerialized]
        static public bool Noisy = false;
        [NonSerialized]
        static long [] histogram = new long [700];

#endif

        static public readonly string internalLambda = String.Intern ("#[internal-lambda]");
        static public readonly string unnamed = String.Intern ("#[unnamed-procedure]");
        static public readonly string let = String.Intern ("#[let-procedure]");

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly TrivialLambda lambda;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Environment environment;

        public TrivialClosure (TrivialLambda lambda, Environment environment)
            : base (TC.PROCEDURE)
        {
            if (environment == null)
                throw new ArgumentNullException ("environment");
            this.lambda = lambda;
            this.environment = environment;
        }

        public string Name
        {
            [DebuggerStepThrough]
            get { return this.lambda.Name; }
        }

        public override string ToString ()
        {
            if (this.lambda.Name == unnamed
                || this.lambda.Name == internalLambda
                || this.lambda.Name == let) {
                StringBuilder sb = new StringBuilder ();
                sb.Append ("#<PROCEDURE " + this.lambda.Name + " (");
                string [] formals = this.lambda.Formals;
                if (formals.Length > 0) {
                    sb.Append (formals [0]);
                    for (int i = 1; i < formals.Length; i++) {
                        sb.Append (" ");
                        sb.Append (formals [i]);
                    }
                }
                sb.Append (")>");
                return sb.ToString ();
            }
            else
                return "#<PROCEDURE " + this.lambda.Name + ">";
        }



        #region IClosure Members

        public int FormalOffset (string name)
        {
            return this.lambda.LexicalOffset (name);
        }

        public Environment Environment
        {
            [DebuggerStepThrough]
            get { return this.environment; }
        }

        public ILambda Lambda
        {
            [DebuggerStepThrough]
            get { return this.lambda; }
        }

        #endregion

        #region IApplicable Members

        public bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
#if DEBUG
            TrivialClosure.applicationCount += 1;
            TrivialClosure.histogram [args.Length] += 1;
#endif
            if (args.Length != this.lambda.Formals.Length)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new StandardEnvironment (this, args);
            answer = null; // keep the compiler happy
            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
#if DEBUG
            TrivialClosure.applicationCount += 1;
            TrivialClosure.histogram [1] += 1;
#endif
            if (this.lambda.Formals.Length != 1)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new SmallEnvironment (this, arg0);
            answer = null; // keep the compiler happy
            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
#if DEBUG
            TrivialClosure.applicationCount += 1;
            TrivialClosure.histogram [2] += 1;
#endif
            if (this.lambda.Formals.Length != 2)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = new SmallEnvironment (this, arg0, arg1);
            answer = null; // keep the compiler happy
            return true;
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { arg0, arg1, arg2 });
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            throw new NotImplementedException ();
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

        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            [DebuggerStepThrough]
            get
            {
                return this.lambda;
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

}
