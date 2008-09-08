using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace Microcode
{
    interface IClosure : IApplicable, ISystemPair
    {
        int FormalOffset (string name);

        Environment Environment { get; }
        ILambda Lambda { get; }
    }

    sealed class Closure : SchemeObject, IClosure
    {
#if DEBUG
        static long applicationCount = 0;
        static public bool Noisy = false;
#endif

        static public readonly string internalLambda = String.Intern ("#[internal-lambda]");
        static public readonly string unnamed = String.Intern ("#[unnamed-procedure]");
        static public readonly string let = String.Intern ("#[let-procedure]");

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Lambda lambda;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Environment environment;

        public Closure (Lambda lambda, Environment environment)
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
            if (this.lambda.Name == unnamed)
            {
                StringBuilder sb = new StringBuilder ();
                string [] formals = this.lambda.Formals;
                sb.Append ("#<PROCEDURE (");
                if (formals.Length > 1)
                {
                    sb.Append (formals [1]);
                    for (int i = 2; i < formals.Length; i++)
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

        [SchemePrimitive ("PROCEDURE?", 1)]
        public static bool IsProcedure (out object answer, object arg)
        {
            answer = arg is Closure;
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

        public bool Apply (out object answer, ref SCode expression, ref Environment environment, object [] args)
        {
#if DEBUG
            Closure.applicationCount += 1;
#endif
            if (args.Length != this.lambda.Formals.Length)
                throw new NotImplementedException ();
            expression = this.lambda.Body;
            environment = this.environment.Extend (this, args);
            answer = null; // keep the compiler happy
            return true;

        }

        public bool Call (out object answer, ref SCode expression, ref Environment environment)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { this });
        }

        public bool Call (out object answer, ref SCode expression, ref Environment environment, object arg0)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { this, arg0 });
        }

        public bool Call (out object answer, ref SCode expression, ref Environment environment, object arg0, object arg1)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { this, arg0, arg1 });
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

//        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
//        {
//#if DEBUG
//            Closure.applicationCount += 1;
//#endif
//            throw new NotImplementedException ();
//            //object [] values = environment.FrameVector;
//            //int nargs = values.Length;
//            //int nparams = this.lambda.Formals.Length;
//            //if (nargs != nparams)
//            //    throw new NotImplementedException ();
//            //if (Noisy && this.lambda.Name != unnamed && this.lambda.Name != let && this.lambda.Name != internalLambda) Debug.WriteLine (this.lambda.Name);

//            //expression = this.lambda.Body;
//            //return true;
//        }

    }

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
        ExtendedLambda lambda;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Environment environment;

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

        [SchemePrimitive ("EXTENDED-PROCEDURE?", 1)]
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
            get { return this.environment; }
        }

        public ILambda Lambda
        {
            get { return this.lambda; }
        }

        #endregion

        #region IApplicable Members

        public bool Call (out object answer, ref SCode expression, ref Environment environment)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { this });
        }

        public bool Call (out object answer, ref SCode expression, ref Environment environment, object arg0)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { this, arg0 });
        }

        public bool Call (out object answer, ref SCode expression, ref Environment environment, object arg0, object arg1)
        {
            return Apply (out answer, ref expression, ref environment, new object [] { this, arg0, arg1 });
        }

        public bool Apply (out object answer, ref SCode expression, ref Environment environment, object [] args)
        {
#if DEBUG
            ExtendedClosure.applicationCount += 1;
#endif
            //object [] rands = environment.FrameVector;
            int nargs = args.Length - 1;
            int nparams = this.lambda.formals.Length - 1; // param 0 is self
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

            int argptr = 1;
            int frameptr = 1;
            object [] framevector = new object [size + 1];
            framevector [0] = this;

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
                // rest flag must be true
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
            expression = this.lambda.body;
            environment = this.environment.Extend (this, framevector);
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

    }
}
