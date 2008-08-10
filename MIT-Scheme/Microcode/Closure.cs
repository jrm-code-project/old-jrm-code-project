using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace Microcode
{
    interface IClosure
    {
        int FormalOffset (string name);

        Environment Environment { get; }
        ILambda Lambda { get; }
    }

    class Closure : SCode, IClosure, ISystemPair
    {
        static public bool Noisy = false;
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

        public ILambda Lambda
        {
            [DebuggerStepThrough]
            get { return this.lambda; }
        }

        public Environment Environment
        {
            [DebuggerStepThrough]   
            get
            {
                return this.environment;
            }
        }

        public string Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.lambda.Name;
            }
        }

        public int FormalOffset (string name)
        {
            return this.lambda.LexicalOffset (name);
        }


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

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            object [] rands = new object [interpreter.Arguments.Length + 1];
            rands [0] = this;
            for (int i = 1; i < rands.Length; i++)
                rands [i] = interpreter.Arguments [i - 1];
            int nargs = rands.Length;
            int nparams = this.lambda.Formals.Length;
            if (nargs != nparams)
                throw new NotImplementedException ();
            if (Noisy && this.lambda.Name != unnamed && this.lambda.Name != let && this.lambda.Name != internalLambda) Debug.WriteLine (this.lambda.Name);
            return interpreter.EvalReduction (this.lambda.Body, new InterpreterEnvironment (rands));
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
        public static void IsProcedure (Interpreter interpreter, object arg)
        {
            interpreter.Return (arg is Closure);
        }

        internal override SCode Optimize (CompileTimeEnvironment ctenv)
        {
            throw new NotImplementedException ();
        }
    }

    class ExtendedClosure : SCode, IClosure, ISystemPair
    {
        static public bool Noisy = false;
        static public readonly string internalLambda = String.Intern ("#[internal-lambda]");
        static public readonly string unnamed = String.Intern ("#[unnamed-procedure]");
        static public readonly string let = String.Intern ("#[let-procedure]");
        public ExtendedLambda lambda;
        public Environment environment; 

        public ExtendedClosure (ExtendedLambda lambda, Environment environment)
            : base (TC.EXTENDED_PROCEDURE)
        {
            this.lambda = lambda;
            if (environment == null)
                throw new ArgumentNullException ("environment");
            this.environment = environment;
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            object [] rands = interpreter.Arguments;
            int nargs = rands.Length;
            int nparams = this.lambda.formals.Length - 1; // param 0 is self
            int formals = (int)this.lambda.required;
            int parms = (int)this.lambda.optional + formals;
            bool rest_flag = this.lambda.rest;
            int auxes = nparams - (parms + (rest_flag ? 1 : 0));

            if (nargs < formals)
            {
                throw new NotImplementedException ("Too few arguments.");
            }
            else if (!rest_flag & (nargs > parms))
            {
                throw new NotImplementedException ("Too many arguments.");
            }
            int size = parms + (rest_flag ? 1 : 0) + auxes;

            int randptr = 0;
            int frameptr = 1;
            object [] framevector = new object [size+1];
            framevector [0] = this;

            if (nargs <= parms)
            {
                int i;
                for (i = (nargs); --i >= 0; )
                    framevector [frameptr++] = rands [randptr++];
                for (i = (parms - nargs); --i >= 0; )
                    framevector [frameptr++] = Constant.DefaultObject;
                if (rest_flag)
                    framevector [frameptr++] = null;
                for (i = auxes; --i >= 0; ) {
                    throw new NotImplementedException ();
                    //framevector [frameptr++] = Constant.Unassigned;
                }
            }
            else
            {
                // rest flag must be true
                int i;
                int listloc;
                for (i = (parms); --i >= 0; )
                    framevector [frameptr++] = rands [randptr++];
                listloc = frameptr++;
                framevector [listloc] = null;
                for (i = auxes; --i >= 0; ) {
                    throw new NotImplementedException ();
                    //framevector [frameptr++] = Constant.Unassigned;
                }
                randptr = rands.Length;
                for (i = (nargs - parms); --i >= 0; )
                    framevector [listloc] = new Cons (rands [--randptr], framevector [listloc]);
            }
            if (Noisy && this.lambda.Name != unnamed && this.lambda.Name != let && this.lambda.Name != internalLambda) Debug.WriteLine (this.lambda.Name);
            return interpreter.EvalReduction (this.lambda.body, new InterpreterEnvironment (framevector));
        }

        public int FormalOffset (string name)
        {
            return this.lambda.LexicalOffset (name);
        }

        [SchemePrimitive ("EXTENDED-PROCEDURE?", 1)]
        public static void IsExtendedProcedure (Interpreter interpreter, object arg)
        {
            interpreter.Return (arg is ExtendedClosure);
        }

        public string Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.lambda.Name;
            }
        }

        #region IClosure Members

        public Environment Environment
        {
            get { return this.environment; }
        }

        public ILambda Lambda
        {
            get { return this.lambda; }
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

        internal override SCode Optimize (CompileTimeEnvironment ctenv)
        {
            throw new NotImplementedException ();
        }
    }
}
