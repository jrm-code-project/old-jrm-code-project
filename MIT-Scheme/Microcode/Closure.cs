using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    class Closure : SCode, ISystemPair
    {
        static public string unnamed = String.Intern ("#[unnamed-procedure]");

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Lambda lambda;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected Environment environment;

        public Closure (Lambda lambda, Environment environment)
        {
            this.lambda = lambda;
            this.environment = environment;
        }

        public Environment Environment
        {
            [DebuggerStepThrough]   
            get
            {
                return this.environment;
            }
        }

        public string [] Formals
        {
            [DebuggerStepThrough]  
            get
            {
                return this.lambda.Formals;
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
            return this.lambda.FormalOffset (name);
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
            object [] rands = interpreter.Arguments;
            int nargs = rands.Length;
            int nparams = this.lambda.formals.Length - 1;
            if (nargs != nparams)
                throw new NotImplementedException ();

            return interpreter.EvalReduction (this.lambda.body, new InterpreterEnvironment (this, rands));
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
                    sb.Append (formals [1].ToString ());
                    for (int i = 2; i < formals.Length; i++)
                    {
                        sb.Append (" ");
                        sb.Append (formals [i].ToString ());
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
    }

    class ExtendedClosure : Closure
    {
        public ExtendedLambda lambda;

        public ExtendedClosure (ExtendedLambda lambda, Environment environment)
            : base (lambda, environment)
        {
            this.lambda = lambda;
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            object [] rands = interpreter.Arguments;
            int nargs = rands.Length;
            int nparams = this.lambda.formals.Length - 1;
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
            int frameptr = 0;
            object [] framevector = new object [size];

            if (nargs <= parms)
            {
                int i;
                for (i = (nargs); --i >= 0; )
                    framevector [frameptr++] = rands [randptr++];
                for (i = (parms - nargs); --i >= 0; )
                    framevector [frameptr++] = Constant.Unassigned;
                if (rest_flag)
                    framevector [frameptr++] = null;
                for (i = auxes; --i >= 0; )
                    framevector [frameptr++] = Constant.Unassigned;
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
                for (i = auxes; --i >= 0; )
                    framevector [frameptr++] = Constant.Unassigned;
                randptr = rands.Length;
                for (i = (nargs - parms); --i >= 0; )
                    framevector [listloc] = new Cons (rands [--randptr], framevector [listloc]);
            }

            return interpreter.EvalReduction (this.lambda.body, new InterpreterEnvironment (this, framevector));
        }

        [SchemePrimitive ("EXTENDED-PROCEDURE?", 1)]
        public static void IsExtendedProcedure (Interpreter interpreter, object arg)
        {
            interpreter.Return (arg is ExtendedClosure);
        }
    }
}
