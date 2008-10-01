using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Microcode
{
    /// <summary>
    /// Base class for lambda expressions.
    /// </summary>
    abstract class LambdaBase : SCode
    {
        static public readonly string internalLambda = String.Intern ("#[internal-lambda]");
        static public readonly string unnamed = String.Intern ("#[unnamed-procedure]");
        static public readonly string let = String.Intern ("#[let-procedure]");

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly string name; 

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly string [] formals;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode body;     
 
        protected LambdaBase (TC typeCode, string name, string [] formals, SCode body)
            : base (typeCode)
        {
            this.name = name;
            this.formals = formals;
            this.body = body;
        }

        public string Name { [DebuggerStepThrough] get { return this.name; } }
        public string [] Formals { [DebuggerStepThrough] get { return this.formals; } }
        public SCode Body { [DebuggerStepThrough] get { return this.body; } }

        public int LexicalOffset (string name)
        {
            for (int i = 0; i < formals.Length; i++)
                if ((object) name == (object) formals [i])
                    return i;
            return -1;
        }

        public override bool MutatesAny (object [] formals)
        {
            // Should check for shadowing.
            return this.body.MutatesAny (formals);
        }

        public override bool UsesAny (object [] formals)
        {
            // Should check for shadowing.
            return this.body.UsesAny (formals);
        }

    }

    /// <summary>
    /// ExtendedLambda supports optional, rest, and aux args.
    /// </summary>
    [Serializable]
    sealed class ExtendedLambda : LambdaBase, ISystemHunk3
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly bool callsTheEnvironment;

        public readonly uint required;

        public readonly uint optional;

        public readonly bool rest;

        ExtendedLambda (string name, string [] formals, SCode body, uint required, uint optional, bool rest)
            : base (TC.EXTENDED_LAMBDA, name, formals, body)
        {
            this.callsTheEnvironment = body.CallsTheEnvironment ();
            this.required = required;
            this.optional = optional;
            this.rest = rest;
        }

        public static SCode Make (string name, string [] formals, object body, uint required, uint optional, bool rest)
        {
            return new ExtendedLambda (name, formals, EnsureSCode (body), required, optional, rest);
        }

        public static SCode Make (Hunk3 init)
        {

            object [] cdrArray = (object []) init.Cxr1;
            string name = (string) cdrArray [0];
            string [] formals = new string [cdrArray.Length - 1];
            for (int i = 0; i < formals.Length; i++)
                formals [i] = (string) cdrArray [i + 1];
            uint code = (uint) (int) (init.Cxr2);
            return Make (name, formals, EnsureSCode (init.Cxr0), (code >> 8) & 0xFF, code & 0xFF, ((code >> 16) & 0x1) == 0x1);
        }

        [SchemePrimitive ("EXTENDED-LAMBDA?", 1, true)]
        public static bool IsExtendedLambda (out object answer, object arg)
        {
            answer = arg is ExtendedLambda;
            return false;
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optimized = this.body.Bind (ctenv.Extend (this));
            return optimized == this.body
                ? this
                : new ExtendedLambda (this.name, this.formals, optimized, this.required, this.optional, this.rest);
        }

        public override bool CallsTheEnvironment ()
        {
            return this.callsTheEnvironment;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            answer = new ExtendedClosure (this, environment);
            return false;
        }

        #region ISystemHunk3 Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr0
        {
            [DebuggerStepThrough]
            get
            {
                return UnwrapQuoted (this.body);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr1
        {
            [DebuggerStepThrough]
            get
            {
                string [] fakeFormals = new string [this.formals.Length + 1];
                fakeFormals [0] = this.name;
                Array.Copy (this.formals, 0, fakeFormals, 1, formals.Length);
                return fakeFormals;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr2
        {
            [DebuggerStepThrough]
            get
            {
                return this.optional + (256 * (this.required + (this.rest ? 256 : 0)));

            }

            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

    }

    /// <summary>
    /// Lambda is the abstract base class for Lambda expressions with
    /// fixed argument lists (no optional, rest, or aux).
    /// </summary>
    [Serializable]
    abstract class Lambda : LambdaBase, ISystemPair
    {
        protected Lambda (string name, string [] formals, SCode body)
            : base (TC.LAMBDA, name, formals, body)
        {
        }

        public static SCode Make (string name, string [] formals, SCode body)
        {
            if (body == null)
                throw new ArgumentNullException ("body");
            if (formals == null)
                throw new ArgumentNullException ("formals");
            if (name == null)
                throw new ArgumentNullException ("name");

            if (Configuration.EnableLexicalAddressing)
                return
                    // If someone in the body calls `the-environment', then we
                    // *must* create a first-class enabled environment.
                    body.CallsTheEnvironment () ? (Lambda) new StandardLambda (name, formals, body)
                    : (body.MutatesAny (formals) || formals.Length == 0) ? (Lambda) new StaticLambda (name, formals, body)
                    : (Lambda) new SimpleLambda (name, formals, body);
            else
                return new StandardLambda (name, formals, body);
        }

        public static SCode Make (object name, object formals, object body)
        {

            if (formals == null)
                throw new ArgumentNullException ("formals");

            string sname = (string) name;
            object [] cdrArray = (object []) formals;
            string [] sformals = new string [cdrArray.Length];

            for (int i = 0; i < sformals.Length; i++)
                sformals [i] = (string) cdrArray [i];

            return Make (sname, sformals, EnsureSCode (body));
        }

        [SchemePrimitive ("LAMBDA?", 1, true)]
        public static bool IsLambda (out object answer, object arg)
        {
            answer = arg is Lambda;
            return false;
        }

        public abstract Closure Close (Environment environment);

        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object ISystemPair.SystemPairCar
        {
            [DebuggerStepThrough]
            get
            {
                return UnwrapQuoted (this.body);
            }

            set
            {
                throw new NotImplementedException ();
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object ISystemPair.SystemPairCdr
        {
            [DebuggerStepThrough]
            get
            {
                string [] fakeFormals = new string [this.formals.Length + 1];
                fakeFormals [0] = this.name;
                Array.Copy (this.formals, 0, fakeFormals, 1, formals.Length);
                return fakeFormals;
            }

            set
            {
                throw new NotImplementedException ();
            }
        }
        #endregion
    }

    /// <summary>
    /// A StandardLambda creates StandardClosures.  These support first-class
    /// environments that allow incremental definition and variable sharing.
    /// </summary>
    [Serializable]
    sealed class StandardLambda : Lambda
    {
        internal StandardLambda (string name, string [] formals, SCode body)
            : base (name, formals, body)
        { }

        public override Closure Close (Environment environment)
        {
            return new StandardClosure (this, environment);
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optimizedBody = this.body.Bind (ctenv.Extend (this));
            return this.body == optimizedBody
                ? this
                : new StandardLambda (this.name, this.formals, optimizedBody);
        }

        public override bool CallsTheEnvironment ()
        {
            return true;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            answer = new StandardClosure (this, environment);
            return false;
        }
    }

    /// <summary>
    /// This is the abstract base class for all lambdas that do not support
    /// incremental definition.  This allows us to optimize for the case
    /// where we know the lexical addresses of free variables and don't
    /// have to worry about shadowing definition.
    /// </summary>
    abstract class StaticLambdaBase : Lambda
    {
        protected StaticLambdaBase (string name, string [] formals, SCode body)
            : base (name, formals, body)
        { }

        public override bool CallsTheEnvironment ()
        {
            return false;
        }
    }

    /// <summary>
    /// A Static lambda does not support incremental definition, but it does
    /// support assignment and unassigned variables.
    /// </summary>
    [Serializable]
    sealed class StaticLambda : StaticLambdaBase
    {
        internal StaticLambda (string name, string [] formals, SCode body)
            : base (name, formals, body)
        { }

        public override Closure Close (Environment environment)
        {
            return new StaticClosure (this, environment);
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optimizedBody = this.body.Bind (ctenv.Extend (this));
            return this.body == optimizedBody
                ? this
                : new StaticLambda (this.name, this.formals, optimizedBody);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            answer = new StaticClosure (this, environment);
            return false;
        }
    }

    /// <summary>
    /// A SimpleLambda creates SimpleClosures.  There can be no incremental
    /// definition, no assignment, and no unassigned variables.  Despite these
    /// restrictions, this is the most popular kind of lambda.
    /// </summary>
    [Serializable]
    sealed class SimpleLambda : Lambda
    {
        internal SimpleLambda (string name, string [] formals, SCode body)
            : base (name, formals, body)
        {
        }

        public override Closure Close (Environment environment)
        {
            return new SimpleClosure (this, environment);
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optimizedBody = this.body.Bind (ctenv.Extend (this));
            return this.body == optimizedBody
                ? this
                : new SimpleLambda (this.name, this.formals, optimizedBody);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            answer = new SimpleClosure (this, environment);
            return false;
        }

        public override bool CallsTheEnvironment ()
        {
            return false;
        }
    }
}
