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
        protected readonly object [] formals;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected SCode body;     
 
        protected LambdaBase (TC typeCode, string name, object [] formals, SCode body)
            : base (typeCode)
        {
            this.name = name;
            this.formals = formals;
            this.body = body;
            // Paranoia:  check for duplicate names
            if (name != "dummy-procedure") {
                for (int i = 0; i < formals.Length - 1; i++)
                    for (int j = i + 1; j < formals.Length; j++)
                        if (formals [i] == formals [j]) Debugger.Break ();
            }
        }

        public string Name { [DebuggerStepThrough] get { return this.name; } }
        public object [] Formals { [DebuggerStepThrough] get { return this.formals; } }
        public SCode Body { [DebuggerStepThrough] get { return this.body; } }

        public int LexicalOffset (object name)
        {
            // This way is *slow*
            // return Array.IndexOf (this.formals, varname);
            for (int i = 0; i < this.formals.Length; i++)
                if (name == this.formals [i])
                    return i;
            return -1;
        }

        public bool IsLetrecLambda ()
        {
            return this.body.IsLetrecBody (this.formals, this.formals);
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

        public abstract BoundVariable IncreaseLexicalDepth (BoundVariable variable);

        //public abstract BoundVariable SimulateDangerousLookup (object name, LexicalMap parent, int safeDepth, int depth);
        //public abstract BoundVariable SimulateLookup (object name, LexicalMap parent);
        //public abstract BoundVariable SimulateStaticLookup (object name, LexicalMap parent, int depth);

        public override SCode Alpha (object from, object to)
        {
            throw new NotImplementedException ();
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ();
        }

        public override bool CallsTheEnvironment ()
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        public override bool IsLetrecBody (object [] formals, object [] remainingFormals)
        {
            throw new NotImplementedException ();
        }
    }

    /// <summary>
    /// Lambda is the abstract base class for Lambda expressions with
    /// fixed argument lists (no optional, rest, or aux).
    /// </summary>
    [Serializable]
    abstract class Lambda : LambdaBase, ISystemPair
    {
        protected Lambda (string name, object [] formals, SCode body)
            : base (TC.LAMBDA, name, formals, body)
        {
        }

        public static SCode Make (string name, object [] formals, SCode body)
        {
            if (body == null)
                throw new ArgumentNullException ("body");
            if (formals == null)
                throw new ArgumentNullException ("formals");
            if (name == null)
                throw new ArgumentNullException ("varname");

                    // If someone in the body calls `the-environment', then we
                    // *must* create a first-class enabled environment.   
            return 
                (body.CallsTheEnvironment () || ! Configuration.EnableStaticBinding) ? (Lambda) StandardLambda.Make (name, formals, body)
                : (formals.Length == 0 || body.MutatesAny(formals) || ! Configuration.EnableSimpleLambda) ? (Lambda) new StaticLambda (name, formals, body)
                : (Lambda) new SimpleLambda (name, formals, body);
        }

        public static SCode Make (object name, object formals, object body)
        {

            if (formals == null)
                throw new ArgumentNullException ("formals");

            string sname = (string) name;
            object [] cdrArray = (object []) formals;

            return Make (sname, cdrArray, EnsureSCode (body));
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
        public object SystemPairCar
        {
            [DebuggerStepThrough]
            get
            {
                return UnwrapQuoted (this.body);
            }

            [DebuggerStepThrough]
            set
            {
                // ugh!  but the advice mechanism uses it.
                this.body = EnsureSCode (value);
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCdr
        {
            //[DebuggerStepThrough]
            get
            {
                object [] fakeFormals = new object [this.formals.Length + 1];
                fakeFormals [0] = this.name;
                Array.Copy (this.formals, 0, fakeFormals, 1, this.formals.Length);
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
    /// ExtendedLambda is the abstract base class for Lambda expressions with
    /// variable argument lists (optional, rest, or aux).
    /// </summary>
    [Serializable]
    abstract class ExtendedLambda : LambdaBase, ISystemHunk3
    {
        public readonly uint required;

        public readonly uint optional;

        public readonly bool rest;

        protected ExtendedLambda (string name, object [] formals, SCode body, uint required, uint optional, bool rest)
            : base (TC.EXTENDED_LAMBDA, name, formals, body)
        {
            this.required = required;
            this.optional = optional;
            this.rest = rest;
        }

        public static SCode Make (string name, object [] formals, SCode body, uint required, uint optional, bool rest)
        {
            if (body == null)
                throw new ArgumentNullException ("body");
            if (formals == null)
                throw new ArgumentNullException ("formals");
            if (name == null)
                throw new ArgumentNullException ("varname");

                    // If someone in the body calls `the-environment', then we
                    // *must* create a first-class enabled environment.   
            return 
                (body.CallsTheEnvironment () || ! Configuration.EnableStaticBinding)
                    ? (ExtendedLambda) StandardExtendedLambda.Make (name, formals, body, required, optional, rest)
                    : (ExtendedLambda) StaticExtendedLambda.Make (name, formals, body, required, optional, rest);
        }

        public static SCode Make (Hunk3 init)
        {

            object [] cdrArray = (object []) init.Cxr1;
            string name = (string) cdrArray [0];
            object [] formals = new object [cdrArray.Length - 1];
            for (int i = 0; i < formals.Length; i++)
                formals [i] = cdrArray [i + 1];
            uint code = (uint) (int) (init.Cxr2);
            return Make (name, formals, EnsureSCode (init.Cxr0), (code >> 8) & 0xFF, code & 0xFF, ((code >> 16) & 0x1) == 0x1);
        }

        [SchemePrimitive ("EXTENDED-LAMBDA?", 1, true)]
        public static bool IsExtendedLambda (out object answer, object arg)
        {
            answer = arg is ExtendedLambda;
            return false;
        }

        #region ISystemHunk3 Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr0
        {
            // [DebuggerStepThrough]
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
            // [DebuggerStepThrough]
            get
            {
                object [] fakeFormals = new object [this.formals.Length + 1];
                fakeFormals [0] = this.name;
                Array.Copy (this.formals, 0, fakeFormals, 1, this.formals.Length);
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
            // [DebuggerStepThrough]
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


        //public override BoundVariable SimulateLookup (object name, LexicalMap parent)
        //{
        //    int offset = LexicalOffset (name);
        //    if (offset == -1)
        //        return parent.SimulateDangerousLookup (name, 0, 0);
        //    else
        //        return Argument.Make (name, offset);
        //}

        //public override BoundVariable SimulateStaticLookup (object name, LexicalMap parent, int depth)
        //{
        //    int offset = LexicalOffset (name);
        //    if (offset == -1)
        //        return parent.SimulateDangerousLookup (name, depth, 0);
        //    else
        //        return LexicalVariable.Make (name, depth, offset);
        //}

        //public override BoundVariable SimulateDangerousLookup (object name, LexicalMap parent, int safeDepth, int depth)
        //{
        //    int offset = LexicalOffset (name);
        //    if (offset == -1)
        //        return parent.SimulateDangerousLookup (name, safeDepth, depth + 1);
        //    else
        //        return DangerousLexicalVariable.Make (name, safeDepth, depth, offset);
        //}
    }

    /// <summary>
    /// Supports option, rest, and aux args, 
    /// and incremental definitions.
    /// </summary>
    [Serializable]
    sealed class StandardExtendedLambda : ExtendedLambda
    {
        StandardExtendedLambda (string name, object [] formals, SCode body, uint required, uint optional, bool rest)
            : base (name, formals, body, required, optional, rest)
        {
        }

        public static new StandardExtendedLambda Make (string name, object [] formals, SCode body, uint required, uint optional, bool rest)
        {
            if (body == null)
                throw new ArgumentNullException ("body");
            if (formals == null)
                throw new ArgumentNullException ("formals");
            if (name == null)
                throw new ArgumentNullException ("varname");

            return new StandardExtendedLambda (name, formals, body, required, optional, rest);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode optimizedBody = this.body.Bind (ctenv.Extend (this));
            return this.body == optimizedBody
                ? this
                : new StandardExtendedLambda (this.name, this.formals, optimizedBody, this.required, this.optional, this.rest);
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
            answer = new StandardExtendedClosure (this, environment);
            return false;
        }


        //public override BoundVariable SimulateLookup (object name, LexicalMap parent)
        //{
        //    throw new NotImplementedException ();
        //}

        //public override BoundVariable SimulateStaticLookup (object name, LexicalMap parent, int depth)
        //{
        //    throw new NotImplementedException ();
        //}

        //public override BoundVariable SimulateDangerousLookup (object name, LexicalMap parent, int safeDepth, int depth)
        //{
        //    throw new NotImplementedException ();
        //}


        public override BoundVariable IncreaseLexicalDepth (BoundVariable variable)
        {
            throw new NotImplementedException ();
        }
    }

    /// <summary>
    /// Supports option, rest, and aux args, 
    /// but not incremental definitions.
    /// </summary>
    [Serializable]
    sealed class StaticExtendedLambda : ExtendedLambda
    {
        StaticExtendedLambda (string name, object [] formals, SCode body, uint required, uint optional, bool rest)
            : base (name, formals, body, required, optional, rest)
        {
        }

        public static new StaticExtendedLambda Make (string name, object [] formals, SCode body, uint required, uint optional, bool rest)
        {
            if (body == null)
                throw new ArgumentNullException ("body");
            if (formals == null)
                throw new ArgumentNullException ("formals");
            if (name == null)
                throw new ArgumentNullException ("varname");

            return new StaticExtendedLambda (name, formals, body, required, optional, rest);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode optimizedBody = this.body.Bind (ctenv.Extend (this));
            return this.body == optimizedBody
                ? this
                : new StaticExtendedLambda (this.name, this.formals, optimizedBody, this.required, this.optional, this.rest);
        }

        public override bool CallsTheEnvironment ()
        {
            return false;
        }
 
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            answer = new StaticExtendedClosure (this, environment);
            return false;
        }

        //public override BoundVariable SimulateLookup (object name, LexicalMap parent)
        //{
        //    int offset = LexicalOffset (name);
        //    if (offset == -1)
        //        return parent.SimulateStaticLookup (name, 1);
        //    else
        //        return Argument.Make (name, offset);
        //}

        //public override BoundVariable SimulateStaticLookup (object name, LexicalMap parent, int depth)
        //{
        //    int offset = LexicalOffset (name);
        //    if (offset == -1)
        //        return parent.SimulateStaticLookup (name, depth + 1);
        //    else
        //        return LexicalVariable.Make (name, depth, offset);
        //}

        //public override BoundVariable SimulateDangerousLookup (object name, LexicalMap parent, int safeDepth, int depth)
        //{
        //    int offset = LexicalOffset (name);
        //    if (offset == -1)
        //        return parent.SimulateDangerousLookup (name, safeDepth, depth + 1);
        //    else
        //        return DangerousLexicalVariable.Make (name, safeDepth, depth, offset);
        //}

        public override BoundVariable IncreaseLexicalDepth (BoundVariable variable)
        {
            return variable.IncreaseStaticLexicalDepth ();
        }
    }

    /// <summary>
    /// A StandardLambda creates StandardClosures.  These support first-class
    /// environments that allow incremental definition and variable sharing.
    /// </summary>
    [Serializable]
    sealed class StandardLambda : Lambda
    {
        StandardLambda (string name, object [] formals, SCode body)
            : base (name, formals, body)
        { }

        public static new StandardLambda Make (string name, object [] formals, SCode body)
        {
           return new StandardLambda (name, formals, body);
        }

        public override Closure Close (Environment environment)
        {
            return new StandardClosure (this, environment);
        }

        public override SCode Bind (LexicalMap ctenv)
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

        //public override BoundVariable SimulateLookup (object name, LexicalMap parent)
        //{
        //    throw new NotImplementedException ();
        //}

        //public override BoundVariable SimulateStaticLookup (object name, LexicalMap parent, int depth)
        //{
        //    throw new NotImplementedException ();
        //}

        //public override BoundVariable SimulateDangerousLookup (object name, LexicalMap parent, int safeDepth, int depth)
        //{
        //    throw new NotImplementedException ();
        //}


        public override BoundVariable IncreaseLexicalDepth (BoundVariable variable)
        {
            throw new NotImplementedException ();
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
        protected StaticLambdaBase (string name, object [] formals, SCode body)
            : base (name, formals, body)
        { }

        public override bool CallsTheEnvironment ()
        {
            return false;
        }
        
        //public override BoundVariable SimulateLookup (object name, LexicalMap parent)
        //{
        //    int offset = LexicalOffset (name);
        //    if (offset == -1)
        //        return parent.SimulateStaticLookup (name, 1);
        //    else
        //        return Argument.Make (name, offset);
        //}

        //public override BoundVariable SimulateStaticLookup (object name, LexicalMap parent, int depth)
        //{
        //    int offset = LexicalOffset (name);
        //    if (offset == -1)
        //        return parent.SimulateStaticLookup (name, depth + 1);
        //    else
        //        return LexicalVariable.Make (name, depth, offset);
        //}

        //public override BoundVariable SimulateDangerousLookup (object name, LexicalMap parent, int safeDepth, int depth)
        //{
        //    int offset = LexicalOffset (name);
        //    if (offset == -1)
        //        return parent.SimulateDangerousLookup (name, safeDepth, depth + 1);
        //    else
        //        return DangerousLexicalVariable.Make (name, safeDepth, depth, offset);
        //}


        public override Closure Close (Environment environment)
        {
            throw new NotImplementedException ();
        }

        public override BoundVariable IncreaseLexicalDepth (BoundVariable variable)
        {
            return variable.IncreaseStaticLexicalDepth ();
        }
    }

    /// <summary>
    /// A Static lambda does not support incremental definition, but it does
    /// support assignment and unassigned variables.
    /// </summary>
    [Serializable]
    sealed class StaticLambda : StaticLambdaBase
    {
        internal StaticLambda (string varname, object [] formals, SCode body)
            : base (varname, formals, body)
        {         
        }

        public override Closure Close (Environment environment)
        {
            return new StaticClosure (this, environment);
        }

        public override SCode Bind (LexicalMap ctenv)
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
    sealed class SimpleLambda : StaticLambdaBase
    {
        internal SimpleLambda (string name, object [] formals, SCode body)
            : base (name, formals, body)
        {
        }

        public override Closure Close (Environment environment)
        {
            return new SimpleClosure (this, environment);
        }

        public override SCode Bind (LexicalMap ctenv)
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
    }
}
