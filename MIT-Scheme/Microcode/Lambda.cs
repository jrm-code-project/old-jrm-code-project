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
    abstract public class LambdaBase : SCode
    {
        static public readonly string internalLambda = String.Intern ("#[internal-lambda]");
        static public readonly string unnamed = String.Intern ("#[unnamed-procedure]");
        static public readonly string let = String.Intern ("#[let-procedure]");

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly string lambdaName; 

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly object [] lambdaFormals;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected SCode lambdaBody;     
 
        protected LambdaBase (TC typeCode, string name, object [] formals, SCode body)
            : base (typeCode)
        {
            this.lambdaName = name;
            this.lambdaFormals = formals;
            this.lambdaBody = body;
            // Paranoia:  check for duplicate names
            if (name != "dummy-procedure") {
                for (int i = 0; i < formals.Length - 1; i++)
                    for (int j = i + 1; j < formals.Length; j++)
                        if (formals [i] == formals [j]) Debugger.Break ();
            }
        }

        public string Name { [DebuggerStepThrough] get { return this.lambdaName; } }
        public object [] Formals { [DebuggerStepThrough] get { return this.lambdaFormals; } }
        public SCode Body { [DebuggerStepThrough] get { return this.lambdaBody; } }

        public int LexicalOffset (object name)
        {
            // This way is *slow*
            // return Array.IndexOf (this.lambdaFormals, varname);
            for (int i = 0; i < this.lambdaFormals.Length; i++)
                if (name == this.lambdaFormals [i])
                    return i;
            return -1;
        }

        public override bool MutatesAny (object [] formals)
        {
            // Should check for shadowing.
            return this.lambdaBody.MutatesAny (formals);
        }

        public abstract BoundVariable IncreaseLexicalDepth (BoundVariable variable);

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

        public override SCode Substitute (object name, object newObject)
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
                throw new ArgumentNullException ("lambdaBody");
            if (formals == null)
                throw new ArgumentNullException ("lambdaFormals");
            if (name == null)
                throw new ArgumentNullException ("varname");

                    // If someone in the lambdaBody calls `the-environment', then we
                    // *must* create a first-class enabled environment.   
            return 
                (body.CallsTheEnvironment () || ! Configuration.EnableStaticBinding) ? (Lambda) StandardLambda.Make (name, formals, body)
                : (formals.Length == 0 || body.MutatesAny(formals) || ! Configuration.EnableSimpleLambda) ? (Lambda) new StaticLambda (name, formals, body)
                : (Lambda) new SimpleLambda (name, formals, body);
        }

        public static SCode Make (object name, object formals, object body)
        {

            if (formals == null)
                throw new ArgumentNullException ("lambdaFormals");

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

        [SchemePrimitive ("LEXPR?", 1, true)]
        public static bool IsLexpr (out object answer, object arg)
        {
            answer = Constant.sharpF;
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
                return UnwrapQuoted (this.lambdaBody);
            }

            [DebuggerStepThrough]
            set
            {
                // ugh!  but the advice mechanism uses it.
                this.lambdaBody = EnsureSCode (value);
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCdr
        {
            //[DebuggerStepThrough]
            get
            {
                object [] fakeFormals = new object [this.lambdaFormals.Length + 1];
                fakeFormals [0] = this.lambdaName;
                Array.Copy (this.lambdaFormals, 0, fakeFormals, 1, this.lambdaFormals.Length);
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
                throw new ArgumentNullException ("lambdaBody");
            if (formals == null)
                throw new ArgumentNullException ("lambdaFormals");
            if (name == null)
                throw new ArgumentNullException ("varname");

            return 
                // If no optional, rest, or aux arguments, just make a regular lambda.
                (required == formals.Length && optional == 0 && rest == false) ? Lambda.Make (name, formals, body)
                // If someone in the lambdaBody calls `the-environment', then we
                // *must* create a first-class enabled environment.   

                : (body.CallsTheEnvironment () || ! Configuration.EnableStaticBinding) ? (ExtendedLambda) StandardExtendedLambda.Make (name, formals, body, required, optional, rest)
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
                return UnwrapQuoted (this.lambdaBody);
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
                object [] fakeFormals = new object [this.lambdaFormals.Length + 1];
                fakeFormals [0] = this.lambdaName;
                Array.Copy (this.lambdaFormals, 0, fakeFormals, 1, this.lambdaFormals.Length);
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


        //public override BoundVariable SimulateLookup (object lambdaName, LexicalMap parent)
        //{
        //    int argOffset = LexicalOffset (lambdaName);
        //    if (argOffset == -1)
        //        return parent.SimulateDangerousLookup (lambdaName, 0, 0);
        //    else
        //        return Argument.Make (lambdaName, argOffset);
        //}

        //public override BoundVariable SimulateStaticLookup (object lambdaName, LexicalMap parent, int argDepth)
        //{
        //    int argOffset = LexicalOffset (lambdaName);
        //    if (argOffset == -1)
        //        return parent.SimulateDangerousLookup (lambdaName, argDepth, 0);
        //    else
        //        return LexicalVariable.Make (lambdaName, argDepth, argOffset);
        //}

        //public override BoundVariable SimulateDangerousLookup (object lambdaName, LexicalMap parent, int safeDepth, int argDepth)
        //{
        //    int argOffset = LexicalOffset (lambdaName);
        //    if (argOffset == -1)
        //        return parent.SimulateDangerousLookup (lambdaName, safeDepth, argDepth + 1);
        //    else
        //        return DangerousLexicalVariable.Make (lambdaName, safeDepth, argDepth, argOffset);
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
                throw new ArgumentNullException ("lambdaBody");
            if (formals == null)
                throw new ArgumentNullException ("lambdaFormals");
            if (name == null)
                throw new ArgumentNullException ("varname");

            return new StandardExtendedLambda (name, formals, body, required, optional, rest);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode optimizedBody = this.lambdaBody.Bind (ctenv.Extend (this));
            return this.lambdaBody == optimizedBody
                ? this
                : ExtendedLambda.Make (this.lambdaName, this.lambdaFormals, optimizedBody, this.required, this.optional, this.rest);
        }

        public override bool CallsTheEnvironment ()
        {
            return true;
        }


        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("StandardExtendedLambda.EvalStep");
#endif
            answer = new StandardExtendedClosure (this, environment);
            return false;
        }


        //public override BoundVariable SimulateLookup (object lambdaName, LexicalMap parent)
        //{
        //    throw new NotImplementedException ();
        //}

        //public override BoundVariable SimulateStaticLookup (object lambdaName, LexicalMap parent, int argDepth)
        //{
        //    throw new NotImplementedException ();
        //}

        //public override BoundVariable SimulateDangerousLookup (object lambdaName, LexicalMap parent, int safeDepth, int argDepth)
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
                throw new ArgumentNullException ("lambdaBody");
            if (formals == null)
                throw new ArgumentNullException ("lambdaFormals");
            if (name == null)
                throw new ArgumentNullException ("varname");

            return new StaticExtendedLambda (name, formals, body, required, optional, rest);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode optimizedBody = this.lambdaBody.Bind (ctenv.Extend (this));
            return this.lambdaBody == optimizedBody
                ? this
                : StaticExtendedLambda.Make (this.lambdaName, this.lambdaFormals, optimizedBody, this.required, this.optional, this.rest);
        }

        public override bool CallsTheEnvironment ()
        {
            return false;
        }
 
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("StaticExtendedLambda.EvalStep");
#endif
            answer = new StaticExtendedClosure (this, environment);
            return false;
        }

        //public override BoundVariable SimulateLookup (object lambdaName, LexicalMap parent)
        //{
        //    int argOffset = LexicalOffset (lambdaName);
        //    if (argOffset == -1)
        //        return parent.SimulateStaticLookup (lambdaName, 1);
        //    else
        //        return Argument.Make (lambdaName, argOffset);
        //}

        //public override BoundVariable SimulateStaticLookup (object lambdaName, LexicalMap parent, int argDepth)
        //{
        //    int argOffset = LexicalOffset (lambdaName);
        //    if (argOffset == -1)
        //        return parent.SimulateStaticLookup (lambdaName, argDepth + 1);
        //    else
        //        return LexicalVariable.Make (lambdaName, argDepth, argOffset);
        //}

        //public override BoundVariable SimulateDangerousLookup (object lambdaName, LexicalMap parent, int safeDepth, int argDepth)
        //{
        //    int argOffset = LexicalOffset (lambdaName);
        //    if (argOffset == -1)
        //        return parent.SimulateDangerousLookup (lambdaName, safeDepth, argDepth + 1);
        //    else
        //        return DangerousLexicalVariable.Make (lambdaName, safeDepth, argDepth, argOffset);
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
            SCode optimizedBody = this.lambdaBody.Bind (ctenv.Extend (this));
            return this.lambdaBody == optimizedBody
                ? this
                : new StandardLambda (this.lambdaName, this.lambdaFormals, optimizedBody);
        }

        public override bool CallsTheEnvironment ()
        {
            return true;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("StandardLambda.EvalStep");
#endif
            answer = new StandardClosure (this, environment);
            return false;
        }

        //public override BoundVariable SimulateLookup (object lambdaName, LexicalMap parent)
        //{
        //    throw new NotImplementedException ();
        //}

        //public override BoundVariable SimulateStaticLookup (object lambdaName, LexicalMap parent, int argDepth)
        //{
        //    throw new NotImplementedException ();
        //}

        //public override BoundVariable SimulateDangerousLookup (object lambdaName, LexicalMap parent, int safeDepth, int argDepth)
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
        
        //public override BoundVariable SimulateLookup (object lambdaName, LexicalMap parent)
        //{
        //    int argOffset = LexicalOffset (lambdaName);
        //    if (argOffset == -1)
        //        return parent.SimulateStaticLookup (lambdaName, 1);
        //    else
        //        return Argument.Make (lambdaName, argOffset);
        //}

        //public override BoundVariable SimulateStaticLookup (object lambdaName, LexicalMap parent, int argDepth)
        //{
        //    int argOffset = LexicalOffset (lambdaName);
        //    if (argOffset == -1)
        //        return parent.SimulateStaticLookup (lambdaName, argDepth + 1);
        //    else
        //        return LexicalVariable.Make (lambdaName, argDepth, argOffset);
        //}

        //public override BoundVariable SimulateDangerousLookup (object lambdaName, LexicalMap parent, int safeDepth, int argDepth)
        //{
        //    int argOffset = LexicalOffset (lambdaName);
        //    if (argOffset == -1)
        //        return parent.SimulateDangerousLookup (lambdaName, safeDepth, argDepth + 1);
        //    else
        //        return DangerousLexicalVariable.Make (lambdaName, safeDepth, argDepth, argOffset);
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
            SCode optimizedBody = this.lambdaBody.Bind (ctenv.Extend (this));
            return this.lambdaBody == optimizedBody
                ? this
                : new StaticLambda (this.lambdaName, this.lambdaFormals, optimizedBody);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("StaticLambda.EvalStep");
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
            SCode optimizedBody = this.lambdaBody.Bind (ctenv.Extend (this));
            return this.lambdaBody == optimizedBody
                ? this
                : new SimpleLambda (this.lambdaName, this.lambdaFormals, optimizedBody);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLambda.EvalStep");
#endif
            answer = new SimpleClosure (this, environment);
            return false;
        }
    }
}
