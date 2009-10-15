using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    /// <summary>
    /// Base class for lambda expressions.
    /// </summary>
    [Serializable]
    abstract public class LambdaBase : SCode
    {
        static public readonly Symbol InternalLambda = Symbol.Make ("#[internal-lambda]");
        static public readonly Symbol Unnamed = Symbol.Make ("#[unnamed-procedure]");
        static public readonly Symbol Let = Symbol.Make ("#[let-procedure]");
        static public readonly Symbol Dummy = Symbol.Make ("dummy-procedure");

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Symbol lambdaName; 

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Symbol [] lambdaFormals;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected ICollection<Symbol> lambdaFreeVariables;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected SCode lambdaBody;

        protected StaticMapping [] staticMapping;

        // Count of times this lambda is closed over.
        internal long closeCount = 0;
        // Count of times this lambda body is evaluated.
        internal long evaluationCount = 0;

        protected LambdaBase (Symbol name, Symbol [] formals, SCode body, ICollection<Symbol> freeVariables, StaticMapping [] staticMapping)
        {
            this.lambdaName = name;
            this.lambdaFormals = formals;
            this.lambdaBody = body;
            this.lambdaFreeVariables = freeVariables;
            this.staticMapping = staticMapping;
#if DEBUG
            // Paranoia:  check for duplicate names
            if (name != Dummy)
                for (int i = 0; i < formals.Length - 1; i++)
                    if (formals [i] != null)
                        for (int j = i + 1; j < formals.Length; j++)
                            if (formals [i] == formals [j]) 
                                Debugger.Break ();

            // //Check for eta-reducible primitive lambdas.  There seem to be
            // //too many of these.
            // Bug found and fixed.
            //if (body is PrimitiveCombination0 &&
            //    formals.Length == 0 )
            //    Debugger.Break ();

            //if (body is PrimitiveCombination1 &&
            //    formals.Length == 1 &&
            //    ((PrimitiveCombination1) body).Operand is Variable &&
            //    ((Variable) ((PrimitiveCombination1) body).Operand).Name == formals [0])
            //    Debugger.Break ();

            //if (body is PrimitiveCombination2 &&
            //    formals.Length == 2 &&
            //    ((PrimitiveCombination2) body).Operand0 is Variable &&
            //    ((PrimitiveCombination2) body).Operand1 is Variable &&
            //    ((Variable) ((PrimitiveCombination2) body).Operand0).Name == formals [0] &&
            //    ((Variable) ((PrimitiveCombination2) body).Operand1).Name == formals [1]) {
            //    Debugger.Break ();
            //}

            //if (body is PrimitiveCombination3 &&
            //    formals.Length == 3 &&
            //    ((PrimitiveCombination3) body).Operand0 is Variable &&
            //    ((PrimitiveCombination3) body).Operand1 is Variable &&
            //    ((PrimitiveCombination3) body).Operand2 is Variable &&
            //    ((Variable) ((PrimitiveCombination3) body).Operand0).Name == formals [0] &&
            //    ((Variable) ((PrimitiveCombination3) body).Operand1).Name == formals [1] &&
            //    ((Variable) ((PrimitiveCombination3) body).Operand2).Name == formals [2])
            //    Debugger.Break ();
#endif   
        }

        protected LambdaBase (Symbol name, Symbol [] formals, SCode body, ICollection<Symbol> freeVariables)
            : this (name, formals, body, freeVariables, null)
        {
        }

        static protected ICollection<Symbol> ConstructFreeVariables (SCode body, Symbol [] formals)
        {
            HashSet<Symbol> freeVariableSet = new HashSet<Symbol> ();
            body.CollectFreeVariables (freeVariableSet);
            foreach (Symbol formal in formals)
                freeVariableSet.Remove (formal);
            freeVariableSet.TrimExcess ();
            return freeVariableSet;
        }

        protected LambdaBase (Symbol name, Symbol [] formals, SCode body)
            : this (name, formals, body, 
            ConstructFreeVariables (body, formals),
            null)
        {        
        }

        public Symbol Name { [DebuggerStepThrough] get { return this.lambdaName; } }
        public Symbol [] Formals { [DebuggerStepThrough] get { return this.lambdaFormals; } }
        public SCode Body { [DebuggerStepThrough] get { return this.lambdaBody; } }

        public ICollection<Symbol> FreeVariables { [DebuggerStepThrough] get { return this.lambdaFreeVariables; } }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            // bug in library, if no lambdaFreeVariables, null ptr exception
            if (this.lambdaFreeVariables.Count > 0) {
                foreach (Symbol variable in this.lambdaFreeVariables)
                    freeVariableSet.Add (variable);
            }
        }

        public int LexicalOffset (object name)
        {
            for (int i = 0; i < this.lambdaFormals.Length; i++)
                if (name == this.lambdaFormals [i])
                    return i;
            return -1;
            // This way is *slow*
            // return Array.IndexOf (this.lambdaFormals, ratorName);
        }

        public int StaticOffset (object name, Environment environment)
        {
            if (this.staticMapping == null) {
                throw new NotImplementedException ("Static mapping should be set by now");
                //Debugger.Break ();
                //this.staticMapping = environment.GetStaticMapping (this.lambdaFreeVariables);
                //StaticMapping.ValidateStaticMapping (this.staticMapping);
            }
            for (int i = 0; i < this.staticMapping.Length; i++)
                if (name == this.staticMapping [i].name)
                    return i;
            return -1;
        }

        public StaticMapping [] GetStaticMapping (Environment environment)
        {
            if (this.staticMapping == null) {
                throw new NotImplementedException ();
                //this.staticMapping = environment.GetStaticMapping (this.lambdaFreeVariables);
                //StaticMapping.ValidateStaticMapping (this.staticMapping);
            }
            return this.staticMapping;
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            // Should check for shadowing.
            return this.lambdaBody.MutatesAny (formals);
        }
    }

    /// <summary>
    /// Lambda is the abstract base class for Lambda expressions with
    /// fixed argument lists (no optional, rest, or aux).
    /// </summary>
    [Serializable]
    abstract class Lambda : LambdaBase, ISystemPair
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.LAMBDA; } }

        protected Lambda (Symbol name, Symbol [] formals, SCode body)
            : base (name, formals, body)
        {
        }

        protected Lambda (Symbol name, Symbol [] formals, SCode body, ICollection<Symbol> freeVariables)
            : base (name, formals, body, freeVariables)
        {
        }

        protected Lambda (Symbol name, Symbol [] formals, SCode body, ICollection<Symbol> freeVariables, StaticMapping [] staticMapping)
            : base (name, formals, body, freeVariables, staticMapping)
        {
        }

        public static Lambda Make (Symbol name, Symbol [] formals, SCode body)
        {
            if (body == null)
                throw new ArgumentNullException ("body");
            if (formals == null)
                throw new ArgumentNullException ("formals");
            if (name == null)
                throw new ArgumentNullException ("name");
            return
                ((! Configuration.EnableLambdaOptimization) ||
                 (! Configuration.EnableStaticLambda) ||
                 (! Configuration.EnableVariableOptimization) ||
                 (! Configuration.EnableStaticBinding) ||
                  body.CallsTheEnvironment ()) ? (Lambda) new StandardLambda (name, formals, body) :
                body.MutatesAny (formals) ? (Lambda) new StaticLambda (name, formals, body) :
                (Lambda) new SimpleLambda (name, formals, body);
        }

        internal static Lambda Make (Symbol name, Symbol [] formals, SCode body, ICollection<Symbol> freeVariables, StaticMapping [] staticMapping)
        {
            if (body == null)
                throw new ArgumentNullException ("body");
            if (formals == null)
                throw new ArgumentNullException ("formals");
            if (name == null)
                throw new ArgumentNullException ("name");
            return
                ((! Configuration.EnableLambdaOptimization) ||
                 (! Configuration.EnableStaticLambda) ||
                (! Configuration.EnableVariableOptimization) ||
                (! Configuration.EnableStaticBinding) ||
                body.CallsTheEnvironment ()) ? (Lambda) new StandardLambda (name, formals, body, freeVariables, staticMapping) :
                body.MutatesAny (formals) ? (Lambda) new StaticLambda (name, formals, body, freeVariables, staticMapping) :
                (Lambda) new SimpleLambda (name, formals, body, freeVariables, staticMapping);
        }

        public static SCode Make (object name, object formals, object body)
        {
            if (formals == null)
                throw new ArgumentNullException ("formals");
            object [] formalsArray = (object []) formals;
            Symbol [] realFormals = new Symbol [formalsArray.Length];
            for (int i = 0; i < formalsArray.Length; i++) {
                realFormals [i] = (Symbol) formalsArray [i];
            }
            return Make ((Symbol) name, (Symbol []) realFormals, EnsureSCode (body));
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
                //this.lambdaFreeVariables = new List<Symbol> (this.lambdaBody.ComputeFreeVariables ().Except<Symbol> (this.lambdaFormals));
                this.lambdaFreeVariables = ConstructFreeVariables (this.lambdaBody, this.lambdaFormals);
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
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.EXTENDED_LAMBDA; } }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly uint required;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly uint optional;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly bool rest;

        protected ExtendedLambda (Symbol name, Symbol [] formals, 
            SCode body, uint required, uint optional, bool rest)
            : base (name, formals, body)
        {
            this.required = required;
            this.optional = optional;
            this.rest = rest;
        }

        protected ExtendedLambda (Symbol name, Symbol [] formals,
    SCode body, uint required, uint optional, bool rest, ICollection<Symbol> freeVariables)
            : base (name, formals, body, freeVariables)
        {
            this.required = required;
            this.optional = optional;
            this.rest = rest;
        }

        protected ExtendedLambda (Symbol name, Symbol [] formals,
SCode body, uint required, uint optional, bool rest, ICollection<Symbol> freeVariables, StaticMapping [] mapping)
            : base (name, formals, body, freeVariables, mapping)
        {
            this.required = required;
            this.optional = optional;
            this.rest = rest;
        }

        public static LambdaBase Make (Symbol name, Symbol [] formals, SCode body, uint required, uint optional, bool rest)
        {
            if (body == null)
                throw new ArgumentNullException ("body");
            if (formals == null)
                throw new ArgumentNullException ("formals");
            if (name == null)
                throw new ArgumentNullException ("name");

            return
                // If no optional, rest, or aux arguments, just make a regular lambda.
                (required == formals.Length &&
                optional == 0 &&
                rest == false) ? (LambdaBase) Lambda.Make (name, formals, body) :
                (LambdaBase) StandardExtendedLambda.Make (name, formals, body, required, optional, rest);
                //body.CallsTheEnvironment () ? (LambdaBase) StandardExtendedLambda.Make (name, formals, body, required, optional, rest) :
                //(LambdaBase) StaticExtendedLambda.Make (name, formals, body, required, optional, rest);
        }

        public static SCode Make (Hunk3 init)
        {
            object [] cdrArray = (object []) init.Cxr1;
            Symbol name = (Symbol) cdrArray [0];
            Symbol [] formals = new Symbol [cdrArray.Length - 1];
            for (int i = 0; i < formals.Length; i++)
                formals [i] = (Symbol) cdrArray [i + 1];
            uint code = (uint) (int) (init.Cxr2);
            SCode body = EnsureSCode (init.Cxr0);
            return Make (name, formals,
                body, (code >> 8) & 0xFF, code & 0xFF, ((code >> 16) & 0x1) == 0x1);
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
    }

    /// <summary>
    /// Concrete class that supports option, rest, and aux args, 
    /// and incremental definitions.
    /// </summary>
    [Serializable]
    sealed class StandardExtendedLambda : ExtendedLambda, ISerializable
    {
        StandardExtendedLambda (Symbol name, Symbol [] formals, SCode body, uint required, uint optional, bool rest)
            : base (name, formals, body, required, optional, rest)
        {
        }

        internal StandardExtendedLambda (Symbol name, Symbol [] formals, SCode body, uint required, uint optional, bool rest, ICollection<Symbol> freeVariables)
            : base (name, formals, body, required, optional, rest, freeVariables)
        {
        }

        internal StandardExtendedLambda (Symbol name, Symbol [] formals, SCode body, uint required, uint optional, bool rest, ICollection<Symbol> freeVariables, StaticMapping []  staticMapping)
            : base (name, formals, body, required, optional, rest, freeVariables, staticMapping)
        {
        }
        internal static new StandardExtendedLambda Make (Symbol name, Symbol [] formals, SCode body, uint required, uint optional, bool rest)
        {
            if (body == null)
                throw new ArgumentNullException ("body");
            if (formals == null)
                throw new ArgumentNullException ("formals");
            if (name == null)
                throw new ArgumentNullException ("name");

            return new StandardExtendedLambda (name, formals, body, required, optional, rest);
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
            this.closeCount += 1;
            answer = new StandardExtendedClosure (this, environment);
            return false;
        }

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialClosure<StandardExtendedLambda> closure = new PartialClosure<StandardExtendedLambda> (this, environment);

            SCode pbody = this.lambdaBody.PartialEval (new PartialStandardExtendedEnvironment (closure)).Residual;

            return new PartialResult (new StandardExtendedLambda (this.lambdaName, this.lambdaFormals, pbody,
                this.required, this.optional, this.rest,
                this.lambdaFreeVariables, closure.StaticMapping));
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public void GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (StandardExtendedLambdaDeserializer));
            // We specially handle the names and formals
            // in order to ensure that they are available when
            // we deserialize the lambda.
            if (this.Name.IsInterned ()) {
                info.AddValue ("name_interned", true);
                info.AddValue ("name", this.Name.ToString ());
            }
            else {
                info.AddValue ("name_interned", false);
                info.AddValue ("name", this.Name);
            }
            info.AddValue ("formalCount", this.Formals.Length);
            for (int i = 0; i < this.Formals.Length; i++) {
                if (this.Formals [i].IsInterned ()) {
                    info.AddValue ("arg_" + i + "_interned", true);
                    info.AddValue ("arg_" + i + "_name", this.Formals [i].ToString ());
                }
                else {
                    info.AddValue ("arg_" + i + "_interned", false);
                    info.AddValue ("arg_" + i, this.Formals [i]);
                }
            }
            info.AddValue ("required", this.required);
            info.AddValue ("optional", this.optional);
            info.AddValue ("rest", this.rest);
            info.AddValue ("freeVariableCount", this.lambdaFreeVariables.Count);
            int index = 0;
            if (this.lambdaFreeVariables.Count > 0) {
                foreach (Symbol freeVariable in this.lambdaFreeVariables) {
                    if (freeVariable.IsInterned ()) {
                        info.AddValue ("fv_" + index + "_interned", true);
                        info.AddValue ("fv_" + index + "_name", freeVariable.ToString ());
                    }
                    else {
                        info.AddValue ("fv_" + index + "_interned", false);
                        info.AddValue ("fv_" + index, freeVariable);
                    }
                    index += 1;
                }
            }
            info.AddValue ("body", this.Body);
        }

        #endregion
    }

    [Serializable]
    sealed class StandardExtendedLambdaDeserializer : ISerializable, IObjectReference
    {
        [NonSerialized]
        Symbol name;

        [NonSerialized]
        Symbol [] formals;

        [NonSerialized]
        HashSet<Symbol> freeVariables;

        [NonSerialized]
        SCode body;

        [NonSerialized]
        uint required;
        [NonSerialized]
        uint optional;
        [NonSerialized]
        bool rest;

        [NonSerialized]
        StandardExtendedLambda realObject;

        StandardExtendedLambdaDeserializer (SerializationInfo info, StreamingContext context)
        {
            this.name = info.GetBoolean ("name_interned") ?
                        Symbol.Make (info.GetString ("name")) :
                        (Symbol) info.GetValue ("name", typeof (Symbol));
            uint formalCount = info.GetUInt32 ("formalCount");
            this.formals = new Symbol [formalCount];
            for (int i = 0; i < formalCount; i++) {
                formals [i] = info.GetBoolean ("arg_" + i + "_interned") ?
                    Symbol.Make (info.GetString ("arg_" + i + "_name")) :
                    (Symbol) info.GetValue ("arg_" + i, typeof (Symbol));
            }
            this.required = info.GetUInt32 ("required");
            this.optional = info.GetUInt32 ("optional");
            this.rest = info.GetBoolean ("rest");
            uint freeVariableCount = info.GetUInt32 ("freeVariableCount");
            this.freeVariables = new HashSet<Symbol> ();
            for (int i = 0; i < freeVariableCount; i++) {
                freeVariables.Add (info.GetBoolean ("fv_" + i + "_interned") ?
                    Symbol.Make (info.GetString ("fv_" + i + "_name")) :
                    (Symbol) info.GetValue ("fv_" + i, typeof (Symbol)));
            }
            this.body = (SCode) info.GetValue ("body", typeof (SCode));
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            throw new NotImplementedException ();
        }
        #endregion

        #region IObjectReference Members

        public object GetRealObject (StreamingContext context)
        {
            if (this.realObject == null)
                this.realObject = new StandardExtendedLambda (this.name, this.formals,
                    this.body, this.required, this.optional, this.rest, this.freeVariables);
            return this.realObject;
        }
        #endregion
    }

    /// <summary>
    /// A StandardLambda creates StandardClosures.  These support first-class
    /// environments that allow incremental definition and variable sharing.
    /// </summary>
    [Serializable]
    sealed class StandardLambda : Lambda, ISerializable
    {
        public StandardLambda (Symbol name, Symbol [] formals, SCode body)
            : base (name, formals, body)
        {
        }

        public StandardLambda (Symbol name, Symbol [] formals, SCode body, ICollection<Symbol> freeVariables)
            : base (name, formals, body, freeVariables)
        {
        }

        public StandardLambda (Symbol name, Symbol [] formals, SCode body, ICollection<Symbol> freeVariables, StaticMapping [] staticMapping)
            : base (name, formals, body, freeVariables, staticMapping)
        {
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
            this.closeCount += 1;
            answer = new StandardClosure (this, environment);
            return false;
        }

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialClosure<StandardLambda> closure = new PartialClosure<StandardLambda> (this, environment);

            SCode pbody = this.lambdaBody.PartialEval (new PartialStandardEnvironment (closure)).Residual;

            return new PartialResult (StandardLambda.Make (this.lambdaName, this.lambdaFormals, pbody, this.lambdaFreeVariables, closure.StaticMapping));
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (StandardLambdaDeserializer));
            // We specially handle the names and formals
            // in order to ensure that they are available when
            // we deserialize the lambda.
            if (this.Name.IsInterned ()) {
                info.AddValue ("name_interned", true);
                info.AddValue ("name", this.Name.ToString ());
            }
            else {
                info.AddValue ("name_interned", false);
                info.AddValue ("name", this.Name);
            }
            info.AddValue ("formalCount", this.Formals.Length);
            for (int i = 0; i < this.Formals.Length; i++) {
                if (this.Formals [i].IsInterned ()) {
                    info.AddValue ("arg_" + i + "_interned", true);
                    info.AddValue ("arg_" + i + "_name", this.Formals [i].ToString ());
                }
                else {
                    info.AddValue ("arg_" + i + "_interned", false);
                    info.AddValue ("arg_" + i, this.Formals [i]);
                }
            }
            info.AddValue ("freeVariableCount", this.lambdaFreeVariables.Count);
            int index = 0;
            if (this.lambdaFreeVariables.Count > 0) {
                foreach (Symbol freeVariable in this.lambdaFreeVariables) {
                    if (freeVariable.IsInterned ()) {
                        info.AddValue ("fv_" + index + "_interned", true);
                        info.AddValue ("fv_" + index + "_name", freeVariable.ToString ());
                    }
                    else {
                        info.AddValue ("fv_" + index + "_interned", false);
                        info.AddValue ("fv_" + index, freeVariable);
                    }
                    index += 1;
                }
            }
            info.AddValue ("body", this.Body);
        }

        #endregion
    }

    [Serializable]
    sealed class StandardLambdaDeserializer : ISerializable, IObjectReference
    {
        [NonSerialized]
        Symbol name;

        [NonSerialized]
        Symbol [] formals;

        [NonSerialized]
        SCode body;

        [NonSerialized]
        HashSet<Symbol> freeVariables;

        [NonSerialized]
        StandardLambda realObject;

        StandardLambdaDeserializer (SerializationInfo info, StreamingContext context)
        {
            this.name = info.GetBoolean ("name_interned") ?
                Symbol.Make (info.GetString ("name")) :
                (Symbol) info.GetValue ("name", typeof (Symbol));
            uint formalCount = info.GetUInt32 ("formalCount");
            this.formals = new Symbol [formalCount];
            for (int i = 0; i < formalCount; i++) {
                formals [i] = info.GetBoolean ("arg_" + i + "_interned") ?
                    Symbol.Make (info.GetString ("arg_" + i + "_name")) :
                    (Symbol) info.GetValue ("arg_" + i, typeof (Symbol));
            }
            uint freeVariableCount = info.GetUInt32 ("freeVariableCount");
            this.freeVariables = new HashSet<Symbol> ();
            for (int i = 0; i < freeVariableCount; i++) {
                freeVariables.Add (info.GetBoolean ("fv_" + i + "_interned") ?
                    Symbol.Make (info.GetString ("fv_" + i + "_name")) :
                    (Symbol) info.GetValue ("fv_" + i, typeof (Symbol)));
            }
            this.body = (SCode) info.GetValue ("body", typeof (SCode));
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            throw new NotImplementedException ();
        }
        #endregion

        #region IObjectReference Members

        public object GetRealObject (StreamingContext context)
        {
            if (this.realObject == null)
                this.realObject = new StandardLambda (this.name, this.formals,
                    this.body, this.freeVariables);
            return this.realObject;
        }
        #endregion
    }

    // Base class for lambdas that do not support incremental variables.
    abstract class StaticLambdaBase : Lambda
    {
        protected StaticLambdaBase (Symbol name, Symbol [] formals, SCode body)
            : base (name, formals, body)
        {
        }

        protected StaticLambdaBase (Symbol name, Symbol [] formals, SCode body, ICollection<Symbol> freeVariables)
            : base (name, formals, body, freeVariables)
        {
        }

        protected StaticLambdaBase (Symbol name, Symbol [] formals, SCode body, ICollection<Symbol> freeVariables, StaticMapping [] staticMapping)
            : base (name, formals, body, freeVariables, staticMapping)
        {
        }

        public override bool CallsTheEnvironment ()
        {
            return false;
        }

    }

    /// <summary>
    /// A StaticLambda creates StaticClosures.  These support first-class
    /// environments that allow incremental definition and variable sharing.
    /// </summary>
    [Serializable]
    sealed class StaticLambda : StaticLambdaBase, ISerializable
    {
        public StaticLambda (Symbol name, Symbol [] formals, SCode body)
            : base (name, formals, body)
        {
        }

        public StaticLambda (Symbol name, Symbol [] formals, SCode body, ICollection<Symbol> freeVariables)
            : base (name, formals, body, freeVariables)
        {
        }

        public StaticLambda (Symbol name, Symbol [] formals, SCode body, ICollection<Symbol> freeVariables, StaticMapping [] staticMapping)
            : base (name, formals, body, freeVariables, staticMapping)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("StaticLambda.EvalStep");
#endif
            this.closeCount += 1;
            answer = new StaticClosure (this, environment.BaseEnvironment, environment.GetValueCells (this.staticMapping));
            return false;
        }

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialClosure<StaticLambda> closure = new PartialClosure<StaticLambda> (this, environment);

            SCode pbody = this.lambdaBody.PartialEval (new PartialStaticEnvironment (closure)).Residual;

            return new PartialResult (StaticLambda.Make (this.lambdaName, this.lambdaFormals, pbody, this.lambdaFreeVariables, closure.StaticMapping));
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (StaticLambdaDeserializer));
            // We specially handle the names and formals
            // in order to ensure that they are available when
            // we deserialize the lambda.
            if (this.Name.IsInterned ()) {
                info.AddValue ("name_interned", true);
                info.AddValue ("name", this.Name.ToString ());
            }
            else {
                info.AddValue ("name_interned", false);
                info.AddValue ("name", this.Name);
            }
            info.AddValue ("formalCount", this.Formals.Length);
            for (int i = 0; i < this.Formals.Length; i++) {
                if (this.Formals [i].IsInterned ()) {
                    info.AddValue ("arg_" + i + "_interned", true);
                    info.AddValue ("arg_" + i + "_name", this.Formals [i].ToString ());
                }
                else {
                    info.AddValue ("arg_" + i + "_interned", false);
                    info.AddValue ("arg_" + i, this.Formals [i]);
                }
            }
            info.AddValue ("freeVariableCount", this.lambdaFreeVariables.Count);
            int index = 0;
            if (this.lambdaFreeVariables.Count > 0) {
                foreach (Symbol freeVariable in this.lambdaFreeVariables) {
                    if (freeVariable.IsInterned ()) {
                        info.AddValue ("fv_" + index + "_interned", true);
                        info.AddValue ("fv_" + index + "_name", freeVariable.ToString ());
                    }
                    else {
                        info.AddValue ("fv_" + index + "_interned", false);
                        info.AddValue ("fv_" + index, freeVariable);
                    }
                    index += 1;
                }
            }
            info.AddValue ("body", this.Body);
        }

        #endregion
    }

    [Serializable]
    sealed class StaticLambdaDeserializer : ISerializable, IObjectReference
    {
        [NonSerialized]
        Symbol name;

        [NonSerialized]
        Symbol [] formals;

        [NonSerialized]
        SCode body;

        [NonSerialized]
        HashSet<Symbol> freeVariables;

        [NonSerialized]
        StaticLambda realObject;

        StaticLambdaDeserializer (SerializationInfo info, StreamingContext context)
        {
            this.name = info.GetBoolean ("name_interned") ?
                Symbol.Make (info.GetString ("name")) :
                (Symbol) info.GetValue ("name", typeof (Symbol));
            uint formalCount = info.GetUInt32 ("formalCount");
            this.formals = new Symbol [formalCount];
            for (int i = 0; i < formalCount; i++) {
                formals [i] = info.GetBoolean ("arg_" + i + "_interned") ?
                    Symbol.Make (info.GetString ("arg_" + i + "_name")) :
                    (Symbol) info.GetValue ("arg_" + i, typeof (Symbol));
            }
            uint freeVariableCount = info.GetUInt32 ("freeVariableCount");
            this.freeVariables = new HashSet<Symbol> ();
            for (int i = 0; i < freeVariableCount; i++) {
                freeVariables.Add (info.GetBoolean ("fv_" + i + "_interned") ?
                    Symbol.Make (info.GetString ("fv_" + i + "_name")) :
                    (Symbol) info.GetValue ("fv_" + i, typeof (Symbol)));
            }
            this.body = (SCode) info.GetValue ("body", typeof (SCode));
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            throw new NotImplementedException ();
        }
        #endregion

        #region IObjectReference Members

        public object GetRealObject (StreamingContext context)
        {
            if (this.realObject == null)
                this.realObject = new StaticLambda (this.name, this.formals,
                    this.body, this.freeVariables);
            return this.realObject;
        }
        #endregion
    }

    /// <summary>
    /// A SimpleLambda creates SimpleClosures.  These are the lightest weight
    /// closures.
    /// </summary>
    [Serializable]
    sealed class SimpleLambda : StaticLambdaBase, ISerializable
    {
        public SimpleLambda (Symbol name, Symbol [] formals, SCode body)
            : base (name, formals, body)
        {
        }

        public SimpleLambda (Symbol name, Symbol [] formals, SCode body, ICollection<Symbol> freeVariables)
            : base (name, formals, body, freeVariables)
        {
        }

        public SimpleLambda (Symbol name, Symbol [] formals, SCode body, ICollection<Symbol> freeVariables, StaticMapping [] staticMapping)
            : base (name, formals, body, freeVariables, staticMapping)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLambda.EvalStep");
            if (this.staticMapping == null)
                throw new NotImplementedException("Static mapping should not be null.");
#endif
            this.closeCount += 1;
            // Use the base environment for lookup.
            answer = new SimpleClosure (this, environment.BaseEnvironment, environment.GetValueCells (this.staticMapping));
            return false;
        }

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialClosure<SimpleLambda> closure = new PartialClosure<SimpleLambda> (this, environment);

            SCode pbody = this.lambdaBody.PartialEval (new PartialSimpleEnvironment (closure)).Residual;

            return new PartialResult (Lambda.Make (this.lambdaName, this.lambdaFormals, pbody, this.lambdaFreeVariables, closure.StaticMapping));
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (SimpleLambdaDeserializer));
            // We specially handle the names and formals
            // in order to ensure that they are available when
            // we deserialize the lambda.
            if (this.Name.IsInterned ()) {
                info.AddValue ("name_interned", true);
                info.AddValue ("name", this.Name.ToString ());
            }
            else {
                info.AddValue ("name_interned", false);
                info.AddValue ("name", this.Name);
            }
            info.AddValue ("formalCount", this.Formals.Length);
            for (int i = 0; i < this.Formals.Length; i++) {
                if (this.Formals [i].IsInterned ()) {
                    info.AddValue ("arg_" + i + "_interned", true);
                    info.AddValue ("arg_" + i + "_name", this.Formals [i].ToString ());
                }
                else {
                    info.AddValue ("arg_" + i + "_interned", false);
                    info.AddValue ("arg_" + i, this.Formals [i]);
                }
            }
            info.AddValue ("freeVariableCount", this.lambdaFreeVariables.Count);
            int index = 0;
            if (this.lambdaFreeVariables.Count > 0) {
                foreach (Symbol freeVariable in this.lambdaFreeVariables) {
                    if (freeVariable.IsInterned ()) {
                        info.AddValue ("fv_" + index + "_interned", true);
                        info.AddValue ("fv_" + index + "_name", freeVariable.ToString ());
                    }
                    else {
                        info.AddValue ("fv_" + index + "_interned", false);
                        info.AddValue ("fv_" + index, freeVariable);
                    }
                    index += 1;
                }
            }
            info.AddValue ("body", this.Body);
        }

        #endregion
    }

    [Serializable]
    sealed class SimpleLambdaDeserializer : ISerializable, IObjectReference
    {
        [NonSerialized]
        Symbol name;

        [NonSerialized]
        Symbol [] formals;

        [NonSerialized]
        SCode body;

        [NonSerialized]
        HashSet <Symbol> freeVariables;

        [NonSerialized]
        SimpleLambda realObject;

        SimpleLambdaDeserializer (SerializationInfo info, StreamingContext context)
        {
            this.name = info.GetBoolean ("name_interned") ?
                Symbol.Make (info.GetString ("name")) :
                (Symbol) info.GetValue ("name", typeof (Symbol));
            uint formalCount = info.GetUInt32 ("formalCount");
            this.formals = new Symbol [formalCount];
            for (int i = 0; i < formalCount; i++) {
                formals [i] = info.GetBoolean ("arg_" + i + "_interned") ?
                    Symbol.Make (info.GetString ("arg_" + i + "_name")) :
                    (Symbol) info.GetValue ("arg_" + i, typeof (Symbol));
            }
            uint freeVariableCount = info.GetUInt32 ("freeVariableCount");
            this.freeVariables = new HashSet<Symbol> ();
            for (int i = 0; i < freeVariableCount; i++) {
                freeVariables.Add (info.GetBoolean ("fv_" + i + "_interned") ?
                    Symbol.Make (info.GetString ("fv_" + i + "_name")) :
                    (Symbol) info.GetValue ("fv_" + i, typeof (Symbol)));
            }
            this.body = (SCode) info.GetValue ("body", typeof (SCode));
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            throw new NotImplementedException ();
        }
        #endregion

        #region IObjectReference Members

        public object GetRealObject (StreamingContext context)
        {
            if (this.realObject == null)
                this.realObject = new SimpleLambda (this.name, this.formals,
                    this.body, this.freeVariables);
            return this.realObject;
        }
        #endregion
    }
}
