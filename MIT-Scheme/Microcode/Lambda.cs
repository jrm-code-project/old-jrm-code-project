using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics; 
using System.Linq;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    /// <summary>
    /// Base class for lambda expressions.
    /// </summary>
    [Serializable]
    abstract class LambdaBase : SCode
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
        protected ICollection<Symbol> freeVariables;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected SCode lambdaBody;

        protected object [] lambdaStatics;
        protected int [] staticMapping;

        public readonly int internalLambdaCount;

        // Count of times this lambda is closed over.
        internal long closeCount = 0;
        // Count of times this lambda body is evaluated.
        internal long evaluationCount = 0;

        protected LambdaBase (Symbol name, Symbol [] formals, SCode body)
        {
            this.lambdaName = name;
            this.lambdaFormals = formals;
            this.lambdaBody = body;
            this.freeVariables = new List<Symbol> (body.ComputeFreeVariables ().Except<Symbol> (formals));
            this.internalLambdaCount = body.LambdaCount ();

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

        public Symbol Name { [DebuggerStepThrough] get { return this.lambdaName; } }
        public Symbol [] Formals { [DebuggerStepThrough] get { return this.lambdaFormals; } }
        public SCode Body { [DebuggerStepThrough] get { return this.lambdaBody; } }

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
            if (this.lambdaStatics == null)
                this.lambdaStatics = environment.GetStaticMappingNames (this.freeVariables);
            for (int i = 0; i < this.lambdaStatics.Length; i++)
                if (name == this.lambdaStatics [i])
                    return i;
            return -1;
        }

        public int [] GetStaticMapping (Environment environment)
        {
            if (this.staticMapping == null)
                this.staticMapping = environment.GetStaticMapping (this.freeVariables);
            return this.staticMapping;
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            // Should check for shadowing.
            return this.lambdaBody.MutatesAny (formals);
        }

        public override ICollection<Symbol> ComputeFreeVariables ()
        {
            return this.freeVariables;
        }

        public PartialClosure PartialClose (Environment environment, int [] mapping)
        {
            if (this.staticMapping != null)
                throw new NotImplementedException ();
            this.staticMapping = mapping;
            return new PartialClosure (this, environment);
        }

        public override int LambdaCount ()
        {
            return 1;
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

        public static Lambda Make (Symbol name, Symbol [] formals, SCode body)
        {
            if (body == null)
                throw new ArgumentNullException ("body");
            if (formals == null)
                throw new ArgumentNullException ("formals");
            if (name == null)
                throw new ArgumentNullException ("name");
            return
                new StandardLambda (name, formals, body);
                //body.CallsTheEnvironment () ? (Lambda) new StandardLambda (name, formals, body) :
                //(Lambda) new StaticLambda (name, formals, body);
                //body.MutatesAny (formals) ? (Lambda) new StaticLambda (name, formals, body) :
                //(Lambda) new SimpleLambda (name, formals, body);
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
                this.freeVariables = new List<Symbol> (this.lambdaBody.ComputeFreeVariables ().Except<Symbol> (this.lambdaFormals));
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

        internal override PartialResult PartialEval (Environment environment)
        {
            PartialResult pbody = this.lambdaBody.PartialEval (environment.PartialExtend (this));
            return new PartialResult (ExtendedLambda.Make (this.lambdaName, this.lambdaFormals, pbody.Residual, this.required, this.optional, this.rest));
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

        internal StandardExtendedClosure Close (Environment environment)
        {
            this.closeCount += 1;
            return new StandardExtendedClosure (this, environment);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("StandardExtendedLambda.EvalStep");
#endif
            answer = environment.CloseOver (this);
            return false;
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
                this.realObject = StandardExtendedLambda.Make (this.name, this.formals,
                    this.body, this.required, this.optional, this.rest);
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
        internal StandardLambda (Symbol name, Symbol [] formals, SCode body)
            : base (name, formals, body)
        {
        }

        internal StandardClosure Close (Environment environment)
        {
            this.closeCount += 1;
            return new StandardClosure (this, environment);
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
            answer = environment.CloseOver(this);
            return false;
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
            info.AddValue ("body", this.Body);
        }

        #endregion

        internal override PartialResult PartialEval (Environment environment)
        {
            Environment extendedEnvironment = environment.PartialExtend (this);
            SCode pbody = this.lambdaBody.PartialEval (extendedEnvironment).Residual;
            return new PartialResult (StandardLambda.Make (this.lambdaName, this.lambdaFormals, pbody));
        }
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
                this.realObject = (StandardLambda) Lambda.Make (this.name, this.formals,
                    this.body);
            return this.realObject;
        }
        #endregion
    }
}
