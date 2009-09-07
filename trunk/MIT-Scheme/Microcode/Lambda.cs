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
        protected SCode lambdaBody;

        // Count of times this lambda is closed over.
        protected int closeCount = 0;
        // Count of times this lambda body is evaluated.
        protected int evaluationCount = 0;

        protected IList<Symbol> freeVars;

        protected LambdaBase (Symbol name, Symbol [] formals, IList<Symbol> freeVariables, SCode body)
        {
            this.lambdaName = name;
            this.lambdaFormals = formals;
            this.lambdaBody = body;
            this.freeVars = freeVariables;
#if DEBUG
            // Paranoia:  check for duplicate names
            if (name != Dummy)
                for (int i = 0; i < formals.Length - 1; i++)
                    if (formals [i] != null)
                        for (int j = i + 1; j < formals.Length; j++)
                            if (formals [i] == formals [j]) 
                                Debugger.Break ();

            // Check for eta-reducible primitive lambdas.  There seem to be
            // too many of these.
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
            // This way is *slow*
            // return Array.IndexOf (this.lambdaFormals, ratorName);
            for (int i = 0; i < this.lambdaFormals.Length; i++)
                if (name == this.lambdaFormals [i])
                    return i;
            return -1;
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            // Should check for shadowing.
            return this.lambdaBody.MutatesAny (formals);
        }

        public override bool CallsTheEnvironment ()
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        public override TC TypeCode
        {
            get { throw new NotImplementedException (); }
        }

        internal abstract ClosureBase Close (Environment environment);

        public override IList<Symbol> FreeVariables ()
        {
            return this.freeVars;
        }

        public PartialClosure PartialClose (Environment environment)
        {
            return new PartialClosure (this, environment);
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

        protected Lambda (Symbol name, Symbol [] formals, IList<Symbol> freeVariables, SCode body)
            : base (name, formals, freeVariables, body)
        {
        }

        public static Lambda Make (Symbol name, Symbol [] formals, IList<Symbol> freeVariables, SCode body)
        {
            if (body == null)
                throw new ArgumentNullException ("body");
            if (formals == null)
                throw new ArgumentNullException ("formals");
            if (name == null)
                throw new ArgumentNullException ("name");
  
            return StandardLambda.Make (name, formals, freeVariables, body);
        }

        public static SCode Make (object name, object formals, object freeVariables, object body)
        {
            if (formals == null)
                throw new ArgumentNullException ("formals");
            object [] formalsArray = (object []) formals;
            Symbol [] realFormals = new Symbol [formalsArray.Length];
            for (int i = 0; i < formalsArray.Length; i++) {
                realFormals [i] = (Symbol) formalsArray [i];
            }
            return Make ((Symbol) name, (Symbol []) realFormals, (IList<Symbol>) freeVariables, EnsureSCode (body));
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
            IList<Symbol> freeVariables,
            SCode body, uint required, uint optional, bool rest)
            : base (name, formals, freeVariables, body)
        {
            this.required = required;
            this.optional = optional;
            this.rest = rest;
        }

        public static LambdaBase Make (Symbol name, Symbol [] formals, IList<Symbol> freeVariables, SCode body, uint required, uint optional, bool rest)
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
                rest == false) ? UnanalyzedLambda.Make (name, formals, 
                    freeVariables, body) :  
                (LambdaBase) StandardExtendedLambda.Make (name, formals, freeVariables, body, required, optional, rest);
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
                new List<Symbol> (body.FreeVariables().Except(formals)),
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

        internal override ClosureBase Close (Environment environment)
        {
            throw new NotImplementedException ();
        }

        public override PartialResult PartialEval (Environment environment)
        {
            PartialResult pbody = this.lambdaBody.PartialEval (environment.PartialExtend (this));
            return new PartialResult (ExtendedLambda.Make (this.lambdaName, this.lambdaFormals, this.freeVars, pbody.Residual, this.required, this.optional, this.rest));
        }
    }

    /// <summary>
    /// Supports option, rest, and aux args, 
    /// and incremental definitions.
    /// </summary>
    [Serializable]
    sealed class StandardExtendedLambda : ExtendedLambda, ISerializable
    {
        StandardExtendedLambda (Symbol name, Symbol [] formals, IList<Symbol> freeVariables, SCode body, uint required, uint optional, bool rest)
            : base (name, formals, freeVariables, body, required, optional, rest)
        {
        }

        public static new StandardExtendedLambda Make (Symbol name, Symbol [] formals, IList<Symbol> freeVariables, SCode body, uint required, uint optional, bool rest)
        {
            if (body == null)
                throw new ArgumentNullException ("body");
            if (formals == null)
                throw new ArgumentNullException ("formals");
            if (name == null)
                throw new ArgumentNullException ("name");

            return new StandardExtendedLambda (name, formals, freeVariables, body, required, optional, rest);
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

        internal override ClosureBase Close (Environment environment)
        {
            throw new NotImplementedException ();
        }
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
                    new List<Symbol> (body.FreeVariables().Except(this.formals)),
                    this.body, this.required, this.optional, this.rest);
            return this.realObject;
        }
        #endregion
    }

    /// <summary>
    /// Supports option, rest, and aux args, 
    /// but not incremental definitions.
    /// </summary>
    [Serializable]
    sealed class StaticExtendedLambda : ExtendedLambda, ISerializable
    {
        StaticExtendedLambda (Symbol name, Symbol [] formals, 
            IList<Symbol> freeVariables, SCode body, uint required, uint optional, bool rest)
            : base (name, formals, freeVariables, body, required, optional, rest)
        {
        }

        public static new StaticExtendedLambda Make (Symbol name, Symbol [] formals, 
            IList<Symbol> freeVariables, SCode body, uint required, uint optional, bool rest)
        {
            if (body == null)
                throw new ArgumentNullException ("body");
            if (formals == null)
                throw new ArgumentNullException ("formals");
            if (name == null)
                throw new ArgumentNullException ("name");

            return new StaticExtendedLambda (name, formals, freeVariables, body, required, optional, rest);
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

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (StaticExtendedLambdaDeserializer));
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

        //public override SCode BindVariables (LexicalMap lexicalMap)
        //{
        //    SCode boundBody = this.lambdaBody.BindVariables (lexicalMap.Extend (this));
        //    return (boundBody == this.lambdaBody) ?
        //        this :
        //        StaticExtendedLambda.Make (this.Name, this.Formals, 
        //        this.freeVars,
        //        boundBody,
        //        this.required, this.optional, this.rest);

        //}

        internal override ClosureBase Close (Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class StaticExtendedLambdaDeserializer : ISerializable, IObjectReference
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
        StaticExtendedLambda realObject;

        StaticExtendedLambdaDeserializer (SerializationInfo info, StreamingContext context)
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
                this.realObject = StaticExtendedLambda.Make (this.name, this.formals, 
                    new List<Symbol> (this.body.FreeVariables().Except(this.formals)),
                    this.body, this.required, this.optional, this.rest);
            return this.realObject;
        }
        #endregion
    }

    [Serializable]
    sealed class UnanalyzedLambda : Lambda, ISerializable
    {
        bool firstClassEnvironment;
        //LambdaBase analyzed;

        UnanalyzedLambda (Symbol name, Symbol [] formals, IList<Symbol> freeVariables, SCode body)
            : base (name, formals, freeVariables, body)
        {
            this.firstClassEnvironment = body.CallsTheEnvironment ();
        }

        public static new LambdaBase Make (Symbol name, Symbol [] formals, IList<Symbol> freeVariables, SCode body)
        {
            return new UnanalyzedLambda (name, formals, freeVariables, body);
        }

        public static new SCode Make (object name, object formals, object freeVariables, object body)
        {
            if (formals == null)
                throw new ArgumentNullException ("formals");
            object [] formalsArray = (object []) formals;
            Symbol [] realFormals = new Symbol [formalsArray.Length];
            for (int i = 0; i < formalsArray.Length; i++) {
                realFormals [i] = (Symbol) formalsArray [i];
            }
            SCode realBody = EnsureSCode (body);
            return Make ((Symbol) name, realFormals, (IList<Symbol>) freeVariables,

                realBody);
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
            SCode realBody = EnsureSCode (body);
            return Make ((Symbol) name, realFormals, new List<Symbol> (realBody.FreeVariables().Except(realFormals)),

                realBody);
        }

        public override bool CallsTheEnvironment ()
        {
            return this.firstClassEnvironment;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("UnanalyzedLambda.EvalStep");
#endif
            throw new NotImplementedException ();
            //if (this.analyzed == null) {
            //    if (this.firstClassEnvironment) {
            //        this.analyzed = StandardLambda.Make (this.lambdaName,
            //            this.lambdaFormals,
            //            this.freeVars,
            //            this.lambdaBody.BindVariables (LexicalMap.Make (closureEnvironment).Extend (this)));
            //    }
            //    else {
            //        this.analyzed = StaticLambda.Make (this.lambdaName,
            //        this.lambdaFormals,
            //        this.freeVars,
            //        this.lambdaBody.BindVariables (LexicalMap.Make (closureEnvironment).Extend (this)));
            //    }
            //}
            //answer = this.analyzed.Close (closureEnvironment);
            //return false;
        }

        

        //public override SCode BindVariables (LexicalMap lexicalMap)
        //{
        //    if (this.firstClassEnvironment) {
        //        return StandardLambda.Make (this.lambdaName,
        //            this.lambdaFormals,
        //            this.freeVars,
        //            this.lambdaBody.BindVariables (lexicalMap.Extend (this)));
        //    }
        //    else {
        //        return StaticLambda.Make (this.lambdaName,
        //    this.lambdaFormals,
        //    this.freeVars,
        //    this.lambdaBody.BindVariables (lexicalMap.Extend (this)));
        //    }

        //}

        internal override ClosureBase Close (Environment environment)
        {
            throw new NotImplementedException ();
            //if (this.firstClassEnvironment) {
            //    return StandardLambda.Make (this.lambdaName,
            //        this.lambdaFormals,
            //        this.freeVars,
            //        this.lambdaBody.BindVariables (LexicalMap.Make (closureEnvironment).Extend (this))).Close(closureEnvironment);
            //}
            //else {
            //    return StaticLambda.Make (this.lambdaName,
            //this.lambdaFormals,
            //this.freeVars,
            //this.lambdaBody.BindVariables (LexicalMap.Make (closureEnvironment).Extend (this))).Close(closureEnvironment);
            //}
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (UnanalyzedLambdaDeserializer));
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

        public override PartialResult PartialEval (Environment environment)
        {
            PartialResult pbody = this.lambdaBody.PartialEval (environment.PartialExtend (this));
            return new PartialResult (Lambda.Make (this.lambdaName, this.lambdaFormals, this.freeVars, pbody.Residual));
        }
    }

    [Serializable]
    sealed class UnanalyzedLambdaDeserializer : ISerializable, IObjectReference
    {
        [NonSerialized]
        Symbol name;

        [NonSerialized]
        Symbol [] formals;

        [NonSerialized]
        SCode body;

        [NonSerialized]
        LambdaBase realObject;

        UnanalyzedLambdaDeserializer (SerializationInfo info, StreamingContext context)
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
                this.realObject = UnanalyzedLambda.Make (this.name, this.formals, 
                    new List<Symbol>(this.body.FreeVariables().Except(this.formals)),
                    this.body);
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
        StandardLambda (Symbol name, Symbol [] formals, IList<Symbol> freeVariables, SCode body)
            : base (name, formals, freeVariables, body)
        { }

        public static new StandardLambda Make (Symbol name, Symbol [] formals, IList<Symbol> freeVariables, SCode body)
        {
           return new StandardLambda (name, formals, freeVariables, body);
        }

        internal override ClosureBase Close (Environment environment)
        {
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
            this.closeCount += 1;
            answer = new StandardClosure (this, environment);
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

        public override PartialResult PartialEval (Environment environment)
        {
            throw new NotImplementedException ();
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
                this.realObject = StandardLambda.Make (this.name, this.formals, 
                    new List<Symbol> (this.body.FreeVariables().Except(this.formals)),
                    this.body);
            return this.realObject;
        }
        #endregion
    }

    /// <summary>
    /// This is the abstract base class for all lambdas that do not support
    /// incremental definition.  This allows us to optimize for the case
    /// where we know the lexical addresses of free variables and don't
    /// have to worry about shadowing definition.
    /// </summary>
    [Serializable]
    abstract class StaticLambdaBase : Lambda
    {
        protected StaticLambdaBase (Symbol name, Symbol [] formals, IList<Symbol> freeVariables, SCode body)
            : base (name, formals, freeVariables, body)
        { }

        public override bool CallsTheEnvironment ()
        {
            return false;
        }
 
        internal override ClosureBase Close (Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    /// <summary>
    /// A Static lambda does not support incremental definition, but it does
    /// support assignment and unassigned variables.
    /// </summary>
    [Serializable]
    sealed class StaticLambda : StaticLambdaBase, ISerializable
    {
        internal StaticLambda (Symbol name, Symbol [] formals, IList<Symbol> freeVariables, SCode body)
            : base (name, formals, freeVariables, body)
        {         
        }

        public static new LambdaBase Make (Symbol name, Symbol [] formals, IList<Symbol> freeVariables, SCode body)
        {
            return body.MutatesAny(formals) ?
                (LambdaBase)new StaticLambda (name, formals, freeVariables, body) :
                (LambdaBase)new SimpleLambda (name, formals, freeVariables, body);
        }

        internal override ClosureBase Close (Environment environment)
        {
            return new StaticClosure (this, environment);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("StaticLambda.EvalStep");
#endif
            answer = (this.freeVars.Count == 0) ?
                (ClosureBase) new NonClosure (this) :
                (ClosureBase) new StaticClosure (this, environment);
            return false;
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
            info.AddValue ("body", this.Body);
        }

        #endregion
        //public override SCode BindVariables (LexicalMap lexicalMap)
        //{
        //    throw new NotImplementedException ();
        //    //SCode boundBody = this.lambdaBody.BindVariables (new LexicalMap (this, lexicalMap));
        //    //return (boundBody == this.lambdaBody) ?
        //    //    this :
        //    //    StaticLambda.Make (this.Name, this.Formals, boundBody);

        //}

        public override PartialResult PartialEval (Environment environment)
        {
            throw new NotImplementedException ();
        }
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
        LambdaBase realObject;

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
            this.body = (SCode) info.GetValue ("body", typeof (SCode));
            //Debugger.Break ();
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            Debugger.Break ();
            throw new NotImplementedException ();
        }
        #endregion

        #region IObjectReference Members

        public object GetRealObject (StreamingContext context)
        {
            if (this.realObject == null)
                this.realObject = StaticLambda.Make (this.name, this.formals, 
                    new List<Symbol>(this.body.FreeVariables().Except(this.formals)),
                    this.body);
            return this.realObject;
        }
        #endregion
    }

    /// <summary>
    /// A SimpleLambda creates SimpleClosures.  There can be no incremental
    /// definition, no assignment, and no unassigned variables.  Despite these
    /// restrictions, this is the most popular kind of lambda.
    /// </summary>
    [Serializable]
    sealed class SimpleLambda : StaticLambdaBase, ISerializable
    {
        internal SimpleLambda (Symbol name, Symbol [] formals, IList<Symbol> freeVariables, SCode body)
            : base (name, formals, freeVariables, body)
        {
        }

        public static new SimpleLambda Make (Symbol name, Symbol [] formals, IList<Symbol> freeVariables, SCode body)
        {
            return new SimpleLambda (name, formals, freeVariables, body);
        }

        internal override ClosureBase Close (Environment environment)
        {
            return (this.freeVars.Count == 0) ?
                (ClosureBase) new SimpleNonClosure (this) :
                (ClosureBase) new SimpleClosure (this, environment);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLambda.EvalStep");
#endif
            answer = (this.freeVars.Count == 0) ?
                (ClosureBase) new SimpleNonClosure (this) :
                (ClosureBase) new SimpleClosure (this, environment);
            return false;
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
            info.AddValue ("body", this.Body);
        }

        #endregion
        //public override SCode BindVariables (LexicalMap lexicalMap)
        //{
        //    throw new NotImplementedException ();
        //    //SCode boundBody = this.lambdaBody.BindVariables (new LexicalMap (this, lexicalMap));
        //    //return (boundBody == this.lambdaBody) ?
        //    //    this :
        //    //    SimpleLambda.Make (this.Name, this.Formals, boundBody);

        //}

        public override PartialResult PartialEval (Environment environment)
        {
            throw new NotImplementedException ();
        }
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
                this.realObject = SimpleLambda.Make (this.name, this.formals, 
                    new List<Symbol>(this.body.FreeVariables().Except(this.formals)),
                    
                    this.body);
            return this.realObject;
        }
        #endregion
    }
}
