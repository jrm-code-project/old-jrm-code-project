using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Serialization;
using System.Security.Permissions;

// Variable lookup is the most time critical thing
// in the interpreter after continuation management.
namespace Microcode
{
    [Serializable]
    class Access : SCode, ISerializable, ISystemPair
    {
        public static Histogram<string> accessedNames = new Histogram<string> ();

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.ACCESS; } }

        public readonly Symbol var;
#if DEBUG
        [NonSerialized]
        public readonly bool breakOnReference;
#endif
        public readonly SCode env;

        protected Access (SCode env, Symbol name)
        {
            this.var = name;
            this.env = env;

#if DEBUG
            if (
                ((name.ToString ()) == "no symbol has this name") ||
                ((name.ToString ()) == "%record?") ||
                //((name.ToString()) == "fixed-objects") ||
                //((name.ToString()) == "grow-table!") ||
                //(((string) name) == "cgen/expression") ||
                //(((string) name) == "make-conditional") ||
                //(((string) name) == "analyze-file") ||
                //(((string) name) == "sf-conditionally") ||
                //(((string) name) == "load-option") ||
                //(((string) name) == "string-copy") ||
                //(((string) name) == "guarantee-port-type") ||
                //((name.ToString()) == "extend-package-closureEnvironment") ||
                false)
                breakOnReference = true;
#endif  
        }

        static public Access Make (SCode env, Symbol name)
        {
            return new Access (env, name);
                //(! Configuration.EnableAccessOptimization) ? new Access (env, name) :
                //(env is Quotation) ? AccessQ.Make ((Quotation) env, name) :
                //new Access (env, name);
        }

        static public Access Make (object env, Symbol name)
        {
            return Make (EnsureSCode (env), name);
        }

        [SchemePrimitive ("ACCESS?", 1, true)]
        public static bool IsAccess (out object answer, object arg0)
        {
            answer = arg0 is Access;
            return false;
        }

        public override bool CallsTheEnvironment ()
        {
            return this.env.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Access.EvalStep");
            NoteCalls (this.env);
#endif        
            accessedNames.Note (this.var.ToString ());
            Control expr = this.env;
            Environment env = environment;
            object ev = null;
            while (expr.EvalStep (out ev, ref expr, ref env)) { };
            if (ev == Interpreter.UnwindStack) throw new NotImplementedException ();

            Environment accessenv = Environment.ToEnvironment (ev);
#if DEBUG
            //if (this.breakOnReference) Debugger.Break ();
#endif
            if (accessenv.DeepSearch (out answer, this.var)) throw new NotImplementedException ();
            return false;
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            return this.env.MutatesAny (formals);
        }

        #region ISystemPair Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            get
            {
                return UnwrapQuoted (this.env);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCdr
        {
            get
            {
                return this.var;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (AccessDeserializer));
            if (this.var.IsInterned ()) {
                info.AddValue ("var_interned", true);
                info.AddValue ("var", this.var.ToString ());
            }
            else {
                info.AddValue ("var_interned", false);
                info.AddValue ("var", this.var);
            }
            info.AddValue ("env", this.env);
        }

        //public override SCode BindVariables (LexicalMap lexicalMap)
        //{
        //    SCode boundEnv = this.env.BindVariables (lexicalMap);
        //    return (boundEnv == this.env) ?
        //        this :
        //        Access.Make (boundEnv, this.var);
        //}

        public override IList<Symbol> FreeVariables ()
        {
            return this.env.FreeVariables ();
        }

        public override PartialResult PartialEval (Environment environment)
        {
            PartialResult penv = this.env.PartialEval (environment);
            return new PartialResult (penv.Residual == this.env ? this : Access.Make (penv.Residual, this.var));
        }
    }

    [Serializable]
    internal sealed class AccessDeserializer : ISerializable, IObjectReference
    {
        Symbol var;
        SCode env;

        AccessDeserializer (SerializationInfo info, StreamingContext context)
        {
            bool name_interned = info.GetBoolean ("var_interned");
            if (name_interned) {
                string name = info.GetString ("var");
                this.var = Symbol.Make (name);
            }
            else {
                this.var = (Symbol) info.GetValue ("var", typeof (Symbol));
            }

            this.env = (SCode) info.GetValue ("env", typeof (SCode));
        }

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        Object IObjectReference.GetRealObject (StreamingContext context)
        {
            return Access.Make (this.env, this.var);
        }

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            throw new NotImplementedException ();
        }


        //[OnDeserializingAttribute]
        //void OnDeserializing (StreamingContext context) {
        //    Debugger.Break ();
        //}

        //[OnDeserializedAttribute]
        //void OnDeserialized (StreamingContext context) {
        //    Debugger.Break ();
        //}
        // Muffle compiler
        Symbol Var { set { this.var = value; } }
        SCode Env { set { this.env = value; } }
    }

    [Serializable]
    sealed class AccessQ : Access, ISystemPair
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly object envQuoted;

        AccessQ (Quotation env, Symbol name)
            : base (env, name)
        {
            this.envQuoted = env.Quoted;
        }

        static public AccessQ Make (Quotation env, Symbol name)
        {
            if ((! (env.Quoted is Boolean)) ||
                ((bool)env.Quoted) != false
                ) throw new NotImplementedException ();
            return
                new AccessQ (env, name);
        }

        public override bool CallsTheEnvironment ()
        {
            return false;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("AccessQ.EvalStep");
#endif
            if (Environment.Global.DeepSearch (out answer, this.var)) throw new NotImplementedException ();
            return false;
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            return false;
        }
    }

    [Serializable]
    class Assignment : SCode, ISerializable, ISystemPair
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.ASSIGNMENT; } }

#if DEBUG
        [NonSerialized]
        protected readonly Type valueType; 
        static Histogram<Type> valueTypeHistogram = new Histogram<Type>();
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Variable target;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode value;

        protected Assignment (Variable target, SCode value)
            : base ()
        {
            this.target = target;
            this.value = value;
#if DEBUG
            this.valueType = value.GetType();
#endif
        }

        static public SCode Make (Variable target, SCode value)
        {
            return
                //(value is Quotation) ? AssignmentQ.Make (target, (Quotation) value) :
                new Assignment (target, value);
        }

        static public SCode Make (object target, object value)
        {
            return Make ((Variable) target, EnsureSCode (value));
        }

        public object Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.target.Name;
            }
        }

        public SCode Value
        {
            [DebuggerStepThrough]
            get
            {
                return this.value;
            }
        }

        public override string ToString ()
        {
            return "#<ASSIGNMENT " + this.target + ">";
        }

        [SchemePrimitive ("ASSIGNMENT?", 1, true)]
        public static bool IsAssignment (out object answer, object arg0)
        {
            answer = arg0 is Assignment;
            return false;
        }

        #region ISystemPair Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            get
            {
                return this.target;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCdr
        {
            get
            {
                return UnwrapQuoted (this.value);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        public override bool CallsTheEnvironment ()
        {
            return this.value.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.value);
            valueTypeHistogram.Note (this.valueType);
            SCode.location = "Assignment.EvalStep";
#endif
            Control expr = this.value;
            Environment env = environment;
            object newValue;
            while (expr.EvalStep (out newValue, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "Assignment.EvalStep.1";
#endif
            if (newValue == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new AssignmentFrame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            if (environment.Assign (out answer, this.target.Name, newValue)) throw new NotImplementedException ();
            return false;
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            return formals.Contains<Symbol> (this.target.Name)
                || this.value.MutatesAny (formals);
        }

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (AssignmentDeserializer));
            info.AddValue ("target", this.target);
            info.AddValue ("value", this.value);
        }

        //public override SCode BindVariables (LexicalMap lexicalMap)
        //{
        //    SCode boundValue = this.value.BindVariables (lexicalMap);
        //    return (boundValue == this.value) ?
        //        this :
        //        Assignment.Make (this.target, boundValue);
        //}

        public override IList<Symbol> FreeVariables ()
        {
            List<Symbol> singleton = new List<Symbol>(1);
            singleton.Add(this.target.Name);
            return new List<Symbol> (this.Value.FreeVariables ().Union (singleton));
        }

        public override PartialResult PartialEval (Environment environment)
        {
            PartialResult val = this.value.PartialEval (environment);
            return new PartialResult (this.value == val.Residual ? this : Assignment.Make (this.target, val.Residual));
        }
    }

    [Serializable]
    internal sealed class AssignmentDeserializer : IObjectReference
    {
        Variable target;
        SCode value;

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public Object GetRealObject (StreamingContext context)
        {
            return Assignment.Make (this.target, this.value);
        }

        // shut up compiler
        Variable Target { set { this.target = value; } }
        SCode Value { set { this.value = value; } }
    }



    [Serializable]
    sealed class AssignmentFrame0 : SubproblemContinuation<Assignment>, ISystemVector
    {
        internal AssignmentFrame0 (Assignment expression, Environment environment)
            : base (expression, environment) { }

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            if (environment.Assign (out answer, this.expression.Name, value)) throw new NotImplementedException ();
            return false;
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { return 3; }
        }

        public object SystemVectorRef (int index)
        {
            switch (index) {
                case 0:
                    return ReturnCode.EXECUTE_ASSIGNMENT_FINISH;
                case 1: return this.expression;
                case 2: return this.environment;
                default:

                    throw new NotImplementedException ();
            }
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion
    }

//    class AssignArg : Assignment
//    {
//#if DEBUG
//        static Histogram<Type> valueTypeHistogram = new Histogram<Type> ();
//#endif
//        readonly LexicalAddress address;

//        AssignArg (Argument target, SCode value)
//            : base (target, value)
//        {
//            this.address = target.Address;
//        }

//        static public SCode Make (Argument target, SCode value)
//        {
//            return
//                new AssignArg (target, value);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.value);
//            valueTypeHistogram.Note (this.valueType);
//            SCode.location = "AssignArg.EvalStep";
//#endif
//            Control expr = this.value;
//            Environment env = closureEnvironment;
//            object newValue;
//            while (expr.EvalStep (out newValue, ref expr, ref env)) { };
//#if DEBUG
//            SCode.location = "AssignArg.EvalStep.1";
//#endif
//            if (newValue == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new AssignmentFrame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//            if (closureEnvironment.AssignArg (out answer, this.address.Offset, newValue)) 
//                throw new NotImplementedException ();
//            return false;
//        }
//    }

//    [Serializable]
//    class AssignmentQ : Assignment
//    {
//        public readonly object assignmentValue;

//        AssignmentQ (Variable target, Quotation value)
//            : base (target, value)
//        {
//           this.assignmentValue = value.Quoted;
//        }

//        static public SCode Make (Variable target, Quotation value)
//        {
//            return
//                new AssignmentQ (target, value);
//        }


//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("AssignmentQ.EvalStep");
//#endif
//            if (closureEnvironment.Assign (out answer, this.target.Name, this.assignmentValue)) 
//                throw new NotImplementedException ();
//            return false;
//        }
//    }

    [Serializable]
    public class Variable : SCode, ISerializable, ISystemHunk3
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.VARIABLE; } }

#if DEBUG
        [NonSerialized]
        public readonly bool breakOnReference;
#endif
        static Dictionary<Symbol,Variable> variableTable = new Dictionary<Symbol, Variable> ();

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Symbol varname;

        protected Variable (Symbol name)
            : base ()
        {
            if (name == null)
                throw new ArgumentNullException ("name");
            this.varname = name;
#if DEBUG
           if (name is Symbol &&
               (
               ((name.ToString()) == "no symbol has this name") ||
               ((name.ToString()) == "%record?") ||
               //((name.ToString()) == "fixed-objects") ||
               //((name.ToString()) == "grow-table!") ||
               //(((string) name) == "cgen/expression") ||
               //(((string) name) == "make-conditional") ||
               //(((string) name) == "analyze-file") ||
               //(((string) name) == "sf-conditionally") ||
               //(((string) name) == "load-option") ||
               //(((string) name) == "string-copy") ||
               //(((string) name) == "guarantee-port-type") ||
               //((name.ToString()) == "extend-package-closureEnvironment") ||
               false)
               )
                breakOnReference = true;
#endif  
        }

        static public Variable Make (Symbol name)
        {
            Variable answer;
            if (!variableTable.TryGetValue (name, out answer)) {
                answer = new Variable (name);
                variableTable.Add (name, answer);
            }
            return answer;
        }

        static public Variable Make (Hunk3 init)
        {
            return Variable.Make ((Symbol) init.Cxr0);
        }

        public Symbol Name
        {
            get
            {
                return this.varname;
            }
        }

        public override string ToString ()
        {
            return "#<VARIABLE " + this.Name + ">";
        }

        [SchemePrimitive ("VARIABLE?", 1, true)]
        public static bool IsVariable (out object answer, object arg0)
        {
            answer = arg0 is Variable;
            return false;
        }

        #region ISystemHunk3 Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr0
        {
            get
            {
                return this.varname;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr1
        {
            get
            {
                throw new NotImplementedException ();
            }
            set
            {
                throw new NotImplementedException ();
            }
        }
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr2
        {
            get
            {
                throw new NotImplementedException ();
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        public override bool CallsTheEnvironment ()
        {
            return false;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Variable.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            //if (Configuration.EnableVariableOptimization && Configuration.EnableLexicalAddressing)
            //    throw new NotImplementedException ("Should not happen, variables should all be bound.");
            if (environment.DeepSearch (out answer, this.varname)) {
                throw new NotImplementedException ();
            }
            return false;
        }
    
        public override bool MutatesAny (Symbol [] formals)
        {
            return false;
        }
                
        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public virtual void GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (VariableDeserializer));
            info.AddValue ("name", this.varname);
        }

        //public override SCode BindVariables (LexicalMap lexicalMap)
        //{
        //    return lexicalMap.LookupVariable (this.varname,
        //        delegate (int d, int o) { return LexicalVariable.Make (this.varname, d, o); },
        //        delegate (int d, int o) { return ShadowableLexicalVariable.Make (this.varname, d, o); },
        //        delegate (StandardEnvironment env) { return AuxVariable.Make (this.varname, env); },
        //        delegate (StandardEnvironment env) { return ShadowableAuxVariable.Make (this.varname, env); },
        //        delegate (StandardEnvironment env) { return FreeVariable.Make (this.varname, env); },
        //        delegate (StandardEnvironment env) { return ShadowableFreeVariable.Make (this.varname, env); },
        //        delegate (GlobalEnvironment env, ValueCell cell) { return GlobalVariable.Make(this.varname, env, cell); },
        //        delegate (GlobalEnvironment env, ValueCell cell) { return ShadowableGlobalVariable.Make (this.varname, env, cell); }
        //    );
        //}

        public virtual Variable IncreaseLexicalDepth ()
        {
            throw new NotImplementedException ();
        }

        public override IList<Symbol> FreeVariables ()
        {
            IList<Symbol> answer = new List<Symbol>(1);
            answer.Add(this.varname);
            return answer;
        }

        PartialResult makeArgument (int offset)
        {
            return new PartialResult (Argument.Make (this.varname, offset));
        }

        PartialResult makeGlobal (GlobalEnvironment env)
        {
            return new PartialResult (GlobalVariable.Make (this.varname, env, null));
        }

        PartialResult makeLexical1 (int offset)
        {
            return new PartialResult (LexicalVariable1.Make (this.varname, offset));
        }

        PartialResult makeLexical (int depth, int offset)
        {
            return new PartialResult (LexicalVariable.Make (this.varname, depth, offset));
        }

        public override PartialResult PartialEval (Environment environment)
        {
            return new PartialResult (this);
            //return environment.LocateVariable<PartialResult> (this.varname,
            //    delegate(ValueCell cell) { throw new NotImplementedException(); },
            //    makeGlobal,
            //    makeArgument,
            //    makeLexical1,
            //    makeLexical
            //);
        }
    }

    [Serializable]
    internal sealed class VariableDeserializer : IObjectReference
    {
        Symbol name;

        // GetRealObject is called after this object is deserialized.
        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public Object GetRealObject (StreamingContext context)
        {
            return Variable.Make (this.name);
        }

        // shut up compiler
        public void SetName (Symbol value) { this.name = value; }
    }

    /// <summary>
    /// A Bound variable has some idea of how to get at the value cell
    /// other than by using a deep search.
    /// </summary>
    [Serializable]
    public abstract class BoundVariable : Variable
    {
        protected BoundVariable (Symbol name)
            : base (name)
        {
        }
    }

//    class NonArgument : BoundVariable
//    {
//        NonArgument (object lambdaName)
//            : base (lambdaName)
//        { }

//        public static new NonArgument Make (object lambdaName)
//        {
//            return new NonArgument (lambdaName);
//        }

//        public override bool EvalStep (out object value, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ();
//            Debug.WriteLineIf (Primitive.Noisy, this.Name);
//            if (this.breakOnReference) {
//                Debugger.Break ();
//            }
//#endif
//            //if (closureEnvironment.DeepSearch (out value, this.Name))
//                throw new NotImplementedException ("Error on lookup of " + this.Name);
//            return false;
//        }
//    }

    /// <summary>
    /// A ShadowableLexicalVariable is one where we know where the binding cell will
    /// be, and we know it cannot move, but it can be shadowed. 
    /// </summary>
    [Serializable]
    class ShadowableLexicalVariable : BoundVariable
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly int depth;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly int offset;

        protected ShadowableLexicalVariable (Symbol name, int depth, int offset)
            : base (name)
        {
            this.depth = depth;
            this.offset = offset;
        }

        public int Depth { get { return this.depth; } }
        public int Offset { get { return this.offset; } }

        public static ShadowableLexicalVariable Make (Symbol name, int depth, int offset)
        {
            return new ShadowableLexicalVariable (name, depth, offset);
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ShadowableLexicalVariable.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            if (environment.DeepSearch (out value, this.varname))
                throw new NotImplementedException ("Error on lookup of " + this.Name);
            return false;
        }
    }

    /// <summary>
    /// A LexicalVariable is one where we know where the binding cell will
    /// be, and we know it cannot move.  We simply go fetch it.
    /// </summary>
    [Serializable]
    class LexicalVariable : BoundVariable
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly int depth;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly int offset;

        protected LexicalVariable (Symbol name, int depth, int offset)
            : base (name)
        {
            if (offset < 0) throw new ArgumentOutOfRangeException ("offset", offset, "Should be non-negative.");
            this.depth = depth;
            this.offset = offset;
        }

        public int Depth { get { return this.depth; } }
        public int Offset { get { return this.offset; } }

        public static LexicalVariable Make (Symbol name, int depth, int offset)
        {
            return (depth == 0) ? Argument.Make (name, offset) :
                   (depth == 1) ? LexicalVariable1.Make (name, offset) :
                   new LexicalVariable (name, depth, offset);
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("LexicalVariable.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            if (environment.LexicalRef (out value, this.varname, this.depth, this.offset))
                throw new NotImplementedException ("Error on lookup of " + this.Name);
            return false;
        }
    }



    /// <summary>
    /// An argument variable is bound in the immediately enclosing lambda.
    /// We can access it by simply grabbing it from the topmost frame
    /// in the closureEnvironment.
    /// </summary>
    [Serializable]
    class Argument : LexicalVariable
    {
        protected Argument (Symbol name, int offset)
            : base (name, 0, offset)
        {
        }

        static public Argument Make (Symbol name, int offset)
        {
            switch (offset) {
                case 0: return new Argument0 (name);
                case 1: return new Argument1 (name);
                default: return new Argument (name, offset);
            }
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Argument.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            value = environment.ArgumentValue (this.offset);
            return false;
        }
    }

    /// <summary>
    /// Argument zero is the most popular argument.
    /// </summary>
    [Serializable]
    sealed class Argument0 : Argument
    {
        internal Argument0 (Symbol name)
            : base (name, 0)
        {
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Argument0.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            value = environment.Argument0Value;
            return false;
        }
    }

    /// <summary>
    /// Argument1 is the second most popular argument.
    /// </summary>
    [Serializable]
    sealed class Argument1 : Argument
    {
        internal Argument1 (Symbol name)
            : base (name, 1)
        {
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Argument1.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            value = environment.Argument1Value;
            return false;
        }
    }


//    /// <summary>
//    /// A DangerousLexicalVariable is one where we know where the binding cell
//    /// is, but it could be shadowed.
//    /// </summary>
//    [Serializable]
//    class DangerousLexicalVariable : BoundVariable
//    {
//        readonly int shadowDepth;
//        readonly int randDepth;
//        readonly int randOffset;

//        DangerousLexicalVariable (object lambdaName, int shadowDepth, int randDepth, int randOffset)
//            : base (lambdaName)
//        {
//            this.shadowDepth = shadowDepth;
//            this.randDepth = randDepth;
//            this.randOffset = randOffset;
//        }

//        public static DangerousLexicalVariable Make (object lambdaName, int shadowDepth, int randDepth, int randOffset)
//        {
//            return new DangerousLexicalVariable (lambdaName, shadowDepth, randDepth, randOffset);
//        }

//        public override bool EvalStep (out object value, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ();
//            Debug.WriteLineIf (Primitive.Noisy, this.Name);
//            if (this.breakOnReference) {
//                Debugger.Break ();
//            }
//#endif
//            if (closureEnvironment.DangerousLexicalRef (out value, this.ratorName, this.shadowDepth, this.randDepth, this.randOffset))
//                throw new NotImplementedException ("Error on lookup of " + this.ratorName);
//            return false;
//        }
//    }

    /// <summary>
    /// A LexicalVariable1 is one where we know where the binding cell will
    /// be in the parent closureEnvironment.  Just grab it.
    /// </summary>
    [Serializable]
    sealed class LexicalVariable1 : LexicalVariable
    {
        LexicalVariable1 (Symbol name, int offset)
            : base (name, 1, offset)
        {
        }

        public static LexicalVariable1 Make (Symbol name, int offset)
        {
            return new LexicalVariable1 (name, offset);
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("LexicalVariable1.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            if (environment.LexicalRef1 (out value, this.varname, this.offset))
                throw new NotImplementedException ("Error on lookup of " + this.Name);
            return false;
        }
    }

    /// <summary>
    /// A FreeVariable is one that we know is not lexically visible at
    /// binding time.  This means that we must deep search for it, but we
    /// can skip the lexical frames.  We cannot cache the value cell unless
    /// it becomes an incremental in the bindingEnvironment.
    /// </summary>
    [Serializable]
    sealed class FreeVariable : BoundVariable
    {
        StandardEnvironment env;
        FreeVariable (Symbol name, StandardEnvironment env)
            : base (name)
        {
            this.env = env;
        }

        public static FreeVariable Make (Symbol name, StandardEnvironment env)
        {
            return new FreeVariable (name, env);
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("FreeVariable.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            // Don't know where it is.
            if (this.env.FreeRef (out value, this.varname)) throw new NotImplementedException ();
            else return false;
        }


        public override Variable IncreaseLexicalDepth ()
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class ShadowableFreeVariable : BoundVariable
    {
        StandardEnvironment env;
        ShadowableFreeVariable (Symbol name, StandardEnvironment env)
            : base (name)
        {
            this.env = env;
        }

        public static ShadowableFreeVariable Make (Symbol name, StandardEnvironment env)
        {
            return new ShadowableFreeVariable (name, env);
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("FreeVariable.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            // Don't know where it is.
            if (environment.DeepSearch (out value, this.varname)) throw new NotImplementedException ();
            else return false;
        }


        public override Variable IncreaseLexicalDepth ()
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class AuxVariable : BoundVariable
    {
        StandardEnvironment env;
        AuxVariable (Symbol name, StandardEnvironment env)
            : base (name)
        {
            this.env = env;
        }

        public static AuxVariable Make (Symbol name, StandardEnvironment env)
        {
            return new AuxVariable (name, env);
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("AuxVariable.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            // Don't know where it is.
            if (environment.DeepSearch (out value, this.varname)) throw new NotImplementedException ();
            else return false;
        }


        public override Variable IncreaseLexicalDepth ()
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class ShadowableAuxVariable : BoundVariable
    {
        StandardEnvironment env;
        ShadowableAuxVariable (Symbol name, StandardEnvironment env)
            : base (name)
        {
            this.env = env;
        }

        public static ShadowableAuxVariable Make (Symbol name, StandardEnvironment env)
        {
            return new ShadowableAuxVariable (name, env);
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ShadowableAuxVariable.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            // Don't know where it is.
            if (environment.DeepSearch (out value, this.varname)) throw new NotImplementedException ();
            else return false;
        }


        public override Variable IncreaseLexicalDepth ()
        {
            throw new NotImplementedException ();
        }
    }


    /// <summary>
    /// A DeepVariable is one that we know is not lexically visible at
    /// binding time.  This means that we must deep search for it, but we
    /// can skip the lexical frames.  We cannot cache the value cell unless
    /// it becomes an incremental in the bindingEnvironment.
    /// </summary>
//    [Serializable]
//    sealed class DeepVariable : BoundVariable
//    {
//#if DEBUG
//        static Histogram<Symbol> variableNameHistogram = new Histogram<Symbol> ();
//#endif
//        readonly int staticDepth;
//        //Environment baseEnvironment;
//        //Environment [] environmentChain;
//        //ValueCell cachedValueCell;
//        //int cachedValueCellDepth;

//         DeepVariable (Symbol name, int staticDepth)
//            : base (name, null)
//        {
//            this.staticDepth = staticDepth;
//            //this.baseEnvironment = baseEnvironment;
//            //this.environmentChain = new Environment [baseEnvironment.GetDepth ()];
//            //if (environmentChain.Length > 4)
//            //    Debugger.Break ();
//            //for (int i = environmentChain.Length - 1; i > 0; i--) {
//            //    environmentChain [i] = baseEnvironment.GetAncestorEnvironment (i);
//            //}
//            //environmentChain [0] = baseEnvironment;
//        }

//        public static DeepVariable Make (Symbol name, int staticDepth)
//        {
//            return new DeepVariable (name, staticDepth);
//        }

//        public override SCode BindVariables (LexicalMap lexicalMap)
//        {
//            throw new NotImplementedException ();
//            //Debugger.Break ();
//            //LexicalBinding binding = lexicalMap.LookupVariableUncached (this);
//            //if (binding == null) {
//            //    int staticDepth = lexicalMap.StaticDepth;
//            //    if (staticDepth == 0)
//            //        return this;
//            //    else
//            //        return DeepVariable.Make (this.varname, staticDepth);
//            //}
//            //else
//            //    return LexicalVariable.Make (this.varname, binding.Binder, binding.Address);
//        }

//        public override bool EvalStep (out object value, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            variableNameHistogram.Note (this.varname);
//            if (this.breakOnReference) {
//                Debugger.Break ();
//            }
//            SCode.location = "DeepVariable.EvalStep";
//#endif
//            if (closureEnvironment.DeepSearchSkip (out value, this.varname, this.staticDepth))
//                throw new NotImplementedException ("Variable DeepSeach failed for " + this.varname.ToString ());
//            else return false;

//            //if (this.cachedValueCell == null) {
//            //    // First time through, 
//            //    // search the closureEnvironment chain to find the nearest
//            //    // place the value cell exists.
//            //    for (int i = 0; i < environmentChain.Length; i++) {
//            //        Environment env = environmentChain [i];
//            //        ValueCell vc = env.SearchFixed (this.varname);
//            //        if (vc != null) {
//            //            this.cachedValueCell = vc;
//            //            this.cachedValueCellDepth = i;
//            //            break;
//            //        }
//            //        vc = env.SearchIncrementals (this.varname);
//            //        if (vc != null) {
//            //            this.cachedValueCell = vc;
//            //            this.cachedValueCellDepth = i;
//            //            break;
//            //        }
//            //    }
//            //}
//            //else {
//            //    // On the subsequent passes, we might shadow the variable
//            //    // with incrementals, but we cannot add fixed bindings.
//            //    for (int i = 0; i < this.cachedValueCellDepth; i++) {
//            //        Environment env = environmentChain [i];
//            //        ValueCell vc = env.SearchIncrementals (this.varname);
//            //        if (vc != null) {
//            //            // Someone shadowed it.
//            //            throw new NotImplementedException ();
//            //        }
//            //    }
//            //    // if we fall through, no one shadowed us and we can
//            //    // simply drop into the case where we fetch from the cell.
//            //}

//            //if (this.cachedValueCell.GetValue (out value))
//            //    throw new NotImplementedException ();
//            //return false;
//        }
//    }


//    /// <summary>
//    /// A DangerousFreeVariable is one that we know is not lexically visible at
//    /// binding time, and could be shadowed.
//    /// </summary>
//    [Serializable]
//    sealed class DangerousFreeVariable : BoundVariable
//    {
//        readonly Environment env;
//        readonly int randDepth;

//        DangerousFreeVariable (object lambdaName, Environment env, int randDepth)
//            : base (lambdaName)
//        {
//            this.env = env;
//            this.randDepth = randDepth;
//        }

//        public static DangerousFreeVariable Make (object lambdaName, Environment env, int randDepth)
//        {
//            return new DangerousFreeVariable (lambdaName, env, randDepth);
//        }

//        public override bool EvalStep (out object value, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ();
//            Debug.WriteLineIf (Primitive.Noisy, this.ratorName);
//            if (this.breakOnReference) {
//                Debugger.Break ();
//            }

//#endif
//            throw new NotImplementedException ();
//        }
//    }

    /// <summary>
    /// A TopLevelVariable is one that we find in the binding-time closureEnvironment.
    /// It cannot be shadowed (except by us), so we can cache the value cell.
    /// </summary>
//    [Serializable]
//    sealed class TopLevelVariable : BoundVariable
//    {
//#if DEBUG
//        static Histogram<object> nameHistogram = new Histogram<object>();
//#endif
//        public readonly ValueCell cell;

//        public TopLevelVariable (Symbol name, ValueCell cell)
//            : base (name, null)
//        {
//            this.cell = cell;
//        }

//        public override bool EvalStep (out object value, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            nameHistogram.Note (this.varname);
//            if (this.breakOnReference) {
//                Debugger.Break ();
//            }
//            SCode.location = "TopLevelVariable.EvalStep";
//#endif
//            if (this.cell.GetValue (out value))
//                throw new NotImplementedException ();
//            return false;
//        }

//        public override Variable IncreaseLexicalDepth ()
//        {
//            return this;
//        }
//    }

//    /// <summary>
//    /// A TopLevelVariable is one that we find in the binding-time closureEnvironment.
//    /// It cannot be shadowed (except by us), so we can cache the value cell.
//    /// </summary>
//    [Serializable]
//    sealed class DangerousTopLevelVariable : BoundVariable
//    {
//        readonly ValueCell cell;
//        int safeDepth;

//        DangerousTopLevelVariable (object lambdaName, ValueCell cell, int safeDepth)
//            : base (lambdaName)
//        {
//            this.cell = cell;
//            this.safeDepth = safeDepth;
//        }

//        static public DangerousTopLevelVariable Make (object lambdaName, ValueCell cell, int safeDepth)
//        {
//            return new DangerousTopLevelVariable (lambdaName, cell, safeDepth);
//        }

//        public override bool EvalStep (out object value, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ();
//            Debug.WriteLineIf (Primitive.Noisy, this.ratorName);
//            if (this.breakOnReference) {
//                Debugger.Break ();
//            }
//#endif
//            throw new NotImplementedException ();
//        }
//    }


    /// <summary>
    /// A global variable is one we know is bound in the global closureEnvironment
    /// and cannot be shadowed.  If we have a binding cell, we can just fetch
    /// the value from there.
    /// </summary>
    [Serializable]
    sealed class GlobalVariable : BoundVariable
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Environment environment;

        ValueCell cell;

        GlobalVariable (Symbol name, Environment environment, ValueCell cell)
            : base (name)
        {
            this.environment = environment;
            this.cell = cell;
        }

        public static GlobalVariable Make (Symbol name, Environment env, ValueCell cell)
        {
            return new GlobalVariable (name, env, cell);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("GlobalVariable.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }

#endif
            if (environment.DeepSearch (out answer, this.varname))
                throw new NotImplementedException ("Variable DeepSeach failed for " + this.varname.ToString ());
            else return false;
        }

        public override Variable IncreaseLexicalDepth ()
        {
            // Global variables are just fetched from the value cell,
            // so we need do nothing.
            return this;
        }
    }

    /// <summary>
    /// A shadowable global variable is one we know is bound in the global closureEnvironment
    /// but could be shadowed.  If we have a binding cell, we can just fetch
    /// the value from there once we test the incrementals in the outer environments.
    /// </summary>
    [Serializable]
    sealed class ShadowableGlobalVariable : BoundVariable
    {
        Environment env;
        ValueCell cell;

        ShadowableGlobalVariable (Symbol name, Environment env, ValueCell cell)
            : base (name)
        {
            this.env = env;
            this.cell = cell;
        }

        public static ShadowableGlobalVariable Make (Symbol name, Environment env, ValueCell cell)
        {
            return new ShadowableGlobalVariable (name, env, cell);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ShadowableGlobalVariable");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            if (environment.DeepSearch (out answer, this.varname))
                throw new NotImplementedException ("Variable DeepSeach failed for " + this.varname.ToString ());
            else return false;
        }
    }

}
