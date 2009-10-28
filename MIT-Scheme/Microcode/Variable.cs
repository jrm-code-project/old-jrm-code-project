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
            if (this.breakOnReference) Debugger.Break ();
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

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialResult penv = this.env.PartialEval (environment);
            return new PartialResult (penv.Residual == this.env ? this : Access.Make (penv.Residual, this.var));
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            this.env.CollectFreeVariables (freeVariableSet);
        }

        internal override SCode SubstituteStatics (object [] statics)
        {
            SCode newEnv = this.env.SubstituteStatics (statics);
            return (newEnv == this.env) ? this : Access.Make (newEnv, this.var);
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

    public interface IVariableSpecializer
    {
        Symbol Name { get; }

        SCode MakeFree ();

        SCode MakeGlobal (GlobalEnvironment globalEnvironment);

        SCode MakeTopLevel (ValueCell cell);

        SCode MakeArgument (int argOffset);

        SCode MakeStatic (int staticOffset);
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
                (target is Argument) ? AssignmentA.Make ((Argument) target, value) :
                (target is GlobalVariable) ? AssignmentG.Make ((GlobalVariable) target, value) :
                (target is StaticVariable) ? AssignmentS.Make ((StaticVariable) target, value) :
                (target is TopLevelVariable) ? AssignmentT.Make ((TopLevelVariable) target, value) :
                new Assignment (target, value);
        }

        static public SCode Make (object target, object value)
        {
            return Make ((Variable) target, EnsureSCode (value));
        }

        public Symbol Name { [DebuggerStepThrough] get { return this.Target.Name; } }

        public Variable Target
        {
            [DebuggerStepThrough]
            get
            {
                return this.target;
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
#if DEBUG
            if (this.target.breakOnReference) {
                Debugger.Break ();
            }
#endif

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

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialResult val = this.value.PartialEval (environment);
            Variable var = environment.LocateVariable (this.Target) as Variable;
            if (var == null)
                throw new NotImplementedException ();

            return new PartialResult (Assignment.Make (var, val.Residual));
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            freeVariableSet.Add (this.target.Name);
            this.value.CollectFreeVariables (freeVariableSet);
        }

        internal override SCode SubstituteStatics (object [] statics)
        {
            SCode newValue = this.value.SubstituteStatics (statics);
            return (newValue == this.value) ? this : Assignment.Make (this.target, newValue);
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
#if DEBUG
            if (this.expression.Target.breakOnReference) {
                Debugger.Break ();
            }
#endif
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

    [Serializable]
    class AssignmentA : Assignment
    {
#if DEBUG
        static Histogram<Type> valueTypeHistogram = new Histogram<Type>();
#endif
        readonly int offset;

        AssignmentA (Argument target, SCode value)
            : base (target, value)
        {
            this.offset = target.Offset;
        }

        public static SCode Make (Argument target, SCode value)
        {
            return new AssignmentA (target, value);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.value);
            valueTypeHistogram.Note (this.valueType);
            SCode.location = "AssignmentA.EvalStep";
#endif
            Control expr = this.value;
            Environment env = environment;
            object newValue;
            while (expr.EvalStep (out newValue, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "AssignmentA.EvalStep.1";
#endif
            if (newValue == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new AssignmentFrame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
#if DEBUG
            if (this.target.breakOnReference) {
                Debugger.Break ();
            }
#endif
            if (environment.AssignArgument (out answer, this.offset, newValue)) throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class AssignmentG : Assignment
    {
#if DEBUG
        static Histogram<Type> valueTypeHistogram = new Histogram<Type> ();
#endif
        AssignmentG (GlobalVariable target, SCode value)
            : base (target, value)
        {
        }

        public static SCode Make (GlobalVariable target, SCode value)
        {
            return new AssignmentG (target, value);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.value);
            valueTypeHistogram.Note (this.valueType);
            SCode.location = "AssignmentG.EvalStep";
#endif
            Control expr = this.value;
            Environment env = environment;
            object newValue;
            while (expr.EvalStep (out newValue, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "AssignmentG.EvalStep.1";
#endif
            if (newValue == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new AssignmentFrame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
#if DEBUG
            if (this.target.breakOnReference) {
                Debugger.Break ();
            }
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class AssignmentS : Assignment
    {
        readonly int offset;
#if DEBUG
        static Histogram<Type> valueTypeHistogram = new Histogram<Type> ();
#endif
        AssignmentS (StaticVariable target, SCode value)
            : base (target, value)
        {
            this.offset = target.Offset;
        }

        public static SCode Make (StaticVariable target, SCode value)
        {
            return new AssignmentS (target, value);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.value);
            valueTypeHistogram.Note (this.valueType);
            SCode.location = "AssignmentS.EvalStep";
#endif
            Control expr = this.value;
            Environment env = environment;
            object newValue;
            while (expr.EvalStep (out newValue, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "AssignmentS.EvalStep.1";
#endif
            if (newValue == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new AssignmentFrame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
#if DEBUG
            if (this.target.breakOnReference) {
                Debugger.Break ();
            }
#endif
            if (environment.AssignStatic (out answer, this.offset, newValue)) throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class AssignmentT : Assignment
    {
        readonly ValueCell cell;
#if DEBUG
        static Histogram<Type> valueTypeHistogram = new Histogram<Type> ();
#endif
        AssignmentT (TopLevelVariable target, SCode value)
            : base (target, value)
        {
            this.cell = target.valueCell;
        }

        public static SCode Make (TopLevelVariable target, SCode value)
        {
            return new AssignmentT (target, value);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.value);
            valueTypeHistogram.Note (this.valueType);
            SCode.location = "AssignmentT.EvalStep";
#endif
            Control expr = this.value;
            Environment env = environment;
            object newValue;
            while (expr.EvalStep (out newValue, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "AssignmentT.EvalStep.1";
#endif
            if (newValue == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new AssignmentFrame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
#if DEBUG
            if (this.target.breakOnReference) {
                Debugger.Break ();
            }
#endif
            return this.cell.Assign (out answer, newValue);
        }
    }

    [Serializable]
    public class Variable : SCode, ISerializable, ISystemHunk3, IVariableSpecializer
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
               //((name.ToString()) == "%char-set-member?") ||
               //((name.ToString()) == "the-console-port") ||
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

        static internal Variable Make (Hunk3 init)
        {
            return Variable.Make ((Symbol) init.Cxr0);
        }

        public Symbol Name
        {[DebuggerStepThrough]
            get
            {
                return this.varname;
            }
        }

        [DebuggerStepThrough]
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


        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            return
                new PartialResult (Configuration.EnableVariableOptimization 
                    ? environment.LocateVariable (this) 
                    : this);
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            freeVariableSet.Add (this.varname);
        }



        #region IVariableSpecializer Members

        public SCode MakeArgument (int argOffset)
        {
            return Configuration.EnableArgumentBinding ? Argument.Make (this.varname, argOffset) : this;
        }

        public SCode MakeFree ()
        {
            return Configuration.EnableArgumentBinding ? FreeVariable.Make (this.varname) : this;
        }

        public SCode MakeGlobal (GlobalEnvironment env)
        {
            return GlobalVariable.Make (this.varname, env, null);
        }

        public SCode MakeStatic (int staticOffset)
        {
            return Configuration.EnableStaticBinding ? StaticVariable.Make (this.varname, staticOffset) :
                   Configuration.EnableArgumentBinding ? FreeVariable.Make (this.varname) :
                   this;
        }

        public SCode MakeTopLevel (ValueCell topLevelCell)
        {
            return TopLevelVariable.Make (this.varname, topLevelCell);
        }

        #endregion

        internal override SCode SubstituteStatics (object [] statics)
        {
            return this;
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
    abstract class BoundVariable : Variable
    {
        protected BoundVariable (Symbol name)
            : base (name)
        {
        }
    }

    /// <summary>
    /// An argument variable is bound in the immediately enclosing lambda.
    /// We can access it by simply grabbing it from the topmost frame
    /// in the closureEnvironment.
    /// </summary>
    [Serializable]
    class Argument : BoundVariable
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly int offset;

        protected Argument (Symbol name, int offset)
            : base (name)
        {
            this.offset = offset;
        }

        public int Offset { get { return this.offset; } }

        static public Argument Make (Symbol name, int offset)
        {
            if (Configuration.EnableArgument0And1)
                switch (offset) {
                    case 0: return new Argument0 (name);
                    case 1: return new Argument1 (name);
                    default: return new Argument (name, offset);
                }
            else
                return new Argument (name, offset);
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

        internal override SCode SubstituteStatics (object [] statics)
        {
            return this;
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

        internal override SCode SubstituteStatics (object [] statics)
        {
            return this;
        }
    }

    /// <summary>
    /// A static variable is bound in the static block of a closure.
    /// </summary>
    [Serializable]
    class StaticVariable : BoundVariable
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly int offset;
#if DEBUG
        static protected Histogram<int> staticOffsets = new Histogram<int> ();
#endif
        protected StaticVariable (Symbol name, int offset)
            : base (name)
        {
            this.offset = offset;
        }

        public int Offset { get { return this.offset; } }

        static public StaticVariable Make (Symbol name, int offset)
        {
            return new StaticVariable (name, offset);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
            staticOffsets.Note (this.offset);
            SCode.location = "StaticVariable.EvalStep";
#endif
            if (environment.StaticValue (out answer, this.varname, this.offset)) {
                throw new NotImplementedException ();
            }
            return false;
        }

        internal override SCode SubstituteStatics (object [] statics)
        {
            object maybeCell = statics [this.offset];
            ValueCell vcell = maybeCell as ValueCell;
            if (vcell == null) {
               // Debug.WriteLine ("Substitute " + this.Name);
                return Quotation.Make (maybeCell);
            }
            else {
                return TopLevelVariable.Make (this.varname, vcell);
                //return this;
            }
        }

    }

    /// <summary>
    /// A global variable is one we know is bound in the global closureEnvironment
    /// and cannot be shadowed.  If we have a binding cell, we can just fetch
    /// the value from there.  There are surprisingly few of these.
    /// </summary>
    [Serializable]
    sealed class GlobalVariable : BoundVariable
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly GlobalEnvironment environment;
        static Dictionary<Symbol, GlobalVariable> cache = new Dictionary<Symbol, GlobalVariable> ();
        ValueCell cell;

        GlobalVariable (Symbol name, GlobalEnvironment environment, ValueCell cell)
            : base (name)
        {
            this.environment = environment;
            this.cell = cell;
        }

        public static GlobalVariable Make (Symbol name, GlobalEnvironment env, ValueCell cell)
        {
            GlobalVariable cached;
            if (!cache.TryGetValue (name, out cached)) {
                cached = new GlobalVariable (name, env, cell);
                cache.Add (name, cached);
            }
            return cached;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("GlobalVariable.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }

#endif
            if (cell != null) {
                return cell.GetValue (out answer);
            }
            else {
                cell = this.environment.GetValueCell (this.Name);
                if (cell != null) { return cell.GetValue (out answer); }
                throw new NotImplementedException ("Broken global variable.");
            }
        }
    }

    [Serializable]
    sealed class TopLevelVariable : Variable
    {
#if DEBUG
        static Histogram<Symbol> hotTopLevelVariables = new Histogram<Symbol>();
#endif
        readonly ValueCell cell;

        TopLevelVariable (Symbol name, ValueCell cell)
            : base (name)
        {
            this.cell = cell;
        }

        public static TopLevelVariable Make (Symbol name, ValueCell cell)
        {
            return new TopLevelVariable (name, cell);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("TopLevelVariable.EvalStep");
            hotTopLevelVariables.Note (this.varname);
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            return cell.GetValue (out answer);
        }

        public ValueCell valueCell { [DebuggerStepThrough] get { return this.cell; } }
    }

    [Serializable]
    sealed class FreeVariable : Variable
    {
        ValueCell lastCell;

        FreeVariable (Symbol name)
            : base (name)
        {
        }

        public static new FreeVariable Make (Symbol name)
        {
            return new FreeVariable (name);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("FreeVariable.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif

            //if (this.lastCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.lastCell, this.varname))
                    throw new NotImplementedException ("Error with free variable " + this.varname);
            //}
            return this.lastCell.GetValue (out answer);
        }
    }
}
