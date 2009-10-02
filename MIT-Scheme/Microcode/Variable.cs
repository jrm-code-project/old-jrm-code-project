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

        public override ICollection<Symbol> ComputeFreeVariables ()
        {
            return this.env.ComputeFreeVariables ();
        }

        internal override PartialResult PartialEval (Environment environment)
        {
            PartialResult penv = this.env.PartialEval (environment);
            return new PartialResult (penv.Residual == this.env ? this : Access.Make (penv.Residual, this.var));
        }

        public override int LambdaCount ()
        {
            return this.env.LambdaCount ();
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

        public override ICollection<Symbol> ComputeFreeVariables ()
        {
            SCode v = this.value;
            ICollection<Symbol> fv = v.ComputeFreeVariables ();
            List<Symbol> x = new List<Symbol> (fv.Union<Symbol> (singletonFreeVariable (this.target.Name)));
            return x;
        }

        internal override PartialResult PartialEval (Environment environment)
        {
            PartialResult val = this.value.PartialEval (environment);
            return new PartialResult (this.value == val.Residual ? this : Assignment.Make (this.target, val.Residual));
        }

        public override int LambdaCount ()
        {
            return this.value.LambdaCount ();
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
    class Variable : SCode, ISerializable, ISystemHunk3
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

        public override ICollection<Symbol> ComputeFreeVariables ()
        {
            return singletonFreeVariable (this.varname);
        }

        PartialResult makeArgument (int offset)
        {
            return new PartialResult (Argument.Make (this.varname, offset));
        }

        PartialResult makeStatic (int offset)
        {
            return new PartialResult (StaticVariable.Make (this.varname, offset));
        }

        PartialResult makeGlobal (GlobalEnvironment env)
        {
            return new PartialResult (GlobalVariable.Make (this.varname, env, null));
        }

        PartialResult makeFree (int skipDepth)
        {
            return new PartialResult (FreeVariable.Make (this.varname, skipDepth));
        }

        internal override PartialResult PartialEval (Environment environment)
        {
            return environment.LocateVariable<PartialResult> (this.varname,
                makeArgument,
                makeStatic,
                makeGlobal,
                makeFree);
        }

        public override int LambdaCount ()
        {
            return 0;
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


    /// <summary>
    /// A static variable is bound in the static block of a closure.
    /// </summary>
    [Serializable]
    class StaticVariable : BoundVariable
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly int offset;
#if DEBUG
        static Histogram<int> staticOffsets = new Histogram<int> ();
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
    sealed class FreeVariable : Variable
    {
        readonly int depth;

        FreeVariable (Symbol name, int depth)
            : base (name)
        {
            this.depth = depth;
        }

        public static FreeVariable Make (Symbol name, int depth)
        {
            return new FreeVariable (name, depth);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("FreeVariable.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            //if (Configuration.EnableVariableOptimization && Configuration.EnableLexicalAddressing)
            //    throw new NotImplementedException ("Should not happen, variables should all be bound.");
            //Environment start = environment;
            //for (int i = 0; i < this.depth; i++)
            //    start = start.Closure.Environment;
            //if (start.DeepSearch (out answer, this.varname)) {
            //    throw new NotImplementedException ();
            //}

            Environment baseEnvironment = environment.GetBaseEnvironment ();
            if (baseEnvironment.FreeReference (out answer, this.varname)) {
                throw new NotImplementedException ();
            }
            return false;
        }
    }
}
