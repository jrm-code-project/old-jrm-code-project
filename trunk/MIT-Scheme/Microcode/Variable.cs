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
    class Access : SCode, ISystemPair
    {
        public readonly Symbol var;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly SCode env;

        protected Access (SCode env, Symbol name)
            : base (TC.ACCESS)
        {
            this.var = name;
            this.env = EnsureSCode (env);
        }

        static public Access Make (SCode env, Symbol name)
        {
            return
                (! Configuration.EnableAccessOptimization) ? new Access (env, name) :
                (env is Quotation) ? AccessQ.Make ((Quotation) env, name) :
                new Access (env, name);
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

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode optEnv = this.env.Bind (ctenv);
            return optEnv == this.env
                ? this
                : new Access (optEnv, this.var);
        }

        public override bool CallsTheEnvironment ()
        {
            return this.env.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Access.EvalStep");
            noteCalls (this.env);
#endif           
            Control expr = this.env;
            Environment env = environment;
            object ev = null;
            while (expr.EvalStep (out ev, ref expr, ref env)) { };
            if (ev == Interpreter.UnwindStack) throw new NotImplementedException ();

            Environment accessenv = Environment.ToEnvironment (ev);
            if (accessenv.DeepSearch (out answer, this.var)) throw new NotImplementedException ();
            return false;
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            return this.env.MutatesAny (formals);
        }
        public override bool Uses (Symbol formal)
        {
            return this.env.Uses (formal);
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

        public override SCode Bind (LexicalMap ctenv)
        {
            return this;
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
        public override bool Uses (Symbol formal)
        {
            return false;
        }
    }



    [Serializable]
    class Assignment : SCode, ISerializable, ISystemPair
    {
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
            : base (TC.ASSIGNMENT)
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

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode optVal = this.value.Bind (ctenv);
            return (optVal == this.value) ? this : Assignment.Make (this.target, optVal);
            //BoundVariable boundTarget = ctenv.Bind (this.target.Name);
            //if (boundTarget is Argument)
            //    return AssignArg.Make ((Argument) boundTarget, optVal);
            //else
            //    return Assignment.Make (this.target, optVal);
        }

        public override bool CallsTheEnvironment ()
        {
            return this.value.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.value);
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

        public override bool Uses (Symbol formal)
        {
            return this.target.Name == formal ||
                this.value.Uses (formal);
        }

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (AssignmentDeserializer));
            info.AddValue ("target", this.target);
            info.AddValue ("value", this.value);
        }
    }

    [Serializable]
    internal sealed class AssignmentDeserializer : IObjectReference
    {
        Variable target;
        SCode value;

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

    class AssignArg : Assignment
    {
#if DEBUG
        static Histogram<Type> valueTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int offset;

        AssignArg (Argument target, SCode value)
            : base (target, value)
        {
            this.offset = target.Offset;
        }

        static public SCode Make (Argument target, SCode value)
        {
            return
                new AssignArg (target, value);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.value);
            valueTypeHistogram.Note (this.valueType);
            SCode.location = "AssignArg.EvalStep";
#endif
            Control expr = this.value;
            Environment env = environment;
            object newValue;
            while (expr.EvalStep (out newValue, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "AssignArg.EvalStep.1";
#endif
            if (newValue == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new AssignmentFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (environment.AssignArg (out answer, this.offset, newValue)) throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class AssignmentQ : Assignment
    {
        public readonly object assignmentValue;

        AssignmentQ (Variable target, Quotation value)
            : base (target, value)
        {
           this.assignmentValue = value.Quoted;
        }

        static public SCode Make (Variable target, Quotation value)
        {
            return
                new AssignmentQ (target, value);
        }


        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("AssignmentQ.EvalStep");
#endif
            if (environment.Assign (out answer, this.target.Name, this.assignmentValue)) 
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    public class Variable : SCode, ISerializable, ISystemHunk3
    {
#if DEBUG
        [NonSerialized]
        public readonly bool breakOnReference;
#endif
        static Dictionary<Symbol,Variable> variableTable = new Dictionary<Symbol, Variable> ();

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Symbol varname;

        protected Variable (Symbol name)
            : base (TC.VARIABLE)
        {
            if (name == null)
                throw new ArgumentNullException ("ratorName");
            this.varname = name;
#if DEBUG
           if (name is Symbol &&
               (
               ((name.ToString()) == "no symbol has this name") ||
               //((name.ToString()) == "fixed-objects") ||
               //((name.ToString()) == "grow-table!") ||
               //(((string) name) == "cgen/expression") ||
               //(((string) name) == "make-conditional") ||
               //(((string) name) == "analyze-file") ||
               //(((string) name) == "sf-conditionally") ||
               //(((string) name) == "load-option") ||
               //(((string) name) == "string-copy") ||
               //(((string) name) == "guarantee-port-type") ||
               //((name.ToString()) == "extend-package-environment") ||
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

        public override SCode Bind (LexicalMap btenv)
        {
            return Configuration.EnableVariableOptimization ? btenv.Bind (this.Name) : this;
        }

        public override bool CallsTheEnvironment ()
        {
            return false;
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Variable.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            if (Configuration.EnableVariableOptimization && Configuration.EnableLexicalAddressing)
                throw new NotImplementedException ("Should not happen, variables should all be bound.");
            else if (environment.DeepSearch (out value, this.varname)) throw new NotImplementedException ("Variable DeepSeach failed for " + this.varname.ToString());
            else return false;
        }
    
        public override bool MutatesAny (Symbol [] formals)
        {
            return false;
        }

        public override bool Uses (Symbol formal)
        {
            return formal == this.Name;
        }
                
        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (VariableDeserializer));
            info.AddValue ("name", this.varname);
        }
    }

    [Serializable]
    internal sealed class VariableDeserializer : IObjectReference
    {
        // This object has no fields (although it could).
        Symbol name;

        // GetRealObject is called after this object is deserialized.
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
    public abstract class BoundVariable : Variable
    {
        protected readonly LambdaBase binder;

        protected BoundVariable (Symbol name, LambdaBase binder)
            : base (name)
        {
            this.binder = binder;
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ("already bound");
        }

        internal LambdaBase Binder
        {
            get
            {
                return this.binder;
            }
        }

        internal abstract BoundVariable IncreaseStaticLexicalDepth ();
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

//        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            Debug.WriteLineIf (Primitive.Noisy, this.Name);
//            if (this.breakOnReference) {
//                Debugger.Break ();
//            }
//#endif
//            //if (environment.DeepSearch (out value, this.Name))
//                throw new NotImplementedException ("Error on lookup of " + this.Name);
//            return false;
//        }
//    }

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

        protected LexicalVariable (Symbol name, LambdaBase binder, int depth, int offset)
            : base (name, binder)
        {
            this.depth = depth;
            this.offset = offset;
        }

        public int Depth { get { return this.depth; } }
        public int Offset { get { return this.offset; } }

        public static LexicalVariable Make (Symbol name, LambdaBase binder, int depth, int offset)
        {
            if (Configuration.EnableLexical1 && depth == 1)
                return LexicalVariable1.Make (name, binder, offset);
            else
                return new LexicalVariable (name, binder, depth, offset);
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("LexicalVariable.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            if (environment.FastLexicalRef (out value, this.Name, this.depth, this.offset))
                throw new NotImplementedException ("Error on lookup of " + this.Name);
            return false;
        }

        internal override BoundVariable IncreaseStaticLexicalDepth ()
        {
            return Make (this.Name, this.binder, this.depth + 1, this.offset);
        }
    }

    /// <summary>
    /// An argument variable is bound in the immediately enclosing lambda.
    /// We can access it by simply grabbing it from the topmost frame
    /// in the environment.
    /// </summary>
    [Serializable]
    class Argument : LexicalVariable
    {
        protected Argument (Symbol name, LambdaBase binder, int offset)
            : base (name, binder, 0, offset)
        {
        }

        static public Argument Make (Symbol name, LambdaBase binder, int offset)
        {
            switch (offset) {
                case 0: return new Argument0 (name, binder);
                case 1: return new Argument1 (name, binder);
                default: return new Argument (name, binder, offset);
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
            value = environment.ArgumentValue (offset);
            return false;
        }
    }

    /// <summary>
    /// Argument zero is the most popular argument.
    /// </summary>
    [Serializable]
    sealed class Argument0 : Argument
    {
        internal Argument0 (Symbol name, LambdaBase binder)
            : base (name, binder, 0)
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
        internal Argument1 (Symbol name, LambdaBase binder)
            : base (name, binder, 1)
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

//        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            Debug.WriteLineIf (Primitive.Noisy, this.Name);
//            if (this.breakOnReference) {
//                Debugger.Break ();
//            }
//#endif
//            if (environment.DangerousLexicalRef (out value, this.ratorName, this.shadowDepth, this.randDepth, this.randOffset))
//                throw new NotImplementedException ("Error on lookup of " + this.ratorName);
//            return false;
//        }
//    }

    /// <summary>
    /// A LexicalVariable1 is one where we know where the binding cell will
    /// be in the parent environment.  Just grab it.
    /// </summary>
    [Serializable]
    sealed class LexicalVariable1 : LexicalVariable
    {
        LexicalVariable1 (Symbol name, LambdaBase binder, int offset)
            : base (name, binder, 1, offset)
        {
        }

        public static LexicalVariable1 Make (Symbol name, LambdaBase binder, int offset)
        {
            return new LexicalVariable1 (name, binder, offset);
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("LexicalVariable1.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            if (environment.FastLexicalRef1 (out value, this.Name, this.offset))
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
        public FreeVariable (Symbol name)
            : base (name, null)
        {
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


        internal override BoundVariable IncreaseStaticLexicalDepth ()
        {
            throw new NotImplementedException ();
        }
    }

    /// <summary>
    /// A FreeVariable is one that we know is not lexically visible at
    /// binding time.  This means that we must deep search for it, but we
    /// can skip the lexical frames.  We cannot cache the value cell unless
    /// it becomes an incremental in the bindingEnvironment.
    /// </summary>
    [Serializable]
    sealed class DeepVariable : BoundVariable, ISerializable
    {
#if DEBUG
        static Histogram<Symbol> variableNameHistogram = new Histogram<Symbol> ();
#endif
        Environment baseEnvironment;
        Environment [] environmentChain;
        ValueCell cachedValueCell;
        int cachedValueCellDepth;

        public DeepVariable (Symbol name, Environment baseEnvironment)
            : base (name, null)
        {
            this.baseEnvironment = baseEnvironment;
            this.environmentChain = new Environment [baseEnvironment.GetDepth()];
            if (environmentChain.Length > 4) 
                Debugger.Break ();
            for (int i = environmentChain.Length - 1; i > 0; i--) {
                environmentChain [i] = baseEnvironment.GetAncestorEnvironment (i);
            }
            environmentChain [0] = baseEnvironment;
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            variableNameHistogram.Note (this.varname);
            if (this.breakOnReference) {
                Debugger.Break ();
            }
            SCode.location = "DeepVariable.EvalStep";
#endif
            if (this.cachedValueCell == null) {
                // First time through, 
                // search the environment chain to find the nearest
                // place the value cell exists.
                for (int i = 0; i < environmentChain.Length; i++) {
                    Environment env = environmentChain [i];
                    ValueCell vc = env.SearchFixed (this.varname);
                    if (vc != null) {
                        this.cachedValueCell = vc;
                        this.cachedValueCellDepth = i;
                        break;
                    }
                    vc = env.SearchIncrementals (this.varname);
                    if (vc != null) {
                        this.cachedValueCell = vc;
                        this.cachedValueCellDepth = i;
                        break;
                    }
                }
            }
            else {
                // On the subsequent passes, we might shadow the variable
                // with incrementals, but we cannot add fixed bindings.
                for (int i = 0; i < this.cachedValueCellDepth; i++) {
                    Environment env = environmentChain [i];
                    ValueCell vc = env.SearchIncrementals (this.varname);
                    if (vc != null) {
                        // Someone shadowed it.
                        throw new NotImplementedException ();
                    }
                }
                // if we fall through, no one shadowed us and we can
                // simply drop into the case where we fetch from the cell.
            }

            if (this.cachedValueCell.GetValue (out value))
                throw new NotImplementedException ();
            return false;
        }

        internal override BoundVariable IncreaseStaticLexicalDepth ()
        {
            return this;
        }
    }


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

//        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
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
    /// A TopLevelVariable is one that we find in the binding-time environment.
    /// It cannot be shadowed (except by us), so we can cache the value cell.
    /// </summary>
    [Serializable]
    sealed class TopLevelVariable : BoundVariable
    {
#if DEBUG
        static Histogram<object> nameHistogram = new Histogram<object>();
#endif
        public readonly ValueCell cell;

        public TopLevelVariable (Symbol name, ValueCell cell)
            : base (name, null)
        {
            this.cell = cell;
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            nameHistogram.Note (this.varname);
            if (this.breakOnReference) {
                Debugger.Break ();
            }
            SCode.location = "TopLevelVariable.EvalStep";
#endif
            if (this.cell.GetValue (out value))
                throw new NotImplementedException ();
            return false;
        }

        internal override BoundVariable IncreaseStaticLexicalDepth ()
        {
            return this;
        }
    }

//    /// <summary>
//    /// A TopLevelVariable is one that we find in the binding-time environment.
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

//        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
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
    /// A global variable is one we know is bound in the global environment
    /// and cannot be shadowed.  If we have a binding cell, we can just fetch
    /// the value from there.
    /// </summary>
    [Serializable]
    sealed class GlobalVariable : BoundVariable
    {
        ValueCell cell;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Environment environment;

        public GlobalVariable (Symbol name, Environment environment)
            : base (name, null)
        {
            this.environment = environment;
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("GlobalVariable.EvalStep");
            if (this.breakOnReference) {
                Debugger.Break ();
            }

#endif
            if (this.cell == null)
                this.cell = this.environment.GetValueCell (this.varname);
            if (this.cell.GetValue (out value))
                throw new NotImplementedException ("Error on lookup of " + this.varname);
            return false;
        }

        internal override BoundVariable IncreaseStaticLexicalDepth ()
        {
            // Global variables are just fetched from the value cell,
            // so we need do nothing.
            return this;
        }
    }

//    /// <summary>
//    /// A dangerous global variable is one we know is bound in the global environment
//    /// but could be shadowed.  If we have a binding cell, we can just fetch
//    /// the value from there once we test the incrementals in the outer environments.
//    /// </summary>
//    [Serializable]
//    sealed class DangerousGlobalVariable : BoundVariable
//    {
//        //ValueCell cell;
//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        readonly Environment environment;
//        readonly int safeDepth;
//        readonly int randDepth;

//        DangerousGlobalVariable (object lambdaName, Environment environment, int safeDepth, int randDepth)
//            : base (lambdaName)
//        {
//            this.environment = environment;
//            this.safeDepth = safeDepth;
//            this.randDepth = randDepth;
//        }

//        public static DangerousGlobalVariable Make (object lambdaName, Environment environment, int safeDepth, int randDepth)
//        {
//            return new DangerousGlobalVariable (lambdaName, environment, safeDepth, randDepth);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ();
//        }

//        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            Debug.WriteLineIf (Primitive.Noisy, this.ratorName);
//            if (this.breakOnReference) {
//                Debugger.Break ();
//            }

//#endif
//            //if (this.cell == null)
//            //    this.cell = this.environment.GetValueCell (this.ratorName);
//            //if (this.cell.GetValue (out value))
//                throw new NotImplementedException ("Error on lookup of " + this.ratorName);
//        }
//    }

}
