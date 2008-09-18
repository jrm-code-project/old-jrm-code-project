using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;


// Variable lookup is the most time critical thing
// in the interpreter after continuation management.
namespace Microcode
{
    [Serializable]
    sealed class Access : SCode, ISystemPair
    {
#if DEBUG
        [NonSerialized]
        static long evaluationCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly string var;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode env;

        public Access (object env, string name)
            : base (TC.ACCESS)
        {
            this.var = name;
            this.env = EnsureSCode (env);
        }

        [SchemePrimitive ("ACCESS?", 1, true)]
        public static bool IsAccess (out object answer, object arg0)
        {
            answer = arg0 is Access;
            return false;
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

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            return new Access (this.env.Bind (ctenv),
                               this.var);
        }
#if DEBUG
        public override string Key ()
        {
            throw new NotImplementedException ();
        }
#endif


        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Access.evaluationCount += 1;
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

        public override IList<object> FreeVariables ()
        {
            return this.env.FreeVariables ();
        }

        public override bool NeedsValueCells (object [] formals)
        {
            return this.env.NeedsValueCells (formals);
        }
    }

    [Serializable]
    sealed class Assignment : SCode, ISystemPair
    {
#if DEBUG
        [NonSerialized]
        static long evaluationCount;
#endif

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly string target;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode value;

        public Assignment (string target, SCode value)
            : base (TC.ASSIGNMENT)
        {
            if (target == null) throw new ArgumentNullException ("target");
            if (value == null) throw new ArgumentNullException ("value");
            this.target = target;
            this.value = value;
        }

        public Assignment (object target, object value)
            : base (TC.ASSIGNMENT)
        {
            Variable vtarget = target as Variable;
            if (vtarget != null) {
                this.target = vtarget.name;
                this.value = EnsureSCode (value);
            }
            else {
                string starget = target as string;
                if (starget != null) {
                    this.target = starget;
                    this.value = EnsureSCode (value);
                }
                else
                    throw new NotImplementedException ();
            }
        }

        public string Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.target;
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
                return  new Variable (this.target);
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

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optVal = this.value.Bind (ctenv);
            return optVal == this.value
                ? this
                : new Assignment (this.target, optVal);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Assignment.evaluationCount += 1;
            Warm ();
#endif
            Control expr = this.value;
            Environment env = environment;
            object newValue = null;
            while (expr.EvalStep (out newValue, ref expr, ref env)) { };
            if (newValue == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new AssignmentFrame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            if (environment.Assign (out answer, this.target, newValue)) throw new NotImplementedException ();
            return false;
        }

        public override IList<object> FreeVariables ()
        {
            IList<object> answer = new List<object> ();
            foreach (object var in this.value.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            if (!answer.Contains (this.target)) answer.Add (this.target);
            return answer;
        }


 
        public override bool NeedsValueCells (object [] formals)
        {
            return formals.Contains<object> (this.target)
                || this.value.NeedsValueCells (formals);
        }
#if DEBUG
        public override string Key ()
        {
            throw new NotImplementedException ();
        }
#endif
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

    [Serializable]
    class Variable : SCode, ISystemHunk3
    {
#if DEBUG
        [NonSerialized]
        static long evaluationCount;
        [NonSerialized]
        protected readonly bool breakOnReference;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly string name;

        public Variable (string name)
            : base (TC.VARIABLE)
        {
            if (name == null)
                throw new ArgumentNullException ("name");
            this.name = name;
#if DEBUG
           if (name == "no symbol has this name"
            //   || name == "copy-record"
            //   || name == "deferred-unparser-methods"
            //   || name == "microcode-identification"
            //   || name == "extend-package-environment"
               )
                breakOnReference = true;
#endif
        }

        public Variable (Hunk3 init)
            : base (TC.VARIABLE)
        {
            this.name = (string) init.Cxr0;
        }

        protected string Name
        {
            get
            {
                return this.name;
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
                return this.name;
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

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            VariableInfo info = ctenv.Lookup (this.name);
            GlobalVariableInfo ginfo = info as GlobalVariableInfo;
            if (ginfo != null) 
                return new GlobalVariable (this.name);
            FreeVariableInfo fvinfo = info as FreeVariableInfo;
            if (fvinfo != null) {
                return new FreeVariable (this.name, fvinfo.Environment);
            }
            else {
                TopLevelVariableInfo tlinfo = info as TopLevelVariableInfo;
                if (tlinfo != null)
                    return new TopLevelVariable (this.name, tlinfo.ValueCell);
                LexicalVariableInfo lvinfo = info as LexicalVariableInfo;
                if (lvinfo != null)
                    return LexicalVariable.Make (this.name, lvinfo);
            }
            return this;
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Variable.evaluationCount += 1;
            Warm ();
            Debug.WriteLineIf (Primitive.Noisy, this.Name);
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            if (environment.DeepSearch (out value, this.name)) throw new NotImplementedException ();
            return false;
        }

        public override IList<object> FreeVariables ()
        {
            return new List<object> { this.name };
        }

        public override bool NeedsValueCells (object [] formals)
        {
            return false;
        }

#if DEBUG
        public override string Key ()
        {
            return "(variable " + this.name + ")";
        }
#endif
    }

    /// <summary>
    /// A FreeVariable is one that we know is not lexically visible at
    /// binding time.  This means that we must deep search for it, but we
    /// can skip the lexical frames.  We cannot cache the value cell unless
    /// it becomes an incremental in the bindingEnvironment.
    /// </summary>
    [Serializable]
    sealed class FreeVariable : Variable
    {
#if DEBUG
        [NonSerialized]
        static long evaluationCount = 0;
        [NonSerialized]
        static long cacheMisses = 0;

        static long CacheHits
        {
            get
            {
                return evaluationCount - cacheMisses;
            }
        }
#endif
        ValueCell cell;
        Dictionary <object,ValueCell> incrementals;
        Environment parentEnvironment;

        public FreeVariable (string name, Environment bindingEnvironment)
            : base (name)
        {
            this.parentEnvironment = bindingEnvironment.Closure.Environment;
            this.incrementals = bindingEnvironment.Incrementals;
            this.incrementals.TryGetValue (this.name, out this.cell);
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            FreeVariable.evaluationCount += 1;
            Warm ();
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            if (this.breakOnReference) {
                Debugger.Break ();
            }

#endif
            if (this.cell != null 
                || this.incrementals.TryGetValue (this.name, out this.cell)) {
                if (this.cell.GetValue (out value)) throw new NotImplementedException ();
                return false;
            }
#if DEBUG
            FreeVariable.cacheMisses += 1;
#endif
            if (this.parentEnvironment.DeepSearch (out value, this.name)) throw new NotImplementedException ();
            return false;
        }
#if DEBUG
        public override string Key ()
        {
            return "(free-variable " + this.name + ")";
        }
#endif
    }

    [Serializable]
    sealed class GlobalVariable : Variable
    {
#if DEBUG
        [NonSerialized]
        static long evaluationCount;
#endif


        public GlobalVariable (string name)
            : base (name)
        {
        }



        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            GlobalVariable.evaluationCount += 1;
            Warm ();
            Debug.WriteLineIf (Primitive.Noisy, this.Name);
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            if (environment.DeepSearch (out value, this.name)) throw new NotImplementedException ();
            return false;
        }

        public override IList<object> FreeVariables ()
        {
            return new List<object> { this.name };
        }

        public override bool NeedsValueCells (object [] formals)
        {
            return false;
        }
#if DEBUG
        public override string Key ()
        {
            return "(global " + this.name + ")";
        }
#endif
    }

    [Serializable]
    class LexicalVariable : Variable
    {
#if DEBUG
        [NonSerialized]
        static long evaluationCount;
        static long [] histogram = new long [32];
#endif
        int depth;
        protected int offset;

        public LexicalVariable (string name, LexicalVariableInfo lvinfo)
            : base (name)
        {
            this.depth = lvinfo.Depth;
            this.offset = lvinfo.Offset;
        }

        public static SCode Make (string name, LexicalVariableInfo lvinfo)
        {
            if (lvinfo.Depth == 0)
                return Argument.Make (name, lvinfo.Offset);
            if (lvinfo.Depth == 1)
                return LexicalVariable1.Make (name, lvinfo);
            else {
                SCode answer;
#if DEBUG
                string key = "(LexicalVariable " + name + " " + lvinfo.Depth.ToString () + " " + lvinfo.Offset.ToString () + ")";
                if (SCode.hashConsTable.TryGetValue (key, out answer)) return answer;
#endif
                answer = new LexicalVariable (name, lvinfo);
#if DEBUG
                SCode.hashConsTable.Add (key, answer);
#endif
                return answer;
            }
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            LexicalVariable.evaluationCount += 1;
            LexicalVariable.histogram [this.depth] += 1;
            Warm ();
            Debug.WriteLineIf (Primitive.Noisy, this.Name);
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            //if (environment.DeepSearch (out value, this.name)) throw new NotImplementedException ();

            //object otherValue;

            //if (environment.LexicalRef (out otherValue, this.name, this.depth, this.offset))
            //    throw new NotImplementedException ();
            //if (otherValue != value)
            //    throw new NotImplementedException ("Values did not match!");
            if (environment.LexicalRef (out value, this.name, this.depth, this.offset))
                throw new NotImplementedException ("Error on lookup of " + this.name);
            return false;
        }

        public override IList<object> FreeVariables ()
        {
            return new List<object> { this.name };
        }

        public override bool NeedsValueCells (object [] formals)
        {
            return false;
        }
#if DEBUG
        public override string Key ()
        {
            return "(LexicalVariable " + this.name + " " + this.depth.ToString () + " " + this.offset.ToString () + ")";
        }
#endif
    }
 
    sealed class LexicalVariable1 : LexicalVariable
    {
#if DEBUG
        [NonSerialized]
        static long evaluationCount;
#endif

        LexicalVariable1 (string name, LexicalVariableInfo lvinfo)
            : base (name, lvinfo)
        {
        }

        public static new SCode Make (string name, LexicalVariableInfo lvinfo)
        {
            SCode answer; 
#if DEBUG
            string key = "LexicalVariable1 " + name + lvinfo.Offset.ToString ();

            if (SCode.hashConsTable.TryGetValue (key, out answer)) return answer;
#endif
            answer = new LexicalVariable1 (name, lvinfo);
#if DEBUG
            SCode.hashConsTable.Add (key, answer);
#endif
            return answer;
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            LexicalVariable1.evaluationCount += 1;
            Warm ();
            Debug.WriteLineIf (Primitive.Noisy, this.Name);
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            //if (environment.DeepSearch (out value, this.name)) throw new NotImplementedException ();

            //object otherValue;

            //if (environment.LexicalRef (out otherValue, this.name, this.depth, this.offset))
            //    throw new NotImplementedException ();
            //if (otherValue != value)
            //    throw new NotImplementedException ("Values did not match!");
            if (environment.LexicalRef1 (out value, this.name, this.offset))
                throw new NotImplementedException ("Error on lookup of " + this.name);
            return false;
        }

        public override IList<object> FreeVariables ()
        {
            return new List<object> { this.name };
        }

        public override bool NeedsValueCells (object [] formals)
        {
            return false;
        }
    }

    /// <summary>
    /// A TopLevelVariable is one that we find in the binding-time environment.
    /// It cannot be shadowed (except by us), so we can cache the value cell.
    /// </summary>
    [Serializable]
    sealed class TopLevelVariable : Variable
    {
#if DEBUG
        [NonSerialized]
        static long evaluationCount = 0;
#endif
        ValueCell cell;

        public TopLevelVariable (string name, ValueCell cell)
            : base (name)
        {
            this.cell = cell;
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            TopLevelVariable.evaluationCount += 1;
            Warm ();
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            if (this.breakOnReference) {
                Debugger.Break ();
            }

#endif
            if (this.cell.GetValue (out value)) throw new NotImplementedException ();
            return false;
        }
#if DEBUG
        public override string Key ()
        {
            return "(top-level-variable " + this.name + ")";
        }
#endif
    }

    [Serializable]
    class Argument : Variable
    {
#if DEBUG
        [NonSerialized]
        static long evaluationCount = 0;
        [NonSerialized]
        static int [] histogram = new int [53];
#endif
        int offset;

        protected Argument (string name, int offset)
            : base (name)
        {
            this.offset = offset;
        }

        static public Variable Make (string name, int offset)
        {
#if DEBUG
            if (offset >= histogram.Length)
                throw new NotImplementedException ();
#endif
            if (offset == 0)
                return new Argument0 (name);
            else if (offset == 1)
                return new Argument1 (name);
            else
                return new Argument (name, offset);
        }


        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Argument.evaluationCount += 1;
            Warm ();
            Argument.histogram [offset] += 1;
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            value = environment.ArgumentValue (offset);
            return false;
        }
#if DEBUG
        public override string Key ()
        {
            return "(argument " + this.offset.ToString () + " " + this.name + ")";
        }
#endif
    }

    [Serializable]
    sealed class Argument0 : Argument
    {
#if DEBUG
        [NonSerialized]
        static long evaluationCount = 0;
#endif
        internal Argument0 (string name)
            : base (name, 0)
        {
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Argument0.evaluationCount += 1;
            Warm ();
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            value = environment.Argument0Value;
            return false;
        }
    }

    [Serializable]
    sealed class Argument1 : Argument
    {
#if DEBUG
        [NonSerialized]
        static long evaluationCount = 0;
#endif
        internal Argument1 (string name)
            : base (name, 1)
        {
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Argument1.evaluationCount += 1;
            Warm ();
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            value = environment.Argument1Value;
            return false;
        }
    }
}
