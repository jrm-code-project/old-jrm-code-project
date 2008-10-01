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

        public override SCode Bind (BindingTimeEnvironment ctenv)
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
            Warm ();
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

        public override bool MutatesAny (object [] formals)
        {
            return this.env.MutatesAny (formals);
        }

        public override bool UsesAny (object [] formals)
        {
            return this.env.UsesAny (formals);
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
    sealed class Assignment : SCode, ISystemPair
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Variable target;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode value;

        public Assignment (Variable target, SCode value)
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
                this.target = vtarget;
                this.value = EnsureSCode (value);
            }
            else
                throw new NotImplementedException ();

        }

        public string Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.target.name;
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

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optVal = this.value.Bind (ctenv);
            return optVal == this.value
                ? this
                : new Assignment (this.target, optVal);
        }

        public override bool CallsTheEnvironment ()
        {
            return this.value.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.value);
#endif
            Control expr = this.value;
            Environment env = environment;
            object newValue;
            while (expr.EvalStep (out newValue, ref expr, ref env)) { };
            if (newValue == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new AssignmentFrame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            if (environment.Assign (out answer, this.target.name, newValue)) throw new NotImplementedException ();
            return false;
        }

        public override bool MutatesAny (object [] formals)
        {
            return formals.Contains<object> (this.target.name)
                || this.value.MutatesAny (formals);
        }

        public override bool UsesAny (object [] formals)
        {
            return formals.Contains<object> (this.target.name)
                || this.value.UsesAny (formals);
        }
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
        protected readonly bool breakOnReference;
#endif
        static Dictionary<object,Variable> variableTable = new Dictionary<object, Variable> ();

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly string name;

        protected Variable (string name)
            : base (TC.VARIABLE)
        {
            if (name == null)
                throw new ArgumentNullException ("name");
            this.name = name;
#if DEBUG
           if (name == "no symbol has this name"
               || name == "syntax*"
               || name == "hash-table/get"
            //   || name == "error"
            //   || name == "loop"
            //   || name == "copy-record"
            //   || name == "deferred-unparser-methods"
            //   || name == "microcode-identification"
            //   || name == "extend-package-environment"
               )
                breakOnReference = true;
#endif
        }

        static public Variable Make (string name)
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
            return Variable.Make ((string) init.Cxr0);
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
            return Configuration.EnableLexicalAddressing
                ? ctenv.BindVariable (this.name)
                : this;
        }

        public override bool CallsTheEnvironment ()
        {
            return false;
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            Debug.WriteLineIf (Primitive.Noisy, this.Name);
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            if (Configuration.EnableLexicalAddressing)
                throw new NotImplementedException ("Should not happen, variables should all be bound.");
            else if (environment.DeepSearch (out value, this.name)) throw new NotImplementedException ();
            else return false;
        }

        public override bool MutatesAny (object [] formals)
        {
            return false;
        }

        public override bool UsesAny (object [] formals)
        {
            for (int i = 0; i < formals.Length; i++)
                if ((object) this.name == (object) formals [i])
                    return true;
            return false;
        }
    }

    /// <summary>
    /// A Bound variable has some idea of how to get at the value cell
    /// other than by using a deep search.
    /// </summary>
    abstract class BoundVariable : Variable
    {
        protected BoundVariable (string name)
            : base (name)
        {
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ("already bound");
        }

    }

    /// <summary>
    /// An argument variable is bound in the immediately enclosing lambda.
    /// We can access it by simply grabbing it from the topmost frame
    /// in the environment.
    /// </summary>
    [Serializable]
    class Argument : BoundVariable
    {
        protected readonly int offset;

        protected Argument (string name, int offset)
            : base (name)
        {
            this.offset = offset;
        }

        internal int Offset
        {
            get { return this.offset; }
        }

        static public Variable Make (string name, int offset)
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
            Warm ();
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
        internal Argument0 (string name)
            : base (name, 0)
        {
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
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
        internal Argument1 (string name)
            : base (name, 1)
        {
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            value = environment.Argument1Value;
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
        readonly int depth;
        readonly int offset;

        LexicalVariable (string name, int depth, int offset)
            : base (name)
        {
            this.depth = depth;
            this.offset = offset;
        }

        public int Depth { get { return this.depth; } }
        public int Offset { get { return this.offset; } }

        public static Variable Make (string name, int depth, int offset)
        {
            return new LexicalVariable (name, depth, offset);
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            Debug.WriteLineIf (Primitive.Noisy, this.Name);
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            if (environment.FastLexicalRef (out value, this.name, this.depth, this.offset))
                throw new NotImplementedException ("Error on lookup of " + this.name);
            return false;
        }
    }

    /// <summary>
    /// A DangerousLexicalVariable is one where we know where the binding cell
    /// is, but it could be shadowed.
    /// </summary>
    [Serializable]
    class DangerousLexicalVariable : Variable
    {
        readonly int shadowDepth;
        readonly int depth;
        readonly int offset;

        DangerousLexicalVariable (string name, int shadowDepth, int depth, int offset)
            : base (name)
        {
            this.shadowDepth = shadowDepth;
            this.depth = depth;
            this.offset = offset;
        }

        public static Variable Make (string name, int shadowDepth, int depth, int offset)
        {
            return new DangerousLexicalVariable (name, shadowDepth, depth, offset);
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            Debug.WriteLineIf (Primitive.Noisy, this.Name);
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            if (environment.DangerousLexicalRef (out value, this.name, this.shadowDepth, this.depth, this.offset))
                throw new NotImplementedException ("Error on lookup of " + this.name);
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
    sealed class FreeVariable : Variable
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Environment environment;

        FreeVariable (string name, Environment environment)
            : base (name)
        {
            this.environment = environment;
        }

        public static Variable Make (string name, Environment environment)
        {
            return new FreeVariable (name, environment);
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            if (this.breakOnReference) {
                Debugger.Break ();
            }

#endif
            // couldn't this be a lexical?
            if (this.environment.FreeRef (out value, this.name))
                throw new NotImplementedException ();
            return false;
        }

    }

    /// <summary>
    /// A DangerousFreeVariable is one that we know is not lexically visible at
    /// binding time, and could be shadowed.
    /// </summary>
    [Serializable]
    sealed class DangerousFreeVariable : Variable
    {
        readonly int shadowDepth;
        readonly int depth;

        DangerousFreeVariable (string name, int shadowingFrame, int depth)
            : base (name)
        {
            this.shadowDepth = shadowingFrame;
            this.depth = depth;
        }

        public static Variable Make (string name, int shadowingFrame, int depth)
        {
#if DEBUG
            // sanity check
            if (shadowingFrame >= depth) throw new NotImplementedException ();
#endif
            return new DangerousFreeVariable (name, shadowingFrame, depth);
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            if (this.breakOnReference) {
                Debugger.Break ();
            }

#endif
            if (environment.DangerousFreeRef(out value, this.name, this.shadowDepth, this.depth))
                throw new NotImplementedException ("Error on lookup of " + this.name);
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
        readonly ValueCell cell;

        TopLevelVariable (string name, ValueCell cell)
            : base (name)
        {
            this.cell = cell;
        }

        static public Variable Make (string name, ValueCell cell)
        {
            return new TopLevelVariable (name, cell);
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            return this.cell.GetValue (out value);
        }
    }

    [Serializable]
    sealed class GlobalVariable : Variable
    {
        ValueCell cell;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Environment environment;

        GlobalVariable (string name, Environment environment)
            : base (name)
        {
            this.environment = environment;
        }

        public static Variable Make (string name, Environment environment)
        {
            return new GlobalVariable (name, environment);
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            if (this.breakOnReference) {
                Debugger.Break ();
            }

#endif
            if (this.cell == null)
                this.cell = this.environment.GetValueCell (this.name);
            if (this.cell.GetValue(out value))
                throw new NotImplementedException ("Error on lookup of " + this.name);
            return false;
        }
    }
}
