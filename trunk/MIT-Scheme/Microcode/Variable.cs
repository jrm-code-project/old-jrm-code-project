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

        public override bool MutatesAny (object [] formals)
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

        public override SCode Substitute (object name, object newObject)
        {
            throw new NotImplementedException ();
        }
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
            Warm ("Assignment.EvalStep");
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

            if (environment.Assign (out answer, this.target.Name, newValue)) throw new NotImplementedException ();
            return false;
        }

        public override bool MutatesAny (object [] formals)
        {
            return formals.Contains<object> (this.target.Name)
                || this.value.MutatesAny (formals);
        }

        public override SCode Substitute (object name, object newObject)
        {
            throw new NotImplementedException ();
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
    public class Variable : SCode, ISystemHunk3
    {
#if DEBUG
        [NonSerialized]
        protected readonly bool breakOnReference;
#endif
        static Dictionary<object,Variable> variableTable = new Dictionary<object, Variable> ();

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly object varname;

        protected Variable (object name)
            : base (TC.VARIABLE)
        {
            if (name == null)
                throw new ArgumentNullException ("varname");
            this.varname = name;
#if DEBUG
           if (name is string &&
               (((string) name) == "no symbol has this lambdaName"
               || ((string)name) == "lambda-wrap-lambdaBody!")
               //|| lambdaName == "syntax*"
               //|| lambdaName == "hash-table/get"
            //   || lambdaName == "error"
            //   || lambdaName == "loop"
            //   || lambdaName == "copy-record"
            //   || lambdaName == "deferred-unparser-methods"
            //   || lambdaName == "microcode-identification"
            //   || lambdaName == "extend-package-environment"
               )
                breakOnReference = true;
#endif  
        }

        static public Variable Make (object name)
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
            return Variable.Make (init.Cxr0);
        }

        public object Name
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
            return btenv.Bind (this.Name);
        }

        public override bool CallsTheEnvironment ()
        {
            return false;
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Variable.EvalStep");
            Debug.WriteIf (Primitive.Noisy, this.Name);
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            if (Configuration.EnableLexicalAddressing)
                throw new NotImplementedException ("Should not happen, variables should all be bound.");
            else if (environment.DeepSearch (out value, this.varname)) throw new NotImplementedException ();
            else return false;
        }
    
        public override bool MutatesAny (object [] formals)
        {
            return false;
        }

        public override SCode Substitute (object name, object newObject)
        {
            throw new NotImplementedException ();
        }
    }

    /// <summary>
    /// A Bound variable has some idea of how to get at the value cell
    /// other than by using a deep search.
    /// </summary>
    public abstract class BoundVariable : Variable
    {
        protected readonly LambdaBase binder;

        protected BoundVariable (object name, LambdaBase binder)
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
        protected readonly int depth;
        protected readonly int offset;

        protected LexicalVariable (object name, LambdaBase binder, int depth, int offset)
            : base (name, binder)
        {
            this.depth = depth;
            this.offset = offset;
        }

        public int Depth { get { return this.depth; } }
        public int Offset { get { return this.offset; } }

        public static LexicalVariable Make (object name, LambdaBase binder, int depth, int offset)
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
            Debug.WriteLineIf (Primitive.Noisy, this.Name);
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
        protected Argument (object name, LambdaBase binder, int offset)
            : base (name, binder, 0, offset)
        {
        }

        static public Argument Make (object name, LambdaBase binder, int offset)
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
        internal Argument0 (object name, LambdaBase binder)
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
        internal Argument1 (object name, LambdaBase binder)
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
//        readonly int argDepth;
//        readonly int argOffset;

//        DangerousLexicalVariable (object lambdaName, int shadowDepth, int argDepth, int argOffset)
//            : base (lambdaName)
//        {
//            this.shadowDepth = shadowDepth;
//            this.argDepth = argDepth;
//            this.argOffset = argOffset;
//        }

//        public static DangerousLexicalVariable Make (object lambdaName, int shadowDepth, int argDepth, int argOffset)
//        {
//            return new DangerousLexicalVariable (lambdaName, shadowDepth, argDepth, argOffset);
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
//            if (environment.DangerousLexicalRef (out value, this.varname, this.shadowDepth, this.argDepth, this.argOffset))
//                throw new NotImplementedException ("Error on lookup of " + this.varname);
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
        LexicalVariable1 (object name, LambdaBase binder, int offset)
            : base (name, binder, 1, offset)
        {
        }

        public static LexicalVariable1 Make (object name, LambdaBase binder, int offset)
        {
            return new LexicalVariable1 (name, binder, offset);
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("LexicalVariable1.EvalStep");
            Debug.WriteLineIf (Primitive.Noisy, this.Name);
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
        public FreeVariable (object name)
            : base (name, null)
        {
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("FreeVariable.EvalStep");
            Debug.WriteLineIf (Primitive.Noisy, this.varname);
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
    sealed class DeepVariable : BoundVariable
    {
#if DEBUG
        static Histogram<string> variableNameHistogram = new Histogram<string> ();
#endif
        Environment baseEnvironment;

        public DeepVariable (object name, Environment baseEnvironment)
            : base (name, null)
        {
            this.baseEnvironment = baseEnvironment;
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            variableNameHistogram.Note ((string)this.varname);
            if (this.breakOnReference) {
                Debugger.Break ();
            }
            SCode.location = "DeepVariable.EvalStep";
#endif
            // Don't know where it is.
            if (baseEnvironment.FreeRef (out value, this.varname)) throw new NotImplementedException ();
            else return false;
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
//        readonly int argDepth;

//        DangerousFreeVariable (object lambdaName, Environment env, int argDepth)
//            : base (lambdaName)
//        {
//            this.env = env;
//            this.argDepth = argDepth;
//        }

//        public static DangerousFreeVariable Make (object lambdaName, Environment env, int argDepth)
//        {
//            return new DangerousFreeVariable (lambdaName, env, argDepth);
//        }

//        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            Debug.WriteLineIf (Primitive.Noisy, this.varname);
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

        public TopLevelVariable (object name, ValueCell cell)
            : base (name, null)
        {
            this.cell = cell;
        }

        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("TopLevelVariable.EvalStep");
            nameHistogram.Note (this.varname);
            Debug.WriteLineIf (Primitive.Noisy, this.varname);
            if (this.breakOnReference) {
                Debugger.Break ();
            }
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
//            Debug.WriteLineIf (Primitive.Noisy, this.varname);
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

        public GlobalVariable (object name, Environment environment)
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
            Debug.WriteLineIf (Primitive.Noisy, this.varname);
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
//        readonly int argDepth;

//        DangerousGlobalVariable (object lambdaName, Environment environment, int safeDepth, int argDepth)
//            : base (lambdaName)
//        {
//            this.environment = environment;
//            this.safeDepth = safeDepth;
//            this.argDepth = argDepth;
//        }

//        public static DangerousGlobalVariable Make (object lambdaName, Environment environment, int safeDepth, int argDepth)
//        {
//            return new DangerousGlobalVariable (lambdaName, environment, safeDepth, argDepth);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ();
//        }

//        public override bool EvalStep (out object value, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            Debug.WriteLineIf (Primitive.Noisy, this.varname);
//            if (this.breakOnReference) {
//                Debugger.Break ();
//            }

//#endif
//            //if (this.cell == null)
//            //    this.cell = this.environment.GetValueCell (this.varname);
//            //if (this.cell.GetValue (out value))
//                throw new NotImplementedException ("Error on lookup of " + this.varname);
//        }
//    }

}
