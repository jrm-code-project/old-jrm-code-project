using System;
using System.Collections.Generic;
using System.Diagnostics;


// Variable lookup is the most time critical thing
// in the interpreter after continuation management.
namespace Microcode
{

    sealed class Access : SCode, ISystemPair
    {
#if DEBUG
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

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            return new Access (this.env.Bind (ctenv),
                               this.var);
        }


        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            Access.evaluationCount += 1;
#endif
            SCode expr = this.env;
            Environment env = environment;
            object ev = null;
            while (expr.EvalStep (out ev, ref expr, ref env)) { };
            if (ev == Interpreter.UnwindStack) throw new NotImplementedException ();

            Environment accessenv = Environment.ToEnvironment (ev);
            if (accessenv.DeepSearch (out answer, this.var)) throw new NotImplementedException ();
            return false;
        }

        public override HashSet<string> FreeVariables ()
        {
            return this.env.FreeVariables();
        }

        public override bool NeedsTheEnvironment ()
        {
            return this.env.NeedsTheEnvironment ();
        }
    }

    //sealed class AccessLookup : Continuation
    //{
    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly string name;

    //    public AccessLookup (Continuation parent, string name)
    //        : base (parent)
    //    {
    //        this.name = name;
    //    }

    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        Environment env = value as Environment;
    //        if (env == null)
    //            env = Environment.Global;
    //        object avalue;
    //        LookupDisposition disp = env.LookupVariable (0, this.name, out avalue);
    //        if (disp == LookupDisposition.OK)
    //            return interpreter.Return (avalue);
    //        throw new NotImplementedException ();
    //    }

    //    public override int FrameSize
    //    {
    //        get { throw new NotImplementedException (); }
    //    }
    //}


    sealed class Assignment : SCode, ISystemPair
    {
#if DEBUG
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

        //internal override object EvalStep (Interpreter interpreter, object etc)
        //{
        //    Assignment.evaluationCount += 1;
        //    return interpreter.EvalNewSubproblem (this.value, new AssignContinue (interpreter.Continuation, this, interpreter.Environment));
        //}

        #region ISystemPair Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            get
            {
                return new Variable (this.target);
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

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            SCode optVal = this.value.Bind (ctenv);
            return optVal == this.value
                ? this
                : new Assignment (this.target, optVal);
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            Assignment.evaluationCount += 1;
#endif
            SCode expr = this.value;
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

        public override HashSet<string> FreeVariables ()
        {
            HashSet<string> valueFree = this.value.FreeVariables ();
            valueFree.Add (this.target);
            return valueFree;
        }

        public override bool NeedsTheEnvironment ()
        {
            return this.value.NeedsTheEnvironment ();
        }
    }

    sealed class AssignmentFrame0 : SubproblemContinuation<Assignment>, ISystemVector
    {
        internal AssignmentFrame0 (Assignment expression, Environment environment)
            : base (expression, environment) { }

        public override bool Continue (out object answer, ref SCode expression, ref Environment environment, object value)
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


    //sealed class AssignContinue : Subproblem<Assignment>
    //{
    //    public AssignContinue (Continuation next, Assignment expression, Environment environment)
    //        : base (next, expression, environment)
    //    {
    //    }

    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        object oldValue;
    //        LookupDisposition disp = ((Environment) (this.Environment)).AssignVariable (this.Expression.Name, value, out oldValue);
    //        if (disp == LookupDisposition.OK)
    //            return interpreter.Return (oldValue);
    //        throw new NotImplementedException ();
    //    }

    //    public override int FrameSize
    //    {
    //        get { return 3; }
    //    }

    //    public override object FrameRef (int offset)
    //    {
    //        if (offset == 0)
    //            return ReturnCode.EXECUTE_ASSIGNMENT_FINISH;
    //        else
    //            throw new NotImplementedException ();
    //    }

    //}

    class Variable : SCode, ISystemHunk3
    {
#if DEBUG
        static long evaluationCount;
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

        string Name
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

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {

            int depth;
            int offset;
            if (ctenv.LexicalAddress (this.name, out depth, out offset)
                && depth == 0 
                && offset > -1)
                return Argument.Make (this.name, offset);

            Dictionary<string,Variable> variableTable = ctenv.VariableTable;
            Variable canonical = null;
            if (variableTable.TryGetValue (this.name, out canonical)) {
                return canonical;
            }
            else {
                variableTable.Add (this.name, this);
                return this;
            }
        }

        public override bool EvalStep (out object value, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            Variable.evaluationCount += 1;
            Debug.WriteLineIf (Primitive.Noisy, this.Name);
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            if (environment.DeepSearch (out value, this.name)) throw new NotImplementedException ();
            return false;
        }

        public override HashSet<string> FreeVariables ()
        {
            HashSet<string> variables = new HashSet<string> ();
            variables.Add (this.name);
            return variables;
        }

        public override bool NeedsTheEnvironment ()
        {
            return false;
        }
    }

    class Argument : Variable
    {
#if DEBUG
        static long evaluationCount = 0;
        static int [] histogram = new int [256 + 128];
#endif
        int offset;

        Argument (string name, int offset)
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
            return new Argument (name, offset);
        }


        public override bool EvalStep (out object value, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            Argument.evaluationCount += 1;
            Argument.histogram [offset] += 1;
            if (this.breakOnReference) {
                Debugger.Break ();
            }
#endif
            if (environment.ArgumentValue (out value, offset)) throw new NotImplementedException ();
            return false;
        }
    }
}
