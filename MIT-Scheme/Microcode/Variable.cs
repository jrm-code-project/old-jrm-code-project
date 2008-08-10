using System;
using System.Diagnostics;

// Variable lookup is the most time critical thing
// in the interpreter.
namespace Microcode
{
    public class CompileTimeEnvironment
    {
        string [] topFrame;

        public CompileTimeEnvironment (string [] topFrame)
        {
            this.topFrame = topFrame;
        }

        public CompileTimeEnvironment Extend (string [] topFrame)
        {
            return new CompileTimeEnvironment (topFrame);
        }

        public int Length
        {
            get
            {
                return (this.topFrame == null) ? 0 : this.topFrame.Length;
            }
        }

        public string this [int index]
        {
            get
            {
                return this.topFrame [index];
            }
        }
    }
    sealed class Access : SCode, ISystemPair
    {
        static long evaluationCount;

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

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Access.evaluationCount += 1;
            return interpreter.EvalNewSubproblem (this.env, new AccessLookup (interpreter.Continuation, this.var));
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

        internal override SCode Optimize (CompileTimeEnvironment ctenv)
        {
            return new Access (this.env.Optimize (ctenv),
                               this.var);
        }
    }

    sealed class AccessLookup : Continuation
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly string name;

        public AccessLookup (Continuation parent, string name)
            : base (parent)
        {
            this.name = name;
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            Environment env = value as Environment;
            if (env == null)
                env = InterpreterEnvironment.Global;
            object avalue;
            LookupDisposition disp = env.LookupVariable (0, this.name, out avalue);
            if (disp == LookupDisposition.OK)
                return interpreter.Return (avalue);
            throw new NotImplementedException ();
        }

        public override int FrameSize
        {
            get { throw new NotImplementedException (); }
        }
    }


    sealed class Assignment : SCode, ISystemPair
    {
        static long evaluationCount;

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

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Assignment.evaluationCount += 1;
            return interpreter.EvalNewSubproblem (this.value, new AssignContinue (interpreter.Continuation, this, interpreter.Environment));
        }

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

        internal override SCode Optimize (CompileTimeEnvironment ctenv)
        {
            return new Assignment (this.target, this.value.Optimize (ctenv));
        }
    }

    sealed class AssignContinue : Subproblem<Assignment>
    {
        public AssignContinue (Continuation next, Assignment expression, Environment environment)
            : base (next, expression, environment)
        {
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            object oldValue;
            LookupDisposition disp = ((Environment) (this.Environment)).AssignVariable (this.Expression.Name, value, out oldValue);
            if (disp == LookupDisposition.OK)
                return interpreter.Return (oldValue);
            throw new NotImplementedException ();
        }

        public override int FrameSize
        {
            get { return 3; }
        }

        public override object FrameRef (int offset)
        {
            if (offset == 0)
                return ReturnCode.EXECUTE_ASSIGNMENT_FINISH;
            else
                throw new NotImplementedException ();
        }

    }

    class Variable : SCode, ISystemHunk3
    {
        static long evaluationCount;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly string name;

        public Variable (string name)
            : base (TC.VARIABLE)
        {
            if (name == null)
                throw new ArgumentNullException ("name");
            this.name = name;
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

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Variable.evaluationCount += 1;
            //if (this.Name == String.Intern("construct-normal-package-from-description")) {
            //    Debug.WriteLine ("FOOFOOFOO");
            //}
            //Debug.WriteLineIf (Primitive.Noisy, this.Name);
            //Debug.WriteLine (this.Name);
            object value;
            LookupDisposition disp = interpreter.Environment.LookupVariable (0, this.name, out value);
            //Debug.WriteLineIf (Primitive.Noisy, "   " + ((value == null) ? "()" : value.ToString()));
            if (disp == LookupDisposition.OK)
                return interpreter.Return (value);
            throw new NotImplementedException ();
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

        internal override SCode Optimize (CompileTimeEnvironment ctenv)
        {
            for (int offset = 0; offset < ctenv.Length; offset += 1) {
                if (ctenv [offset] == this.name) {
                        //return Argument.Make (this.name, offset);
                }
            }
            return this;
        }
    }

    class Argument : Variable
    {
        static long evaluationCount = 0;
        static int [] histogram = new int [256 + 128];
        int offset;

        Argument (string name, int offset)
            : base (name)
        {
            this.offset = offset;
        }

        static public Variable Make (string name, int offset)
        {
            if (offset >= histogram.Length)
                throw new NotImplementedException ();
            return new Argument (name, offset);
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Argument.evaluationCount += 1;
            Argument.histogram [offset] += 1;
            //if (this.Name == String.Intern("construct-normal-package-from-description")) {
            //    Debug.WriteLine ("FOOFOOFOO");
            //}
            //Debug.WriteLineIf (Primitive.Noisy, this.Name);
            //Debug.WriteLine (this.Name);
            //object value;
            //ValueCell other = interpreter.Argvector [this.offset];
            //return interpreter.Return (etc [this.offset].Value);
            
            //LookupDisposition disp = interpreter.Environment.Argument (this.offset, out value);
            ////Debug.WriteLineIf (Primitive.Noisy, "   " + ((value == null) ? "()" : value.ToString()));
            //if (disp == LookupDisposition.OK) {
            //    if (other.Value != value)
            //        throw new NotImplementedException ();
            //    return interpreter.Return (value);
            //}
            throw new NotImplementedException ();
        }
    }
}
