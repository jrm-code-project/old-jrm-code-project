using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    public abstract class SCode
    {
        // return value is so this is an expression
        internal abstract object EvalStep (Interpreter interpreter, object etc);

        static bool SelfEvaluating (object obj)
        {

            return (obj == null) 
                || obj is char []
                || obj is object []
                || obj is char
                || obj is double
                || obj is int
                || obj is long
                || obj is Boolean
                || obj is Complex
                || obj is Cons 
                || obj is Constant
                || obj is Primitive
                || obj is Ratnum
                || obj is ReferenceTrap
                || obj is ReturnCode
                || obj is String
                ;
        }

        // In this implementation, the evaluator only sees SCode
        // and never sees self-evaluating objects.  To present the
        // illusion of self-evaluating objects, we wrap them in
        // SCode quotations when necessary and strip off the quotation
        // when the objects are taken apart through the generic
        // constructors.
        static internal SCode EnsureSCode (object obj)
        {
            if (SelfEvaluating (obj))
                return Quotation.Make (obj);
            else {
                SCode sobj = obj as SCode;
                if (sobj == null) {
                    throw new NotImplementedException ();
                }
                return sobj;
            }
        }

        static internal object UnwrapQuoted (object obj)
        {
            Quotation qobj = obj as Quotation;
            if (qobj == null)
                return obj;
            object quoted = qobj.Quoted;
            return SelfEvaluating(quoted) ? quoted : obj;
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
        {
            this.var = name;
            this.env = EnsureSCode(env);
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
                throw new NotImplementedException();
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
                throw new NotImplementedException();
            }
        }

        #endregion
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
        {
            if (target == null) throw new ArgumentNullException ("target");
            if (value == null) throw new ArgumentNullException ("value");
            this.target = target;
            this.value = value;
        }

        public Assignment (object target, object value)
        {
            Variable vtarget = target as Variable;
            if (vtarget != null) {
                this.target = vtarget.name;
                this.value = EnsureSCode (value);
            }
            else {
                string starget = target as String;
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
            throw new NotImplementedException();
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

    sealed class Comment : SCode, ISystemPair
    {
        static long evaluationCount;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object text;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode code;

        public Comment (object code, object text)
        {
            if (code == null) throw new ArgumentNullException ("code");
            // comment text can be null
            this.code = EnsureSCode(code);
            this.text = text;
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Comment.evaluationCount += 1;
            return interpreter.EvalReduction (this.code);
        }

        #region ISystemPair Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            get
            {
                return this.text;
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
                return UnwrapQuoted(this.code);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion
    }

    sealed class Conditional : SCode, ISystemHunk3
    {
        static long evaluationCount;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode predicate;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode consequent;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode alternative;

        Conditional (SCode predicate, SCode consequent, SCode alternative)
        {
            if (predicate == null) throw new ArgumentNullException ("predicate");
            if (consequent == null) throw new ArgumentNullException ("consequent");
            if (alternative == null) throw new ArgumentNullException ("alternative");
            this.predicate = predicate;
            this.consequent = consequent;
            this.alternative = alternative;
        }

        public static SCode Make (object predicate, object consequent, object alternative)
        {
            return new Conditional (EnsureSCode(predicate), EnsureSCode(consequent), EnsureSCode(alternative));
        }

        public static SCode Make (Hunk3 elements)
        {
            if (elements == null) throw new ArgumentNullException ("elements");
            return new Conditional (SCode.EnsureSCode (elements.Cxr0), 
                SCode.EnsureSCode (elements.Cxr1), 
                SCode.EnsureSCode (elements.Cxr2));
        }

        public SCode Predicate
        {
            [DebuggerStepThrough]
            get
            {
                return this.predicate;
            }
        }

        public SCode Consequent
        {
            [DebuggerStepThrough]
            get
            {
                return this.consequent;
            }
        }

        public SCode Alternative
        {
            [DebuggerStepThrough]
            get
            {
                return this.alternative;
            }
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Conditional.evaluationCount += 1;
            return interpreter.EvalNewSubproblem (this.predicate, new ConditionalDecide (interpreter.Continuation, this, interpreter.Environment));
        }

        #region ISystemHunk3 Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr0
        {
            get
            {
                return UnwrapQuoted (this.predicate);
            }
            set
            {
                throw new NotImplementedException();
            }
        }
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr1
        {
            get
            {
                return UnwrapQuoted(this.consequent);
            }
            set
            {
                throw new NotImplementedException();
            }
        }
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr2
        {
            get
            {
                return UnwrapQuoted(this.alternative);
            }
            set
            {
                throw new NotImplementedException();
            }
        }

        #endregion
    }

    sealed class ConditionalDecide : Subproblem<Conditional>
    {
        public ConditionalDecide (Continuation next, Conditional expression, Environment environment)
            : base (next, expression, environment)
        {
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            return interpreter.EvalReduction ((value is bool && (bool) value == false)
                                                  ? this.Expression.Alternative
                                                  : this.Expression.Consequent, this.Environment);
        }

        public override int FrameSize
        {
            get { throw new NotImplementedException (); }
        }
    }


    sealed class Definition : SCode, ISystemPair
    {
        static long evaluationCount;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly string name;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode value;

        public Definition (string name, SCode value)
        {
            if (name == null) throw new ArgumentNullException ("name");
            if (value == null) throw new ArgumentNullException ("value");
            this.name = name;
            this.value = value;
        }

        public Definition (object name, object value)
        {
            if (name == null) throw new ArgumentNullException ("name");
            this.name = (string) name;
            this.value = EnsureSCode (value);
        }

        public string Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.name;
            }
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Definition.evaluationCount += 1;
            return interpreter.EvalNewSubproblem (this.value, new DefineContinue (interpreter.Continuation, this, interpreter.Environment));
        }

        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
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
    }

    sealed class DefineContinue : Subproblem<Definition>
    {
        public DefineContinue (Continuation next, Definition definition, Environment environment)
            : base (next, definition, environment)
        {
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            LookupDisposition disp = this.Environment.DefineVariable (this.Expression.Name, value);
            // like MIT Scheme, discard old value and return name.
            if (disp == LookupDisposition.OK)
               return interpreter.Return (this.Expression.Name);
            else {
                throw new NotImplementedException ();
            }
        }

        public override int FrameSize
        {
            get { throw new NotImplementedException (); }
        }
    }

    sealed class Delay : SCode
    {
        static long evaluationCount;
        [System.Diagnostics.DebuggerBrowsable (System.Diagnostics.DebuggerBrowsableState.Never)]
        readonly SCode body;

        public Delay (object body)
        {
            this.body = EnsureSCode(body);
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Delay.evaluationCount += 1;
            return interpreter.Return (new Promise (this.body, interpreter.Environment));
        }
    }

    sealed class Disjunction : SCode
    {
        static long evaluationCount;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode predicate;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode alternative;

        public Disjunction (object predicate, object alternative)
        {
            if (predicate == null) throw new ArgumentNullException ("predicate");
            if (alternative == null) throw new ArgumentNullException ("alternative");
            this.predicate = EnsureSCode(predicate);
            this.alternative = EnsureSCode(alternative);
        }

        public SCode Alternative
        {
            [DebuggerStepThrough]
            get
            {
                return this.alternative;
            }
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Disjunction.evaluationCount += 1;
            return interpreter.EvalNewSubproblem (this.predicate, new DisjunctionDecide (interpreter.Continuation, this, interpreter.Environment));
        }
    }

    sealed class DisjunctionDecide : Subproblem<Disjunction>
    {
        public DisjunctionDecide (Continuation next, Disjunction disjunction, Environment environment)
            : base (next, disjunction, environment)
        {
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            if (value is bool && (bool) value == false)
            {
                return interpreter.EvalReduction (this.Expression.Alternative, this.Environment);
            }
            else
                return interpreter.Return (value);
        }

        public override int FrameSize
        {
            get { throw new NotImplementedException (); }
        }
    }

    sealed class ExitInterpreter : Continuation
    {
        public ExitInterpreter ()
            : base (null)
        {
        }

 
        internal override object Invoke (Interpreter interpreter, object value)
        {
            throw new ExitInterpreterException (Termination.RETURN_FROM_INTERPRETER);
        }

        public override int FrameSize
        {
            get { return 2; }
        }

        public override object FrameRef (int offset)
        {
            if (offset == 0)
                return new ReturnAddress (ReturnCode.HALT);
            else
                throw new NotImplementedException ();
        }

        public override int SystemVectorSize
        {
            get
            {
                return FrameSize;
            }
        }
    }



    class Lambda : SCode, ISystemPair
    {
        static long evaluationCount;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly string [] formals;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode body;

        public Lambda (SCode body, string [] formals)
        {
            if (body == null)
                throw new ArgumentNullException ("body");
            if (formals == null)
                throw new ArgumentNullException ("formals");
            if (formals [0] == "guarded-system-loader")
                throw new NotImplementedException ();
            this.body = body;
            this.formals = formals;
        }

        public Lambda (object body, object formals)
        {
            if ((string) (((object []) formals) [0]) == "guarded-system-loader")
                Debug.WriteLine ("Closing");

            //SCode sbody = body as SCode;

            object [] cdrArray = (object []) formals;
            string [] sformals = new string [cdrArray.Length];
            for (int i = 0; i < sformals.Length; i++)
                sformals [i] = (string) cdrArray [i];
            this.body = EnsureSCode (body);// (sbody == null) ? Quotation.Make (body) : sbody;
            this.formals = sformals;
        }

        public SCode Body
        {
            [DebuggerStepThrough]
            get
            {
                return this.body;
            }
        }

        public string Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.formals [0];
            }
        }

        public string [] Formals
        {
            [DebuggerStepThrough]
            get
            {
                return this.formals;
            }
        }

        static int [] formalOffsetCount = new int [512 + 256];
        public int FormalOffset (string name)
        {
            for (int i = 0; i < formals.Length; i++)
                if (Object.ReferenceEquals (name, formals [i])) {
                    formalOffsetCount [i] += 1;
                    return i - 1;
                }
            return -1;
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Lambda.evaluationCount += 1;
            return interpreter.Return (new Closure (this, interpreter.Environment));
        }


        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            get
            {
                return UnwrapQuoted (this.body);
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
                return this.formals;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion
    }

    // You'd think this would inherit from Lambda, but 
    // it is easier if it is disjoint.
    sealed class ExtendedLambda : SCode, ISystemHunk3
    {
        static long evaluationCount;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly string [] formals;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly SCode body;

        public readonly uint required;

        public readonly uint optional;

        public readonly bool rest;

        public ExtendedLambda (object body, string [] formals, uint required, uint optional, bool rest)
        {
            this.body = EnsureSCode (body);
            this.formals = formals;
            this.required = required;
            this.optional = optional;
            this.rest = rest;
        }

        public ExtendedLambda (Hunk3 init)
        {
            object [] cdrArray = (object []) init.Cxr1;
            string [] sformals = new string [cdrArray.Length];
            for (int i = 0; i < sformals.Length; i++)
                sformals [i] = (string) cdrArray [i];
            this.body = EnsureSCode (init.Cxr0);
            this.formals = sformals;
            uint code = (uint)(int)(init.Cxr2);
            this.optional = code & 0xFF;
            this.required = (code >> 8) & 0xFF;
            this.rest = ((code >> 16) & 0x1) == 0x1;
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            ExtendedLambda.evaluationCount += 1;
            return interpreter.Return (new ExtendedClosure (this, interpreter.Environment));
        }

        public string Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.formals [0];
            }
        }

        static int [] formalOffsetCount = new int [16];
        public int FormalOffset (string name)
        {
            for (int i = 0; i < formals.Length; i++)
                if (Object.ReferenceEquals (name, formals [i])) {
                    formalOffsetCount [i] += 1;
                    return i - 1;
                }
            return -1;
        }


        #region ISystemHunk3 Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr0
        {
            get
            {
                return UnwrapQuoted (this.body);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr1
        {
            [DebuggerStepThrough]
            get
            {
                return this.formals;
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
                return this.optional + (256 * (this.required + (this.rest ? 256 : 0)));

            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion
    }

    sealed class Quotation : SCode, ISystemPair
    {
        static int evaluationCount;
        // Space optimization.
        static Dictionary<object, Quotation> table = new Dictionary<object, Quotation> (8000);
        static Quotation QuoteNull;

        //static int cacheHits;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object item;

        Quotation (object item)
        {
            this.item = item;
        }

        static bool cacheItem (object item)
        {
            return (item is bool)
                || (item is char)
                || (item is int)
                || (item is string)
                || (item is Constant)
                || (item is Primitive)
                ;
        }

        public static Quotation Make (object item)
        {
            if (item == null) {
                if (QuoteNull == null)
                    QuoteNull = new Quotation (null);
                return QuoteNull;
            }
            //else if (cacheItem (item)) {
            //    Quotation probe;
            //    cacheHits++;
            //    if (table.TryGetValue (item, out probe) != true) {
            //        cacheHits--;
            //        probe = new Quotation (item);
            //        table.Add (item, probe);
            //    }
            //    return probe;
            //}
            else
                return new Quotation (item);
        }

        public object Quoted
        {
            [DebuggerStepThrough]
            get
            {
                return this.item;
            }
        }

        public override string ToString ()
        {
            if (this.item == null)
                return "#<SCODE-QUOTE NULL>";
            else
                return "#<SCODE-QUOTE " + this.item.ToString () + ">";
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Quotation.evaluationCount += 1;
            return interpreter.Return (this.item);
        }

        #region ISystemPair Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            get
            {
                return this.item;
            }
            set
            {
                throw new NotImplementedException();
            }
        }
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCdr
        {
            get
            {
                throw new NotImplementedException();
            }
            set
            {
                throw new NotImplementedException();
            }
        }

        #endregion
    }

    sealed class RestoreInterruptMask : Continuation
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly int old_mask;

        public RestoreInterruptMask (Continuation next, int old_mask)
            : base (next)
        {
            this.old_mask = old_mask;
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            interpreter.InterruptMask = this.old_mask;
            return interpreter.Return (value);
        }

        public override int FrameSize
        {
            get { throw new NotImplementedException (); }
        }
    }


    sealed class Sequence2 : SCode, ISystemPair
    {
        static long evaluationCount;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode first;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode second;

        public Sequence2 (object first, object second)
        {
            if (first == null)
                throw new ArgumentNullException ("first");
            this.first = EnsureSCode(first);
            this.second = EnsureSCode(second);
        }

        public SCode First
        {
            [DebuggerStepThrough]
            get
            {
                return this.first;
            }
        }

        public SCode Second
        {
            [DebuggerStepThrough]
            get
            {
                return this.second;
            }
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Sequence2.evaluationCount += 1;
            return interpreter.EvalNewSubproblem (this.first, new Sequence2Second (interpreter.Continuation, this, interpreter.Environment));
        }

        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            get
            {
                return UnwrapQuoted(this.first) ;
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
                return UnwrapQuoted(this.second);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion
    }

    sealed class Sequence2Second : Subproblem<Sequence2>
    {
        public Sequence2Second (Continuation next, Sequence2 sequence, Environment environment)
            : base (next, sequence, environment)
        {
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            return interpreter.EvalReduction (this.Expression.Second, this.Environment);
        }

        public override int FrameSize
        {
            get { return 3; }
        }
    }


    sealed class Sequence3 : SCode, ISystemHunk3
    {
        static long evaluationCount;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode first;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode second;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode third;

        public Sequence3 (object first, object second, object third)
        {
            if (first == null)
                throw new ArgumentNullException ("first");
            if (second == null)
                throw new ArgumentNullException ("second");
            if (third == null)
                throw new ArgumentNullException ("third");
            this.first = EnsureSCode(first);
            this.second = EnsureSCode(second);
            this.third = EnsureSCode(third);
        }

        public Sequence3 (Hunk3 init)
        {
            this.first = EnsureSCode (init.Cxr0);
            this.second = EnsureSCode (init.Cxr1);
            this.third = EnsureSCode (init.Cxr2);
        }

        public SCode First
        {
            [DebuggerStepThrough]
            get
            {
                return this.first;
            }
        }

        public SCode Second
        {
            [DebuggerStepThrough]
            get
            {
                return this.second;
            }
        }

        public SCode Third
        {
            [DebuggerStepThrough]
            get
            {
                return this.third;
            }
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Sequence3.evaluationCount += 1;
            return interpreter.EvalNewSubproblem (this.first, 
                new Sequence3Second (interpreter.Continuation, this, interpreter.Environment)); 
        }

        #region ISystemHunk3 Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr0
        {
            get
            {
                return UnwrapQuoted(this.first);
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
                return UnwrapQuoted(this.second);
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
                return UnwrapQuoted(this.third);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion
    }

    class Sequence3Second : Subproblem<Sequence3>
    {
        public Sequence3Second (Continuation parent, Sequence3 expression, Environment environment)
            : base (parent, expression, environment)
        {
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            return interpreter.EvalReuseSubproblem (this.Expression.Second, this.Environment, new Sequence3Third (this.parent, this.Expression, this.Environment));
        }

        public override int FrameSize
        {
            get { return 3; }
        }
    }

    sealed class Sequence3Third : Subproblem<Sequence3>
    {
        public Sequence3Third (Continuation next, Sequence3 expression, Environment environment)
            : base (next, expression, environment)
        {
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            return interpreter.EvalReduction (this.Expression.Third, this.Environment);
        }

        public override int FrameSize
        {
            get { return 3; }
        }
    }

    sealed class StackMarker : Continuation
    {
        readonly object mark1;
        readonly object mark2;

        public StackMarker (Continuation next, object mark1, object mark2)
            : base (next)
        {
            this.mark1 = mark1;
            this.mark2 = mark2;
        }
 
        internal override object Invoke (Interpreter interpreter, object value)
        {
            return interpreter.Return (value);
        }

        public override int FrameSize
        {
            get { return 2; }
        }
    }


    sealed class TheEnvironment : SCode
    {
        static long evaluationCount;
        public TheEnvironment ()
        {
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            TheEnvironment.evaluationCount += 1;
            return interpreter.Return (interpreter.Environment);
        }
    }

    class Variable : SCode, ISystemHunk3
    {
        static long evaluationCount;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly string name;

        public Variable (string name)
        {
            if (name == null)
                throw new ArgumentNullException ("name");
            this.name = name;
        }

        public Variable (Hunk3 init)
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
            throw new NotImplementedException();
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
    }
}
