using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    public abstract class SCode: SchemeObject
    {
        protected SCode (TC typeCode) : base (typeCode) { }

	// Abstract functions that define the SCode API
        public abstract SCode Bind (CompileTimeEnvironment ctenv);

        public abstract bool EvalStep (out object answer, ref SCode expression, ref Environment environment);
        public abstract HashSet<string> FreeVariables ();
        public abstract bool NeedsTheEnvironment ();

        static bool SelfEvaluating (object obj)
        {
            return (obj == null) 
                || obj is char []
                || obj is object []
                || obj is char
                || obj is double
                || obj is int
                || obj is long
                || obj is string
                || obj is Boolean
                || obj is Complex
                || obj is Cons 
                || obj is Constant
                || obj is Primitive
                || obj is Ratnum
                || obj is ReferenceTrap
                || obj is ReturnCode
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

    sealed class Comment : SCode, ISystemPair
    {
#if DEBUG
        static long evaluationCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object text;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode code;

        public Comment (object code, object text)
            :base (TC.COMMENT)
        {
            if (code == null) throw new ArgumentNullException ("code");
            // comment text can be null
            this.code = EnsureSCode(code);
            this.text = text;
        }

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            SCode optimizedCode = this.code.Bind (ctenv);
            return optimizedCode == this.code
                ? this
                : new Comment (optimizedCode,
                                this.text);
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            Comment.evaluationCount += 1;
#endif
            expression = this.code;
            answer = null;
            return true;
        }

        public override HashSet<string> FreeVariables ()
        {
            return this.code.FreeVariables ();
        }

        public override bool NeedsTheEnvironment ()
        {
            return this.code.NeedsTheEnvironment();
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
#if DEBUG
        static long evaluationCount;
#endif

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode predicate;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode consequent;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode alternative;

        Conditional (SCode predicate, SCode consequent, SCode alternative)
            : base (TC.CONDITIONAL)
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

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            SCode optPred = this.predicate.Bind (ctenv);
            SCode optCons = this.consequent.Bind (ctenv);
            SCode optAlt = this.alternative.Bind (ctenv);
            return optPred == this.predicate
                && optCons == this.consequent
                && optAlt == this.alternative
                ? this
                : new Conditional (optPred, optCons, optAlt);
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            Conditional.evaluationCount += 1;
#endif
            SCode unev = this.predicate;
            Environment env = environment;
            while (unev.EvalStep (out answer, ref unev, ref env)) { };
            if (answer == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;

            }

            expression = (answer is bool) && (bool) answer == false
                ? this.alternative
                : this.consequent;
            return true;
        }

        public override HashSet<string> FreeVariables ()
        {
            HashSet<string> freeVariables = this.predicate.FreeVariables ();
            freeVariables.UnionWith (this.consequent.FreeVariables ());
            freeVariables.UnionWith (this.alternative.FreeVariables ());
            return freeVariables;
        }

        public override bool NeedsTheEnvironment ()
        {
            return this.predicate.NeedsTheEnvironment ()
                || this.consequent.NeedsTheEnvironment ()
                || this.alternative.NeedsTheEnvironment ();
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

    sealed class ConditionalFrame : SubproblemContinuation<Conditional>, ISystemVector
    {
        public ConditionalFrame (Conditional conditional, Environment environment)
            : base (conditional, environment)
        {
        }

        public override bool Continue (out object answer, ref SCode expression, ref Environment environment, object value)
        {
            throw new NotImplementedException ();
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { throw new NotImplementedException (); }
        }

        public object SystemVectorRef (int index)
        {
            throw new NotImplementedException ();
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion


    }

    //sealed class ConditionalDecide : Subproblem<Conditional>
    //{
    //    public ConditionalDecide (Continuation next, Conditional expression, Environment environment)
    //        : base (next, expression, environment)
    //    {
    //    }

    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        return interpreter.EvalReduction ((value is bool && (bool) value == false)
    //                                              ? this.Expression.Alternative
    //                                              : this.Expression.Consequent, this.Environment);
    //    }

    //    public override int FrameSize
    //    {
    //        get { throw new NotImplementedException (); }
    //    }
    //}


    sealed class Definition : SCode, ISystemPair
    {
#if DEBUG
        static long evaluationCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly string name;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode value;

        public Definition (string name, SCode value)
            : base (TC.DEFINITION)
        {
            if (name == null) throw new ArgumentNullException ("name");
            if (value == null) throw new ArgumentNullException ("value");
            this.name = name;
            this.value = value;
        }

        public Definition (object name, object value)
            :base (TC.DEFINITION)
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

        public SCode Value
        {
            [DebuggerStepThrough]
            get
            {
                return this.value;
            }
        }

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            return new Definition (this.name, this.value.Bind (ctenv));
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            Definition.evaluationCount += 1;
#endif
            SCode expr = this.value;
            Environment env = environment;
            object value = null;
            while (expr.EvalStep (out value, ref expr, ref env)) { };
            if (value == Interpreter.UnwindStack) throw new NotImplementedException();

            //if (name == "copy-record")
            //    Debugger.Break ();
            if (environment.Define (this.name, value))
                throw new NotImplementedException ();
            answer = this.name;
            return false;
        }

        public override HashSet<string> FreeVariables ()
        {
            HashSet<string> variables = this.value.FreeVariables ();
            variables.Remove (this.name);
            return variables;
        }

        public override bool NeedsTheEnvironment ()
        {
            return this.value.NeedsTheEnvironment ();
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

    //sealed class DefineContinue : Subproblem<Definition>
    //{
    //    public DefineContinue (Continuation next, Definition definition, Environment environment)
    //        : base (next, definition, environment)
    //    {
    //    }

    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        LookupDisposition disp = this.Environment.DefineVariable (this.Expression.Name, value);
    //        // like MIT Scheme, discard old value and return name.
    //        if (disp == LookupDisposition.OK)
    //           return interpreter.Return (this.Expression.Name);
    //        else {
    //            throw new NotImplementedException ();
    //        }
    //    }

    //    public override int FrameSize
    //    {
    //        get { throw new NotImplementedException (); }
    //    }
    //}

    sealed class Delay : SCode
    {
#if DEBUG
        static long evaluationCount;
#endif
        [System.Diagnostics.DebuggerBrowsable (System.Diagnostics.DebuggerBrowsableState.Never)]
        readonly SCode body;

        public Delay (object body)
            : base (TC.DELAY)
        {
            this.body = EnsureSCode(body);
        }

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            return new Delay (this.body.Bind (ctenv));
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            Delay.evaluationCount += 1;
#endif
            answer = new Promise (this.body, environment);
            return false;
        }

        public override HashSet<string> FreeVariables ()
        {
            return this.body.FreeVariables ();
        }

        public override bool NeedsTheEnvironment ()
        {
            return this.body.NeedsTheEnvironment ();
        }
    }

    sealed class Disjunction : SCode, ISystemPair
    {
#if DEBUG
        static long evaluationCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode predicate;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode alternative;

        public Disjunction (object predicate, object alternative)
            : base (TC.DISJUNCTION)
        {
            this.predicate = EnsureSCode (predicate);
            this.alternative = EnsureSCode (alternative);
        }

        public SCode Predicate
        {
            [DebuggerStepThrough]
            get
            {
                return this.predicate;
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

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            return new Disjunction (this.predicate.Bind (ctenv),
                                    this.alternative.Bind (ctenv));
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            Disjunction.evaluationCount += 1;
#endif
            Environment env = environment;
            SCode pred = this.predicate;
            while (pred.EvalStep (out answer, ref pred, ref env)) { };
            if (answer == Interpreter.UnwindStack) throw new NotImplementedException ();

            if (answer is bool && (bool) answer == false) {
                // tail call alternative
                expression = this.alternative;
                return true;
            }
            else {
                // return thing
                return false;
            }
        }

        public override HashSet<string> FreeVariables ()
        {
            HashSet<string> freeVariables = this.predicate.FreeVariables ();
            freeVariables.UnionWith (this.alternative.FreeVariables ());
            return freeVariables;
        }

        public override bool NeedsTheEnvironment ()
        {
            return this.predicate.NeedsTheEnvironment ()
                || this.alternative.NeedsTheEnvironment ();
        }

        #region ISystemPair Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            get
            {
                return this.predicate;
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
                return this.alternative;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

    }

    //sealed class DisjunctionDecide : Subproblem<Disjunction>
    //{
    //    public DisjunctionDecide (Continuation next, Disjunction disjunction, Environment environment)
    //        : base (next, disjunction, environment)
    //    {
    //    }

    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        if (value is bool && (bool) value == false)
    //        {
    //            return interpreter.EvalReduction (this.Expression.Alternative, this.Environment);
    //        }
    //        else
    //            return interpreter.Return (value);
    //    }

    //    public override int FrameSize
    //    {
    //        get { throw new NotImplementedException (); }
    //    }
    //}

    //sealed class ExitInterpreter : Continuation
    //{
    //    public ExitInterpreter ()
    //        : base (null)
    //    {
    //    }

 
    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        throw new ExitInterpreterException (Termination.RETURN_FROM_INTERPRETER);
    //    }

    //    public override int FrameSize
    //    {
    //        get { return 2; }
    //    }

    //    public override object FrameRef (int offset)
    //    {
    //        if (offset == 0)
    //            return new ReturnAddress (ReturnCode.HALT);
    //        else
    //            throw new NotImplementedException ();
    //    }

    //    public override int SystemVectorSize
    //    {
    //        get
    //        {
    //            return FrameSize;
    //        }
    //    }
    //}


    sealed class Quotation : SCode, ISystemPair
    {
#if DEBUG
        static long evaluationCount;
#endif
        // Space optimization.
        static Dictionary<object, Quotation> table = new Dictionary<object, Quotation> (8000);
        static Quotation QuoteNull;

        //static int cacheHits;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object item;

        Quotation (object item)
            : base (TC.SCODE_QUOTE)
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

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            return this;
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            Quotation.evaluationCount += 1;
#endif
            answer = this.item;
            return false;
        }

        public override HashSet<string> FreeVariables ()
        {
            return new HashSet<string> ();
        }

        public override bool NeedsTheEnvironment ()
        {
            return false;
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

    sealed class Sequence2 : SCode, ISystemPair
    {
#if DEBUG
        static long evaluationCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode first;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode second;

        public Sequence2 (object first, object second)
            : base (TC.SEQUENCE_2)
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

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            return new Sequence2 (this.first.Bind (ctenv),
                                  this.second.Bind (ctenv));
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            Sequence2.evaluationCount += 1;
#endif
            SCode first = this.first;
            Environment env = environment;
            while (first.EvalStep (out answer, ref first, ref env)) { };
            if (answer == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Sequence2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            } 

            expression = this.second;
            return true; //tailcall  to second
        }

        public override HashSet<string> FreeVariables ()
        {
            HashSet<string> freeVariables = this.first.FreeVariables ();
            freeVariables.UnionWith (this.second.FreeVariables ());
            return freeVariables;
        }

        public override bool NeedsTheEnvironment ()
        {
            return this.first.NeedsTheEnvironment ()
                || this.second.NeedsTheEnvironment ();
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

    sealed class Sequence2Frame0 : SubproblemContinuation<Sequence2>, ISystemVector
    {

        internal Sequence2Frame0 (Sequence2 expression, Environment environment)
            :base (expression, environment)
        {
        }

        //public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        //{
        //    SCode expr = ((RewindState) environment).PopFrame ();
        //    Environment env = environment;
        //    while (expr.EvalStep (out answer, ref expr, ref env)) { };
        //    if (answer == Interpreter.UnwindStack) {
        //        ((UnwinderState) env).AppendContinuationFrames (this.continuation);
        //        //((UnwinderState) env).AppendContinuationFrames ((RewindState) environment.OldFrames);
        //        environment = env;
        //        return false;
        //    }

        //    expression = this.expression.Second;
        //    environment = this.environment;
        //    return true; //tailcall  to second
        //}

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { return 3; }
        }

        public object SystemVectorRef (int index)
        {
            switch (index) {
                case 0: return ReturnCode.SEQ_2_DO_2;
                default:
                    throw new NotImplementedException ();
            }
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

        public override bool Continue (out object answer, ref SCode expression, ref Environment environment, object value)
        {
            answer = value;
            expression = this.expression.Second;
            return true; //tailcall  to second
        }
    }

    //sealed class Sequence2Second : Subproblem<Sequence2>
    //{
    //    public Sequence2Second (Continuation next, Sequence2 sequence, Environment environment)
    //        : base (next, sequence, environment)
    //    {
    //    }

    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        return interpreter.EvalReduction (this.Expression.Second, this.Environment);
    //    }

    //    public override int FrameSize
    //    {
    //        get { return 3; }
    //    }
    //}

    sealed class Sequence3 : SCode, ISystemHunk3
    {
#if DEBUG
        static long evaluationCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode first;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode second;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode third;

        public Sequence3 (object first, object second, object third)
            : base (TC.SEQUENCE_3)
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
            : base (TC.SEQUENCE_3)
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

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            SCode optFirst = this.first.Bind (ctenv);
            SCode optSecond = this.second.Bind (ctenv);
            SCode optThird = this.third.Bind (ctenv);
            return optFirst == this.first
                && optSecond == this.second
                && optThird == this.third
                ? this
                : new Sequence3 (optFirst, optSecond, optThird);
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            Sequence3.evaluationCount += 1;
#endif

            SCode expr = this.first;
            Environment env = environment;
            while (expr.EvalStep (out answer, ref expr, ref env)) { };
            if (answer == Interpreter.UnwindStack) throw new NotImplementedException ();

            expr = this.second;
            env = environment;
            while (expr.EvalStep (out answer, ref expr, ref env)) { };
            if (answer  == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Sequence3Frame1 (this, environment));
                environment = env;
                return false;
            } 

            // Tail call into third part.
            expression = this.third;
            return true;
        }

        public override HashSet<string> FreeVariables ()
        {
            HashSet<string> freeVariables = this.first.FreeVariables ();
            freeVariables.UnionWith (this.second.FreeVariables ());
            freeVariables.UnionWith (this.third.FreeVariables ());
            return freeVariables;
       }

        public override bool NeedsTheEnvironment ()
        {
            return this.first.NeedsTheEnvironment()
                || this.second.NeedsTheEnvironment ()
                || this.third.NeedsTheEnvironment();
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

    sealed class Sequence3Frame1 : SubproblemContinuation<Sequence3>, ISystemVector
    {
        internal Sequence3Frame1 (Sequence3 expression, Environment environment)
            :base (expression, environment)
        {
        }

        //public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        //{
        //    SCode expr = ((RewindState) environment).PopFrame ();
        //    Environment env = environment;
        //    while (expr.EvalStep (out answer, ref expr, ref env)) { };
        //    if (answer == Interpreter.UnwindStack) {
        //        ((UnwinderState) env).AppendContinuationFrames (this.continuation);
        //        environment = env;
        //        return false;
        //    }

        //    // Tail call into third part.
        //    expression = this.expression.Third;
        //    environment = this.environment;
        //    return true;
        //}

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { return 3; }
        }

        public object SystemVectorRef (int index)
        {
            switch (index) {
                case 0: return ReturnCode.SEQ_3_DO_2;
                default:
                    throw new NotImplementedException ();
            }
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

        public override bool Continue (out object answer, ref SCode expression, ref Environment environment, object value)
        {
            answer = value;
            // Tail call into third part.
            expression = this.expression.Third;
            return true;
        }
    }

    sealed class Sequence3Frame2 : SubproblemContinuation<Sequence3>, ISystemVector
    {
        internal Sequence3Frame2 (Sequence3 expression, Environment environment)
            :base (expression, environment)
        {
        }
        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { throw new NotImplementedException (); }
        }

        public object SystemVectorRef (int index)
        {
            throw new NotImplementedException ();
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion



        public override bool Continue (out object answer, ref SCode expression, ref Environment environment, object value)
        {
            throw new NotImplementedException ();
        }
    }

    //class Sequence3Second : Subproblem<Sequence3>
    //{
    //    public Sequence3Second (Continuation parent, Sequence3 expression, Environment environment)
    //        : base (parent, expression, environment)
    //    {
    //    }

    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        return interpreter.EvalReuseSubproblem (this.Expression.Second, this.Environment, new Sequence3Third (this.parent, this.Expression, this.Environment));
    //    }

    //    public override int FrameSize
    //    {
    //        get { return 3; }
    //    }
    //}

    //sealed class Sequence3Third : Subproblem<Sequence3>
    //{
    //    public Sequence3Third (Continuation next, Sequence3 expression, Environment environment)
    //        : base (next, expression, environment)
    //    {
    //    }

    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        return interpreter.EvalReduction (this.Expression.Third, this.Environment);
    //    }

    //    public override int FrameSize
    //    {
    //        get { return 3; }
    //    }
    //}


    sealed class TheEnvironment : SCode
    {
#if DEBUG
        static long evaluationCount;
#endif
        public TheEnvironment ()
            : base (TC.THE_ENVIRONMENT)
        {
        }

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            return this;
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            TheEnvironment.evaluationCount += 1;
#endif
            answer = environment;
            return false;
        }

        public override HashSet<string> FreeVariables ()
        {
            return new HashSet<string> ();
        }

        public override bool NeedsTheEnvironment ()
        {
            return true;
        }
    }

}
