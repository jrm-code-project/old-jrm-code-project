using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Microcode
{
    [Serializable]
    public abstract class SCode: Control
    {
#if DEBUG
        protected static Dictionary<string,SCode> hashConsTable = new Dictionary<string, SCode> ();
        protected static long counter;
        protected readonly long serialNumber;

        static Dictionary<SCode,long> hotCode = new Dictionary<SCode,long>();

        protected void Warm ()
        {
            if (hotCode.ContainsKey (this))
                hotCode [this] += 1;
            else
                hotCode [this] = 1;
        }

        static public SortedList<long, List<SCode>> HotCode
        {
            get
            {
                SortedList<long, List<SCode>> hotties = new SortedList<long, List<SCode>> ();
                foreach (KeyValuePair<SCode,long> entry in hotCode) {
                    List<SCode> probe = null;
                    if (!hotties.TryGetValue (entry.Value, out probe)) {
                        probe = new List<SCode> ();
                        hotties.Add (entry.Value, probe);
                    }

                    probe.Add (entry.Key);
                }
                return hotties;
            }
        }
#endif

        protected SCode (TC typeCode) : base (typeCode) { 
#if DEBUG
            this.serialNumber = SCode.counter++;
#endif
        }

	// Abstract functions that define the SCode API
        public abstract SCode Bind (BindingTimeEnvironment ctenv);

        public abstract IList<object> FreeVariables ();
#if DEBUG
        // for hash consing
        public abstract string Key ();
#endif
        public abstract bool NeedsValueCells (object [] formals);

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

    [Serializable]
    sealed class Comment : SCode, ISystemPair
    {
#if DEBUG
        [NonSerialized]
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

        [SchemePrimitive ("COMMENT?", 1, true)]
        public static bool IsComment (out object answer, object arg)
        {
            answer = arg is Comment;
            return false;
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optimizedCode = this.code.Bind (ctenv);
            return optimizedCode == this.code
                ? this
                : new Comment (optimizedCode,
                                this.text);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Comment.evaluationCount += 1;
            Warm ();
#endif
            expression = this.code;
            answer = null;
            return true;
        }

        public override IList<object> FreeVariables ()
        {
            throw new NotImplementedException ();
        }

        public override bool NeedsValueCells (object [] formals)
        {
            throw new NotImplementedException ();
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

#if DEBUG

        public override string Key ()
        {
            throw new NotImplementedException ();
        }
#endif
    }

    [Serializable]
    sealed class Conditional : SCode, ISystemHunk3
    {
#if DEBUG
        [NonSerialized]
        static long evaluationCount;
        [NonSerialized]
        static long disjunctions;
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
            SCode pred = EnsureSCode (predicate);
            SCode cons = EnsureSCode (consequent);
            if (pred is Variable
                && cons is Variable
                && ((Variable) pred).name == ((Variable) cons).name) {
#if DEBUG
                Conditional.disjunctions += 1;
#endif
                return new Disjunction (pred, alternative);
            }
            else
                return new Conditional (EnsureSCode (predicate), EnsureSCode (consequent), EnsureSCode (alternative));
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
        [SchemePrimitive ("CONDITIONAL?", 1, true)]
        public static bool IsConditional (out object answer, object arg)
        {
            answer = arg is Conditional;
            return false;
        }
        public override SCode Bind (BindingTimeEnvironment ctenv)
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

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Conditional.evaluationCount += 1;
            Warm ();
#endif
            Control unev = this.predicate;
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

        public override IList<object> FreeVariables ()
        {
            IList<object> answer = new List<object> ();
            foreach (object var in this.predicate.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            foreach (object var in this.consequent.FreeVariables())
                if (!answer.Contains (var)) answer.Add (var);
            foreach (object var in this.alternative.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            return answer;
        }

       public override bool NeedsValueCells (object [] formals)
        {
            return this.predicate.NeedsValueCells (formals)
                || this.consequent.NeedsValueCells (formals)
                || this.alternative.NeedsValueCells (formals);
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

#if DEBUG
        public override string Key ()
        {
            return "cond-" + this.serialNumber.ToString ();
        }
#endif
    }

    [Serializable]
    sealed class ConditionalFrame : SubproblemContinuation<Conditional>, ISystemVector
    {
        public ConditionalFrame (Conditional conditional, Environment environment)
            : base (conditional, environment)
        {
        }

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            expression = (value is bool) && (bool) value == false
                  ? this.expression.Alternative
                  : this.expression.Consequent;
            answer = value;
            return true;
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

    [Serializable]
    sealed class Definition : SCode, ISystemPair
    {
#if DEBUG
        [NonSerialized]
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

        [SchemePrimitive ("DEFINITION?", 1, true)]
        public static bool IsDefinition (out object answer, object arg)
        {
            answer = arg is Definition;
            return false;
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            return new Definition (this.name, this.value.Bind (ctenv));
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Definition.evaluationCount += 1;
#endif
            Control expr = this.value;
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

        public override IList<object> FreeVariables ()
        {
            throw new NotImplementedException ();
        }

        public override bool NeedsValueCells (object [] formals)
        {
            throw new NotImplementedException ();
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

#if DEBUG

        public override string Key ()
        {
            throw new NotImplementedException ();
        }
#endif
    }

    [Serializable]
    sealed class Delay : SCode
    {
#if DEBUG
        [NonSerialized]
        static long evaluationCount;
#endif
        [System.Diagnostics.DebuggerBrowsable (System.Diagnostics.DebuggerBrowsableState.Never)]
        readonly SCode body;

        public Delay (object body)
            : base (TC.DELAY)
        {
            this.body = EnsureSCode(body);
        }

        [SchemePrimitive ("DELAY?", 1, true)]
        public static bool IsDelay (out object answer, object arg)
        {
            answer = arg is Delay;
            return false;
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            return new Delay (this.body.Bind (ctenv));
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Delay.evaluationCount += 1;
            Warm ();
#endif
            answer = new Promise (this.body, environment);
            return false;
        }
        public override IList<object> FreeVariables ()
        {
            return this.body.FreeVariables ();
        }

        public override bool NeedsValueCells (object [] formals)
        {
            return this.body.NeedsValueCells (formals);
        }
#if DEBUG
        public override string Key ()
        {
            return "(delay " + this.serialNumber.ToString () + ")";
        }
#endif
    }

    [Serializable]
    sealed class Disjunction : SCode, ISystemPair
    {
#if DEBUG
        [NonSerialized]
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

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            return new Disjunction (this.predicate.Bind (ctenv),
                                    this.alternative.Bind (ctenv));
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Disjunction.evaluationCount += 1;
            Warm ();
#endif
            Environment env = environment;
            Control pred = this.predicate;
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
        public override IList<object> FreeVariables ()
        {
            IList<object> answer = new List<object> ();
            foreach (object var in this.predicate.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            foreach (object var in this.alternative.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            return answer;
        }

        public override bool NeedsValueCells (object [] formals)
        {
            return this.predicate.NeedsValueCells (formals)
                || this.alternative.NeedsValueCells (formals);
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


#if DEBUG

        public override string Key ()
        {
            return "disjunction-" + this.serialNumber.ToString();
        }
#endif
    }

    [Serializable]
    sealed class Quotation : SCode, ISystemPair
    {
#if DEBUG
        [NonSerialized]
        static long evaluationCount;
#endif
        // Space optimization.
        [NonSerialized]
        static Dictionary<object, Quotation> table = new Dictionary<object, Quotation> (8000);
        [NonSerialized]
        static Quotation QuoteNull;

        static int cacheHits;

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
            else if (cacheItem (item)) {
                Quotation probe;
                cacheHits++;
                if (table.TryGetValue (item, out probe) != true) {
                    cacheHits--;
                    probe = new Quotation (item);
                    table.Add (item, probe);
                }
                return probe;
            }
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

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            return this;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Quotation.evaluationCount += 1;
            Warm ();
            //if (this.item is int && (int) this.item == 1) Debugger.Break ();
#endif
            answer = this.item;
            return false;
        }

        public override IList<object> FreeVariables ()
        {
            return new List<object>();
        }

        public override bool NeedsValueCells (object [] formals)
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
#if DEBUG
        public override string Key ()
        {
            return "(Quotation " + serialNumber.ToString();
        }
#endif
    }

    [Serializable]
    sealed class Sequence2 : SCode, ISystemPair
    {
#if DEBUG
        [NonSerialized]
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

        [SchemePrimitive ("SEQUENCE2?", 1, true)]
        public static bool IsSequence2 (out object answer, object arg)
        {
            answer = arg is Sequence2;
            return false;
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            return new Sequence2 (this.first.Bind (ctenv),
                                  this.second.Bind (ctenv));
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Sequence2.evaluationCount += 1;
            Warm ();
#endif
            Control first = this.first;
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

        public override IList<object> FreeVariables ()
        {
            IList<object> answer = new List<object> ();
            foreach (object var in this.first.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            foreach (object var in this.second.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            return answer;
        }

        public override bool NeedsValueCells (object [] formals)
        {
            return this.first.NeedsValueCells (formals)
                || this.second.NeedsValueCells (formals);
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
#if DEBUG
        public override string Key ()
        {
            throw new NotImplementedException ();
        }
#endif
    }

    [Serializable]
    sealed class Sequence2Frame0 : SubproblemContinuation<Sequence2>, ISystemVector
    {

        internal Sequence2Frame0 (Sequence2 expression, Environment environment)
            :base (expression, environment)
        {
        }

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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            answer = value;
            expression = this.expression.Second;
            return true; //tailcall  to second
        }
    }

    [Serializable]
    sealed class Sequence3 : SCode, ISystemHunk3
    {
#if DEBUG
        [NonSerialized]
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
        [SchemePrimitive ("SEQUENCE3?", 1, true)]
        public static bool IsSequence3 (out object answer, object arg)
        {
            answer = arg is Sequence3;
            return false;
        }
        public override SCode Bind (BindingTimeEnvironment ctenv)
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

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Sequence3.evaluationCount += 1;
            Warm ();
#endif

            Control expr = this.first;
            Environment env = environment;
            while (expr.EvalStep (out answer, ref expr, ref env)) { };
            if (answer == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Sequence3Frame0 (this, environment));
                environment = env;
                return false;
            }

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

        public override IList<object> FreeVariables ()
        {
            IList<object> answer = new List<object> ();
            foreach (object var in this.first.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            foreach (object var in this.second.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            foreach (object var in this.third.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            return answer;
        }

        public override bool NeedsValueCells (object [] formals)
        {
            return this.first.NeedsValueCells (formals)
                || this.second.NeedsValueCells (formals)
                || this.third.NeedsValueCells (formals);
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
#if DEBUG
        public override string Key ()
        {
            throw new NotImplementedException ();
        }
#endif
    }

    [Serializable]
    sealed class Sequence3Frame0 : SubproblemContinuation<Sequence3>, ISystemVector
    {
        internal Sequence3Frame0 (Sequence3 expression, Environment environment)
            : base (expression, environment)
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



        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            Control expr = this.expression.Second;
            Environment env = this.environment;
            while (expr.EvalStep (out answer, ref expr, ref env)) { };
            if (answer == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Sequence3Frame1 (this.expression, this.environment));
                environment = env;
                return false;
            }

            // Tail call into third part.
            expression = this.expression.Third;
            return true;
        }
    }

    [Serializable]
    sealed class Sequence3Frame1 : SubproblemContinuation<Sequence3>, ISystemVector
    {
        internal Sequence3Frame1 (Sequence3 expression, Environment environment)
            :base (expression, environment)
        {
        }


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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            answer = value;
            // Tail call into third part.
            expression = this.expression.Third;
            return true;
        }
    }

    [Serializable]
    sealed class TheEnvironment : SCode
    {
#if DEBUG
        [NonSerialized]
        static long evaluationCount;
#endif
        public TheEnvironment ()
            : base (TC.THE_ENVIRONMENT)
        {
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            return this;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            TheEnvironment.evaluationCount += 1;
#endif
            answer = environment;
            return false;
        }

        public override IList<object> FreeVariables ()
        {
            throw new NotImplementedException ();
        }

        public override bool NeedsValueCells (object [] formals)
        {
            throw new NotImplementedException ();
        }
#if DEBUG
        public override string Key ()
        {
            throw new NotImplementedException ();
        }
#endif
    }

}
