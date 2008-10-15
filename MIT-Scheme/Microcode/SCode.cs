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
        static long evaluations;

        static Histogram<Type> scodeHistogram = new Histogram<Type>();

        static Histogram<SCode> hotSCode = new Histogram<SCode>();

        static Dictionary<Type,Histogram<Type>> callerTable = new Dictionary<Type, Histogram<Type>> ();

        protected void noteCalls (SCode callee)
        {
            Type calleeType = callee.GetType ();
            Histogram<Type> histogram;
            if (!callerTable.TryGetValue (calleeType, out histogram)) {
                histogram = new Histogram<Type> ();
                callerTable.Add (calleeType, histogram);
            }
            histogram.Note (this.GetType ());
        }

        protected void Warm ()
        {
            evaluations += 1;
            hotSCode.Note (this);
            scodeHistogram.Note (this.GetType ());
        }
#endif

        protected SCode (TC typeCode) : base (typeCode) { 
        }

	// Abstract functions that define the SCode API
        public abstract SCode Alpha (object from, object to);
        public abstract SCode Bind (LexicalMap ctenv);
        public abstract bool CallsTheEnvironment ();
        public abstract bool IsLetrecBody (object [] formals, object [] remainingFormals);
        public abstract bool MutatesAny (object [] formals);
        public abstract bool UsesAny (object [] formals);

#if DEBUG
        // for hash consing
        public virtual string Key ()
        {
            return this.GetType ().Name.ToString () + "-" + this.SerialNumber;
        }
#endif

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

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode optimizedCode = this.code.Bind (ctenv);
            return optimizedCode == this.code
                ? this
                : new Comment (optimizedCode, this.text);
        }

        public override bool CallsTheEnvironment ()
        {
            throw new NotImplementedException ();
        }

        public override bool UsesAny (object [] formals)
        {
            throw new NotImplementedException ();
        }

        public override bool MutatesAny (object [] formals)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.code);
#endif
            expression = this.code;
            answer = null; // happy compiler
            return true;
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


        public override SCode Alpha (object from, object to)
        {
            throw new NotImplementedException ();
        }

        public override bool IsLetrecBody (object [] formals, object [] remainingFormals)
        {
            throw new NotImplementedException ();
        }
    }

     [Serializable]
    sealed class Definition : SCode, ISystemPair
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object name;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode value;

        public Definition (object name, SCode value)
            : base (TC.DEFINITION)
        {
            if (name == null) throw new ArgumentNullException ("varname");
            if (value == null) throw new ArgumentNullException ("value");
            this.name = name;
            this.value = value;
        }

        public Definition (object name, object value)
            :base (TC.DEFINITION)
        {
            if (name == null) throw new ArgumentNullException ("varname");
            this.name = name;
            this.value = EnsureSCode (value);
        }

        public object Name
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

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode boundValue = this.value.Bind (ctenv);
            return boundValue == this.value
                ? this
                : new Definition (this.name, boundValue);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.value);
#endif
            object value;    
            Control expr = this.value;
            Environment env = environment;
            while (expr.EvalStep (out value, ref expr, ref env)) { };
            if (value == Interpreter.UnwindStack) throw new NotImplementedException();
            if (environment.Define (this.name, value)) throw new NotImplementedException ();
            answer = this.name;
            return false;
        }

        public override bool MutatesAny (object [] formals)
        {
            throw new NotImplementedException ();
        }

        public override bool CallsTheEnvironment ()
        {
            throw new NotImplementedException ();
        }

        public override bool UsesAny (object [] formals)
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


        public override SCode Alpha (object from, object to)
        {
            throw new NotImplementedException ();
        }

        public override bool IsLetrecBody (object [] formals, object [] remainingFormals)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class Delay : SCode
    {
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

        public override SCode Bind (LexicalMap ctenv)
        {
            return new Delay (this.body.Bind (ctenv));
        }

        public override bool CallsTheEnvironment ()
        {
            return this.body.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
             Warm ();
#endif
            answer = new Promise (this.body, environment);
            return false;
        }

        public override bool MutatesAny (object [] formals)
        {
            return this.body.MutatesAny (formals);
        }

        public override bool UsesAny (object [] formals)
        {
            throw new NotImplementedException ();
        }

        public override SCode Alpha (object from, object to)
        {
            throw new NotImplementedException ();
        }

        public override bool IsLetrecBody (object [] formals, object [] remainingFormals)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class Quotation : SCode, ISystemPair
    {
        // Space optimization.
        [NonSerialized]
        static Dictionary<object, Quotation> table = new Dictionary<object, Quotation> (8000);

        [NonSerialized]
        static int cacheHits;

        // We need to special case this.
        [NonSerialized]
        static Quotation QuoteNull;

        // We don't need to special case this, but it makes it much
        // easier to detect LETREC.
        [NonSerialized]
        static Quotation QuoteUnassigned;

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
            if (item == null)
                return Quotation.Null;
            else if (item == ReferenceTrap.Unassigned)
                return Quotation.Unassigned;
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

        public static Quotation Null
        {
            get
            {
                if (QuoteNull == null)
                    QuoteNull = new Quotation (null);
                return QuoteNull;
            }
        }


        public static Quotation Unassigned
        {
            get
            {
                if (QuoteUnassigned == null)
                    QuoteUnassigned = new Quotation (ReferenceTrap.Unassigned);
                return QuoteUnassigned;
            }
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
            Warm ();
#endif
            answer = this.item;
            return false;
        }

        public override bool MutatesAny (object [] formals)
        {
            return false;
        }

        public override bool UsesAny (object [] formals)
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

        public override SCode Alpha (object from, object to)
        {
            throw new NotImplementedException ();
        }

        public override bool IsLetrecBody (object [] formals, object [] remainingFormals)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class Sequence2 : SCode, ISystemPair
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode first;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode second;

        protected Sequence2 (SCode first, SCode second)
            : base (TC.SEQUENCE_2)
        {
            if (first == null)
                throw new ArgumentNullException ("first");
            if (second == null)
                throw new ArgumentNullException ("second");
            this.first = first;
            this.second = second;
        }

        static public SCode Make (SCode first, SCode second)
        {
            if (first is Argument)
                throw new NotImplementedException ();
            return (Configuration.EnableSuperOperators && second is Argument) ? Sequence2SA.Make (first, (Argument) second)
                : new Sequence2 (first, second);
        }

        static public SCode Make (object first, object second)
        {
            return Make (EnsureSCode (first), EnsureSCode (second));
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

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode optFirst = this.first.Bind (ctenv);
            SCode optSecond = this.second.Bind (ctenv);
            return

                // (Configuration.EnableSuperOperators && optSecond is Argument) ? Sequence2SA.Make (optFirst, (Argument) optSecond)
                // :(Configuration.EnableSuperOperators && optSecond is Quotation) ? Sequence2SQ.Make (optFirst, (Quotation) optSecond)
                //: 
                (optFirst == this.first && optSecond == this.second) ? this
                : Sequence2.Make (optFirst, optSecond);
        }

        public override bool CallsTheEnvironment ()
        {
            return this.first.CallsTheEnvironment ()
                || this.second.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.first);
            noteCalls (this.second);
#endif
            Object ev;
            Control first = this.first;
            Environment env = environment;
            while (first.EvalStep (out ev, ref first, ref env)) { };
            if (ev == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Sequence2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            } 

            expression = this.second;
            answer = null;
            return true; //tailcall  to second
        }

        public override bool MutatesAny (object [] formals)
        {
            return this.first.MutatesAny (formals)
                || this.second.MutatesAny (formals);
        }

        public override bool UsesAny (object [] formals)
        {
            return this.first.UsesAny (formals)
                || this.second.UsesAny (formals);
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


        public override SCode Alpha (object from, object to)
        {
            throw new NotImplementedException ();
        }

        public override bool IsLetrecBody (object [] formals, object [] remainingFormals)
        {
            Assignment s1 = this.first as Assignment;
            if (s1 == null)
                return false;
            if (s1.Value is LambdaBase) {
                object [] newRemaining = Misc.Remove (s1.Name, remainingFormals);
                if (newRemaining.Length == 0)
                    return ! this.second.MutatesAny (formals);
                if (newRemaining == remainingFormals)
                    return false;
                return this.second.IsLetrecBody (formals, newRemaining);
            }
            else
                return false;
    
        }
    }

    [Serializable]
    class Sequence2SA : Sequence2
    {
        int a2offset;

        protected Sequence2SA (SCode first, Argument second)
            : base (first, second)
        {
            this.a2offset = second.Offset;
        }

        static public SCode Make (SCode first, Argument second)
        {
            return
                (second is Argument0) ? Sequence2SA0.Make (first, (Argument0) second)
                : new Sequence2SA (first, second);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ("Unnecessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.first);
#endif
            Control first = this.first;
            Environment env = environment;
            while (first.EvalStep (out answer, ref first, ref env)) { };
            if (answer == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Sequence2Frame0 (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            answer = environment.ArgumentValue (this.a2offset);
            return false;
        }
    }

    [Serializable]
    class Sequence2SA0 : Sequence2SA
    {
        Sequence2SA0 (SCode first, Argument0 second)
            : base (first, second)
        {
        }

        static public SCode Make (SCode first, Argument0 second)
        {
            return new Sequence2SA0 (first, second);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ("Unnecessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.first);
#endif
            Control first = this.first;
            Environment env = environment;
            while (first.EvalStep (out answer, ref first, ref env)) { };
            if (answer == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Sequence2Frame0 (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            answer = environment.Argument0Value;
            return false;
        }
    }


    [Serializable]
    sealed class Sequence2Frame0 : SubproblemContinuation<Sequence2>, ISystemVector
    {
        public Sequence2Frame0 (Sequence2 expression, Environment environment)
            :base (expression, environment)
        {
        }

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            answer = value;
            expression = this.expression.Second;
            return true; //tailcall  to second
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
    }

//    [Serializable]
//    class Sequence2SQ : Sequence2
//    {
//        public object quoted;

//        protected Sequence2SQ (SCode first, Quotation second)
//            : base (first, second)
//        {
//            this.quoted = second.Quoted;
//        }

//        static public SCode Make (SCode first, Quotation second)
//        {
//            return new Sequence2SQ (first, second);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Unnecessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.first);
//#endif
//            Control first = this.first;
//            Environment env = environment;
//            while (first.EvalStep (out answer, ref first, ref env)) { };
//            if (answer == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Sequence2SQFrame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            answer = this.quoted;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class Sequence2SQFrame0 : SubproblemContinuation<Sequence2SQ>, ISystemVector
//    {
//        public Sequence2SQFrame0 (Sequence2SQ expression, Environment environment)
//            : base (expression, environment)
//        {
//        }

//        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
//        {
//            answer = this.expression.quoted;
//            return false; 
//        }

//        #region ISystemVector Members

//        public int SystemVectorSize
//        {
//            get { return 3; }
//        }

//        public object SystemVectorRef (int index)
//        {
//            switch (index) {
//                case 0: return ReturnCode.SEQ_2_DO_2;
//                default:
//                    throw new NotImplementedException ();
//            }
//        }

//        public object SystemVectorSet (int index, object newValue)
//        {
//            throw new NotImplementedException ();
//        }

//        #endregion
//    }


    [Serializable]
    sealed class Sequence3 : SCode, ISystemHunk3
    {
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
        public override SCode Bind (LexicalMap ctenv)
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
            Warm ();
            noteCalls (this.first);
            noteCalls (this.second);
            noteCalls (this.third);
#endif

            object ev;
            Control expr = this.first;
            Environment env = environment;

            while (expr.EvalStep (out ev, ref expr, ref env)) { };
            if (ev == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Sequence3Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            expr = this.second;
            env = environment;
            while (expr.EvalStep (out ev, ref expr, ref env)) { };
            if (ev  == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Sequence3Frame1 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            } 

            // Tail call into third part.
            expression = this.third;
            answer = null;
            return true;
        }

        public override bool MutatesAny (object [] formals)
        {
            return this.first.MutatesAny (formals)
                || this.second.MutatesAny (formals)
                || this.third.MutatesAny (formals);
        }

        public override bool UsesAny (object [] formals)
        {
            return this.first.UsesAny (formals)
                || this.second.UsesAny (formals)
                || this.third.UsesAny (formals);
        }

        public override bool CallsTheEnvironment ()
        {
            return this.first.CallsTheEnvironment ()
                || this.second.CallsTheEnvironment ()
                || this.third.CallsTheEnvironment ();
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


        public override SCode Alpha (object from, object to)
        {
            throw new NotImplementedException ();
        }


        public override bool IsLetrecBody (object [] formals, object [] remainingFormals)
        {
            Assignment s1 = this.first as Assignment;
            if (s1 == null)
                return false;
            if (s1.Value is LambdaBase) {
                object [] newRemaining = Misc.Remove (s1.Name, remainingFormals);
                if (newRemaining.Length == 0)
                    return !this.second.MutatesAny (formals)
                        && !this.third.MutatesAny (formals);
                if (newRemaining == remainingFormals)
                    return false;

                Assignment s2 = this.second as Assignment;
                if (s2 == null)
                    return false;
                if (s2.Value is LambdaBase) {
                    object [] newnewRemaining = Misc.Remove (s2.Name, newRemaining);
                    if (newnewRemaining.Length == 0)
                        return !this.third.MutatesAny (formals);
                    if (newnewRemaining == newRemaining)
                        return false;
                    return this.third.IsLetrecBody (formals, newnewRemaining);
                }
                else return false;


            }
            else
                return false;
    

        }
    }

    [Serializable]
    sealed class Sequence3Frame0 : SubproblemContinuation<Sequence3>, ISystemVector
    {
        public Sequence3Frame0 (Sequence3 expression, Environment environment)
            : base (expression, environment)
        {
        }

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
    sealed class Sequence3Frame1 : SubproblemContinuation<Sequence3>, ISystemVector
    {
        public Sequence3Frame1 (Sequence3 expression, Environment environment)
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
        static TheEnvironment singleton;

        TheEnvironment ()
            : base (TC.THE_ENVIRONMENT)
        {
        }

        public static SCode Make ()
        {
            if (singleton == null)
                singleton = new TheEnvironment();
            return singleton;
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            return this;
        }

        public override bool CallsTheEnvironment ()
        {
            return true;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            answer = environment;
            return false;
        }

        public override bool MutatesAny (object [] formals)
        {
            throw new NotImplementedException ();
        }

        public override bool UsesAny (object [] formals)
        {
            throw new NotImplementedException ();
        }

        public override SCode Alpha (object from, object to)
        {
            throw new NotImplementedException ();
        }

        public override bool IsLetrecBody (object [] formals, object [] remainingFormals)
        {
            throw new NotImplementedException ();
        }
    }
}
