using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;
using System.Threading;

namespace Microcode
{
    class TypePair
    {
        static Dictionary <Type, Dictionary <Type, TypePair>> pairTable = new Dictionary<Type, Dictionary<Type, TypePair>> ();

        Type caller;
        Type callee;

        TypePair (Type caller, Type callee)
        {
            this.caller = caller;
            this.callee = callee;
        }

        [DebuggerStepThrough]
        public static TypePair Make (Type caller, Type callee)
        {
            Dictionary <Type, TypePair> subtable;
            if (!pairTable.TryGetValue (caller, out subtable)) {
                subtable = new Dictionary<Type, TypePair> ();
                pairTable.Add (caller, subtable);
            }
            TypePair answer;
            if (!subtable.TryGetValue (callee, out answer)) {
                answer = new TypePair (caller, callee);
                subtable.Add (callee, answer);
            }
            return answer;
        }
    }

    /// <summary>
    /// Represent a Scheme program.  Must be public.
    /// </summary>
    [Serializable]
    public abstract class SCode: Control
    {
#if DEBUG
        internal static long evaluations;

        internal static Histogram<Type> scodeHistogram = new Histogram<Type>();

        internal static Histogram<SCode> hotSCode = new Histogram<SCode>();
        internal static Histogram<String> topOfStack = new Histogram<String> ();
        internal static String location;
        internal static Dictionary<Type,Histogram<Type>> callerTable = new Dictionary<Type, Histogram<Type>> ();

        internal static Histogram<TypePair> callTable = new Histogram<TypePair> ();
        static public Timer topOfStackTimer;
        static Random random = new Random ();
        public static void TopOfStackProbe (object ignore)
        {
            string loc = location;
            if (loc != null && loc != "-")
                topOfStack.Note (loc);
            topOfStackTimer.Change (1, random.Next (1, 5));
        }

        [DebuggerStepThrough]
        protected void NoteCalls (SCode callee)
        {
            string oldLocation = SCode.location;
            SCode.location = "-";

            Type callerType = this.GetType ();
            Type calleeType = callee.GetType ();
//            if (callee is Argument0
//                && ! (this is Assignment)
//                && ! (this is Combination)
//                && ! (this is Combination1L)
//                && ! (this is ConditionalL)
//&& ! (this is Disjunction)
//                && ! (this is PrimitiveCombination3)
//                && ! (this is Sequence3)
//                                && !(this is SimpleLet1)
//                    )
//                Debugger.Break ();
            Histogram<Type> histogram;
            if (!callerTable.TryGetValue (calleeType, out histogram)) {
                histogram = new Histogram<Type> ();
                callerTable.Add (calleeType, histogram);
            }
            histogram.Note (callerType);
            callTable.Note (TypePair.Make (callerType, calleeType));
            SCode.location = oldLocation;
        }

        static long warm_break = 100000000;

        [DebuggerStepThrough]
        protected void Warm (String location)
        {
            SCode.location = "-";
            evaluations += 1;
            if (evaluations >= warm_break) {
                warm_break *= 10;
                Debugger.Break ();
            }
            hotSCode.Note (this);
            //Type t = this.GetType ();
            scodeHistogram.Note (this.GetType());
            SCode.location = location;
        }
#endif

        [DebuggerStepThrough]
        protected SCode () : base () { 
        }

        // Utility for make.
        public static SCode Unimplemented () { throw new NotImplementedException (); }

	    // Abstract functions that define the SCode API
        public abstract bool CallsTheEnvironment ();
        /// <summary>
        /// Collect the free variables by mutating the set that is passed in.
        /// </summary>
        /// <param name="freeVariableSet"></param>
        public abstract void CollectFreeVariables (HashSet<Symbol> freeVariableSet);
        // Nicer functional version doesn't perform as well.
        //public abstract ICollection<Symbol> ComputeFreeVariables ();
        public abstract bool MutatesAny (Symbol [] formals);

        /// <summary>
        /// Returns the residual program after partially evaluating the SCode.
        /// </summary>
        /// <param name="closureEnvironment"></param>
        /// <returns></returns>
        internal abstract PartialResult PartialEval (PartialEnvironment environment);

#if DEBUG
        // for hash consing
        public virtual string Key ()
        {
            return this.GetType ().Name.ToString ();
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
                || obj is ReturnAddress
                || obj is ReturnCode
                || obj is Symbol
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
    class Comment : SCode, ISystemPair
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.COMMENT; } }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object text;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode code;

        protected Comment (SCode code, object text)
            :base ()
        {
            if (code == null) throw new ArgumentNullException ("code");
            // comment text can be null
            this.code = code;
            this.text = text;
        }

        public static SCode Make (object code, object text)
        {
            if (code == null) throw new ArgumentNullException ("code");
            return new Comment(EnsureSCode (code), text);
        }

        [SchemePrimitive ("COMMENT?", 1, true)]
        public static bool IsComment (out object answer, object arg)
        {
            answer = arg is Comment;
            return false;
        }

        public override bool CallsTheEnvironment ()
        {
            return this.code.CallsTheEnvironment ();
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            return this.code.MutatesAny (formals);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Comment.EvalStep");
            NoteCalls (this.code);
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

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialResult result = this.code.PartialEval (environment);
            return new PartialResult (result.Residual == this.code ? this : Comment.Make (result.Residual, this.text));
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            this.code.CollectFreeVariables (freeVariableSet);
        }
    }

     [Serializable]
    sealed class Definition : SCode, ISystemPair
    {
         [DebuggerBrowsable (DebuggerBrowsableState.Never)]
         public override TC TypeCode { get { return TC.DEFINITION; } }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Symbol name;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode value;

        Definition (Symbol name, SCode value)
            : base ()
        {
            this.name = name;
            this.value = value;
        }

         public static SCode Make (Symbol name, object value)
        {
            if (name == null) throw new ArgumentNullException ("name");
            return new Definition (name, EnsureSCode (value));
        }

        public Symbol Name
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

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Definition.EvalStep");
            NoteCalls (this.value);
#endif
            object value;    
            Control expr = this.value;
            Environment env = environment;
            while (expr.EvalStep (out value, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "Definition.EvalStep";
#endif
            if (value == Interpreter.UnwindStack) throw new NotImplementedException();
            if (environment.Define (this.name, value)) throw new NotImplementedException ();
            answer = this.name;
            return false;
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            throw new NotImplementedException ();
        }

        public override bool CallsTheEnvironment ()
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

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialResult pvalue = this.value.PartialEval (environment);
            return new PartialResult (pvalue.Residual == this.value ? this : Definition.Make (this.name, pvalue.Residual));
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class Delay : SCode, ISystemPair
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.DELAY; } }

        [System.Diagnostics.DebuggerBrowsable (System.Diagnostics.DebuggerBrowsableState.Never)]
        readonly SCode body;

        Delay (SCode body)
            : base ()
        {
            this.body = body;
        }

        public Delay (object body, object other)
            : base ()
        {
            this.body = EnsureSCode (body);
        }

        static public Delay Make (object body)
        {
            return new Delay (EnsureSCode (body));
        }

        [SchemePrimitive ("DELAY?", 1, true)]
        public static bool IsDelay (out object answer, object arg)
        {
            answer = arg is Delay;
            return false;
        }

        public override bool CallsTheEnvironment ()
        {
            return this.body.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
             Warm ("Delay.EvalStep");
#endif
            answer = new Promise (this.body, environment);
            return false;
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            return this.body.MutatesAny (formals);
        }

        #region ISystemPair Members

        public object SystemPairCar
        {
            get
            {
                return this.body;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        public object SystemPairCdr
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

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialResult partialBody = this.body.PartialEval (environment);
            return new PartialResult (partialBody.Residual == this.body ? this : Delay.Make (partialBody.Residual));
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            this.body.CollectFreeVariables (freeVariableSet);
        }
    }

    [Serializable]
    sealed class Quotation : SCode, ISerializable, ISystemPair
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.SCODE_QUOTE; } }

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
            : base ()
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

        public override bool CallsTheEnvironment ()
        {
            return false;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            
            Warm ("Quotation.EvalStep");
#endif
            answer = this.item;
            return false;
        }

        public override bool MutatesAny (Symbol [] formals)
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

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (QuotationDeserializer));
            info.AddValue ("quotedObject", this.Quoted);
        }
        #endregion

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            return new PartialResult (this);
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            return;
        }
    }

    [Serializable]
    internal sealed class QuotationDeserializer : IObjectReference
    {
        object quotedObject;

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public Object GetRealObject (StreamingContext context)
        {
            return Quotation.Make (this.quotedObject);
        }

        object Quoted { set { this.quotedObject = value; } }
    }

    [Serializable]
    class Sequence2 : SCode, ISerializable, ISystemPair
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.SEQUENCE_2; } }

#if DEBUG
        static public Histogram<Type> firstTypeHistogram = new Histogram<Type> ();
        static public Histogram<Type> secondTypeHistogram = new Histogram<Type> ();
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public Type firstType;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public Type secondType;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode first;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode second;

        protected Sequence2 (SCode first, SCode second)
            : base ()
        {
            this.first = first;
            this.second = second;
#if DEBUG
            this.firstType = first.GetType ();
            this.secondType = second.GetType ();
#endif
        }

        static SCode SwapConditional (Conditional first, SCode second)
        {
            //Debug.Write ("\n; Sequence2.SwapConditional");
            return Conditional.Make (first.Predicate,
                                    Sequence2.Make (first.Consequent, second),
                                    Sequence2.Make (first.Alternative, second));
        }

        static SCode Flatten (SCode first, Sequence2 second)
        {
            //Debug.Write ("\n; Sequence2.Flatten(a)");
            return Sequence3.Make (first, second.First, second.Second);
        }

        static SCode Flatten (Sequence2 first, SCode second)
        {
            //Debug.Write ("\n; Sequence2.Flatten(b)");
            return Sequence3.Make (first.First, first.Second, second);
        }

        static SCode Flatten (Sequence3 first, SCode second)
        {
            //Debug.Write ("\n; Sequence2.Flatten(c)");
            return Sequence2.Make (first.First, 
                        Sequence3.Make (first.Second, 
                                        first.Third, second));
        }

        static public SCode Make (SCode first, SCode second)
        {
            return new Sequence2 (first, second);
               // (! Configuration.EnableSequence2Optimization) ? new Sequence2 (first, second) :
               // (Configuration.EnableCodeRewriting &&
               //  Configuration.EnableSequenceConditionalSwap &&
               //  first is Conditional) ? SwapConditional ((Conditional) first, second) :
               // (Configuration.EnableSuperOperators && 
               //  first is LexicalVariable) ? Sequence2L.Make ((LexicalVariable) first, second) :
               // (Configuration.EnableSuperOperators && 
               //  Configuration.EnableSequenceSpecialization &&
               //  first is Quotation) ? Sequence2Q.Make ((Quotation) first, second) :
               //(Configuration.EnableCodeRewriting &&
               // first is Sequence2) ? Flatten ((Sequence2) first, second) :
               //(Configuration.EnableCodeRewriting &&
               // first is Sequence3) ? Flatten ((Sequence3) first, second) :
               // (Configuration.EnableSuperOperators &&
               // Configuration.EnableSequenceSpecialization &&
               // second is LexicalVariable) ? Sequence2SL.Make (first, (LexicalVariable) second) :
               // (Configuration.EnableSuperOperators &&
               //  Configuration.EnableSequenceSpecialization &&
               // second is Quotation) ? Sequence2SQ.Make (first, (Quotation) second) :
               // (Configuration.EnableCodeRewriting &&
               // second is Sequence2) ? Flatten (first, (Sequence2) second) :

               // new Sequence2 (first, second);
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

        public override bool CallsTheEnvironment ()
        {
            return this.first.CallsTheEnvironment ()
                || this.second.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.first);
            firstTypeHistogram.Note (this.firstType);
            NoteCalls (this.second);
            secondTypeHistogram.Note (this.secondType);
            SCode.location = "Sequence2.EvalStep";
#endif
            Object ev;
            Control first = this.first;
            Environment env = environment;
            while (first.EvalStep (out ev, ref first, ref env)) { };
#if DEBUG
            SCode.location = "Sequence2.EvalStep";
#endif
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

        public override bool MutatesAny (Symbol [] formals)
        {
            return this.first.MutatesAny (formals)
                || this.second.MutatesAny (formals);
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (Sequence2Deserializer));
            info.AddValue ("first", this.first);
            info.AddValue ("second", this.second);
        }

        #endregion

        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            get
            {
                return UnwrapQuoted (this.first);
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
                return UnwrapQuoted (this.second);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialResult r0 = this.first.PartialEval (environment);
            PartialResult r1 = this.second.PartialEval (environment);
            return new PartialResult (r0.Residual == this.first &&
                r1.Residual == this.second ? this : Sequence2.Make (r0.Residual, r1.Residual));
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            this.first.CollectFreeVariables (freeVariableSet);
            this.second.CollectFreeVariables (freeVariableSet);
        }
    }

    [Serializable]
    internal sealed class Sequence2Deserializer : IObjectReference
    {
        SCode first;
        SCode second;

        // GetRealObject is called after this object is deserialized.
        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public Object GetRealObject (StreamingContext context)
        {
            return Sequence2.Make (this.first, this.second);
        }

        public void SetFirst (SCode value) { this.first = value; }
        public void SetSecond (SCode value) { this.second = value; }
    }

//    /// <summary>
//    /// This one is odd, but it signals that the variable is intended
//    /// to be ignored.
//    /// </summary>
//    [Serializable]
//    class Sequence2L : Sequence2
//    {
//        protected Sequence2L (LexicalVariable first, SCode second)
//            : base (first, second)
//        {
//        }

//        static SCode Simplify (SCode second)
//        {
//            //Debug.Write ("\n; Sequence2L.Simplify");
//            return second;
//        }

//        static public SCode Make (LexicalVariable first, SCode second)
//        {
//            return
//                (Configuration.EnableCodeRewriting &&
//                 Configuration.EnableSequenceSimplification) ? Simplify(second) :
//                new Sequence2L (first, second);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.second);
//            SCode.location = "Sequence2L.EvalStep";
//#endif
//            expression = this.second;
//            answer = null;
//            return true;
//        }
//    }

//    /// <summary>
//    /// This one is weird, but we sometimes see declarations.
//    /// </summary>
//    [Serializable]
//    class Sequence2Q : Sequence2
//    {
//        protected Sequence2Q (Quotation first, SCode second)
//            : base (first, second)
//        {
//        }

//        static SCode Simplify (SCode second)
//        {
//            //Debug.Write ("\n; Sequence2Q.Simplify");
//            return second;
//        }

//        static public SCode Make (Quotation first, SCode second)
//        {
//            return 
//                (Configuration.EnableCodeRewriting &&
//                 Configuration.EnableSequenceSimplification &&
//                 !(first.Quoted is object [])) ? Simplify(second) :
//                new Sequence2Q (first, second);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.second);
//            SCode.location = "Sequence2Q.EvalStep";
//#endif
//            expression = this.second;
//            answer = null;
//            return true;
//        }
//    }

//    class Sequence2SL : Sequence2
//    {
//        public readonly object secondName;
//        public readonly LexicalAddress secondAddress;

//        protected Sequence2SL (SCode first, LexicalVariable second)
//            : base (first, second)
//        {
//            this.secondName = second.Name;
//            this.secondAddress = second.Address;
//        }

//        static public SCode Make (SCode first, LexicalVariable second)
//        {
//            return 
//                (second is Argument) ? Sequence2SA.Make (first, (Argument) second) :
//                (second is LexicalVariable1) ? Sequence2SL1.Make (first, (LexicalVariable1) second) :
//                new Sequence2SL (first, second);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.first);
//            SCode.location = "Sequence2SL.EvalStep";
//#endif
//            Control first = this.first;
//            Environment env = closureEnvironment;
//            while (first.EvalStep (out answer, ref first, ref env)) { };
//#if DEBUG
//                        SCode.location = "Sequence2SL.EvalStep.1";
//#endif
//            if (answer == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Sequence2Frame0 (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            if (closureEnvironment.FastLexicalRef (out answer, this.secondName, this.secondAddress))
//                throw new NotImplementedException ();
//            return false;
//        }
//    }

//    class Sequence2SA : Sequence2SL
//    {
//        protected Sequence2SA (SCode first, Argument second)
//            : base (first, second)
//        {
//        }

//        static public SCode Make (SCode first, Argument second)
//        {
//            return
//                (second is Argument0) ? Sequence2SA0.Make (first, (Argument0) second) :
//                (second is Argument1) ? Sequence2SA1.Make (first, (Argument1) second) :
//                new Sequence2SA (first, second);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Sequenc2SA.EvalStep");
//            NoteCalls (this.first);
//#endif
//            Control first = this.first;
//            Environment env = closureEnvironment;
//            while (first.EvalStep (out answer, ref first, ref env)) { };
//            if (answer == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Sequence2Frame0 (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            answer = closureEnvironment.ArgumentValue (this.secondOffset);
//            return false;
//        }
//    }

//    sealed class Sequence2SA0 : Sequence2SA
//    {
//        Sequence2SA0 (SCode first, Argument0 second)
//            : base (first, second)
//        {
//        }

//        static public SCode Make (SCode first, Argument0 second)
//        {
//            return new Sequence2SA0 (first, second);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Sequence2SA0.EvalStep");
//            NoteCalls (this.first);
//#endif
//            Control first = this.first;
//            Environment env = closureEnvironment;
//            while (first.EvalStep (out answer, ref first, ref env)) { };
//            if (answer == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new Sequence2SA0Frame0 (this, closureEnvironment));
//                closureEnvironment = env;
//                answer = Interpreter.Unwind;
//                return false;
//            }

//            answer = closureEnvironment.Argument0Value;
//            return false;
//        }
//    }

//    sealed class Sequence2SA0Frame0 : SubproblemContinuation<Sequence2SA0>, ISystemVector
//    {
//        public Sequence2SA0Frame0 (Sequence2SA0 expression, Environment closureEnvironment)
//            : base (expression, closureEnvironment)
//        {
//        }

//        public override bool Continue (out object answer, ref Control expression, ref Environment closureEnvironment, object value)
//        {
//            answer = this.closureEnvironment.Argument0Value;
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

//    sealed class Sequence2SA1 : Sequence2SA
//    {
//        Sequence2SA1 (SCode first, Argument1 second)
//            : base (first, second)
//        {
//        }

//        static public SCode Make (SCode first, Argument1 second)
//        {
//            return new Sequence2SA1 (first, second);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Sequence2SA1.EvalStep");
//            NoteCalls (this.first);
//#endif
//            Control first = this.first;
//            Environment env = closureEnvironment;
//            while (first.EvalStep (out answer, ref first, ref env)) { };
//            if (answer == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Sequence2Frame0 (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            answer = closureEnvironment.Argument1Value;
//            return false;
//        }
//    }

//    sealed class Sequence2SL1 : Sequence2SL
//    {
//        Sequence2SL1 (SCode first, LexicalVariable1 second)
//            : base (first, second)
//        {
//        }

//        static public SCode Make (SCode first, LexicalVariable1 second)
//        {
//            return new Sequence2SL1 (first, second);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Sequence2SL1.EvalStep");
//            NoteCalls (this.first);
//#endif
//            Control first = this.first;
//            Environment env = closureEnvironment;
//            while (first.EvalStep (out answer, ref first, ref env)) { };
//            if (answer == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Sequence2Frame0 (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            if (closureEnvironment.FastLexicalRef1 (out answer, this.secondName, this.secondOffset))
//                throw new NotImplementedException ();
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class Sequence2SQ : Sequence2
//    {
//#if DEBUG
//        static public new Histogram<Type> firstTypeHistogram = new Histogram<Type>();
//        static public Histogram<object> quotedHistogram = new Histogram<object> ();
//#endif
//        public object quoted;

//        Sequence2SQ (SCode first, Quotation second)
//            : base (first, second)
//        {
//            this.quoted = second.Quoted;
//        }

//        static public SCode Make (SCode first, Quotation second)
//        {
//            return 
//                //(Configuration.EnableTrueUnspecific && second.Quoted == Constant.Unspecific)? first
//                //: (first is Conditional) ? Conditional.Make (((Conditional)first).Predicate,
//                //                                             Sequence2.Make (((Conditional)first).Consequent, second),
//                //                                             Sequence2.Make (((Conditional) first).Alternative, second))
//                new Sequence2SQ (first, second);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.first);
//            firstTypeHistogram.Note (this.firstType);
//            if (this.quoted != null)
//                quotedHistogram.Note (this.quoted);
//            SCode.location = "Sequence2SQ.EvalStep";
//#endif
//            Control first = this.first;
//            Environment env = closureEnvironment;
//            while (first.EvalStep (out answer, ref first, ref env)) { };
//#if DEBUG
//            SCode.location = "Sequence2SQ.EvalStep.1";
//#endif
//            if (answer == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new Sequence2SQFrame0 (this, closureEnvironment));
//                closureEnvironment = env;
//                answer = Interpreter.Unwind;
//                return false;
//            }

//            answer = this.quoted;
//            return false;
//        }
//    }

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

    //[Serializable]
    //sealed class Sequence2SQFrame0 : SubproblemContinuation<Sequence2SQ>, ISystemVector
    //{
    //    public Sequence2SQFrame0 (Sequence2SQ expression, Environment closureEnvironment)
    //        : base (expression, closureEnvironment)
    //    {
    //    }

    //    public override bool Continue (out object answer, ref Control expression, ref Environment closureEnvironment, object value)
    //    {
    //        answer = this.expression.quoted;
    //        return false;
    //    }

    //    #region ISystemVector Members

    //    public int SystemVectorSize
    //    {
    //        get { return 3; }
    //    }

    //    public object SystemVectorRef (int index)
    //    {
    //        switch (index) {
    //            case 0: return ReturnCode.SEQ_2_DO_2;
    //            default:
    //                throw new NotImplementedException ();
    //        }
    //    }

    //    public object SystemVectorSet (int index, object newValue)
    //    {
    //        throw new NotImplementedException ();
    //    }

    //    #endregion
    //}

    [Serializable]
    sealed class Sequence3 : SCode, ISerializable, ISystemHunk3
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.SEQUENCE_3; } }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode first;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode second;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode third;

        Sequence3 (SCode first, SCode second, SCode third)
            : base ()
        {
            this.first = first;
            this.second = second;
            this.third = third;
        }

        public Sequence3 (Hunk3 init)
            : base ()
        {
            this.first = EnsureSCode (init.Cxr0);
            this.second = EnsureSCode (init.Cxr1);
            this.third = EnsureSCode (init.Cxr2);
        }

        static SCode Simplify (SCode first, SCode second)
        {
            //Debug.Write ("\n; Sequence3.Simplify");
            return Sequence2.Make (first, second);
        }

        static SCode SwapConditional (Conditional first, SCode second, SCode third)
        {
            //Debug.Write ("\n; Sequence3.SwapConditional(1)");
            return
               Conditional.Make (first.Predicate,
                                 Sequence3.Make (first.Consequent, second, third),
                                 Sequence3.Make (first.Alternative, second, third));
        }

        static SCode SwapConditional (SCode first, Conditional second, SCode third)
        {
            //Debug.Write ("\n; Sequence3.SwapConditional(2)");
            return 
               Sequence2.Make (first,         
                               Conditional.Make (second.Predicate,
                                    Sequence2.Make (second.Consequent, third),
                                    Sequence2.Make (second.Alternative, third)));
        }

        static SCode Flatten (SCode first, SCode second, Sequence2 third)
        {
            //Debug.Write ("\n; Sequence3.Flatten(1)");
            return
                Sequence2.Make (first,
                                Sequence3.Make (second,
                                                third.First,
                                                third.Second));
        }

        static SCode Flatten (SCode first, Sequence2 second, SCode third)
        {
            //Debug.Write ("\n; Sequence3.Flatten(2)");
            return
                Sequence2.Make (first,
                                Sequence3.Make (second.First,
                                                second.Second,
                                                third));
        }

        static SCode Flatten (SCode first, Sequence3 second, SCode third)
        {
            //Debug.Write ("\n; Sequence3.Flatten(3)");
            return
                Sequence3.Make (first,
                                second.First,
                                Sequence3.Make (second.Second,
                                                second.Third,
                                                third));
        }

        static SCode Flatten (Sequence2 first, SCode second, SCode third)
        {
            //Debug.Write ("\n; Sequence3.Flatten(4)");
            return
                Sequence2.Make (first.First,
                                Sequence3.Make (first.Second,
                                                second,
                                                third));
        }

        static SCode Flatten (Sequence3 first, SCode second, SCode third)
        {
            //Debug.Write ("\n; Sequence3.Flatten(5)");
            return
                Sequence3.Make (first.First,
                                first.Second,
                                Sequence3.Make (first.Third,
                                                second,
                                                third));
        }

        public static SCode Make (SCode first, SCode second, SCode third)
        {
            return new Sequence3 (first, second, third);
                //(! Configuration.EnableSequence3Optimization) ? new Sequence3 (first, second, third) :
                ////: (Configuration.EnableTrueUnspecific && third is Quotation && ((Quotation) third).Quoted == Constant.Unspecific) ? Sequence2.Make (first, second)
                ////: 
                //(Configuration.EnableCodeRewriting &&
                // Configuration.EnableSequenceConditionalSwap &&
                // first is Conditional) ? SwapConditional ((Conditional) first, second, third) :
                //(Configuration.EnableCodeRewriting &&
                // Configuration.EnableSequenceConditionalSwap &&
                // second is Conditional) ? SwapConditional (first, (Conditional) second, third) :
                //(Configuration.EnableCodeRewriting &&
                // Configuration.EnableSequenceSimplification &&
                // (first is Variable ||
                // first is Quotation)) ? Simplify (second, third) :
                //(Configuration.EnableCodeRewriting &&
                // Configuration.EnableSequenceSimplification &&
                // (second is Variable ||
                // second is Quotation)) ? Simplify (first, third) :
                // (Configuration.EnableCodeRewriting &&
                // Configuration.EnableFlattenSequence &&
                // third is Sequence2) ? Flatten (first, second, (Sequence2) third) :
                //(Configuration.EnableCodeRewriting &&
                // Configuration.EnableFlattenSequence &&
                // second is Sequence2) ? Flatten (first, (Sequence2) second, third) :
                //(Configuration.EnableCodeRewriting &&
                // Configuration.EnableFlattenSequence &&
                // second is Sequence3) ? Flatten (first, (Sequence3) second, third) :
                //(Configuration.EnableCodeRewriting &&
                // Configuration.EnableFlattenSequence &&
                // first is Sequence2) ? Flatten ((Sequence2) first, second, third) :
                //(Configuration.EnableCodeRewriting &&
                // Configuration.EnableFlattenSequence &&
                // first is Sequence3) ? Flatten ((Sequence3) first, second, third) :
                //new Sequence3 (first, second, third);
        }

        public static SCode Make (object first, object second, object third)
        {
            return Make (EnsureSCode (first), EnsureSCode (second), EnsureSCode (third));
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

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Sequence3.EvalStep");
            NoteCalls (this.first);
            NoteCalls (this.second);
            NoteCalls (this.third);
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

        public override bool MutatesAny (Symbol [] formals)
        {
            return this.first.MutatesAny (formals)
                || this.second.MutatesAny (formals)
                || this.third.MutatesAny (formals);
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


        //public override SCode BindVariables (LexicalMap lexicalMap)
        //{
        //    SCode boundFirst = this.first.BindVariables (lexicalMap);
        //    SCode boundSecond = this.second.BindVariables (lexicalMap);
        //    SCode boundThird = this.third.BindVariables (lexicalMap);
        //    return (boundFirst == this.first &&
        //        boundSecond == this.second &&
        //        boundThird == this.third) ?
        //        this :
        //        Sequence3.Make (boundFirst, boundSecond, boundThird);
        //}

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (Sequence3Deserializer));
            info.AddValue ("first", this.first);
            info.AddValue ("second", this.second);
            info.AddValue ("third", this.third);
        }

        #endregion

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialResult r0 = this.first.PartialEval (environment);
            PartialResult r1 = this.second.PartialEval (environment);
            PartialResult r2 = this.third.PartialEval (environment);
            return new PartialResult (r0.Residual == this.first &&
                r1.Residual == this.second &&
                r2.Residual == this.third ? this : Sequence3.Make (r0.Residual, r1.Residual, r2.Residual));
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            this.first.CollectFreeVariables (freeVariableSet);
            this.second.CollectFreeVariables (freeVariableSet);
            this.third.CollectFreeVariables (freeVariableSet);
        }
    }

    [Serializable]
    sealed class Sequence3Deserializer : ISerializable, IObjectReference
    {
        [NonSerialized]
        SCode first;

        [NonSerialized]
        SCode second;

        [NonSerialized]
        SCode third;

        [NonSerialized]
        SCode realObject;

        Sequence3Deserializer (SerializationInfo info, StreamingContext context)
        {
            this.first = (SCode) info.GetValue("first", typeof(SCode));
            this.second = (SCode) info.GetValue("second", typeof(SCode));
            this.third =  (SCode) info.GetValue("third", typeof(SCode));
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            throw new NotImplementedException ();
        }
        #endregion

        #region IObjectReference Members

        public object GetRealObject (StreamingContext context)
        {
            if (this.realObject == null)
                this.realObject = Sequence3.Make (this.first, this.second, this.third);
            return this.realObject;
        }
        #endregion
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
            get { return 3; }
        }

        public object SystemVectorRef (int index)
        {
            switch (index) {
                case 0: return ReturnCode.SEQ_3_DO_3;
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
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.THE_ENVIRONMENT; } }

        static TheEnvironment singleton = new TheEnvironment ();

        TheEnvironment ()
            : base ()
        {
        }

        public static SCode Make ()
        {
            return singleton;
        }

        public override bool CallsTheEnvironment ()
        {
            return true;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("TheEnvironment.EvalStep");
#endif
            answer = environment;
            return false;
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            throw new NotImplementedException ();
        }

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            return new PartialResult (this);
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            throw new NotImplementedException ();
        }
    }
}
