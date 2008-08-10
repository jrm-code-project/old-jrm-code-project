using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    public abstract class SCode: SchemeObject
    {
        protected SCode (TC typeCode) : base (typeCode) { }

        // return value is so this is an expression
        internal abstract object EvalStep (Interpreter interpreter, object etc);
        internal abstract SCode Optimize (CompileTimeEnvironment ctenv);

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
        static long evaluationCount;
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

        internal override SCode Optimize (CompileTimeEnvironment ctenv)
        {
            return new Comment (this.code.Optimize (ctenv),
                                this.text);
        }
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

        internal override SCode Optimize (CompileTimeEnvironment ctenv)
        {
            return new Conditional (this.predicate.Optimize (ctenv),
                                    this.consequent.Optimize (ctenv),
                                    this.alternative.Optimize (ctenv));
        }
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

        internal override SCode Optimize (CompileTimeEnvironment ctenv)
        {
            return new Definition (this.name, this.value.Optimize (ctenv));
        }
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
            : base (TC.DELAY)
        {
            this.body = EnsureSCode(body);
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Delay.evaluationCount += 1;
            return interpreter.Return (new Promise (this.body, interpreter.Environment));
        }

        internal override SCode Optimize (CompileTimeEnvironment ctenv)
        {
            return new Delay (this.body.Optimize (ctenv));
        }
    }

    sealed class Disjunction : SCode, ISystemPair
    {
        static long evaluationCount;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode predicate;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode alternative;

        public Disjunction (object predicate, object alternative)
            : base (TC.DISJUNCTION)
        {
            this.predicate = EnsureSCode(predicate);
            this.alternative = EnsureSCode(alternative);
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

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Disjunction.evaluationCount += 1;
            return interpreter.EvalNewSubproblem (this.predicate, new DisjunctionDecide (interpreter.Continuation, this, interpreter.Environment));
        }

        internal override SCode Optimize (CompileTimeEnvironment ctenv)
        {
            return new Disjunction (this.predicate.Optimize (ctenv),
                                    this.alternative.Optimize (ctenv));
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

        internal override SCode Optimize (CompileTimeEnvironment ctenv)
        {
            return this;
        }
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

        internal override SCode Optimize (CompileTimeEnvironment ctenv)
        {
            return new Sequence2 (this.first.Optimize (ctenv),
                                  this.second.Optimize (ctenv));
        }
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

        internal override SCode Optimize (CompileTimeEnvironment ctenv)
        {
            return new Sequence3 (this.first.Optimize (ctenv),
                                  this.second.Optimize (ctenv),
                                  this.third.Optimize (ctenv));
        }
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
            : base (TC.THE_ENVIRONMENT)
        {
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            TheEnvironment.evaluationCount += 1;
            return interpreter.Return (interpreter.Environment);
        }

        internal override SCode Optimize (CompileTimeEnvironment ctenv)
        {
            return this;
        }
    }

}
