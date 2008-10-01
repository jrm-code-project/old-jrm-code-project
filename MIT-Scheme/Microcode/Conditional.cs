using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Microcode
{
    [Serializable]
    class Conditional : SCode, ISystemHunk3
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type>();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();

        protected readonly Type predicateType;
        protected readonly Type consequentType;
        protected readonly Type alternativeType;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode predicate;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode consequent;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode alternative;

        protected Conditional (SCode predicate, SCode consequent, SCode alternative)
            : base (TC.CONDITIONAL)
        {
            if (predicate == null) throw new ArgumentNullException ("predicate");
            if (consequent == null) throw new ArgumentNullException ("consequent");
            if (alternative == null) throw new ArgumentNullException ("alternative");
            this.predicate = predicate;
            this.consequent = consequent;
            this.alternative = alternative;
#if DEBUG
            this.predicateType = predicate.GetType();
            this.consequentType = consequent.GetType();
            this.alternativeType = alternative.GetType();
#endif
        }

        public static SCode Make (SCode predicate, SCode consequent, SCode alternative)
        {
            if (predicate is Variable
                && consequent is Variable
                && ((Variable) predicate).name == ((Variable) consequent).name) {
                return new Disjunction (predicate, alternative);
            }
            return new Conditional (predicate, consequent, alternative);
        }


        public static SCode Make (object predicate, object consequent, object alternative)
        {
            return Make (EnsureSCode (predicate), EnsureSCode (consequent), EnsureSCode (alternative));
        }

        public static SCode Make (Hunk3 elements)
        {
            if (elements == null) throw new ArgumentNullException ("elements");
            return Conditional.Make (SCode.EnsureSCode (elements.Cxr0),
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
            if (Configuration.EnableSuperOperators)
                return (optPred is Variable
                    && optCons is Variable
                    && ((Variable) optPred).name == ((Variable) optCons).name) ? new Disjunction (optPred, optCons)
                    : (optPred is PrimitiveCombination1) ? PrimitiveConditional1.Make (((PrimitiveCombination1) optPred).Operator, ((PrimitiveCombination1) optPred).Operand, optCons, optAlt)
                    : (optPred is PrimitiveCombination2) ? PrimitiveConditional2.Make (((PrimitiveCombination2) optPred).Rator, ((PrimitiveCombination2) optPred).Rand0, ((PrimitiveCombination2) optPred).Rand1, optCons, optAlt)
                    : (optCons is Quotation) ? ConditionalSQS.Make (optPred, (Quotation) optCons, optAlt)
                    : (optPred == this.predicate
                    && optCons == this.consequent
                    && optAlt == this.alternative) ? this
                    : new Conditional (optPred, optCons, optAlt);
            else
                return (optPred == this.predicate
                && optCons == this.consequent
                && optAlt == this.alternative) ? this
                : new Conditional (optPred, optCons, optAlt);
        }

        public override bool CallsTheEnvironment ()
        {
            return this.predicate.CallsTheEnvironment ()
                || this.consequent.CallsTheEnvironment ()
                || this.alternative.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note(this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note(this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }

        public override bool MutatesAny (object [] formals)
        {
            return this.predicate.MutatesAny (formals)
                || this.consequent.MutatesAny (formals)
                || this.alternative.MutatesAny (formals);
        }

        public override bool UsesAny (object [] formals)
        {
            return this.predicate.UsesAny (formals)
                || this.consequent.UsesAny (formals)
                || this.alternative.UsesAny (formals);
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
                throw new NotImplementedException ();
            }
        }
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr1
        {
            get
            {
                return UnwrapQuoted (this.consequent);
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
                return UnwrapQuoted (this.alternative);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion
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
    class ConditionalSQS : Conditional
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly object consequentQuoted;

        protected ConditionalSQS (SCode predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentQuoted = consequent.Quoted;
        }

        public static SCode Make (SCode predicate, Quotation consequent, SCode alternative)
        {
            return new ConditionalSQS (predicate, consequent, alternative);
        }


        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ("Shouldn't be necessary.");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
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

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = this.consequentQuoted;
                return false;
            }
        }
    }

    [Serializable]
    class PrimitiveConditional1 : Conditional
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();

        Type arg0Type;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Primitive1 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        protected PrimitiveMethod1 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode arg0;

        protected PrimitiveConditional1 (Primitive1 procedure, SCode arg0, SCode consequent, SCode alternative)
            : base (PrimitiveCombination1.Make(procedure, arg0), consequent, alternative)
        {
            this.procedure = procedure;
            this.method = procedure.Method;
            this.arg0 = arg0;
#if DEBUG
            this.arg0Type = this.arg0.GetType();
#endif
        }

        public static SCode Make (Primitive1 procedure, SCode arg0, SCode consequent, SCode alternative)
        {
            return (arg0 is Argument) ? PrimitiveConditional1A.Make (procedure, (Argument) arg0, consequent, alternative)
                :  new PrimitiveConditional1 (procedure, arg0, consequent, alternative);
        }

 
        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ("shouldn't be necessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.arg0);
            procedureHistogram.Note (this.procedure);
            arg0TypeHistogram.Note (this.arg0Type);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0 = null;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            //Control unev = this.predicate;
            //Environment env = environment;
            //while (unev.EvalStep (out answer, ref unev, ref env)) { };
            //if (answer == Interpreter.UnwindStack) {
            //    ((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
            //    environment = env;
            //    answer = Interpreter.UnwindStack;
            //    return false;

            //}

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PrimitiveConditional1A : PrimitiveConditional1
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        int arg0offset;

        PrimitiveConditional1A (Primitive1 procedure, Argument arg0, SCode consequent, SCode alternative)
            : base (procedure, arg0, consequent, alternative)
        {
            this.arg0offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 procedure, Argument arg0, SCode consequent, SCode alternative)
        {
            return new PrimitiveConditional1A (procedure, arg0, consequent, alternative);
        }


        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ("shouldn't be necessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            procedureHistogram.Note (this.procedure);
#endif
            //Control unev0 = this.arg0;
            //Environment env = environment;
            //object ev0 = null;
            //while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            //if (ev0 == Interpreter.UnwindStack) {
            //    throw new NotImplementedException ();
            //    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
            //    //answer = Interpreter.UnwindStack;
            //    //environment = env;
            //    //return false;
            //}

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue(this.arg0offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            //Control unev = this.predicate;
            //Environment env = environment;
            //while (unev.EvalStep (out answer, ref unev, ref env)) { };
            //if (answer == Interpreter.UnwindStack) {
            //    ((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
            //    environment = env;
            //    answer = Interpreter.UnwindStack;
            //    return false;

            //}

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls(this.alternative);
                alternativeTypeHistogram.Note(this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note(this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PrimitiveConditional2 : Conditional
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();

        protected Type rand0Type;
        protected Type rand1Type;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Primitive2 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        protected PrimitiveMethod2 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode arg0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode arg1;

        protected PrimitiveConditional2 (Primitive2 procedure, SCode arg0, SCode arg1, SCode consequent, SCode alternative)
            : base (PrimitiveCombination2.Make (procedure, arg0, arg1), consequent, alternative)
        {
            this.procedure = procedure;
            this.method = this.procedure.Method;
            this.arg0 = arg0;
            this.arg1 = arg1;
#if DEBUG
            rand0Type = arg0.GetType ();
            rand1Type = arg1.GetType ();
#endif
        }

        public static SCode Make (Primitive2 procedure, SCode arg0, SCode arg1, SCode consequent, SCode alternative)
        {
            return 
                (arg0 is Argument) ? PrimitiveConditional2AS.Make (procedure, (Argument) arg0, arg1, consequent, alternative)
                : (arg0 is LexicalVariable) ? PrimitiveConditional2LS.Make (procedure, (LexicalVariable) arg0, arg1, consequent, alternative)
                : (arg0 is Quotation) ? PrimitiveConditional2QS.Make (procedure, (Quotation) arg0, arg1, consequent, alternative)
                : (arg1 is LexicalVariable) ? PrimitiveConditional2SL.Make (procedure, arg0, (LexicalVariable) arg1, consequent, alternative)
                : (arg1 is Quotation) ? PrimitiveConditional2SQ.Make (procedure, arg0, (Quotation) arg1, consequent, alternative)
                : new PrimitiveConditional2 (procedure, arg0, arg1, consequent, alternative);
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ("shouldn't be necessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.arg0);
            noteCalls (this.arg1);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.arg1;
            Environment env = environment;
            object ev1 = null;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.arg0;
            env = environment;
            object ev0 = null;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note(this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PrimitiveConditional2AS : PrimitiveConditional2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly int a0offset;

        protected PrimitiveConditional2AS (Primitive2 procedure, Argument arg0, SCode arg1, SCode consequent, SCode alternative)
            : base (procedure, arg0, arg1, consequent, alternative)
        {
            this.a0offset = arg0.Offset;
        }

        public static SCode Make (Primitive2 procedure, Argument arg0, SCode arg1, SCode consequent, SCode alternative)
        {
            return 
                (arg1 is LexicalVariable) ? PrimitiveConditional2AL.Make (procedure, arg0, (LexicalVariable) arg1, consequent, alternative)
                : (arg1 is Quotation) ? PrimitiveConditional2AQ.Make (procedure, arg0, (Quotation) arg1, consequent, alternative)
                : new PrimitiveConditional2AS (procedure, arg0, arg1, consequent, alternative);
        }


        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ("shouldn't be necessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.arg1);
#endif
            Control unev = this.arg1;
            Environment env = environment;
            object ev1 = null;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            //unev = this.arg0;
            //env = environment;
            //object ev0 = null;
            //while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            //if (ev0 == Interpreter.UnwindStack) {
            //    throw new NotImplementedException ();
            //    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
            //    //answer = Interpreter.UnwindStack;
            //    //environment = env;
            //    //return false;
            //}

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue(this.a0offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

                        if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note(this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PrimitiveConditional2AL : PrimitiveConditional2AS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly string l1name;
        protected readonly int l1depth;
        protected readonly int l1offset;


        protected PrimitiveConditional2AL (Primitive2 procedure, Argument arg0, LexicalVariable arg1, SCode consequent, SCode alternative)
            : base (procedure, arg0, arg1, consequent, alternative)
        {

            this.l1name = arg1.name;
            this.l1depth = arg1.Depth;
            this.l1offset = arg1.Offset;
        }

        public static SCode Make (Primitive2 procedure, Argument arg0, LexicalVariable arg1, SCode consequent, SCode alternative)
        {
            return new PrimitiveConditional2AL (procedure, arg0, arg1, consequent, alternative);
        }


        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ("shouldn't be necessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            //Control unev = this.arg1;
            //Environment env = environment;
            //object ev1 = null;
            //while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            //if (ev1 == Interpreter.UnwindStack) {
            //    throw new NotImplementedException ();
            //}

            //unev = this.arg0;
            //env = environment;
            //object ev0 = null;
            //while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            //if (ev0 == Interpreter.UnwindStack) {
            //    throw new NotImplementedException ();
            //    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
            //    //answer = Interpreter.UnwindStack;
            //    //environment = env;
            //    //return false;
            //}

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev1 = null;
            if (environment.FastLexicalRef (out ev1, this.l1name, this.l1depth, this.l1offset))
                throw new NotImplementedException ();
            if (this.method (out answer, environment.ArgumentValue (this.a0offset), ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
                        if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note(this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PrimitiveConditional2AQ : PrimitiveConditional2AS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly object q1quoted;


        PrimitiveConditional2AQ (Primitive2 procedure, Argument arg0, Quotation arg1, SCode consequent, SCode alternative)
            : base (procedure, arg0, arg1, consequent, alternative)
        {
            this.q1quoted = arg1.Quoted;
        }

        public static SCode Make (Primitive2 procedure, Argument arg0, Quotation arg1, SCode consequent, SCode alternative)
        {
            return new PrimitiveConditional2AQ (procedure, arg0, arg1, consequent, alternative);
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ("shouldn't be necessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            procedureHistogram.Note (this.procedure);
#endif
            //Control unev = this.arg1;
            //Environment env = environment;
            //object ev1 = null;
            //while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            //if (ev1 == Interpreter.UnwindStack) {
            //    throw new NotImplementedException ();
            //}

            //unev = this.arg0;
            //env = environment;
            //object ev0 = null;
            //while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            //if (ev0 == Interpreter.UnwindStack) {
            //    throw new NotImplementedException ();
            //    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
            //    //answer = Interpreter.UnwindStack;
            //    //environment = env;
            //    //return false;
            //}

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, environment.ArgumentValue (this.a0offset), this.q1quoted)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

                        if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note(this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PrimitiveConditional2LS : PrimitiveConditional2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly string l0name;
        protected readonly int l0depth;
        protected readonly int l0offset;

        PrimitiveConditional2LS (Primitive2 procedure, LexicalVariable arg0, SCode arg1, SCode consequent, SCode alternative)
            : base (procedure, arg0, arg1, consequent, alternative)
        {
            this.l0name = arg0.name;
            this.l0depth = arg0.Depth;
            this.l0offset = arg0.Offset;
        }

        public static SCode Make (Primitive2 procedure, LexicalVariable arg0, SCode arg1, SCode consequent, SCode alternative)
        {
            return new PrimitiveConditional2LS (procedure, arg0, arg1, consequent, alternative);
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ("shouldn't be necessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.arg1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.arg1;
            Environment env = environment;
            object ev1 = null;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev0 = null;
            if (environment.FastLexicalRef (out ev0, this.l0name, this.l0depth, this.l0offset))
                throw new NotImplementedException ();

            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }

        }
    }

    [Serializable]
    class PrimitiveConditional2QS : PrimitiveConditional2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly object quotedValue;

        protected PrimitiveConditional2QS (Primitive2 procedure, Quotation arg0, SCode arg1, SCode consequent, SCode alternative)
            : base (procedure, arg0, arg1, consequent, alternative)
        {
            this.quotedValue = arg0.Quoted;
        }

        public static SCode Make (Primitive2 procedure, Quotation arg0, SCode arg1, SCode consequent, SCode alternative)
        {
            return (consequent is Quotation) ? PrimitiveConditional2QSQS.Make (procedure, arg0, arg1, (Quotation) consequent, alternative)
                : new PrimitiveConditional2QS (procedure, arg0, arg1, consequent, alternative);
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ("shouldn't be necessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.arg1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.arg1;
            Environment env = environment;
            object ev1 = null;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, this.quotedValue, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PrimitiveConditional2SL : PrimitiveConditional2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly string l1name;
        protected readonly int l1depth;
        protected readonly int l1offset;

        PrimitiveConditional2SL (Primitive2 procedure, SCode arg0, LexicalVariable arg1, SCode consequent, SCode alternative)
            : base (procedure, arg0, arg1, consequent, alternative)
        {
            this.l1name = arg1.name;
            this.l1depth = arg1.Depth;
            this.l1offset = arg1.Offset;
        }

        public static SCode Make (Primitive2 procedure, SCode arg0, LexicalVariable arg1, SCode consequent, SCode alternative)
        {
            return new PrimitiveConditional2SL (procedure, arg0, arg1, consequent, alternative);
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ("shouldn't be necessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.arg0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            object ev1 = null;
            if (environment.FastLexicalRef (out ev1, this.l1name, this.l1depth, this.l1offset))
                throw new NotImplementedException ();

            Control unev = this.arg0;
            Environment env = environment;
            object ev0 = null;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PrimitiveConditional2SQ : PrimitiveConditional2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly object quotedValue;

        PrimitiveConditional2SQ (Primitive2 procedure, SCode arg0, Quotation arg1, SCode consequent, SCode alternative)
            : base (procedure, arg0, arg1, consequent, alternative)
        {
            this.quotedValue = arg1.Quoted;
        }

        public static SCode Make (Primitive2 procedure, SCode arg0, Quotation arg1, SCode consequent, SCode alternative)
        {
            return new PrimitiveConditional2SQ (procedure, arg0, arg1, consequent, alternative);
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ("shouldn't be necessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.arg0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
#endif
            Control unev = this.arg0;
            Environment env = environment;
            object ev0 = null;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, ev0, this.quotedValue)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
                        if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note(this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                return true;
            }
        }
    }

    [Serializable]
    class PrimitiveConditional2QSQS : PrimitiveConditional2QS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly object quotedConsequent;

        protected PrimitiveConditional2QSQS (Primitive2 procedure, Quotation arg0, SCode arg1, Quotation consequent, SCode alternative)
            : base (procedure, arg0, arg1, consequent, alternative)
        {
            this.quotedConsequent = consequent.Quoted;
        }

        public static SCode Make (Primitive2 procedure, Quotation arg0, SCode arg1, Quotation consequent, SCode alternative)
        {
            return (arg1 is Argument) ? PrimitiveConditional2QAQS.Make (procedure, arg0, (Argument) arg1, consequent, alternative)
                : new PrimitiveConditional2QSQS (procedure, arg0, arg1, consequent, alternative);
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ("shouldn't be necessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.arg1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.arg1;
            Environment env = environment;
            object ev1 = null;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, this.quotedValue, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note(this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = this.quotedConsequent;
                return false;
            }
        }
    }

    [Serializable]
    class PrimitiveConditional2QAQS : PrimitiveConditional2QSQS
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        [NonSerialized]
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly int a1offset;

        PrimitiveConditional2QAQS (Primitive2 procedure, Quotation arg0, Argument arg1, Quotation consequent, SCode alternative)
            : base (procedure, arg0, arg1, consequent, alternative)
        {
            this.a1offset = arg1.Offset;
        }

        public static SCode Make (Primitive2 procedure, Quotation arg0, Argument arg1, Quotation consequent, SCode alternative)
        {
            return new PrimitiveConditional2QAQS (procedure, arg0, arg1, consequent, alternative);
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            throw new NotImplementedException ("shouldn't be necessary");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            procedureHistogram.Note (this.procedure);
#endif


            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            if (this.method (out answer, this.quotedValue, environment.ArgumentValue(this.a1offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                return true;
            }
            else {
                answer = this.quotedConsequent;
                return false;
            }
        }
    }

}