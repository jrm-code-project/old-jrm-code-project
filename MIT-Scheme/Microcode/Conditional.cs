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
            return
                (Configuration.EnableSuperOperators && predicate is Argument && consequent is Argument && alternative is Argument) ? ConditionalAAA.Make ((Argument) predicate, (Argument) consequent, (Argument) alternative)
                : (Configuration.EnableSuperOperators && predicate is Argument && consequent is Argument) ? ConditionalAAS.Make ((Argument) predicate, (Argument) consequent, alternative)
                : (Configuration.EnableSuperOperators && predicate is Argument && alternative is Argument) ? ConditionalASA.Make ((Argument) predicate, consequent, (Argument) alternative)
                : (Configuration.EnableSuperOperators && consequent is Argument && alternative is Argument) ? ConditionalSAA.Make (predicate, (Argument) consequent, (Argument) alternative)
                : (Configuration.EnableSuperOperators && predicate is Argument) ? ConditionalASS.Make ((Argument) predicate, consequent, alternative)
                : (Configuration.EnableSuperOperators && consequent is Argument) ? ConditionalSAS.Make (predicate, (Argument) consequent, alternative)
                : (Configuration.EnableSuperOperators && alternative is Argument) ? ConditionalSSA.Make (predicate, consequent, (Argument) alternative)
                : new Conditional (predicate, consequent, alternative);
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

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode optPred = this.predicate.Bind (ctenv);
            SCode optCons = this.consequent.Bind (ctenv);
            SCode optAlt = this.alternative.Bind (ctenv);
            return 
                //(optPred is Variable
            //    && optCons is Variable
            //    && ((Variable) optPred).varname == ((Variable) optCons).varname) ? new Disjunction (optPred, optCons)
            //    : 
            //(Configuration.EnableSuperOperators && optPred is PrimitiveConditional1) ? ConditionalPC1.Make ((PrimitiveConditional1) optPred, optCons, optAlt)
            //: (Configuration.EnableSuperOperators && optPred is PrimitiveConditional2) ? ConditionalPC2.Make ((PrimitiveConditional2) optPred, optCons, optAlt)
            //: (Configuration.EnableSuperOperators && optPred is PrimitiveCombination1) ? PrimitiveConditional1.Make (((PrimitiveCombination1) optPred).Operator, ((PrimitiveCombination1) optPred).Operand, optCons, optAlt)
            //    : (Configuration.EnableSuperOperators && optPred is PrimitiveCombination2) ? PrimitiveConditional2.Make (((PrimitiveCombination2) optPred).Rator, ((PrimitiveCombination2) optPred).Rand0, ((PrimitiveCombination2) optPred).Rand1, optCons, optAlt)
            //: (Configuration.EnableSuperOperators && optCons is Quotation) ? ConditionalSQS.Make (optPred, (Quotation) optCons, optAlt)
            //: (Configuration.EnableSuperOperators && optPred is Argument) ? ConditionalA.Make ((Argument) optPred, optCons, optAlt)
            //    : 
            (optPred == this.predicate
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
            object ev; 
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
            if (ev == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;

            }

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note(this.alternativeType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note(this.consequentType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #t");
#endif
                expression = this.consequent;
                answer = null;
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
    sealed class Disjunction : SCode, ISystemPair
    {
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

        public override SCode Bind (LexicalMap ctenv)
        {
            return new Disjunction (this.predicate.Bind (ctenv),
                                    this.alternative.Bind (ctenv));
        }

        public override bool CallsTheEnvironment ()
        {
            return this.predicate.CallsTheEnvironment ()
                || this.alternative.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.predicate);
#endif
            Environment env = environment;
            Control pred = this.predicate;
            object ev;
            while (pred.EvalStep (out ev, ref pred, ref env)) { };
            if (ev == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new DisjunctionFrame (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            if (ev is bool && (bool) ev == false) {
#if DEBUG
                noteCalls (this.alternative);
#endif
                // tail call alternative
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                // return answer
                answer = ev;
                return false;
            }
        }

        public override bool MutatesAny (object [] formals)
        {
            return this.predicate.MutatesAny (formals)
                || this.alternative.MutatesAny (formals);
        }

        public override bool UsesAny (object [] formals)
        {
            return this.predicate.UsesAny (formals)
                || this.alternative.UsesAny (formals);
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
    sealed class DisjunctionFrame : SubproblemContinuation<Disjunction>, ISystemVector
    {
        public DisjunctionFrame (Disjunction disjunction, Environment environment)
            : base (disjunction, environment)
        {
        }

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            answer = value;
            if (value is bool && (bool) value == false) {
                // tail call alternative
                expression = this.expression.Alternative;
                return true;
            }
            else {
                return false;
            }
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
    class ConditionalASS : Conditional
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected int argOffset;

        protected ConditionalASS (Argument predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.argOffset = predicate.Offset;
        }

        public static SCode Make (Argument predicate, SCode consequent, SCode alternative)
        {
            return new ConditionalASS (predicate, consequent, alternative);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev = environment.ArgumentValue (this.argOffset);

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #t");
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class ConditionalSAS : Conditional
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected int consequentOffset;

        protected ConditionalSAS (SCode predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (SCode predicate, Argument consequent, SCode alternative)
        {
            return new ConditionalSAS (predicate, consequent, alternative);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            predicateTypeHistogram.Note (this.predicateType);
            noteCalls (this.predicate);
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
            if (ev == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;

            }

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                Debug.WriteLineIf (Primitive.Noisy, "    => #t");
#endif
                expression = this.consequent;
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class ConditionalSSA : Conditional
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif
        protected int alternativeOffset;

        protected ConditionalSSA (SCode predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (SCode predicate, SCode consequent, Argument alternative)
        {
            return new ConditionalSSA (predicate, consequent, alternative);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
            if (ev == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;

            }

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #t");
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }

        }
    }

    [Serializable]
    class ConditionalAAS : Conditional
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
#endif
        protected int predicateOffset;
        protected int consequentOffset;
        protected ConditionalAAS (Argument predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateOffset = predicate.Offset;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (Argument predicate, Argument consequent, SCode alternative)
        {
            return new ConditionalAAS (predicate, consequent, alternative);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                noteCalls (this.alternative);

                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                Debug.WriteLineIf (Primitive.Noisy, "    => #t");
#endif
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }

        }
    }


    [Serializable]
    class ConditionalASA : Conditional
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
#endif
        protected int predicateOffset;
        protected int alternativeOffset;

        protected ConditionalASA (Argument predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateOffset = predicate.Offset;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (Argument predicate, SCode consequent, Argument alternative)
        {
            return new ConditionalASA (predicate, consequent, alternative);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG

                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                expression = this.alternative;
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #t");
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }


        }
    }


    [Serializable]
    class ConditionalSAA : Conditional
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif
        protected int consequentOffset;
        protected int alternativeOffset;

        protected ConditionalSAA (SCode predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (SCode predicate, Argument consequent, Argument alternative)
        {
            return new ConditionalSAA (predicate, consequent, alternative);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
            if (ev == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;

            }
            if ((ev is bool) && (bool) ev == false) {
#if DEBUG

                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                Debug.WriteLineIf (Primitive.Noisy, "    => #t");
#endif
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }

        }
    }


    [Serializable]
    class ConditionalAAA : Conditional
    {

        protected ConditionalAAA (Argument predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (Argument predicate, Argument consequent, Argument alternative)
        {
            return new ConditionalAAA (predicate, consequent, alternative);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();

        }
    }


//    [Serializable]
//    class ConditionalSQS : Conditional
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        protected readonly object consequentQuoted;

//        protected ConditionalSQS (SCode predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentQuoted = consequent.Quoted;
//        }

//        public static SCode Make (SCode predicate, Quotation consequent, SCode alternative)
//        {
//            return new ConditionalSQS (predicate, consequent, alternative);
//        }


//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Shouldn't be necessary.");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            Control unev = this.predicate;
//            Environment env = environment;
//            while (unev.EvalStep (out answer, ref unev, ref env)) { };
//            if (answer == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;

//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = this.consequentQuoted;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalPC1 : Conditional
//    {
//        Primitive1 procedure;

//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        [NonSerialized]
//        protected PrimitiveMethod1 method;

//        SCode arg0;
//        SCode pcCons;
//        SCode pcAlt;

//        protected ConditionalPC1 (PrimitiveConditional1 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.procedure = predicate.procedure;
//            this.method = this.procedure.Method;
//            this.arg0 = predicate.arg0;
//            this.pcCons = predicate.Consequent;
//            this.pcAlt = predicate.Alternative;
//        }

//        public static SCode Make (PrimitiveConditional1 predicate, SCode consequent, SCode alternative)
//        {
//            return new ConditionalPC1 (predicate, consequent, alternative);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            Control unev = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            unev = ((answer is bool) && (bool) answer == false)
//                ? this.pcAlt
//                : this.pcCons;
//            env = environment;
//            while (unev.EvalStep (out answer, ref unev, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }
//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }


//    [Serializable]
//    class ConditionalPC2 : Conditional
//    {
//        Primitive2 procedure;

//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        [NonSerialized]
//        protected PrimitiveMethod2 method;

//        SCode arg0;
//        SCode arg1;
//        SCode pcCons;
//        SCode pcAlt;

//        protected ConditionalPC2 (PrimitiveConditional2 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.procedure = predicate.procedure;
//            this.method = this.procedure.Method;
//            this.arg0 = predicate.arg0;
//            this.arg1 = predicate.arg1;
//            this.pcCons = predicate.Consequent;
//            this.pcAlt = predicate.Alternative;
//        }

//        public static SCode Make (PrimitiveConditional2 predicate, SCode consequent, SCode alternative)
//        {
//            return new ConditionalPC2 (predicate, consequent, alternative);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            Control unev = this.arg1;
//            Environment env = environment;
//            object ev1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//            }

//            unev = this.arg0;
//            env = environment;
//            object ev0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            unev = ((answer is bool) && (bool) answer == false)
//                ? this.pcAlt
//                : this.pcCons;
//            env = environment;
//            while (unev.EvalStep (out answer, ref unev, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }
//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//}

//    [Serializable]
//    class PrimitiveConditional1 : Conditional
//    {
//#if DEBUG
//        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
//        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

//        protected Type arg0Type;
//#endif
//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        public readonly Primitive1 procedure;

//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        [NonSerialized]
//        protected PrimitiveMethod1 method;

//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        public readonly SCode arg0;

//        protected PrimitiveConditional1 (Primitive1 procedure, SCode arg0, SCode consequent, SCode alternative)
//            : base (PrimitiveCombination1.Make (procedure, arg0), consequent, alternative)
//        {
//            this.procedure = procedure;
//            this.method = procedure.Method;
//            this.arg0 = arg0;
//#if DEBUG
//            this.arg0Type = this.arg0.GetType ();
//#endif
//        }

//        public static SCode Make (Primitive1 procedure, SCode arg0, SCode consequent, SCode alternative)
//        {
//            return (arg0 is Argument) ? PrimitiveConditional1A.Make (procedure, (Argument) arg0, consequent, alternative)
//                 : (consequent is Quotation) ? PrimitiveConditional1SQS.Make (procedure, arg0, (Quotation) consequent, alternative)
//                 : new PrimitiveConditional1 (procedure, arg0, consequent, alternative);
//        }


//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg0);
//            procedureHistogram.Note (this.procedure);
//            arg0TypeHistogram.Note (this.arg0Type);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            //Control unev = this.predicate;
//            //Environment env = environment;
//            //while (unev.EvalStep (out answer, ref unev, ref env)) { };
//            //if (answer == Interpreter.UnwindStack) {
//            //    ((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
//            //    environment = env;
//            //    answer = Interpreter.UnwindStack;
//            //    return false;

//            //}

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PrimitiveConditional1A : PrimitiveConditional1
//    {
//#if DEBUG
//        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        int arg0offset;

//        protected PrimitiveConditional1A (Primitive1 procedure, Argument arg0, SCode consequent, SCode alternative)
//            : base (procedure, arg0, consequent, alternative)
//        {
//            this.arg0offset = arg0.Offset;
//        }

//        public static SCode Make (Primitive1 procedure, Argument arg0, SCode consequent, SCode alternative)
//        {
//            return 
//                (arg0 is Argument0) ? PrimitiveConditional1A0.Make (procedure, (Argument0) arg0, consequent, alternative)
//                :(arg0 is Argument1) ? PrimitiveConditional1A1.Make (procedure, (Argument1) arg0, consequent, alternative)
//                : new PrimitiveConditional1A (procedure, arg0, consequent, alternative);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            procedureHistogram.Note (this.procedure);
//#endif
//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, environment.ArgumentValue (this.arg0offset))) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            //Control unev = this.predicate;
//            //Environment env = environment;
//            //while (unev.EvalStep (out answer, ref unev, ref env)) { };
//            //if (answer == Interpreter.UnwindStack) {
//            //    ((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
//            //    environment = env;
//            //    answer = Interpreter.UnwindStack;
//            //    return false;

//            //}

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PrimitiveConditional1A0 : PrimitiveConditional1A
//    {
//#if DEBUG
//        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        PrimitiveConditional1A0 (Primitive1 procedure, Argument0 arg, SCode consequent, SCode alternative)
//            : base (procedure, arg, consequent, alternative)
//        {
//        }

//        public static SCode Make (Primitive1 procedure, Argument0 arg, SCode consequent, SCode alternative)
//        {
//            return
//                (procedure == Primitive.Find ("PAIR?", 1)) ? PrimitiveIsPairA0.Make (procedure, arg, consequent, alternative)
//            : (procedure == Primitive.Find ("NULL?", 1)) ? PrimitiveIsNullA0.Make (procedure, arg, consequent, alternative)
//             :   new PrimitiveConditional1A0 (procedure, arg, consequent, alternative);
//        }


//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            procedureHistogram.Note (this.procedure);
//#endif
//            //Control unev0 = this.arg0;
//            //Environment env = environment;
//            //object ev0 = null;
//            //while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            //if (ev0 == Interpreter.UnwindStack) {
//            //    throw new NotImplementedException ();
//            //    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
//            //    //answer = Interpreter.UnwindStack;
//            //    //environment = env;
//            //    //return false;
//            //}

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, environment.Argument0Value)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            //Control unev = this.predicate;
//            //Environment env = environment;
//            //while (unev.EvalStep (out answer, ref unev, ref env)) { };
//            //if (answer == Interpreter.UnwindStack) {
//            //    ((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
//            //    environment = env;
//            //    answer = Interpreter.UnwindStack;
//            //    return false;

//            //}

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PrimitiveConditional1A1 : PrimitiveConditional1
//    {
//#if DEBUG
//        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected PrimitiveConditional1A1 (Primitive1 procedure, Argument1 arg, SCode consequent, SCode alternative)
//            : base (procedure, arg, consequent, alternative)
//        {
//        }

//        public static SCode Make (Primitive1 procedure, Argument1 arg, SCode consequent, SCode alternative)
//        {           return
//                (procedure == Primitive.Find ("PAIR?", 1)) ? PrimitiveIsPairA1.Make ( arg, consequent, alternative)
//            : (procedure == Primitive.Find ("NULL?", 1)) ? PrimitiveIsNullA1.Make ( arg, consequent, alternative)
 
//            :  new PrimitiveConditional1A1 (procedure, arg, consequent, alternative);
//        }


//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            procedureHistogram.Note (this.procedure);
//#endif
//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, environment.Argument1Value)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            //Control unev = this.predicate;
//            //Environment env = environment;
//            //while (unev.EvalStep (out answer, ref unev, ref env)) { };
//            //if (answer == Interpreter.UnwindStack) {
//            //    ((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
//            //    environment = env;
//            //    answer = Interpreter.UnwindStack;
//            //    return false;

//            //}

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PrimitiveIsNullA0 : PrimitiveConditional1A
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        PrimitiveIsNullA0 (Primitive1 procedure, Argument0 arg, SCode consequent, SCode alternative)
//            : base (procedure, arg, consequent, alternative)
//        {
//        }

//        public static SCode Make (Primitive1 procedure, Argument0 arg, SCode consequent, SCode alternative)
//        {
//            return (consequent is Quotation) ? PrimitiveIsNullA0QS.Make (procedure, arg, (Quotation) consequent, alternative)
//                : new PrimitiveIsNullA0 (procedure, arg, consequent, alternative);
//        }


//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif

//            if (environment.Argument0Value == null) {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//            }
//            answer = null;
//            return true;
//        }
//    }

//    [Serializable]
//    class PrimitiveIsNullA0QS : PrimitiveConditional1A
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected object quoted;
//        PrimitiveIsNullA0QS (Primitive1 procedure, Argument0 arg, Quotation consequent, SCode alternative)
//            : base (procedure, arg, consequent, alternative)
//        {
//            this.quoted = consequent.Quoted;
//        }

//        public static SCode Make (Primitive1 procedure, Argument0 arg, Quotation consequent, SCode alternative)
//        {
//            return  new PrimitiveIsNullA0QS (procedure, arg, consequent, alternative);
//        }


//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif

//            if (environment.Argument0Value == null) {
//                answer = this.quoted;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif          
//                expression = this.alternative;
//                answer = null;  
//                return true;
//            }

//        }
//    }
    
//    [Serializable]
//    class PrimitiveIsPairA0 : PrimitiveConditional1A
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected PrimitiveIsPairA0 (Primitive1 procedure, Argument0 arg, SCode consequent, SCode alternative)
//            : base (procedure, arg, consequent, alternative)
//        {
//        }

//        public static SCode Make (Primitive1 procedure, Argument0 arg, SCode consequent, SCode alternative)
//        {
//            return (consequent is PrimitiveCarA0) 
//                ? PrimitiveIsPairA0CarA0.Make (procedure, arg, (PrimitiveCarA0) consequent, alternative)
//                : new PrimitiveIsPairA0 (procedure, arg, consequent, alternative);
//        }


//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif

//            if (environment.Argument0Value is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;

//            }
//            answer = null;
//            return true;
//        }
//    }

//    [Serializable]
//    class PrimitiveIsPairA0CarA0 : PrimitiveIsPairA0
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        PrimitiveIsPairA0CarA0 (Primitive1 procedure, Argument0 arg, PrimitiveCarA0 consequent, SCode alternative)
//            : base (procedure, arg, consequent, alternative)
//        {
//        }

//        public static SCode Make (Primitive1 procedure, Argument0 arg, PrimitiveCarA0 consequent, SCode alternative)
//        {
//            return  new PrimitiveIsPairA0CarA0 (procedure, arg, consequent, alternative);
//        }


//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            Cons a0 = environment.Argument0Value as Cons;
//            if (a0 == null) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;

//            }
//            else {
//                answer = a0.Car;
//                return false;
//            }

//        }
//    }

//    [Serializable]
//    class PrimitiveIsNullA1 : PrimitiveConditional1A1
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        PrimitiveIsNullA1 (Argument1 arg, SCode consequent, SCode alternative)
//            : base ((Primitive1)Primitive.Find ("NULL?", 1), arg, consequent, alternative)
//        {
//        }

//        public static SCode Make (Argument1 arg, SCode consequent, SCode alternative)
//        {
//            return // (consequent is Quotation) ? PrimitiveIsNullA0QS.Make (procedure, arg, (Quotation) consequent, alternative)
//            //    : 
//            new PrimitiveIsNullA1 (arg, consequent, alternative);
//        }


//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif

//            if (environment.Argument1Value == null) {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//            }
//            answer = null;
//            return true;
//        }
//    }

//    [Serializable]
//    class PrimitiveIsPairA1 : PrimitiveConditional1A1
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected PrimitiveIsPairA1 (Argument1 arg, SCode consequent, SCode alternative)
//            : base ((Primitive1)Primitive.Find ("PAIR?", 1), arg, consequent, alternative)
//        {
//        }

//        public static SCode Make (Argument1 arg, SCode consequent, SCode alternative)
//        {
//            return 
//                //(consequent is PrimitiveCarA0)
//                //? PrimitiveIsPairA0CarA0.Make (procedure, arg, (PrimitiveCarA0) consequent, alternative)
//                //: 
//                new PrimitiveIsPairA1 (arg, consequent, alternative);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif

//            if (environment.Argument1Value is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;

//            }
//            answer = null;
//            return true;
//        }
//    }


//    [Serializable]
//    class PrimitiveConditional1SQS : PrimitiveConditional1
//    {
//#if DEBUG
//        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
//        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected readonly object quoted;

//        protected PrimitiveConditional1SQS (Primitive1 procedure, SCode arg0, Quotation consequent, SCode alternative)
//            : base (procedure, arg0, consequent, alternative)
//        {
//            this.quoted = consequent.Quoted;
//        }

//        public static SCode Make (Primitive1 procedure, SCode arg0, Quotation consequent, SCode alternative)
//        {
//            return new PrimitiveConditional1SQS (procedure, arg0, consequent, alternative);
//        }


//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg0);
//            procedureHistogram.Note (this.procedure);
//            arg0TypeHistogram.Note (this.arg0Type);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            //Control unev = this.predicate;
//            //Environment env = environment;
//            //while (unev.EvalStep (out answer, ref unev, ref env)) { };
//            //if (answer == Interpreter.UnwindStack) {
//            //    ((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
//            //    environment = env;
//            //    answer = Interpreter.UnwindStack;
//            //    return false;

//            //}

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = this.quoted;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PrimitiveConditional2 : Conditional
//    {
//#if DEBUG
//        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
//        [NonSerialized]
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

//        protected Type rand0Type;
//        protected Type rand1Type;
//#endif
//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        public readonly Primitive2 procedure;

//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        [NonSerialized]
//        protected PrimitiveMethod2 method;

//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        public readonly SCode arg0;

//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        public readonly SCode arg1;

//        protected PrimitiveConditional2 (Primitive2 procedure, SCode arg0, SCode arg1, SCode consequent, SCode alternative)
//            : base (PrimitiveCombination2.Make (procedure, arg0, arg1), consequent, alternative)
//        {
//            this.procedure = procedure;
//            this.method = this.procedure.Method;
//            this.arg0 = arg0;
//            this.arg1 = arg1;
//#if DEBUG
//            rand0Type = arg0.GetType ();
//            rand1Type = arg1.GetType ();
//#endif
//        }

//        public static SCode Make (Primitive2 procedure, SCode arg0, SCode arg1, SCode consequent, SCode alternative)
//        {
//            return
//                (Configuration.EnableSuperOperators && arg0 is Argument) ? PrimitiveConditional2AS.Make (procedure, (Argument) arg0, arg1, consequent, alternative)
//                : (Configuration.EnableSuperOperators && arg0 is LexicalVariable) ? PrimitiveConditional2LS.Make (procedure, (LexicalVariable) arg0, arg1, consequent, alternative)
//                : (Configuration.EnableSuperOperators && arg0 is Quotation) ? PrimitiveConditional2QS.Make (procedure, (Quotation) arg0, arg1, consequent, alternative)
//               // : (arg1 is LexicalVariable) ? PrimitiveConditional2SL.Make (procedure, arg0, (LexicalVariable) arg1, consequent, alternative)
//                : (arg1 is Quotation) ? PrimitiveConditional2SQ.Make (procedure, arg0, (Quotation) arg1, consequent, alternative)
//                : new PrimitiveConditional2 (procedure, arg0, arg1, consequent, alternative);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg0);
//            noteCalls (this.arg1);
//            procedureHistogram.Note (this.procedure);
//            rand0TypeHistogram.Note (this.rand0Type);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            Control unev = this.arg1;
//            Environment env = environment;
//            object ev1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//            }

//            unev = this.arg0;
//            env = environment;
//            object ev0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PrimitiveConditional2AS : PrimitiveConditional2
//    {
//#if DEBUG
//        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
//        [NonSerialized]
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        protected readonly int a0offset;

//        protected PrimitiveConditional2AS (Primitive2 procedure, Argument arg0, SCode arg1, SCode consequent, SCode alternative)
//            : base (procedure, arg0, arg1, consequent, alternative)
//        {
//            this.a0offset = arg0.Offset;
//        }

//        public static SCode Make (Primitive2 procedure, Argument arg0, SCode arg1, SCode consequent, SCode alternative)
//        {
//            return
//                (arg0 is Argument0) ? PrimitiveConditional2A0S.Make (procedure, (Argument0) arg0, arg1, consequent, alternative)
//                : (arg0 is Argument1) ? PrimitiveConditional2A1S.Make (procedure, (Argument1) arg0, arg1, consequent, alternative)
//                : new PrimitiveConditional2AS (procedure, arg0, arg1, consequent, alternative);
//        }


//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg1);
//#endif
//            Control unev = this.arg1;
//            Environment env = environment;
//            object ev1 = null;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//            }

//            //unev = this.arg0;
//            //env = environment;
//            //object ev0 = null;
//            //while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            //if (ev0 == Interpreter.UnwindStack) {
//            //    throw new NotImplementedException ();
//            //    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
//            //    //answer = Interpreter.UnwindStack;
//            //    //environment = env;
//            //    //return false;
//            //}

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, environment.ArgumentValue (this.a0offset), ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PrimitiveConditional2A0S : PrimitiveConditional2AS
//    {
//#if DEBUG
//        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
//        [NonSerialized]
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected PrimitiveConditional2A0S (Primitive2 procedure, Argument0 arg0, SCode arg1, SCode consequent, SCode alternative)
//            : base (procedure, arg0, arg1, consequent, alternative)
//        {
//        }

//        public static SCode Make (Primitive2 procedure, Argument0 arg0, SCode arg1, SCode consequent, SCode alternative)
//        {
//            return (arg1 is Quotation)
//                ? PrimitiveConditional2A0Q.Make (procedure, arg0, (Quotation)arg1, consequent, alternative)
//                : new PrimitiveConditional2A0S (procedure, arg0, arg1, consequent, alternative);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg1);
//            procedureHistogram.Note (this.procedure);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            Control unev = this.arg1;
//            Environment env = environment;
//            object ev1 = null;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//            }

//            //unev = this.arg0;
//            //env = environment;
//            //object ev0 = null;
//            //while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            //if (ev0 == Interpreter.UnwindStack) {
//            //    throw new NotImplementedException ();
//            //    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
//            //    //answer = Interpreter.UnwindStack;
//            //    //environment = env;
//            //    //return false;
//            //}

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, environment.Argument0Value, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PrimitiveConditional2A1S : PrimitiveConditional2
//    {
//#if DEBUG
//        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
//        [NonSerialized]
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected PrimitiveConditional2A1S (Primitive2 procedure, Argument1 arg0, SCode arg1, SCode consequent, SCode alternative)
//            : base (procedure, arg0, arg1, consequent, alternative)
//        {
//        }

//        public static SCode Make (Primitive2 procedure, Argument1 arg0, SCode arg1, SCode consequent, SCode alternative)
//        {
//            return new PrimitiveConditional2A1S (procedure, arg0, arg1, consequent, alternative);
//        }


//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg1);
//#endif
//            Control unev = this.arg1;
//            Environment env = environment;
//            object ev1 = null;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//            }

//            //unev = this.arg0;
//            //env = environment;
//            //object ev0 = null;
//            //while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            //if (ev0 == Interpreter.UnwindStack) {
//            //    throw new NotImplementedException ();
//            //    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
//            //    //answer = Interpreter.UnwindStack;
//            //    //environment = env;
//            //    //return false;
//            //}

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, environment.Argument1Value, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PrimitiveConditional2A0Q : PrimitiveConditional2A0S
//    {
//#if DEBUG
//        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
//        [NonSerialized]
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected object quoted;

//        protected PrimitiveConditional2A0Q (Primitive2 procedure, Argument0 arg0, Quotation arg1, SCode consequent, SCode alternative)
//            : base (procedure, arg0, arg1, consequent, alternative)
//        {
//            this.quoted = arg1.Quoted;
//        }

//        public static SCode Make (Primitive2 procedure, Argument0 arg0, Quotation arg1, SCode consequent, SCode alternative)
//        {
//            return new PrimitiveConditional2A0Q (procedure, arg0, arg1, consequent, alternative);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            procedureHistogram.Note (this.procedure);
//#endif

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, environment.Argument0Value, this.quoted)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }


//    [Serializable]
//    class PrimitiveConditional2AL : PrimitiveConditional2AS
//    {
//#if DEBUG
//        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
//        [NonSerialized]
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        protected readonly string l1name;
//        protected readonly int l1depth;
//        protected readonly int l1offset;


//        protected PrimitiveConditional2AL (Primitive2 procedure, Argument arg0, LexicalVariable arg1, SCode consequent, SCode alternative)
//            : base (procedure, arg0, arg1, consequent, alternative)
//        {

//            this.l1name = arg1.varname;
//            this.l1depth = arg1.Depth;
//            this.l1offset = arg1.Offset;
//        }

//        public static SCode Make (Primitive2 procedure, Argument arg0, LexicalVariable arg1, SCode consequent, SCode alternative)
//        {
//            return new PrimitiveConditional2AL (procedure, arg0, arg1, consequent, alternative);
//        }


//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            //Control unev = this.arg1;
//            //Environment env = environment;
//            //object ev1 = null;
//            //while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            //if (ev1 == Interpreter.UnwindStack) {
//            //    throw new NotImplementedException ();
//            //}

//            //unev = this.arg0;
//            //env = environment;
//            //object ev0 = null;
//            //while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            //if (ev0 == Interpreter.UnwindStack) {
//            //    throw new NotImplementedException ();
//            //    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
//            //    //answer = Interpreter.UnwindStack;
//            //    //environment = env;
//            //    //return false;
//            //}

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            object ev1 = null;
//            if (environment.FastLexicalRef (out ev1, this.l1name, this.l1depth, this.l1offset))
//                throw new NotImplementedException ();
//            if (this.method (out answer, environment.ArgumentValue (this.a0offset), ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }
//                        if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note(this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PrimitiveConditional2AQ : PrimitiveConditional2AS
//    {
//#if DEBUG
//        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
//        [NonSerialized]
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

//#endif
//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        protected readonly object q1quoted;


//        PrimitiveConditional2AQ (Primitive2 procedure, Argument arg0, Quotation arg1, SCode consequent, SCode alternative)
//            : base (procedure, arg0, arg1, consequent, alternative)
//        {
//            this.q1quoted = arg1.Quoted;
//        }

//        public static SCode Make (Primitive2 procedure, Argument arg0, Quotation arg1, SCode consequent, SCode alternative)
//        {
//            return new PrimitiveConditional2AQ (procedure, arg0, arg1, consequent, alternative);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            procedureHistogram.Note (this.procedure);
//#endif
//            //Control unev = this.arg1;
//            //Environment env = environment;
//            //object ev1 = null;
//            //while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            //if (ev1 == Interpreter.UnwindStack) {
//            //    throw new NotImplementedException ();
//            //}

//            //unev = this.arg0;
//            //env = environment;
//            //object ev0 = null;
//            //while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            //if (ev0 == Interpreter.UnwindStack) {
//            //    throw new NotImplementedException ();
//            //    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
//            //    //answer = Interpreter.UnwindStack;
//            //    //environment = env;
//            //    //return false;
//            //}

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, environment.ArgumentValue (this.a0offset), this.q1quoted)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//                        if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note(this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PrimitiveConditional2LS : PrimitiveConditional2
//    {
//#if DEBUG
//        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
//        [NonSerialized]
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        protected readonly object l0name;
//        protected readonly int l0depth;
//        protected readonly int l0offset;

//        PrimitiveConditional2LS (Primitive2 procedure, LexicalVariable arg0, SCode arg1, SCode consequent, SCode alternative)
//            : base (procedure, arg0, arg1, consequent, alternative)
//        {
//            this.l0name = arg0.Name;
//            this.l0depth = arg0.Depth;
//            this.l0offset = arg0.Offset;
//        }

//        public static SCode Make (Primitive2 procedure, LexicalVariable arg0, SCode arg1, SCode consequent, SCode alternative)
//        {
//            return new PrimitiveConditional2LS (procedure, arg0, arg1, consequent, alternative);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg1);
//            procedureHistogram.Note (this.procedure);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            Control unev = this.arg1;
//            Environment env = environment;
//            object ev1 = null;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            object ev0 = null;
//            if (environment.FastLexicalRef (out ev0, this.l0name, this.l0depth, this.l0offset))
//                throw new NotImplementedException ();

//            if (this.method (out answer, ev0, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }
//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                return true;
//            }

//        }
//    }

//    [Serializable]
//    class PrimitiveConditional2QS : PrimitiveConditional2
//    {
//#if DEBUG
//        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        protected readonly object quotedValue;

//        protected PrimitiveConditional2QS (Primitive2 procedure, Quotation arg0, SCode arg1, SCode consequent, SCode alternative)
//            : base (procedure, arg0, arg1, consequent, alternative)
//        {
//            this.quotedValue = arg0.Quoted;
//        }

//        public static SCode Make (Primitive2 procedure, Quotation arg0, SCode arg1, SCode consequent, SCode alternative)
//        {
//            return //(consequent is Quotation) ? PrimitiveConditional2QSQS.Make (procedure, arg0, arg1, (Quotation) consequent, alternative)
//                //: 
//                new PrimitiveConditional2QS (procedure, arg0, arg1, consequent, alternative);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg1);
//            procedureHistogram.Note (this.procedure);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            Control unev = this.arg1;
//            Environment env = environment;
//            object ev1 = null;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, this.quotedValue, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PrimitiveConditional2SL : PrimitiveConditional2
//    {
//#if DEBUG
//        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        protected readonly string l1name;
//        protected readonly int l1depth;
//        protected readonly int l1offset;

//        PrimitiveConditional2SL (Primitive2 procedure, SCode arg0, LexicalVariable arg1, SCode consequent, SCode alternative)
//            : base (procedure, arg0, arg1, consequent, alternative)
//        {
//            this.l1name = arg1.varname;
//            this.l1depth = arg1.Depth;
//            this.l1offset = arg1.Offset;
//        }

//        public static SCode Make (Primitive2 procedure, SCode arg0, LexicalVariable arg1, SCode consequent, SCode alternative)
//        {
//            return new PrimitiveConditional2SL (procedure, arg0, arg1, consequent, alternative);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg0);
//            procedureHistogram.Note (this.procedure);
//            rand0TypeHistogram.Note (this.rand0Type);
//#endif
//            object ev1 = null;
//            if (environment.FastLexicalRef (out ev1, this.l1name, this.l1depth, this.l1offset))
//                throw new NotImplementedException ();

//            Control unev = this.arg0;
//            Environment env = environment;
//            object ev0 = null;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }
//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PrimitiveConditional2SQ : PrimitiveConditional2
//    {
//#if DEBUG
//        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        protected readonly object quotedValue;

//        PrimitiveConditional2SQ (Primitive2 procedure, SCode arg0, Quotation arg1, SCode consequent, SCode alternative)
//            : base (procedure, arg0, arg1, consequent, alternative)
//        {
//            this.quotedValue = arg1.Quoted;
//        }

//        public static SCode Make (Primitive2 procedure, SCode arg0, Quotation arg1, SCode consequent, SCode alternative)
//        {
//            return new PrimitiveConditional2SQ (procedure, arg0, arg1, consequent, alternative);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg0);
//            procedureHistogram.Note (this.procedure);
//            rand0TypeHistogram.Note (this.rand0Type);
//#endif
//            Control unev = this.arg0;
//            Environment env = environment;
//            object ev0 = null;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, ev0, this.quotedValue)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }
//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PrimitiveConditional2QSQS : PrimitiveConditional2QS
//    {
//#if DEBUG
//        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
//        [NonSerialized]
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        protected readonly object quotedConsequent;

//        protected PrimitiveConditional2QSQS (Primitive2 procedure, Quotation arg0, SCode arg1, Quotation consequent, SCode alternative)
//            : base (procedure, arg0, arg1, consequent, alternative)
//        {
//            this.quotedConsequent = consequent.Quoted;
//        }

//        public static SCode Make (Primitive2 procedure, Quotation arg0, SCode arg1, Quotation consequent, SCode alternative)
//        {
//            return (arg1 is Argument) ? PrimitiveConditional2QAQS.Make (procedure, arg0, (Argument) arg1, consequent, alternative)
//                : new PrimitiveConditional2QSQS (procedure, arg0, arg1, consequent, alternative);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg1);
//            procedureHistogram.Note (this.procedure);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            Control unev = this.arg1;
//            Environment env = environment;
//            object ev1 = null;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, this.quotedValue, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note(this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = this.quotedConsequent;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PrimitiveConditional2QAQS : PrimitiveConditional2QSQS
//    {
//#if DEBUG
//        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
//        [NonSerialized]
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        protected readonly int a1offset;

//        PrimitiveConditional2QAQS (Primitive2 procedure, Quotation arg0, Argument arg1, Quotation consequent, SCode alternative)
//            : base (procedure, arg0, arg1, consequent, alternative)
//        {
//            this.a1offset = arg1.Offset;
//        }

//        public static SCode Make (Primitive2 procedure, Quotation arg0, Argument arg1, Quotation consequent, SCode alternative)
//        {
//            return new PrimitiveConditional2QAQS (procedure, arg0, arg1, consequent, alternative);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("shouldn't be necessary");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            procedureHistogram.Note (this.procedure);
//#endif


//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//#endif
//            if (this.method (out answer, this.quotedValue, environment.ArgumentValue(this.a1offset))) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    Control cExpression = tci.Expression;
//                    Environment cEnvironment = tci.Environment;
//                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//                }
//            }

//            if ((answer is bool) && (bool) answer == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                return true;
//            }
//            else {
//                answer = this.quotedConsequent;
//                return false;
//            }
//        }
//    }

}