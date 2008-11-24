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

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Type predicateType;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Type consequentType;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
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

        static SCode SpecialMake (Quotation predicate, SCode consequent, SCode alternative)
        {
            throw new NotImplementedException ();
        }

        static SCode StandardMake (SCode predicate, SCode consequent, SCode alternative)
        {
            return
                (Configuration.EnableSuperOperators && predicate is PrimitiveCombination1) ? PCond1.Make ((PrimitiveCombination1) predicate, consequent, alternative)
                : (Configuration.EnableSuperOperators && predicate is PrimitiveCombination2) ? PCond2.Make ((PrimitiveCombination2) predicate, consequent, alternative)
                : (Configuration.EnableSuperOperators && predicate is LexicalVariable) ? ConditionalL.Make ((LexicalVariable) predicate, consequent, alternative)
                : (Configuration.EnableSuperOperators && consequent is LexicalVariable) ? ConditionalSL.Make (predicate, (LexicalVariable) consequent, alternative)
                : (Configuration.EnableSuperOperators && consequent is Quotation) ? ConditionalSQ.Make (predicate, (Quotation) consequent, alternative)
                : (Configuration.EnableSuperOperators && alternative is LexicalVariable) ? ConditionalSSL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (Configuration.EnableSuperOperators && alternative is Quotation) ? ConditionalSSQ.Make (predicate, consequent, (Quotation) alternative)
                : new Conditional (predicate, consequent, alternative);
        }

        static SCode SpecialMake (SCode predicate, bool consequent, bool alternative)
        {
            if (consequent == true && alternative == false) {
                if (predicate is Conditional)
                    return Unimplemented ();
                else
                    // In this case, we're canonicalizing a return value to a boolean.
                    return StandardMake (predicate, Quotation.Make (consequent), Quotation.Make (alternative));
            }
            else if (consequent == false && alternative == true) {
                return 
                    
                    PrimitiveCombination1.Make (Primitive.Not, predicate);
            }
            else
                // Should be impossible.
               return Unimplemented ();
        }

        static SCode RewriteAsSequence (SCode predicate, Quotation consequent)
        {
            Console.WriteLine ("; rewrite as sequence");
            return Sequence2.Make (predicate, consequent);
        }

        static SCode SpecialMake (SCode predicate, Quotation consequent, Quotation alternative)
        {
            object arg0 = consequent.Quoted;
            object arg1 = alternative.Quoted;
            return (Configuration.EnableMergeConditionalResult &&
                     ((arg0 == null) && (arg1 == null))
                || ((arg1 != null) &&
                    ((arg0 == arg1)
                     || ((arg0 is Int32 && arg1 is Int32) && ((int) arg0 == (int) arg1))
                     || ((arg0 is char && arg1 is char) && ((char) arg0 == (char) arg1))
                     || ((arg0 is bool && arg1 is bool) && ((bool) arg0 == (bool) arg1))))) ? RewriteAsSequence (predicate, consequent)
                     : (arg0 is Boolean && arg1 is Boolean) ? SpecialMake (predicate, (bool) arg0, (bool) arg1)
                     : (arg0 == Constant.Unspecific && Configuration.EnableTrueUnspecific) ? SpecialMake (predicate, alternative, alternative)
                     : (arg1 == Constant.Unspecific && Configuration.EnableTrueUnspecific) ? SpecialMake (predicate, consequent, consequent)
                     : StandardMake (predicate, consequent, alternative);
            
        }

        public static SCode Make (SCode predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is Quotation) ? SpecialMake ((Quotation) predicate, consequent, alternative)
                : (consequent is Quotation && alternative is Quotation) ? SpecialMake (predicate, (Quotation) consequent, (Quotation) alternative)
                : StandardMake (predicate, consequent, alternative);
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
                 (optPred == this.predicate
                  && optCons == this.consequent
                  && optAlt == this.alternative) ? this
                : Conditional.Make (optPred, optCons, optAlt);
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
            Warm ("-");
            noteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "Conditional.EvalStep";
#endif
            object ev; 
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
#if DEBUG
                        SCode.location = "Conditional.EvalStep.1";
#endif
            if (ev == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;

            }

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note(this.alternativeType);
                SCode.location = "Conditional.EvalStep.2";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                noteCalls (this.consequent);
                consequentTypeHistogram.Note(this.consequentType);
                SCode.location = "Conditional.EvalStep.3";
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

        public override SCode Substitute (object name, object newObject)
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
    class ConditionalL : Conditional
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object predicateName;
        public readonly int predicateDepth;
        public readonly int predicateOffset;

        protected ConditionalL (LexicalVariable predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateName = predicate.Name;
            this.predicateDepth = predicate.Depth;
            this.predicateOffset = predicate.Offset;
        }

        public static SCode Make (LexicalVariable predicate, SCode consequent, SCode alternative)
        {
            return 
                (predicate is Argument) ? ConditionalA.Make ((Argument) predicate, consequent, alternative)
                : (predicate is LexicalVariable1) ? ConditionalL1.Make ((LexicalVariable1) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? Unimplemented()
            : (consequent is Quotation) ? ConditionalLQ.Make (predicate, (Quotation) consequent, alternative)
            : (alternative is LexicalVariable) ? ConditionalLSL.Make (predicate, consequent, (LexicalVariable) alternative)
            : (alternative is Quotation) ? ConditionalLSQ.Make (predicate, consequent, (Quotation) alternative)
                
            :   new ConditionalL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev;
            if (environment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

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
    class ConditionalA : ConditionalL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalA (Argument predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is Argument0) ? ConditionalA0.Make ((Argument0) predicate, consequent, alternative)
                :  (predicate is Argument1) ? ConditionalA1.Make ((Argument1) predicate, consequent, alternative)
                : (consequent is LexicalVariable) ? ConditionalAL.Make (predicate, (LexicalVariable) consequent, alternative)
            : (consequent is Quotation) ? ConditionalAQ.Make (predicate, (Quotation) consequent, alternative)
            : (alternative is LexicalVariable) ? ConditionalASL.Make (predicate, consequent, (LexicalVariable) alternative)
            : (alternative is Quotation) ? ConditionalASQ.Make (predicate, consequent, (Quotation) alternative)
            : new ConditionalA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

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
    class ConditionalA0 : ConditionalA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalA0 (Argument0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? ConditionalA0L.Make (predicate, (LexicalVariable) consequent, alternative)
            : (consequent is Quotation) ? ConditionalA0Q.Make (predicate, (Quotation) consequent, alternative)
            : (alternative is LexicalVariable) ? ConditionalA0SL.Make (predicate, consequent, (LexicalVariable) alternative)
            : (alternative is Quotation) ? ConditionalA0SQ.Make (predicate, consequent, (Quotation) alternative)
            : new ConditionalA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA0.EvalStep");
#endif
            object ev = environment.Argument0Value;

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
    class ConditionalA0L : ConditionalA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalA0L (Argument0 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument0 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? ConditionalA0A.Make (predicate, (Argument) consequent, alternative)
            : (consequent is LexicalVariable1) ? ConditionalA0L1.Make (predicate, (LexicalVariable1) consequent, alternative)
            : (alternative is LexicalVariable) ? Unimplemented()
            : (alternative is Quotation) ? Unimplemented ()

            : new ConditionalA0L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalA0.EvalStep");
#endif
            object ev = environment.Argument0Value;

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
    class ConditionalA0A : ConditionalA0L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalA0A (Argument0 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument0 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? Disjunction.Make (predicate, alternative)
            : (consequent is Argument1) ? ConditionalA0A1.Make (predicate, (Argument1) consequent, alternative)
            : (alternative is LexicalVariable) ? Unimplemented ()
            : (alternative is Quotation) ? Unimplemented ()

            : new ConditionalA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalA0.EvalStep");
#endif
            object ev = environment.Argument0Value;

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
    class ConditionalA0A1 : ConditionalA0A
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalA0A1 (Argument0 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument0 predicate, Argument1 consequent, SCode alternative)
        {
            return
                 (alternative is LexicalVariable) ? Unimplemented ()
            : (alternative is Quotation) ? Unimplemented ()

            : new ConditionalA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA0A1.EvalStep");
#endif
            object ev = environment.Argument0Value;

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
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    [Serializable]
    class ConditionalA0L1 : ConditionalA0L
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalA0L1 (Argument0 predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument0 predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? ConditionalA0L1L.Make (predicate, consequent, (LexicalVariable) alternative)
               : (alternative is Quotation) ? Unimplemented ()
            : new ConditionalA0L1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalA0.EvalStep");
#endif
            object ev = environment.Argument0Value;

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
                Debug.WriteLineIf (Primitive.Noisy, "    => #t");
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    class ConditionalA0L1L : ConditionalA0L1
    {
        protected ConditionalA0L1L (Argument0 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument0 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? Unimplemented()
               : (alternative is LexicalVariable1) ? Unimplemented ()
            : new ConditionalA0L1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalA0.EvalStep");
#endif
            object ev = environment.Argument0Value;

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
                noteCalls (this.consequent);
                Debug.WriteLineIf (Primitive.Noisy, "    => #t");
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }



    [Serializable]
    class ConditionalA0Q : ConditionalA0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object consequentValue;
        protected ConditionalA0Q (Argument0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (Argument0 predicate, Quotation consequent, SCode alternative)
        {
            return
              (alternative is LexicalVariable) ? ConditionalA0QL.Make (predicate, consequent, (LexicalVariable) alternative)
            : (alternative is Quotation) ? ConditionalA0QQ.Make (predicate, consequent, (Quotation) alternative)
            : new ConditionalA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA0Q.EvalStep");
#endif
            object ev = environment.Argument0Value;

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
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class ConditionalA0QL : ConditionalA0Q
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object alternativeName;
        protected readonly int alternativeDepth;
        protected readonly int alternativeOffset;

        protected ConditionalA0QL (Argument0 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (Argument0 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
              (alternative is Argument) ? ConditionalA0QA.Make (predicate, consequent, (Argument) alternative)
            : (alternative is LexicalVariable1) ? Unimplemented()

            : new ConditionalA0QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalA0.EvalStep");
#endif
            object ev = environment.Argument0Value;

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
    class ConditionalA0QA : ConditionalA0QL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif


        protected ConditionalA0QA (Argument0 predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (Argument0 predicate, Quotation consequent, Argument alternative)
        {
            return
              (alternative is Argument0) ? Unimplemented()
            : (alternative is Argument1) ? Unimplemented ()
            : new ConditionalA0QA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalA0.EvalStep");
#endif
            object ev = environment.Argument0Value;

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
    class ConditionalA0QQ : ConditionalA0Q
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object alternativeValue;
        protected ConditionalA0QQ (Argument0 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (Argument0 predicate, Quotation consequent, Quotation alternative)
        {
            object isEq;
            ObjectModel.Eq (out isEq, consequent.Quoted, alternative.Quoted);
            if ((isEq is bool) && (bool) isEq == false)
                return new ConditionalA0QQ (predicate, consequent, alternative);
            else
                // Same value either way, so punt.
                return consequent;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalA0.EvalStep");
#endif
            object ev = environment.Argument0Value;

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
    class ConditionalA0SL : ConditionalA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object alternativeName;
        protected readonly int alternativeDepth;
        protected readonly int alternativeOffset;

        protected ConditionalA0SL (Argument0 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (Argument0 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? ConditionalA0SA.Make (predicate, consequent, (Argument) alternative)
                :(alternative is LexicalVariable1) ? ConditionalA0SL1.Make (predicate, consequent, (LexicalVariable1) alternative)

            : new ConditionalA0SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalA0.EvalStep");
#endif
            object ev = environment.Argument0Value;

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

    class ConditionalA0SA : ConditionalA0SL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalA0SA (Argument0 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument0 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? ConditionalA0SQ.Make (predicate, consequent, Quotation.Make (false))
                : (alternative is Argument1) ? ConditionalA0SA1.Make (predicate, consequent, (Argument1) alternative)

            : new ConditionalA0SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA0SA.EvalStep");
#endif
            object ev = environment.Argument0Value;

            if ((ev is bool) && (bool) ev == false) {
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

    class ConditionalA0SA1 : ConditionalA0SA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalA0SA1 (Argument0 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument0 predicate, SCode consequent, Argument1 alternative)
        {
            return
                 new ConditionalA0SA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA0SA1.EvalStep");
#endif
            object ev = environment.Argument0Value;

            if ((ev is bool) && (bool) ev == false) {
                answer = environment.Argument1Value;
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


    class ConditionalA0SL1 : ConditionalA0SL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalA0SL1 (Argument0 predicate, SCode consequent, LexicalVariable1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument0 predicate, SCode consequent, LexicalVariable1 alternative)
        {
            return
                 new ConditionalA0SL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA0SL.EvalStep");
#endif
            object ev = environment.Argument0Value;

            if ((ev is bool) && (bool) ev == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
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
    class ConditionalA0SQ : ConditionalA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object alternativeValue;

        protected ConditionalA0SQ (Argument0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (Argument0 predicate, SCode consequent, Quotation alternative)
        {
            return
                new ConditionalA0SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA0SQ.EvalStep");
#endif
            object ev = environment.Argument0Value;

            if ((ev is bool) && (bool) ev == false) {
                answer = this.alternativeValue;
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
    class ConditionalA1 : ConditionalA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalA1 (Argument1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument1 predicate, SCode consequent, SCode alternative)
        {
            return
                 (consequent is LexicalVariable) ? ConditionalA1L.Make (predicate, (LexicalVariable) consequent, alternative)
                : (consequent is Quotation) ? ConditionalA1Q.Make (predicate, (Quotation) consequent, alternative)
                : (alternative is LexicalVariable) ? ConditionalA1SL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? ConditionalA1SQ.Make (predicate, consequent, (Quotation) alternative)
                : new ConditionalA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA1.EvalStep");
#endif
            object ev = environment.Argument1Value;

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
    class ConditionalA1L : ConditionalA1
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected ConditionalA1L (Argument1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                 (consequent is Argument) ? ConditionalA1A.Make (predicate, (Argument) consequent, alternative)
                : (consequent is LexicalVariable1) ? Unimplemented()
                : (alternative is LexicalVariable) ? Unimplemented()
                : (alternative is Quotation) ? Unimplemented()
                : new ConditionalA1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA1L.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }

    class ConditionalA1A : ConditionalA1L
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalA1A (Argument1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument1 predicate, Argument consequent, SCode alternative)
        {
            return
                 (consequent is Argument0) ? Unimplemented()
                : (consequent is Argument1) ? Disjunction.Make (predicate, alternative)
                : (alternative is LexicalVariable) ? Unimplemented ()
                : (alternative is Quotation) ? Unimplemented ()
                : new ConditionalA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA1A.EvalStep");
#endif
            throw new NotImplementedException ();
        }
    }


    [Serializable]
    class ConditionalA1Q : ConditionalA1
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object consequentValue;

        protected ConditionalA1Q (Argument1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (Argument1 predicate, Quotation consequent, SCode alternative)
        {
            return
               (alternative is LexicalVariable) ? Unimplemented()
            : (alternative is Quotation) ? ConditionalA1QQ.Make (predicate, consequent, (Quotation) alternative)
            : new ConditionalA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA1Q.EvalStep");
#endif
            object ev = environment.Argument1Value;

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
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class ConditionalA1QQ : ConditionalA1Q
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalA1QQ (Argument1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument1 predicate, Quotation consequent, Quotation alternative)
        {
            return
             new ConditionalA1QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

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
    class ConditionalA1SL : ConditionalA1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object alternativeName;
        protected readonly int alternativeDepth;
        protected readonly int alternativeOffset;

        protected ConditionalA1SL (Argument1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (Argument1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? ConditionalA1SA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? Unimplemented()
                : new ConditionalA1SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

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

    class ConditionalA1SA : ConditionalA1SL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalA1SA (Argument1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? ConditionalA1SA0.Make (predicate, consequent, (Argument0) alternative)
                : (alternative is Argument1) ? ConditionalA1SQ.Make (predicate, consequent, Quotation.Make(false))
                : new ConditionalA1SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

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

    class ConditionalA1SA0 : ConditionalA1SA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalA1SA0 (Argument1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument1 predicate, SCode consequent, Argument0 alternative)
        {
            return new ConditionalA1SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA1SA0.EvalStep");
#endif
            object ev = environment.Argument1Value;

            if ((ev is bool) && (bool) ev == false) {
                answer = environment.Argument0Value;
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
    class ConditionalA1SQ : ConditionalA1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object alternativeValue;

        protected ConditionalA1SQ (Argument1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (Argument1 predicate, SCode consequent, Quotation alternative)
        {
            return
              new ConditionalA1SQ (predicate, consequent, (Quotation) alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA1SQ.EvalStep");
#endif
            object ev = environment.Argument1Value;

            if ((ev is bool) && (bool) ev == false) {
                answer = this.alternativeValue;
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
    class ConditionalAL : ConditionalA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object consequentName;
        protected readonly int consequentDepth;
        protected readonly int consequentOffset;

        protected ConditionalAL (Argument predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (Argument predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? ConditionalAA.Make (predicate, (Argument) consequent, alternative)
            : (consequent is LexicalVariable1) ? Unimplemented()
            : (alternative is LexicalVariable) ? Unimplemented()
            : (alternative is Quotation) ? Unimplemented()
            : new ConditionalAL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

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
    class ConditionalAA : ConditionalAL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected ConditionalAA (Argument predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? Unimplemented()
            : (consequent is Argument1) ? Unimplemented ()
            : (alternative is LexicalVariable) ? Unimplemented ()
            : (alternative is Quotation) ? ConditionalAAQ.Make (predicate, consequent, (Quotation) alternative)
            : new ConditionalAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

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
    class ConditionalAAQ : ConditionalAA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object alternativeValue;

        protected ConditionalAAQ (Argument predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (Argument predicate, Argument consequent, Quotation alternative)
        {
            return
                new ConditionalAAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

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
    class ConditionalAQ : ConditionalA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalAQ (Argument predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument predicate, Quotation consequent, SCode alternative)
        {
            return
                 (alternative is LexicalVariable) ? Unimplemented ()
            : (alternative is Quotation) ? ConditionalAQQ.Make (predicate, consequent, (Quotation) alternative)
            : new ConditionalAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

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
    class ConditionalAQQ : ConditionalAQ
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object alternativeValue;

        protected ConditionalAQQ (Argument predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (Argument predicate, Quotation consequent, Quotation alternative)
        {
            return
                  new ConditionalAQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

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
    class ConditionalASL : ConditionalA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object alternativeName;
        protected readonly int alternativeDepth;
        protected readonly int alternativeOffset;

        protected ConditionalASL (Argument predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (Argument predicate, SCode consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? ConditionalASA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? Unimplemented()
                : new ConditionalASL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

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
    class ConditionalASQ : ConditionalA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object alternativeValue;

        protected ConditionalASQ (Argument predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (Argument predicate, SCode consequent, Quotation alternative)
        {
            return
                 new ConditionalASQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalASQ.EvalStep");
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

            if ((ev is bool) && (bool) ev == false) {
                answer = this.alternativeValue;
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
    class ConditionalL1 : ConditionalL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalL1 (LexicalVariable1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (LexicalVariable1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is LexicalVariable) ? ConditionalL1L.Make (predicate, (LexicalVariable) consequent, alternative)
            : (consequent is Quotation) ? ConditionalL1Q.Make (predicate, (Quotation) consequent, alternative)
            : (alternative is LexicalVariable) ? ConditionalL1SL.Make (predicate, consequent, (LexicalVariable) alternative)
            : (alternative is Quotation) ? ConditionalL1SQ.Make (predicate, consequent, (Quotation) alternative)
            : new ConditionalL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalL1.EvalStep");
#endif
            object ev;
            if (environment.FastLexicalRef1 (out ev, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

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
    class ConditionalL1L : ConditionalL1
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object consequentName;
        protected readonly int consequentDepth;
        protected readonly int consequentOffset;

        protected ConditionalL1L (LexicalVariable1 predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
        this.consequentDepth = consequent.Depth;
        this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (LexicalVariable1 predicate, LexicalVariable consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented ()
            : (alternative is Quotation) ? ConditionalL1LQ.Make (predicate, consequent, (Quotation) alternative)
            : new ConditionalL1L (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalL1L.EvalStep");
#endif
            object ev;
            if (environment.FastLexicalRef1 (out ev, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

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
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class ConditionalL1LQ : ConditionalL1L
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object alternativeValue;


        protected ConditionalL1LQ (LexicalVariable1 predicate, LexicalVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (LexicalVariable1 predicate, LexicalVariable consequent, Quotation alternative)
        {
            return new ConditionalL1LQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev;
            if (environment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

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
    class ConditionalL1Q : ConditionalL1
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object consequentValue;
        protected ConditionalL1Q (LexicalVariable1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (LexicalVariable1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? ConditionalL1QL.Make (predicate, consequent, (LexicalVariable) alternative)
            : (alternative is Quotation) ? ConditionalL1QQ.Make (predicate, consequent, (Quotation) alternative)
            : new ConditionalL1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalL1Q.EvalStep");
#endif
            object ev;
            if (environment.FastLexicalRef1 (out ev, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

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
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class ConditionalL1QL : ConditionalL1Q
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object alternativeName;
        protected readonly int alternativeDepth;
        protected readonly int alternativeOffset;
         
        protected ConditionalL1QL (LexicalVariable1 predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (LexicalVariable1 predicate, Quotation consequent, LexicalVariable alternative)
        {
            return
                (alternative is Argument) ? Unimplemented()
                :  (alternative is LexicalVariable1) ? Unimplemented()
            : new ConditionalL1QL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev;
            if (environment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

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
    class ConditionalL1QQ : ConditionalL1Q
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object alternativeValue;
        protected ConditionalL1QQ (LexicalVariable1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (LexicalVariable1 predicate, Quotation consequent, Quotation alternative)
        {
            return
                 new ConditionalL1QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev;
            if (environment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

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
    class ConditionalL1SL : ConditionalL1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object alternativeName;
        protected readonly int alternativeDepth;
        protected readonly int alternativeOffset;

        protected ConditionalL1SL (LexicalVariable1 predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;

        }

        public static SCode Make (LexicalVariable1 predicate, SCode consequent, LexicalVariable alternative)
        {
            return
               (alternative is Argument) ? ConditionalL1SA.Make (predicate, consequent, (Argument) alternative)
            : (alternative is LexicalVariable1) ? Unimplemented()
            : new ConditionalL1SL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev;
            if (environment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

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
    class ConditionalL1SA : ConditionalL1SL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalL1SA (LexicalVariable1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (LexicalVariable1 predicate, SCode consequent, Argument alternative)
        {
            return
               (alternative is Argument0) ? ConditionalL1SA0.Make (predicate, consequent, (Argument0) alternative)
            : (alternative is Argument1) ? Unimplemented ()
            : new ConditionalL1SA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev;
            if (environment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

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
    class ConditionalL1SA0 : ConditionalL1SA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalL1SA0 (LexicalVariable1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {

        }

        public static SCode Make (LexicalVariable1 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new ConditionalL1SA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalL1SA0.EvalStep");
#endif
            object ev;
            if (environment.FastLexicalRef1 (out ev, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if ((ev is bool) && (bool) ev == false) {
                answer = environment.Argument0Value;
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
    class ConditionalLQ : ConditionalL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object consequentValue;

        protected ConditionalLQ (LexicalVariable predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (LexicalVariable predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is LexicalVariable) ? Unimplemented ()
            : (alternative is Quotation) ? Unimplemented ()
            : new ConditionalLQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev;
            if (environment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

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
    class ConditionalLSQ : ConditionalL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected ConditionalLSQ (LexicalVariable predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (LexicalVariable predicate, SCode consequent, Quotation alternative)
        {
            return
                 new ConditionalLSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalLSQ.EvalStep");
#endif
            object ev;
            if (environment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

            if ((ev is bool) && (bool) ev == false) {
                answer = this.alternativeValue;
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
    class ConditionalL1SQ : ConditionalL1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected readonly object alternativeValue;
        protected ConditionalL1SQ (LexicalVariable1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (LexicalVariable1 predicate, SCode consequent, Quotation alternative)
        {
            return new ConditionalL1SQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalL.EvalStep");
#endif
            object ev;
            if (environment.FastLexicalRef1 (out ev, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if ((ev is bool) && (bool) ev == false) {
                answer = this.alternativeValue;
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
    class ConditionalSL : Conditional
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected ConditionalSL (SCode predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (SCode predicate, LexicalVariable consequent, SCode alternative)
        {
            return 
                (consequent is Argument) ? ConditionalSA.Make (predicate, (Argument) consequent, alternative)
                : (consequent is LexicalVariable1) ? ConditionalSL1.Make (predicate, (LexicalVariable1) consequent, alternative)
                : new ConditionalSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalSL.EvalStep");
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
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class ConditionalSA : ConditionalSL
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalSA (SCode predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (SCode predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? ConditionalSA0.Make (predicate, (Argument0) consequent, alternative)
                : (consequent is Argument1) ? ConditionalSA1.Make (predicate, (Argument1) consequent, alternative)
                : new ConditionalSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalSA.EvalStep");
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
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
        }
    }

    [Serializable]
    class ConditionalSA0 : ConditionalSA
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalSA0 (SCode predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (SCode predicate, Argument0 consequent, SCode alternative)
        {
            return
                 new ConditionalSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalSA0.EvalStep");
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
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class ConditionalSA1 : ConditionalSA
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalSA1 (SCode predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (SCode predicate, Argument1 consequent, SCode alternative)
        {
            return
                 new ConditionalSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalSA1.EvalStep");
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
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    [Serializable]
    class ConditionalSL1 : ConditionalSL
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalSL1 (SCode predicate, LexicalVariable1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (SCode predicate, LexicalVariable1 consequent, SCode alternative)
        {
            return
                 new ConditionalSL1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveConditionalSL1.EvalStep");
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
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                if (environment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }



    [Serializable]
    class ConditionalSQ : Conditional
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected object consequentValue;

        protected ConditionalSQ (SCode predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (SCode predicate, Quotation consequent, SCode alternative)
        {
            return 
                (alternative is LexicalVariable) ? ConditionalSQL.Make (predicate, consequent, (LexicalVariable) alternative)
                : (alternative is Quotation) ? ConditionalSQQ.Make (predicate, consequent, (Quotation) alternative)
                : new ConditionalSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalSQ.EvalStep");
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
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                expression = this.consequent;
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class ConditionalSQL : ConditionalSQ
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected ConditionalSQL (SCode predicate, Quotation consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (SCode predicate, Quotation consequent, LexicalVariable alternative)
        {
            return new ConditionalSQL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalSQL.EvalStep");
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
                expression = this.alternative;
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                expression = this.consequent;
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class ConditionalSQQ : ConditionalSQ
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif
        protected object alternativeValue;

        protected ConditionalSQQ (SCode predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (SCode predicate, Quotation consequent, Quotation alternative)
        {
            if (consequent.Quoted == alternative.Quoted) {
                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, consequent);
            }
            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, alternative);
            }
            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
                return Sequence2.Make (predicate, consequent);
            }
            return new ConditionalSQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalSQQ.EvalStep");
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
                expression = this.alternative;
                answer = this.alternativeValue;
                return false;
            }
            else {
                expression = this.consequent;
                answer = this.consequentValue;
                return false;
            }
        }
    }


    [Serializable]
    class ConditionalSSL : Conditional
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected ConditionalSSL (SCode predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (SCode predicate, SCode consequent, LexicalVariable alternative)
        {
            return new ConditionalSSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalSSL.EvalStep");
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
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
    class ConditionalSSQ : Conditional
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif
        protected object alternativeQuoted;

        protected ConditionalSSQ (SCode predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeQuoted = alternative.Quoted;
        }

        public static SCode Make (SCode predicate, SCode consequent, Quotation alternative)
        {
            return new ConditionalSSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "ConditionalSSQ.EvalStep";
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
#if DEBUG
                        SCode.location = "ConditionalSSQ.EvalStep.1";
#endif
            if (ev == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;

            }

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "ConditionalSSQ.EvalStep.A";
#endif
                answer = this.alternativeQuoted;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "ConditionalSSQ.EvalStep.C";
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }

        }
    }



    // ---------------------------


    [Serializable]
    class ConditionalLLL : Conditional
    {
        public readonly object predicateName;
        public readonly int predicateDepth;
        public readonly int predicateOffset;

        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected ConditionalLLL (LexicalVariable predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateName = predicate.Name;
            this.predicateDepth = predicate.Depth;
            this.predicateOffset = predicate.Offset;

            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;

            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (LexicalVariable predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new ConditionalLLL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class ConditionalSLL : Conditional
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected ConditionalSLL (SCode predicate, LexicalVariable consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;

            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (SCode predicate, LexicalVariable consequent, LexicalVariable alternative)
        {
            return new ConditionalSLL (predicate, consequent, alternative);
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
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            } 
        }
    }

    [Serializable]
    class ConditionalLSL : ConditionalL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
#endif

        public readonly object alternativeName;
        public readonly int alternativeDepth;
        public readonly int alternativeOffset;

        protected ConditionalLSL (LexicalVariable predicate, SCode consequent, LexicalVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeDepth = alternative.Depth;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (LexicalVariable predicate, SCode consequent, LexicalVariable alternative)
        {
            return 
                (alternative is Argument) ? ConditionalLSA.Make (predicate, consequent, (Argument) alternative)
                : (alternative is LexicalVariable1) ? Unimplemented()
                : new ConditionalLSL (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
 #if DEBUG
            Warm ();
#endif
            object ev;
            if (environment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

            if ((ev is bool) && (bool) ev == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;

            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class ConditionalLSA : ConditionalLSL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalLSA (LexicalVariable predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (LexicalVariable predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? Unimplemented()
                : (alternative is Argument1) ? Unimplemented ()
                : new ConditionalLSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ();
#endif
            object ev;
            if (environment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

            if ((ev is bool) && (bool) ev == false) {
                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;

            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }


    [Serializable]
    class ConditionalLLS : Conditional
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
#endif

        public readonly object predicateName;
        public readonly int predicateDepth;
        public readonly int predicateOffset;

        public readonly object consequentName;
        public readonly int consequentDepth;
        public readonly int consequentOffset;

        protected ConditionalLLS (LexicalVariable predicate, LexicalVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateName = predicate.Name;
            this.predicateDepth = predicate.Depth;
            this.predicateOffset = predicate.Offset;

            this.consequentName = consequent.Name;
            this.consequentDepth = consequent.Depth;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (LexicalVariable predicate, LexicalVariable consequent, SCode alternative)
        {
            return new ConditionalLLS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev;
            if (environment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
                throw new NotImplementedException ();

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
                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

 
    [Serializable]
    class Disjunction : SCode, ISystemPair
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
        public readonly Type predicateType;
        public readonly Type alternativeType;
#endif

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode predicate;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode alternative;

        protected Disjunction (SCode predicate, SCode alternative)
            : base (TC.DISJUNCTION)
        {
            this.predicate = predicate;
            this.alternative = alternative;
#if DEBUG
            this.predicateType = predicate.GetType();
            this.alternativeType = alternative.GetType();
#endif
        }

        public static SCode Make (SCode predicate, SCode alternative)
        {
            return 
                (predicate is Conditional) ? Conditional.Make (((Conditional) predicate).Predicate,
                                                               Disjunction.Make (((Conditional) predicate).Consequent,
                                                                                 alternative),
                                                               Disjunction.Make (((Conditional) predicate).Alternative,
                                                                                 alternative))
                : (predicate is Disjunction) ? Unimplemented()
                : (predicate is PrimitiveIsBigFixnum
                    || predicate is PrimitiveIsBigFlonum
                    || predicate is PrimitiveIsEq
                    || predicate is PrimitiveIsFixnum
                    || predicate is PrimitiveIsRatnum
                    ) ? Conditional.Make (predicate, Quotation.Make(Constant.sharpT), alternative)
                : (predicate is Sequence2) ? Sequence2.Make (((Sequence2) predicate).First,
                                                             Disjunction.Make (((Sequence2) predicate).Second, alternative))
                : (predicate is Sequence3) ? Unimplemented()
                : (predicate is Quotation) ? ((((Quotation) predicate).Quoted is Boolean 
                                             && ((bool)((Quotation) predicate).Quoted) == false) ? alternative : predicate)
                : (alternative is Quotation) ? DisjunctionSQ.Make (predicate, (Quotation) alternative)
                : new Disjunction (predicate, alternative);
        }

        public static SCode Make (object predicate, object alternative)
        {
            return Make (EnsureSCode (predicate), EnsureSCode (alternative));
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

        [SchemePrimitive ("DISJUNCTION?", 1, true)]
        public static bool IsDisjunction (out object answer, object arg)
        {
            answer = arg is Disjunction;
            return false;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Disjunction.EvalStep");
            noteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
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
                alternativeTypeHistogram.Note (this.alternativeType);
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


        public override SCode Substitute (object name, object newObject)
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
    class DisjunctionSQ : Disjunction
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly object alternativeValue;

        protected DisjunctionSQ (SCode predicate, Quotation alternative)
            : base (predicate, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (SCode predicate, Quotation alternative)
        {
            return new DisjunctionSQ (predicate, alternative);
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

            answer = (ev is bool && (bool) ev == false)
                ? this.alternativeValue
                : ev;

                return false;
 
        }


    }


//    [Serializable]
//    class ConditionalASS : Conditional
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected int argOffset;

//        protected ConditionalASS (Argument predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.argOffset = predicate.Offset;
//        }

//        public static SCode Make (Argument predicate, SCode consequent, SCode alternative)
//        {
//            return new ConditionalASS (predicate, consequent, alternative);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ();
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            object ev = environment.ArgumentValue (this.argOffset);

//            if ((ev is bool) && (bool) ev == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//                Debug.WriteLineIf (Primitive.Noisy, "    => #t");
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalSAS : Conditional
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected int consequentOffset;

//        protected ConditionalSAS (SCode predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentOffset = consequent.Offset;
//        }

//        public static SCode Make (SCode predicate, Argument consequent, SCode alternative)
//        {
//            return (consequent is Argument0) ? ConditionalSA0S.Make (predicate, (Argument0) consequent, alternative)
//                : (consequent is Argument1) ? ConditionalSA1S.Make (predicate, (Argument1) consequent, alternative)
//                : new ConditionalSAS (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            predicateTypeHistogram.Note (this.predicateType);
//            noteCalls (this.predicate);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = environment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;

//            }

//            if ((ev is bool) && (bool) ev == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//            else {
//#if DEBUG
//                Debug.WriteLineIf (Primitive.Noisy, "    => #t");
//#endif
//                expression = this.consequent;
//                answer = environment.ArgumentValue (this.consequentOffset);
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalSA0S : Conditional
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalSA0S (SCode predicate, Argument0 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, Argument0 consequent, SCode alternative)
//        {
//            return
//                (predicate is PrimComb2EqCarA0L) ? PCond2EqCarA0LA0S.Make ((PrimComb2EqCarA0L) predicate, consequent, alternative)
                
//                : new ConditionalSA0S (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            predicateTypeHistogram.Note (this.predicateType);
//            noteCalls (this.predicate);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = environment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;

//            }

//            if ((ev is bool) && (bool) ev == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//            else {
//#if DEBUG
//                Debug.WriteLineIf (Primitive.Noisy, "    => #t");
//#endif
//                expression = this.consequent;
//                answer = environment.Argument0Value;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalSA1S : ConditionalSAS
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalSA1S (SCode predicate, Argument1 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, Argument1 consequent, SCode alternative)
//        {
//            return new ConditionalSA1S (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            predicateTypeHistogram.Note (this.predicateType);
//            noteCalls (this.predicate);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = environment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;

//            }

//            if ((ev is bool) && (bool) ev == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//            else {
//#if DEBUG
//                Debug.WriteLineIf (Primitive.Noisy, "    => #t");
//#endif
//                expression = this.consequent;
//                answer = environment.Argument1Value;
//                return false;
//            }
//        }
//    }


//    [Serializable]
//    class PCond2EqCarA0LA0S : ConditionalSA0S
//    {
        
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object lambdaName;
//        public readonly int argDepth;
//        public readonly int argOffset;

//        protected PCond2EqCarA0LA0S (PrimComb2EqCarA0L predicate, Argument0 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.lambdaName = predicate.lambdaName;
//            this.argDepth = predicate.argDepth;
//            this.argOffset = predicate.argOffset;
//        }

//        public static SCode Make (PrimComb2EqCarA0L predicate, Argument0 consequent, SCode alternative)
//        {
//            return new PCond2EqCarA0LA0S (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.lambdaName, this.argDepth, this.argOffset))
//                throw new NotImplementedException ();

//            Cons ev0 = environment.Argument0Value as Cons;
//            if (ev0 == null) throw new NotImplementedException ();

//            object ev;
//            if (ObjectModel.Eq (out ev, ev0.Car, ev1))
//                throw new NotImplementedException ();

//            if ((ev is bool) && (bool) ev == false) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//                Debug.WriteLineIf (Primitive.Noisy, "    => #f");
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//            else {
//#if DEBUG
//                Debug.WriteLineIf (Primitive.Noisy, "    => #t");
//#endif
//                answer = environment.Argument0Value;
//                return false;
//            }
//        }
//    }

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
    class ConditionalASA : ConditionalASL
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
#endif

        protected ConditionalASA (Argument predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument predicate, SCode consequent, Argument alternative)
        {
            return 
                (alternative is Argument0) ? Unimplemented()
                : (alternative is Argument1) ? Unimplemented()
                : new ConditionalASA (predicate, consequent, alternative);
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
//    class PrimitiveConditional1A : PrimitiveConditional1
//    {
//#if DEBUG
//        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        int arg0offset;

//        protected PrimitiveConditional1A (PrimitiveCombination1A predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.arg0offset = predicate.argOffset;
//        }

//        public static SCode Make (PrimitiveCombination1A predicate, SCode consequent, SCode alternative)
//        {
//            return
//                (predicate is PrimitiveCombination1A0) ? PrimitiveConditional1A0.Make ((PrimitiveCombination1A0) predicate, consequent, alternative)
//                : (predicate is PrimitiveCombination1A1) ? PrimitiveConditional1A1.Make ((PrimitiveCombination1A1) predicate, consequent, alternative)
//                : new PrimitiveConditional1A (predicate, consequent, alternative);
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
//        protected PrimitiveConditional1A0 (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
//        {
//            return
//                (predicate.Operator == Primitive.Find ("NULL?", 1)) ? PrimitiveConditionalIsNullA0.Make (predicate, consequent, alternative)     
//                : (predicate.Operator == Primitive.Find ("PAIR?", 1)) ? PrimitiveConditionalIsPairA0.Make (predicate, consequent, alternative)
//                : (predicate.Operator == Primitive.Find ("%RECORD?", 1)) ? PrimitiveConditionalIsRecordA0.Make (predicate, consequent, alternative)
//              :  new PrimitiveConditional1A0 (predicate, consequent, alternative);
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

//        protected PrimitiveConditional1A1 (PrimitiveCombination1A1 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1A1 predicate, SCode consequent, SCode alternative)
//        {
//            return
//                 (predicate.Operator == Primitive.Find ("NULL?", 1)) ? PrimitiveConditionalIsNullA1.Make (predicate, consequent, alternative)     
//                : (predicate.Operator == Primitive.Find ("PAIR?", 1)) ? PrimitiveConditionalIsPairA1.Make (predicate, consequent, alternative)
//                : new PrimitiveConditional1A1 (predicate, consequent, alternative);
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
//    class PrimitiveConditionalIsNullA0 : PrimitiveConditional1A0
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        PrimitiveConditionalIsNullA0 (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static new SCode Make (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
//        {
//            return (consequent is Quotation) ? PrimitiveIsNullA0QS.Make (predicate, (Quotation) consequent, alternative)
//                : new PrimitiveConditionalIsNullA0 (predicate, consequent, alternative);
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
//        PrimitiveIsNullA0QS (PrimitiveCombination1A0 predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.quoted = consequent.Quoted;
//        }

//        public static SCode Make (PrimitiveCombination1A0 predicate, Quotation consequent, SCode alternative)
//        {
//            return new PrimitiveIsNullA0QS (predicate, consequent, alternative);
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
//    class PrimitiveConditionalIsPairA0 : PrimitiveConditional1A0
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected PrimitiveConditionalIsPairA0 (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static new SCode Make (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
//        {
//            return (consequent is PrimitiveCarA0) ? PrimitiveIsPairA0CarA0.Make (predicate, (PrimitiveCarA0) consequent, alternative)
//                : (consequent is Quotation) ? PrimitiveIsPairA0QS.Make (predicate, (Quotation) consequent, alternative)
//                : (alternative is Quotation) ? PrimitiveIsPairA0SQ.Make (predicate, consequent, (Quotation) alternative)
//                :  new PrimitiveConditionalIsPairA0 (predicate, consequent, alternative);
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
//    class PrimitiveIsPairA0CarA0 : PrimitiveConditionalIsPairA0
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        PrimitiveIsPairA0CarA0 (PrimitiveCombination1A0 predicate, PrimitiveCarA0 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveCombination1A0 predicate, PrimitiveCarA0 consequent, SCode alternative)
//        {
//            return new PrimitiveIsPairA0CarA0 (predicate, consequent, alternative);
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
//    class PrimitiveIsPairA0QS : PrimitiveConditionalIsPairA0
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        public object quoted;
//        protected PrimitiveIsPairA0QS (PrimitiveCombination1A0 predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.quoted = consequent.Quoted;
//        }

//        public static SCode Make (PrimitiveCombination1A0 predicate, Quotation consequent, SCode alternative)
//        {
//            return new PrimitiveIsPairA0QS (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            if (environment.Argument0Value is Cons) {
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
//    class PrimitiveIsPairA0SQ : PrimitiveConditionalIsPairA0
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//#endif
//        public object quoted;
//        protected PrimitiveIsPairA0SQ (PrimitiveCombination1A0 predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.quoted = alternative.Quoted;
//        }

//        public static SCode Make (PrimitiveCombination1A0 predicate, SCode consequent, Quotation alternative)
//        {
//            return new PrimitiveIsPairA0SQ (predicate, consequent, alternative);
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
//                answer = null;
//                return true;
//            }
//            else {
//                answer = this.quoted;
//                return false;

//            }
//        }
//    }

//    [Serializable]
//    class PrimitiveConditionalIsRecordA0 : PrimitiveConditional1A0
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected PrimitiveConditionalIsRecordA0 (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static new SCode Make (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
//        {
//            return //(consequent is Quotation) ? PrimitiveIsNullA0QS.Make (predicate, (Quotation) consequent, alternative)
//                //: 
//                new PrimitiveConditionalIsRecordA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif

//            if (environment.Argument0Value is Record) {
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
//    class PrimitiveConditionalIsNullA1 : PrimitiveConditional1A1
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected PrimitiveConditionalIsNullA1 (PrimitiveCombination1A1 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static new SCode Make (PrimitiveCombination1A1 predicate, SCode consequent, SCode alternative)
//        {
//            return // (consequent is Quotation) ? PrimitiveIsNullA0QS.Make (procedure, arg, (Quotation) consequent, alternative)
//                //    : 
//            new PrimitiveConditionalIsNullA1 (predicate, consequent, alternative);
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
//    class PrimitiveConditionalIsPairA1 : PrimitiveConditional1A1
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected PrimitiveConditionalIsPairA1 (PrimitiveCombination1A1 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static new SCode Make (PrimitiveCombination1A1 predicate, SCode consequent, SCode alternative)
//        {
//            return
//                //(consequent is PrimitiveCarA0)
//                //? PrimitiveIsPairA0CarA0.Make (procedure, arg, (PrimitiveCarA0) consequent, alternative)
//                //: 
//                new PrimitiveConditionalIsPairA1 (predicate, consequent, alternative);
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


//   [Serializable]
//    class PrimitiveConditional1SQS : PrimitiveConditional1
//    {
//#if DEBUG
//        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
//        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected readonly object quoted;

//        protected PrimitiveConditional1SQS (PrimitiveCombination1 predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.quoted = consequent.Quoted;
//        }

//        public static SCode Make (PrimitiveCombination1 predicate, Quotation consequent, SCode alternative)
//        {
//            return new PrimitiveConditional1SQS (predicate, consequent, alternative);
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
//    class PrimitiveIsEq : PrimitiveConditional2
//    {
//#if DEBUG
//        [NonSerialized]
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

//#endif
//        protected PrimitiveIsEq (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
//        {
//            return new PrimitiveIsEq (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg0);
//            noteCalls (this.arg1);
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
//            if (ObjectModel.Eq (out answer, ev0, ev1))
//                throw new NotImplementedException ();
//            //if (this.method (out answer, ev0, ev1)) {
//            //    TailCallInterpreter tci = answer as TailCallInterpreter;
//            //    if (tci != null) {
//            //        answer = null; // dispose of the evidence
//            //        // set up the interpreter for a tail call
//            //        Control cExpression = tci.Expression;
//            //        Environment cEnvironment = tci.Environment;
//            //        while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
//            //    }
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
