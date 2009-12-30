using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    [Serializable]
    class Conditional : SCode, ISerializable, ISystemHunk3
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.CONDITIONAL; } }

#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

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
            : base ()
        {
            this.predicate = predicate;
            this.consequent = consequent;
            this.alternative = alternative;
#if DEBUG
            this.predicateType = predicate.GetType ();
            this.consequentType = consequent.GetType ();
            this.alternativeType = alternative.GetType ();
#endif
        }

        static SCode SpecialMake (Quotation predicate, SCode consequent, SCode alternative)
        {
            //Debug.Write ("\n; Folding conditional");
            object val = predicate.Quoted;
            return (val is bool && (bool) val == false) ? alternative : consequent;
        }

        static SCode StandardMake (SCode predicate, SCode consequent, SCode alternative)
        {
            return
                (! Configuration.EnableConditionalOptimization) ? new Conditional(predicate, consequent, alternative) :
                (! Configuration.EnableConditionalSpecialization) ? new Conditional (predicate, consequent, alternative) :
                (Configuration.EnablePrimitiveConditional1 &&
                 predicate is PrimitiveCombination1) ? PCond1.Make ((PrimitiveCombination1) predicate, consequent, alternative) :
                (Configuration.EnablePrimitiveConditional2 && 
                 predicate is PrimitiveCombination2) ? PCond2.Make ((PrimitiveCombination2) predicate, consequent, alternative) :
                (predicate is Argument) ? ConditionalA.Make ((Argument) predicate, consequent, alternative) :
                (predicate is Quotation) ? ConditionalQ.Make ((Quotation) predicate, consequent, alternative) :
                (predicate is StaticVariable) ? ConditionalS.Make ((StaticVariable) predicate, consequent, alternative) :
                (consequent is Argument) ? ConditionalXA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? ConditionalXQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? ConditionalXS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? ConditionalXXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? ConditionalXXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? ConditionalXXS.Make (predicate, consequent, (StaticVariable) alternative) :
                new Conditional (predicate, consequent, alternative);
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
            //Console.WriteLine ("; rewrite as sequence");
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

        static SCode SpecialMake (Disjunction pred, SCode cons, SCode alt)
        {
            // Debug.Write ("\n; Distribute conditional.");
            return Conditional.Make (pred.Predicate, cons, Conditional.Make (pred.Alternative, cons, alt));
        }

        static SCode SpecialMake (Conditional pred, SCode cons, SCode alt)
        {
            // Debug.Write ("\n; Distribute conditional.");
            return Conditional.Make (pred.Predicate,
                                     Conditional.Make (pred.Consequent,
                                                       cons,
                                                       alt),
                                     Conditional.Make (pred.Alternative,
                                                       cons,
                                                       alt));


            //SCode b0 = pred.predicate;
            //if (pred.consequent is Quotation &&
            //    pred.alternative is Quotation) {
            //    object pc = ((Quotation) pred.consequent).Quoted;
            //    object pa = ((Quotation) pred.alternative).Quoted;
            //    bool pcFalse = pc is bool && (bool) pc == false;
            //    bool paFalse = pa is bool && (bool) pa == false;
            //    if (pcFalse && paFalse)
            //        Debugger.Break ();
            //    else if (pcFalse && !paFalse)
            //        Debugger.Break ();
            //    else if (!pcFalse && paFalse)
            //        return Conditional.Make (b0, cons, alt);
            //    else if (!pcFalse && !paFalse)
            //        Debugger.Break ();
            //    else
            //        throw new NotImplementedException ();
            //}
            //else if (pred.consequent is Conditional) {
            //    SCode b1 = ((Conditional) pred.consequent).predicate;
            //    SCode x = ((Conditional) pred.consequent).consequent;
            //    SCode y = ((Conditional) pred.consequent).alternative;
            //    if (x is Quotation &&
            //        y is Quotation)
            //        Debugger.Break ();
            //    return StandardMake (pred, cons, alt);
            //}
            //else if (pred.alternative is Conditional) {
            //    SCode b2 = ((Conditional) pred.alternative).predicate;
            //    SCode x = ((Conditional) pred.alternative).consequent;
            //    SCode y = ((Conditional) pred.alternative).alternative;
            //    if (x is Quotation &&
            //        y is Quotation)
            //        Debugger.Break ();
            //    return StandardMake (pred, cons, alt);
            //}
            //else if (pred.predicate is Variable &&
            //         pred.consequent is Quotation) {
            //    object q = ((Quotation) pred.consequent).Quoted;
            //    if (q is bool && (bool) q == false)
            //        return StandardMake (pred, cons, alt);
            //    else
            //        Debugger.Break ();
            //}
            //else if (pred.predicate is Variable &&
            //         pred.alternative is Variable &&
            //         ((Variable) pred.predicate).Name == ((Variable) pred.alternative).Name) {
            //    Debugger.Break ();
            //}

            //return StandardMake (pred, cons, alt);
        }

        public static SCode SpecialMake (Sequence2 predicate, SCode consequent, SCode alternative)
        {
            // Debug.Write ("\n; Conditional w sequence2 predicate.");
            return Sequence2.Make (predicate.First,
                                   Conditional.Make (predicate.Second, consequent, alternative));
        }

        public static SCode SpecialMake (Sequence3 predicate, SCode consequent, SCode alternative)
        {
            // Debug.Write ("\n; Conditional w sequence3 predicate.");
            return Sequence3.Make (predicate.First,
                                   predicate.Second,
                                   Conditional.Make (predicate.Third, consequent, alternative));
        }

        public static SCode RewriteAsDisjunction (SCode predicate, SCode alternative)
        {
            // Debug.Write ("\n; Rewrite conditonal as disjunction.");
            return Disjunction.Make (predicate, alternative);
        }

        public static SCode Make (SCode predicate, SCode consequent, SCode alternative)
        {
            return (!Configuration.EnableConditionalOptimization) ? new Conditional (predicate, consequent, alternative) :
                StandardMake (predicate, consequent, alternative);
            //(predicate is Conditional && 
            //Configuration.EnableCodeRewriting &&
            //Configuration.EnableConditionalDistribution) ? SpecialMake ((Conditional) predicate, consequent, alternative) :
            //(predicate is Disjunction &&
            //Configuration.EnableCodeRewriting &&
            //Configuration.EnableConditionalDistribution) ? SpecialMake ((Disjunction) predicate, consequent, alternative) :
            //(predicate is Quotation &&
            // Configuration.EnableFoldConditional) ? SpecialMake ((Quotation) predicate, consequent, alternative) :
            // (predicate is Sequence2) ? SpecialMake ((Sequence2) predicate, consequent, alternative) :
            // (predicate is Sequence3) ? SpecialMake ((Sequence3) predicate, consequent, alternative) :
            //(consequent is Quotation && alternative is Quotation) ? SpecialMake (predicate, (Quotation) consequent, (Quotation) alternative) :
            //(predicate is Variable &&
            // consequent is Variable &&
            // ((Variable) predicate).Name == ((Variable) consequent).Name &&
            // Configuration.EnableDisjunctionConversion) ? RewriteAsDisjunction (predicate, alternative) :
            //(predicate is Variable &&
            // alternative is Variable &&
            // ((Variable) predicate).Name == ((Variable) alternative).Name) ? Conditional.Make (predicate, consequent, Quotation.Make(false)) :
            // (consequent is Variable &&
            //  alternative is Variable &&
            //  ((Variable) consequent).Name == ((Variable) alternative).Name) ? Sequence2.Make (predicate, consequent) :
            //    StandardMake (predicate, consequent, alternative);
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


        //public override SCode BindVariables (LexicalMap lexicalMap)
        //{
        //    SCode boundPredicate = this.predicate.BindVariables (lexicalMap);
        //    SCode boundConsequent = this.consequent.BindVariables (lexicalMap);
        //    SCode boundAlternative = this.alternative.BindVariables (lexicalMap);
        //    return (boundPredicate == this.predicate &&
        //        boundConsequent == this.consequent &&
        //        boundAlternative == this.alternative) ?
        //        this :
        //        Conditional.Make (boundPredicate, boundConsequent, boundAlternative);
        //}

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
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "Conditional.EvalStep";
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Conditional.EvalStep";
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
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "Conditional.EvalStep";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "Conditional.EvalStep";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            return this.predicate.MutatesAny (formals)
                || this.consequent.MutatesAny (formals)
                || this.alternative.MutatesAny (formals);
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (ConditionalDeserializer));
            info.AddValue ("predicate", this.predicate);
            info.AddValue ("consequent", this.consequent);
            info.AddValue ("alternative", this.alternative);
        }

        #endregion

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

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialResult p = this.predicate.PartialEval (environment);
            PartialResult c = this.consequent.PartialEval (environment);
            PartialResult a = this.alternative.PartialEval (environment);
            return new PartialResult (p.Residual == this.predicate &&
                c.Residual == this.consequent &&
                a.Residual == this.alternative ? this : Conditional.Make (p.Residual, c.Residual, a.Residual));
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            this.predicate.CollectFreeVariables(freeVariableSet);
            this.consequent.CollectFreeVariables (freeVariableSet);
            this.alternative.CollectFreeVariables (freeVariableSet);
        }
    }

    [Serializable]
    internal sealed class ConditionalDeserializer : IObjectReference
    {
        SCode predicate;
        SCode consequent;
        SCode alternative;

        // GetRealObject is called after this object is deserialized.
        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public Object GetRealObject (StreamingContext context)
        {
            return Conditional.Make (this.predicate, this.consequent, this.alternative);
        }

        public void SetPredicate (SCode value) { this.predicate = value; }
        public void SetConsequent (SCode value) { this.consequent = value; }
        public void SetAlternative (SCode value) { this.alternative = value; }
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
    class ConditionalA : Conditional
    {
        protected readonly int predicateOffset;
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalA (Argument predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateOffset = predicate.Offset;
        }

        public static SCode Make (Argument predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is Argument0) ? ConditionalA0.Make ((Argument0) predicate, consequent, alternative) :
                (predicate is Argument1) ? ConditionalA1.Make ((Argument1) predicate, consequent, alternative) :
                (consequent is Quotation) ? ConditionalAQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is Quotation) ? ConditionalAXQ.Make (predicate, consequent, (Quotation) alternative) :
                new ConditionalA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA");
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalA";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "ConditionalA";
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
                (consequent is Quotation) ? ConditionalA0Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is Quotation) ? ConditionalA0XQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? ConditionalA0XS.Make (predicate, consequent, (StaticVariable) alternative) :
                new ConditionalA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA0");
#endif
            object ev = environment.Argument0Value;

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalA0";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "ConditionalA0";
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
        static Histogram<object> consequentValueHistogram = new Histogram<object> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected ConditionalA0Q (Argument0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (Argument0 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Quotation) ? ConditionalA0QQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? ConditionalA0QS.Make (predicate, consequent, (StaticVariable) alternative) :
                new ConditionalA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA0Q");
#endif
            object ev = environment.Argument0Value;

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalA0Q";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                consequentValueHistogram.Note (this.consequentValue);
                SCode.location = "ConditionalA0Q";
#endif
                answer = this.consequentValue;
                return false;
            }
        }

    }

    [Serializable]
    class ConditionalA0QQ : ConditionalA0Q
    {
        public readonly object alternativeValue;

        protected ConditionalA0QQ (Argument0 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (Argument0 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new ConditionalA0QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA0QQ");
#endif
            object ev = environment.Argument0Value;

            if ((ev is bool) && (bool) ev == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    class ConditionalA0QS : ConditionalA0Q
    {
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected ConditionalA0QS (Argument0 predicate, Quotation consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (Argument0 predicate, Quotation consequent, StaticVariable alternative)
        {
            return
                new ConditionalA0QS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA0QS");
#endif
            object ev = environment.Argument0Value;

            if ((ev is bool) && (bool) ev == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }


    [Serializable]
    class ConditionalA0XQ : ConditionalA0
    {
#if DEBUG
        static Histogram<object> alternativeValueHistogram = new Histogram<object> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected ConditionalA0XQ (Argument0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (Argument0 predicate, SCode consequent, Quotation alternative)
        {
            return
                new ConditionalA0XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA0XQ");
#endif
            object ev = environment.Argument0Value;

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                alternativeValueHistogram.Note (this.alternativeValue == null ? "-Null-" : this.alternativeValue);
                SCode.location = "ConditionalA0XQ";
#endif
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);

                SCode.location = "ConditionalA0XQ";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }

    }

    [Serializable]
    class ConditionalA0XS : ConditionalA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected ConditionalA0XS (Argument0 predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (Argument0 predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new ConditionalA0XS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA0XS");
#endif
            object ev = environment.Argument0Value;

            if ((ev is bool) && (bool) ev == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);

                SCode.location = "ConditionalA0XQ";
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
                (consequent is Quotation) ? ConditionalA1Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is Quotation) ? ConditionalA1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new ConditionalA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA1");
#endif
            object ev = environment.Argument1Value;

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalA1";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "ConditionalA1";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }

    }

    [Serializable]
    class ConditionalA1Q : ConditionalA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected ConditionalA1Q (Argument1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (Argument1 predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Quotation) ? ConditionalA1QQ.Make (predicate, consequent, (Quotation) alternative) :
                new ConditionalA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA1Q");
#endif
            object ev = environment.Argument1Value;

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalA1";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                answer = consequentValue;
                return false;
            }
        }

    }

    [Serializable]
    sealed class ConditionalA1QQ : ConditionalA1Q
    {
        public readonly object alternativeValue;
        protected ConditionalA1QQ (Argument1 predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (Argument1 predicate, Quotation consequent, Quotation alternative)
        {
            return
                new ConditionalA1QQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA1QQ");
#endif
            object ev = environment.Argument1Value;

            if ((ev is bool) && (bool) ev == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }

    }

    [Serializable]
    sealed class ConditionalA1XQ : ConditionalA1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        ConditionalA1XQ (Argument1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (Argument1 predicate, SCode consequent, Quotation alternative)
        {
            return
                new ConditionalA1XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA1XQ");
#endif
            object ev = environment.Argument1Value;

            if ((ev is bool) && (bool) ev == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "ConditionalA1XQ";
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
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected ConditionalAQ (Argument predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (Argument predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Quotation) ? ConditionalAQQ.Make (predicate, consequent, (Quotation) alternative) :
                new ConditionalAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalAQ");
#endif
            object ev = environment.ArgumentValue(this.predicateOffset);

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalAQ";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                answer = consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    sealed class ConditionalAQQ : ConditionalAQ
    {
        public readonly object alternativeValue;
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
#if DEBUG
            Warm ("ConditionalAQQ");
#endif
            object ev = environment.ArgumentValue(this.predicateOffset);

            if ((ev is bool) && (bool) ev == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    sealed class ConditionalAXQ : ConditionalA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        ConditionalAXQ (Argument predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (Argument predicate, SCode consequent, Quotation alternative)
        {
            return
                new ConditionalAXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalAXQ");
#endif
            object ev = environment.ArgumentValue(this.predicateOffset);

            if ((ev is bool) && (bool) ev == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "ConditionalAXQ";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }

    }

    [Serializable]
    class ConditionalQ : Conditional
    {

        protected ConditionalQ (Quotation predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            
        }

        public static SCode Make (Quotation predicate, SCode consequent, SCode alternative)
        {
            Debugger.Break ();
            object pred = predicate.Quoted;
            return (pred is Boolean && (bool) pred == false)
                ? alternative
                : consequent;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

    }

    [Serializable]
    class ConditionalS : Conditional
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol predicateName;
        public readonly int predicateOffset;

        protected ConditionalS (StaticVariable predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateName = predicate.Name;
            this.predicateOffset = predicate.Offset;
        }

        public static SCode Make (StaticVariable predicate, SCode consequent, SCode alternative)
        {
            return 
                (consequent is Quotation) ? ConditionalSQ.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is Quotation) ? ConditionalSXQ.Make (predicate, consequent, (Quotation) alternative) :
                new ConditionalS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "ConditionalS.EvalStep";
#endif
            object ev;
            if (environment.StaticValue (out ev, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalS.EvalStep";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "ConditionalS.EvalStep";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class ConditionalSQ : ConditionalS
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected ConditionalSQ (StaticVariable predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (StaticVariable predicate, Quotation consequent, SCode alternative)
        {
            return
                new ConditionalSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "ConditionalSQ.EvalStep";
#endif
            object ev;
            if (environment.StaticValue (out ev, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalSQ.EvalStep";
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
    class ConditionalSXQ : ConditionalS
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected ConditionalSXQ (StaticVariable predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (StaticVariable predicate, SCode consequent, Quotation alternative)
        {
            return
                new ConditionalSXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "ConditionalSXQ.EvalStep";
#endif
            object ev;
            if (environment.StaticValue (out ev, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if ((ev is bool) && (bool) ev == false) {
                answer = this.alternativeValue;
                return false;

            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "ConditionalSXQ.EvalStep";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class ConditionalXA : Conditional
    {
        protected readonly int consequentOffset;
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalXA (SCode predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (SCode predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? ConditionalXA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? ConditionalXA1.Make (predicate, (Argument1) consequent, alternative) :
                (alternative is Argument) ? ConditionalXAA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? ConditionalXAQ.Make (predicate, consequent, (Quotation) alternative) :
                new ConditionalXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "ConditionalSA.EvalStep";
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "ConditionalSA.EvalStep.1";
#endif
            if (ev == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;

            }

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalSA.EvalStep.1";
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
    class ConditionalXA0 : ConditionalXA
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalXA0 (SCode predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (SCode predicate, Argument0 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? ConditionalXA0A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? ConditionalXA0Q.Make (predicate, consequent, (Quotation) alternative) :
                new ConditionalXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXA0.EvalStep");
            NoteCalls (this.predicate);
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
                //answer = Interpreter.Unwind;
                //return false;

            }

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class ConditionalXA0A : ConditionalXA0
    {
        protected readonly int alternativeOffset;
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalXA0A (SCode predicate, Argument0 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (SCode predicate, Argument0 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? ConditionalXA0A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? ConditionalXA0A1.Make (predicate, consequent, (Argument1) alternative) :
                new ConditionalXA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalXA0A.EvalStep");
            NoteCalls (this.predicate);
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
                //answer = Interpreter.Unwind;
                //return false;

            }

            answer = ((ev is bool) && (bool) ev == false) ?
                environment.ArgumentValue (this.alternativeOffset) :
                environment.Argument0Value;
            return false;
            
        }
    }

    [Serializable]
    sealed class ConditionalXA0A0 : ConditionalXA0A
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif

        ConditionalXA0A0 (SCode predicate, Argument0 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (SCode predicate, Argument0 consequent, Argument0 alternative)
        {
            return
                new ConditionalXA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXA0A0.EvalStep");
            NoteCalls (this.predicate);
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

            answer = environment.Argument0Value;
            return false;
        }
    }

    [Serializable]
    sealed class ConditionalXA0A1 : ConditionalXA0A
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif

        ConditionalXA0A1 (SCode predicate, Argument0 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (SCode predicate, Argument0 consequent, Argument1 alternative)
        {
            return
                new ConditionalXA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXA0A1.EvalStep");
            NoteCalls (this.predicate);
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

            answer = ((ev is bool) && (bool) ev == false) ?
                environment.Argument1Value :
                environment.Argument0Value;
            return false;
        }
    }

    [Serializable]
    sealed class ConditionalXA0Q : ConditionalXA0
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        ConditionalXA0Q (SCode predicate, Argument0 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (SCode predicate, Argument0 consequent, Quotation alternative)
        {
            return
                new ConditionalXA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "ConditionalXA0Q.EvalStep";
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "ConditionalXA0Q.EvalStep";
#endif
            if (ev == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;

            }

            answer = ((ev is bool) && (bool) ev == false) ? this.alternativeValue : environment.Argument0Value;
            return false;
        }
    }

    [Serializable]
    class ConditionalXA1 : ConditionalXA
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalXA1 (SCode predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (SCode predicate, Argument1 consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? ConditionalXA1A.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? ConditionalXA1Q.Make (predicate, consequent, (Quotation) alternative) :
                new ConditionalXA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXA1.EvalStep");
            NoteCalls (this.predicate);
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
                //answer = Interpreter.Unwind;
                //return false;

            }

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
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
    class ConditionalXA1A : ConditionalXA1
    {
        protected readonly int alternativeOffset;
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalXA1A (SCode predicate, Argument1 consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (SCode predicate, Argument1 consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? ConditionalXA1A0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? ConditionalXA1A1.Make (predicate, consequent, (Argument1) alternative) :
                new ConditionalXA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
#if DEBUG
            Warm ("ConditionalXA1A.EvalStep");
            NoteCalls (this.predicate);
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
                //answer = Interpreter.Unwind;
                //return false;

            }

            answer = ((ev is bool) && (bool) ev == false) ?
                environment.ArgumentValue (this.alternativeOffset) :
                environment.Argument0Value;
            return false;

        }
    }

    [Serializable]
    sealed class ConditionalXA1A0 : ConditionalXA1A
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif

        ConditionalXA1A0 (SCode predicate, Argument1 consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (SCode predicate, Argument1 consequent, Argument0 alternative)
        {
            return
                new ConditionalXA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXA1A0.EvalStep");
            NoteCalls (this.predicate);
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

            answer = ((ev is bool) && (bool) ev == false) ? environment.Argument0Value : environment.Argument1Value;
            return false;
        }
    }

    [Serializable]
    sealed class ConditionalXA1A1 : ConditionalXA1A
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif

        ConditionalXA1A1 (SCode predicate, Argument1 consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (SCode predicate, Argument1 consequent, Argument1 alternative)
        {
            return
                new ConditionalXA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXA1A1.EvalStep");
            NoteCalls (this.predicate);
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

            answer = environment.Argument1Value;
            return false;
        }
    }

    [Serializable]
    sealed class ConditionalXA1Q : ConditionalXA1
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        ConditionalXA1Q (SCode predicate, Argument1 consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (SCode predicate, Argument1 consequent, Quotation alternative)
        {
            return
                new ConditionalXA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "ConditionalXA1Q.EvalStep";
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "ConditionalXA1Q.EvalStep";
#endif
            if (ev == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;

            }

            answer = ((ev is bool) && (bool) ev == false) ? this.alternativeValue : environment.Argument1Value;
            return false;
        }
    }

    [Serializable]
    class ConditionalXAA : ConditionalXA
    {
        readonly int alternativeOffset;
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalXAA (SCode predicate, Argument consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (SCode predicate, Argument consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? ConditionalXAA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? ConditionalXAA1.Make (predicate, consequent, (Argument1) alternative) :
                new ConditionalXAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXAA.EvalStep");
            NoteCalls (this.predicate);
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

            answer = environment.ArgumentValue (((ev is bool) && (bool) ev == false) ? this.alternativeOffset : this.consequentOffset);
            return false;
        }
    }

    [Serializable]
    sealed class ConditionalXAA0 : ConditionalXAA
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif

        ConditionalXAA0 (SCode predicate, Argument consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (SCode predicate, Argument consequent, Argument0 alternative)
        {
            return
                new ConditionalXAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXAA0.EvalStep");
            NoteCalls (this.predicate);
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

            answer = ((ev is bool) && (bool) ev == false) ?
                environment.Argument0Value :
                environment.ArgumentValue (this.consequentOffset);
            return false;
        }
    }

    [Serializable]
    sealed class ConditionalXAA1 : ConditionalXAA
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif

        ConditionalXAA1 (SCode predicate, Argument consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (SCode predicate, Argument consequent, Argument1 alternative)
        {
            return
                new ConditionalXAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXAA1.EvalStep");
            NoteCalls (this.predicate);
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

            answer = ((ev is bool) && (bool) ev == false) ?
                environment.Argument1Value :
                environment.ArgumentValue (this.consequentOffset);
            return false;
        }
    }

    [Serializable]
    sealed class ConditionalXAQ : ConditionalXA
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        ConditionalXAQ (SCode predicate, Argument consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (SCode predicate, Argument consequent, Quotation alternative)
        {
            return
                new ConditionalXAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "ConditionalXAQ.EvalStep";
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "ConditionalXAQ.EvalStep.1";
#endif
            if (ev == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;

            }

            answer = ((ev is bool) && (bool) ev == false) ? this.alternativeValue : environment.ArgumentValue (this.consequentOffset);
            return false;
        }
    }

//    [Serializable]
//    class ConditionalXA0A : ConditionalXA0
//    {
//        protected readonly int alternativeOffset;
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalXA0A (SCode predicate, Argument0 consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeOffset = alternative.Offset;
//        }

//        public static SCode Make (SCode predicate, Argument0 consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? ConditionalXA0A0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? ConditionalXA0A1.Make (predicate, consequent, (Argument1) alternative) :
//                new ConditionalXA0A (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("ConditionalXA0A.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = environment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            answer = ((ev is bool) && (bool) ev == false) ? environment.ArgumentValue (this.alternativeOffset) : environment.Argument0Value;
//            return false;
//        }
//    }

    [Serializable]
    class ConditionalXQ : Conditional
    {
        protected readonly object consequentValue;
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalXQ (SCode predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (SCode predicate, Quotation consequent, SCode alternative)
        {
            return
                (alternative is Argument) ? ConditionalXQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? ConditionalXQQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? ConditionalXQS.Make (predicate, consequent, (StaticVariable) alternative) :
                new ConditionalXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "ConditionalXQ";
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
#if DEBUG
                        SCode.location = "ConditionalXQ";
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
                NoteCalls(this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalXQ";
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
    class ConditionalXQA : ConditionalXQ
    {
        readonly int alternativeOffset;
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalXQA (SCode predicate, Quotation consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (SCode predicate, Quotation consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? ConditionalXQA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? ConditionalXQA1.Make (predicate, consequent, (Argument1) alternative) :
                new ConditionalXQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXQA.EvalStep");
            NoteCalls (this.predicate);
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

            answer = ((ev is bool) && (bool) ev == false) ?
                environment.ArgumentValue (this.alternativeOffset) :
                this.consequentValue;
                return false;
        }
    }

    [Serializable]
    sealed class ConditionalXQA0 : ConditionalXQA
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif

        ConditionalXQA0 (SCode predicate, Quotation consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (SCode predicate, Quotation consequent, Argument0 alternative)
        {
            return
                new ConditionalXQA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXQA0.EvalStep");
            NoteCalls (this.predicate);
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

            answer = ((ev is bool) && (bool) ev == false) ?
                environment.Argument0Value :
                this.consequentValue;
                return false;
        }
    }

    [Serializable]
    sealed class ConditionalXQA1 : ConditionalXQA
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif

        ConditionalXQA1 (SCode predicate, Quotation consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (SCode predicate, Quotation consequent, Argument1 alternative)
        {
            return
                new ConditionalXQA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "ConditionalXQA1.EvalStep";
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

            answer = ((ev is bool) && (bool) ev == false) ?
                environment.Argument1Value :
                this.consequentValue;
            return false;
        }
    }

    [Serializable]
    sealed class ConditionalXQS : ConditionalXQ
    {
        readonly object alternativeName;
        readonly int alternativeOffset;
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif

        ConditionalXQS (SCode predicate, Quotation consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (SCode predicate, Quotation consequent, StaticVariable alternative)
        {
            return
                new ConditionalXQS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXQS.EvalStep");
            NoteCalls (this.predicate);
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
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = this.consequentValue;
                return false;
            }
        }
    }

    [Serializable]
    sealed class ConditionalXQQ : ConditionalXQ
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif
        readonly object alternativeQuoted;

        ConditionalXQQ (SCode predicate, Quotation consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeQuoted = alternative.Quoted;
        }

        public static SCode Make (SCode predicate, Quotation consequent, Quotation alternative)
        {
            return new ConditionalXQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "ConditionalXQQ.EvalStep";
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "ConditionalXQQ.EvalStep";
#endif
            if (ev == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;

            }

            answer = ((ev is bool) && (bool) ev == false) ?
                this.alternativeQuoted :
                this.consequentValue;
            return false;
        }
    }

    [Serializable]
    class ConditionalXS : Conditional
    {
        protected readonly Symbol consequentName;
        public readonly int consequentOffset;
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalXS (SCode predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (SCode predicate, StaticVariable consequent, SCode alternative)
        {
            return
                //(alternative is Argument) ? ConditionalXQA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? ConditionalXSQ.Make (predicate, consequent, (Quotation) alternative) :
                new ConditionalXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "ConditionalXS";
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "ConditionalXS";
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
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalXS";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class ConditionalXSQ : ConditionalXS
    {
        protected readonly object alternativeValue;
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalXSQ (SCode predicate, StaticVariable consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (SCode predicate, StaticVariable consequent, Quotation alternative)
        {
            return
                new ConditionalXSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "ConditionalXSQ";
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "ConditionalXSQ";
#endif
            if (ev == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            if ((ev is bool) && (bool) ev == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }

    [Serializable]
    class ConditionalXXA : Conditional
    {
        readonly int alternativeOffset;
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalXXA (SCode predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (SCode predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? ConditionalXXA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? ConditionalXXA1.Make (predicate, consequent, (Argument1) alternative) :
                new ConditionalXXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXXA.EvalStep");
            NoteCalls (this.predicate);
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
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    sealed class ConditionalXXA0 : ConditionalXXA
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        ConditionalXXA0 (SCode predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (SCode predicate, SCode consequent, Argument0 alternative)
        {
            return
                new ConditionalXXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXXA0.EvalStep");
            NoteCalls (this.predicate);
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
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);

#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    sealed class ConditionalXXA1 : ConditionalXXA
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        ConditionalXXA1 (SCode predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (SCode predicate, SCode consequent, Argument1 alternative)
        {
            return
                new ConditionalXXA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "ConditionalXXA1.EvalStep";
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
                answer = environment.Argument1Value;
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    sealed class ConditionalXXS : Conditional
    {
        readonly object alternativeName;
        readonly int alternativeOffset;
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        ConditionalXXS (SCode predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (SCode predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new ConditionalXXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXXS.EvalStep");
            NoteCalls (this.predicate);
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
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    sealed class ConditionalXXQ : Conditional
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif
        object alternativeQuoted;

        ConditionalXXQ (SCode predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeQuoted = alternative.Quoted;
        }

        public static SCode Make (SCode predicate, SCode consequent, Quotation alternative)
        {
            return new ConditionalXXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "ConditionalXXQ.EvalStep";
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "ConditionalXXQ.EvalStep";
#endif
            if (ev == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;

            }

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "ConditionalXXQ.EvalStep";
#endif
                answer = this.alternativeQuoted;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "ConditionalXXQ.EvalStep";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }






////    [Serializable]
////    class ConditionalASS : Conditional
////    {
////#if DEBUG
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        protected int randOffset;

////        protected ConditionalASS (Argument predicate, SCode consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////            this.randOffset = predicate.Offset;
////        }

////        public static SCode Make (Argument predicate, SCode consequent, SCode alternative)
////        {
////            return new ConditionalASS (predicate, consequent, alternative);
////        }

////        public override SCode Bind (LexicalMap ctenv)
////        {
////            throw new NotImplementedException ();
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
////        {
////#if DEBUG
////            Warm ();
////#endif
////            object ev = environment.ArgumentValue (this.randOffset);

////            if ((ev is bool) && (bool) ev == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////                
////#endif
////                expression = this.alternative;
////                answer = null;
////                return true;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////                
////#endif
////                expression = this.consequent;
////                answer = null;
////                return true;
////            }
////        }
////    }

////    [Serializable]
////    class ConditionalSAS : Conditional
////    {
////#if DEBUG
////        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        protected int consequentOffset;

////        protected ConditionalSAS (SCode predicate, Argument consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////            this.consequentOffset = consequent.Offset;
////        }

////        public static SCode Make (SCode predicate, Argument consequent, SCode alternative)
////        {
////            return (consequent is Argument0) ? ConditionalSA0S.Make (predicate, (Argument0) consequent, alternative)
////                : (consequent is Argument1) ? ConditionalSA1S.Make (predicate, (Argument1) consequent, alternative)
////                : new ConditionalSAS (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
////        {
////#if DEBUG
////            Warm ();
////            predicateTypeHistogram.Note (this.predicateType);
////            NoteCalls (this.predicate);
////#endif
////            object ev;
////            Control unev = this.predicate;
////            Environment env = environment;

////            while (unev.EvalStep (out ev, ref unev, ref env)) { };
////            if (ev == Interpreter.UnwindStack) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
////                //environment = env;
////                //answer = Interpreter.Unwind;
////                //return false;

////            }

////            if ((ev is bool) && (bool) ev == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////                
////#endif
////                expression = this.alternative;
////                answer = null;
////                return true;
////            }
////            else {
////#if DEBUG
////                
////#endif
////                expression = this.consequent;
////                answer = environment.ArgumentValue (this.consequentOffset);
////                return false;
////            }
////        }
////    }

////    [Serializable]
////    class ConditionalSA0S : Conditional
////    {
////#if DEBUG
////        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif

////        protected ConditionalSA0S (SCode predicate, Argument0 consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////        }

////        public static SCode Make (SCode predicate, Argument0 consequent, SCode alternative)
////        {
////            return
////                (predicate is PrimComb2EqCarA0L) ? PCond2EqCarA0LA0S.Make ((PrimComb2EqCarA0L) predicate, consequent, alternative)
                
////                : new ConditionalSA0S (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
////        {
////#if DEBUG
////            Warm ();
////            predicateTypeHistogram.Note (this.predicateType);
////            NoteCalls (this.predicate);
////#endif
////            object ev;
////            Control unev = this.predicate;
////            Environment env = environment;

////            while (unev.EvalStep (out ev, ref unev, ref env)) { };
////            if (ev == Interpreter.UnwindStack) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
////                //environment = env;
////                //answer = Interpreter.Unwind;
////                //return false;

////            }

////            if ((ev is bool) && (bool) ev == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////                
////#endif
////                expression = this.alternative;
////                answer = null;
////                return true;
////            }
////            else {
////#if DEBUG
////                
////#endif
////                expression = this.consequent;
////                answer = environment.Argument0Value;
////                return false;
////            }
////        }
////    }

////    [Serializable]
////    class ConditionalSA1S : ConditionalSAS
////    {
////#if DEBUG
////        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif

////        protected ConditionalSA1S (SCode predicate, Argument1 consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////        }

////        public static SCode Make (SCode predicate, Argument1 consequent, SCode alternative)
////        {
////            return new ConditionalSA1S (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
////        {
////#if DEBUG
////            Warm ();
////            predicateTypeHistogram.Note (this.predicateType);
////            NoteCalls (this.predicate);
////#endif
////            object ev;
////            Control unev = this.predicate;
////            Environment env = environment;

////            while (unev.EvalStep (out ev, ref unev, ref env)) { };
////            if (ev == Interpreter.UnwindStack) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
////                //environment = env;
////                //answer = Interpreter.Unwind;
////                //return false;

////            }

////            if ((ev is bool) && (bool) ev == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////                
////#endif
////                expression = this.alternative;
////                answer = null;
////                return true;
////            }
////            else {
////#if DEBUG
////                
////#endif
////                expression = this.consequent;
////                answer = environment.Argument1Value;
////                return false;
////            }
////        }
////    }



//    [Serializable]
//    class ConditionalAAS : Conditional
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
//#endif
//        protected int predicateOffset;
//        protected int consequentOffset;
//        protected ConditionalAAS (Argument predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.predicateOffset = predicate.Offset;
//            this.consequentOffset = consequent.Offset;
//        }

//        public static SCode Make (Argument predicate, Argument consequent, SCode alternative)
//        {
//            return new ConditionalAAS (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            object ev = environment.ArgumentValue (this.predicateOffset);

//            if ((ev is bool) && (bool) ev == false) {
//#if DEBUG
//                NoteCalls (this.alternative);

                
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//            else {
//#if DEBUG
                
//#endif
//                answer = environment.ArgumentValue (this.consequentOffset);
//                return false;
//            }

//        }
//    }


//    [Serializable]
//    class ConditionalASA : ConditionalASL
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
//#endif

//        protected ConditionalASA (Argument predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (Argument predicate, SCode consequent, Argument alternative)
//        {
//            return 
//                (alternative is Argument0) ? Unimplemented()
//                : (alternative is Argument1) ? ConditionalASA1.Make (predicate, consequent, (Argument1) alternative)
//                : new ConditionalASA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("ConditionalASA.EvalStep");
//#endif
//            object ev = environment.ArgumentValue (this.predicateOffset);

//            if ((ev is bool) && (bool) ev == false) {
//#if DEBUG

                
//#endif
//                expression = this.alternative;
//                answer = environment.ArgumentValue (this.alternativeOffset);
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
                
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }


//        }
//    }

//    [Serializable]
//    class ConditionalASA1 : ConditionalASA
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalASA1 (Argument predicate, SCode consequent, Argument1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (Argument predicate, SCode consequent, Argument1 alternative)
//        {
//            return
//                 new ConditionalASA1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("ConditionalASA1.EvalStep");
//#endif
//            object ev = environment.ArgumentValue (this.predicateOffset);

//            if ((ev is bool) && (bool) ev == false) {
//                answer = environment.Argument1Value;
//                return false;
//            }
//            else {
//#if DEBUG
//                NoteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
                
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }


//        }
//    }



//    [Serializable]
//    class ConditionalSAA : Conditional
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//#endif
//        protected int consequentOffset;
//        protected int alternativeOffset;

//        protected ConditionalSAA (SCode predicate, Argument consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentOffset = consequent.Offset;
//            this.alternativeOffset = alternative.Offset;
//        }

//        public static SCode Make (SCode predicate, Argument consequent, Argument alternative)
//        {
//            return new ConditionalSAA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = environment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;

//            }
//            if ((ev is bool) && (bool) ev == false) {
//#if DEBUG

                
//#endif
//                answer = environment.ArgumentValue (this.alternativeOffset);
//                return false;
//            }
//            else {
//#if DEBUG
                
//#endif
//                answer = environment.ArgumentValue (this.consequentOffset);
//                return false;
//            }

//        }
//    }


//    [Serializable]
//    class ConditionalAAA : Conditional
//    {

//        protected ConditionalAAA (Argument predicate, Argument consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {

//        }

//        public static SCode Make (Argument predicate, Argument consequent, Argument alternative)
//        {
//            return new ConditionalAAA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();

//        }
//    }


 




    //    [Serializable]
    //    class ConditionalA : ConditionalL
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif
    //        protected ConditionalA (Argument predicate, SCode consequent, SCode alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        public static SCode Make (Argument predicate, SCode consequent, SCode alternative)
    //        {
    //            return
    //                (predicate is Argument0) ? ConditionalA0.Make ((Argument0) predicate, consequent, alternative) :
    //                (predicate is Argument1) ? ConditionalA1.Make ((Argument1) predicate, consequent, alternative) :
    //                (consequent is LexicalVariable) ? ConditionalAL.Make (predicate, (LexicalVariable) consequent, alternative) :
    //                (consequent is Quotation) ? ConditionalAQ.Make (predicate, (Quotation) consequent, alternative) :
    //                (alternative is LexicalVariable) ? ConditionalASL.Make (predicate, consequent, (LexicalVariable) alternative) :
    //                (alternative is Quotation) ? ConditionalASQ.Make (predicate, consequent, (Quotation) alternative) :
    //                new ConditionalA (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA.EvalStep");
    //#endif
    //            object ev = environment.ArgumentValue (this.predicateOffset);

    //            if ((ev is bool) && (bool) ev == false) {
    //#if DEBUG
    //                NoteCalls (this.alternative);
    //                alternativeTypeHistogram.Note (this.alternativeType);
    //#endif
    //                expression = this.alternative;
    //                answer = null;
    //                return true;
    //            }
    //            else {
    //#if DEBUG
    //                NoteCalls (this.consequent);
    //                consequentTypeHistogram.Note (this.consequentType);
    //#endif
    //                expression = this.consequent;
    //                answer = null;
    //                return true;
    //            }
    //        }
    //    }

    //    [Serializable]
    //    class ConditionalA0 : ConditionalA
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif

    //        protected ConditionalA0 (Argument0 predicate, SCode consequent, SCode alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        public static SCode Make (Argument0 predicate, SCode consequent, SCode alternative)
    //        {
    //            return
    //                (consequent is LexicalVariable) ? ConditionalA0L.Make (predicate, (LexicalVariable) consequent, alternative) :
    //                (consequent is Quotation) ? ConditionalA0Q.Make (predicate, (Quotation) consequent, alternative) :
    //                (alternative is LexicalVariable) ? ConditionalA0SL.Make (predicate, consequent, (LexicalVariable) alternative) :
    //                (alternative is Quotation) ? ConditionalA0SQ.Make (predicate, consequent, (Quotation) alternative) :
    //                new ConditionalA0 (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA0.EvalStep");
    //#endif
    //            object ev = environment.Argument0Value;

    //            if ((ev is bool) && (bool) ev == false) {
    //#if DEBUG
    //                NoteCalls (this.alternative);
    //                alternativeTypeHistogram.Note (this.alternativeType);
    //#endif
    //                expression = this.alternative;
    //                answer = null;
    //                return true;
    //            }
    //            else {
    //#if DEBUG
    //                NoteCalls (this.consequent);
    //                consequentTypeHistogram.Note (this.consequentType);
    //#endif
    //                expression = this.consequent;
    //                answer = null;
    //                return true;
    //            }
    //        }
    //    }


    //    [Serializable]
    //    class ConditionalA0A : ConditionalA0L
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif

    //        protected ConditionalA0A (Argument0 predicate, Argument consequent, SCode alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        public static SCode Make (Argument0 predicate, Argument consequent, SCode alternative)
    //        {
    //            return
    //                (consequent is Argument0) ? ConditionalA0A0.Make (predicate, (Argument0) consequent, alternative) : //RewriteAsDisjunction (predicate, alternative) :
    //                (consequent is Argument1) ? ConditionalA0A1.Make (predicate, (Argument1) consequent, alternative) :
    //                (alternative is LexicalVariable) ? Unimplemented () :
    //                (alternative is Quotation) ? Unimplemented () :
    //                new ConditionalA0A (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //            throw new NotImplementedException ();
    //#if DEBUG
    //            Warm ("ConditionalA0.EvalStep");
    //#endif
    //            object ev = environment.Argument0Value;

    //            if ((ev is bool) && (bool) ev == false) {
    //#if DEBUG
    //                NoteCalls (this.alternative);
    //                alternativeTypeHistogram.Note (this.alternativeType);
    //#endif
    //                expression = this.alternative;
    //                answer = null;
    //                return true;
    //            }
    //            else {
    //#if DEBUG
    //                NoteCalls (this.consequent);
    //                consequentTypeHistogram.Note (this.consequentType);
    //#endif
    //                expression = this.consequent;
    //                answer = null;
    //                return true;
    //            }
    //        }
    //    }

    //    [Serializable]
    //    class ConditionalA0A0 : ConditionalA0A
    //    {
    //#if DEBUG
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif

    //        protected ConditionalA0A0 (Argument0 predicate, Argument0 consequent, SCode alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        public static SCode Make (Argument0 predicate, Argument0 consequent, SCode alternative)
    //        {
    //            return
    //                (alternative is LexicalVariable) ? Unimplemented () :
    //                (alternative is Quotation) ? Unimplemented () :
    //                new ConditionalA0A0 (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA0A0.EvalStep");
    //#endif
    //            object ev = environment.Argument0Value;

    //            if ((ev is bool) && (bool) ev == false) {
    //#if DEBUG
    //                NoteCalls (this.alternative);
    //                alternativeTypeHistogram.Note (this.alternativeType);
    //#endif
    //                expression = this.alternative;
    //                answer = null;
    //                return true;
    //            }
    //            else {
    //                answer = ev;
    //                return false;
    //            }
    //        }
    //    }



    //    [Serializable]
    //    class ConditionalA0A1 : ConditionalA0A
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif

    //        protected ConditionalA0A1 (Argument0 predicate, Argument1 consequent, SCode alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        public static SCode Make (Argument0 predicate, Argument1 consequent, SCode alternative)
    //        {
    //            return
    //                (alternative is LexicalVariable) ? Unimplemented () :
    //                (alternative is Quotation) ? ConditionalA0A1Q.Make (predicate, consequent, (Quotation) alternative) :
    //                new ConditionalA0A1 (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA0A1.EvalStep");
    //#endif
    //            object ev = environment.Argument0Value;

    //            if ((ev is bool) && (bool) ev == false) {
    //#if DEBUG
    //                NoteCalls (this.alternative);
    //                alternativeTypeHistogram.Note (this.alternativeType);
    //#endif
    //                expression = this.alternative;
    //                answer = null;
    //                return true;
    //            }
    //            else {
    //                answer = environment.Argument1Value;
    //                return false;
    //            }
    //        }
    //    }

    //    [Serializable]
    //    class ConditionalA0A1Q : ConditionalA0A1
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif
    //        public readonly object alternativeValue;

    //        protected ConditionalA0A1Q (Argument0 predicate, Argument1 consequent, Quotation alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //            alternativeValue = alternative.Quoted;
    //        }

    //        public static SCode Make (Argument0 predicate, Argument1 consequent, Quotation alternative)
    //        {
    //            return
    //                new ConditionalA0A1Q (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA0A1Q.EvalStep");
    //#endif
    //            object ev = environment.Argument0Value;
    //            answer = ((ev is bool) && (bool) ev == false) ? this.alternativeValue : environment.Argument1Value;
    //            return false;
    //        }
    //    }



    

    //    [Serializable]
    //    class ConditionalA0Q : ConditionalA0
    //    {
    //#if DEBUG
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif
    //        protected readonly object consequentValue;
    //        protected ConditionalA0Q (Argument0 predicate, Quotation consequent, SCode alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //            this.consequentValue = consequent.Quoted;
    //        }

    //        public static SCode Make (Argument0 predicate, Quotation consequent, SCode alternative)
    //        {
    //            return
    //                (alternative is LexicalVariable) ? ConditionalA0QL.Make (predicate, consequent, (LexicalVariable) alternative) :
    //                (alternative is Quotation) ? ConditionalA0QQ.Make (predicate, consequent, (Quotation) alternative) :
    //                new ConditionalA0Q (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA0Q.EvalStep");
    //#endif
    //            object ev = environment.Argument0Value;

    //            if ((ev is bool) && (bool) ev == false) {
    //#if DEBUG
    //                NoteCalls (this.alternative);
    //                alternativeTypeHistogram.Note (this.alternativeType);
    //#endif
    //                expression = this.alternative;
    //                answer = null;
    //                return true;
    //            }
    //            else {
    //                answer = this.consequentValue;
    //                return false;
    //            }
    //        }
    //    }


    //    [Serializable]
    //    class ConditionalA0QA : ConditionalA0QL
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif

    //        protected ConditionalA0QA (Argument0 predicate, Quotation consequent, Argument alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        public static SCode Make (Argument0 predicate, Quotation consequent, Argument alternative)
    //        {
    //            return
    //                (alternative is Argument0) ? Unimplemented() :
    //                (alternative is Argument1) ? Unimplemented () :
    //                new ConditionalA0QA (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //            throw new NotImplementedException ();
    //        }
    //    }

    //    [Serializable]
    //    class ConditionalA0QQ : ConditionalA0Q
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif
    //        protected readonly object alternativeValue;
    //        protected ConditionalA0QQ (Argument0 predicate, Quotation consequent, Quotation alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //            this.alternativeValue = alternative.Quoted;
    //        }

    //        public static SCode Make (Argument0 predicate, Quotation consequent, Quotation alternative)
    //        {
    //            //Debugger.Break ();
    //            //object isEq;
    //            //ObjectModel.Eq (out isEq, consequent.Quoted, alternative.Quoted);
    //            //if ((isEq is bool) && (bool) isEq == false)
    //            //    return new ConditionalA0QQ (predicate, consequent, alternative);
    //            //else
    //            //    // Same value either way, so punt.
    //            //    return consequent;

    //            return new ConditionalA0QQ (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA0QQ.EvalStep");
    //#endif
    //            object ev0 = environment.Argument0Value;
    //            answer = (ev0 is bool && (bool) ev0 == false) ? this.alternativeValue : this.consequentValue;
    //            return false;
    //        }
    //    }



    //    [Serializable]
    //    class ConditionalA0SA : ConditionalA0SL
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif
    //        protected ConditionalA0SA (Argument0 predicate, SCode consequent, Argument alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        static SCode Rewrite (Argument0 predicate, SCode consequent)
    //        {
    //            Debug.Write ("\n Consequent = predicate.");
    //            return Conditional.Make (predicate, consequent, Quotation.Make (false));
    //        }

    //        public static SCode Make (Argument0 predicate, SCode consequent, Argument alternative)
    //        {
    //            return
    //                (alternative is Argument0) ? ConditionalA0SA0.Make (predicate, consequent, (Argument0) alternative) : 
    //                (alternative is Argument1) ? ConditionalA0SA1.Make (predicate, consequent, (Argument1) alternative) :
    //                new ConditionalA0SA (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA0SA.EvalStep");
    //#endif
    //            object ev = environment.Argument0Value;

    //            if ((ev is bool) && (bool) ev == false) {
    //                answer = environment.ArgumentValue (this.alternativeOffset);
    //                return false;
    //            }
    //            else {
    //#if DEBUG
    //                NoteCalls (this.consequent);
    //                consequentTypeHistogram.Note (this.consequentType);
    //#endif
    //                expression = this.consequent;
    //                answer = null;
    //                return true;
    //            }
    //        }
    //    }

    //    [Serializable]
    //    sealed class ConditionalA0SA0 : ConditionalA0SA
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //#endif
    //        ConditionalA0SA0 (Argument0 predicate, SCode consequent, Argument0 alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        public static SCode Make (Argument0 predicate, SCode consequent, Argument0 alternative)
    //        {
    //            return
    //                ConditionalA0SQ.Make (predicate, consequent, Quotation.Make (Constant.sharpF));
    //                 //new ConditionalA0SA0 (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA0SA0.EvalStep");
    //#endif
    //            object ev = environment.Argument0Value;

    //            if ((ev is bool) && (bool) ev == false) {
    //                answer = ev;
    //                return false;
    //            }
    //            else {
    //#if DEBUG
    //                NoteCalls (this.consequent);
    //                consequentTypeHistogram.Note (this.consequentType);
    //#endif
    //                expression = this.consequent;
    //                answer = null;
    //                return true;
    //            }
    //        }
    //    }

    //    [Serializable]
    //    sealed class ConditionalA0SA1 : ConditionalA0SA
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //#endif
    //        ConditionalA0SA1 (Argument0 predicate, SCode consequent, Argument1 alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        public static SCode Make (Argument0 predicate, SCode consequent, Argument1 alternative)
    //        {
    //            return
    //                 new ConditionalA0SA1 (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA0SA1.EvalStep");
    //#endif
    //            object ev = environment.Argument0Value;

    //            if ((ev is bool) && (bool) ev == false) {
    //                answer = environment.Argument1Value;
    //                return false;
    //            }
    //            else {
    //#if DEBUG
    //                NoteCalls (this.consequent);
    //                consequentTypeHistogram.Note (this.consequentType);
    //#endif
    //                expression = this.consequent;
    //                answer = null;
    //                return true;
    //            }
    //        }
    //    }

 
    //    [Serializable]
    //    sealed class ConditionalA0SQ : ConditionalA0
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //#endif
    //        public readonly object alternativeValue;

    //        ConditionalA0SQ (Argument0 predicate, SCode consequent, Quotation alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //            this.alternativeValue = alternative.Quoted;
    //        }

    //        public static SCode Make (Argument0 predicate, SCode consequent, Quotation alternative)
    //        {
    //            return
    //                new ConditionalA0SQ (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA0SQ.EvalStep");
    //#endif
    //            object ev = environment.Argument0Value;

    //            if ((ev is bool) && (bool) ev == false) {
    //                answer = this.alternativeValue;
    //                return false;
    //            }
    //            else {
    //#if DEBUG
    //                NoteCalls (this.consequent);
    //                consequentTypeHistogram.Note (this.consequentType);
    //#endif
    //                expression = this.consequent;
    //                answer = null;
    //                return true;
    //            }
    //        }
    //    }

    //    [Serializable]
    //    class ConditionalA1 : ConditionalA
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif
    //        protected ConditionalA1 (Argument1 predicate, SCode consequent, SCode alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        public static SCode Make (Argument1 predicate, SCode consequent, SCode alternative)
    //        {
    //            return
    //                (consequent is LexicalVariable) ? ConditionalA1L.Make (predicate, (LexicalVariable) consequent, alternative) :
    //                (consequent is Quotation) ? ConditionalA1Q.Make (predicate, (Quotation) consequent, alternative) :
    //                (alternative is LexicalVariable) ? ConditionalA1SL.Make (predicate, consequent, (LexicalVariable) alternative) :
    //                (alternative is Quotation) ? ConditionalA1SQ.Make (predicate, consequent, (Quotation) alternative) :
    //                new ConditionalA1 (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA1.EvalStep");
    //#endif
    //            object ev = environment.Argument1Value;

    //            if ((ev is bool) && (bool) ev == false) {
    //#if DEBUG
    //                NoteCalls (this.alternative);
    //                alternativeTypeHistogram.Note (this.alternativeType);
    //#endif
    //                expression = this.alternative;
    //                answer = null;
    //                return true;
    //            }
    //            else {
    //#if DEBUG
    //                NoteCalls (this.consequent);
    //                consequentTypeHistogram.Note (this.consequentType);
    //#endif
    //                expression = this.consequent;
    //                answer = null;
    //                return true;
    //            }
    //        }
    //    }


    //    [Serializable]
    //    class ConditionalA1A : ConditionalA1L
    //    {
    //#if DEBUG
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif
    //        protected ConditionalA1A (Argument1 predicate, Argument consequent, SCode alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        public static SCode Make (Argument1 predicate, Argument consequent, SCode alternative)
    //        {
    //            return
    //                (consequent is Argument0) ? Unimplemented() :
    //                (consequent is Argument1) ? ConditionalA1A1.Make (predicate, (Argument1) consequent, alternative) :
    //                (alternative is LexicalVariable) ? Unimplemented () :
    //                (alternative is Quotation) ? Unimplemented () :
    //                new ConditionalA1A (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //            throw new NotImplementedException ();
    //        }
    //    }

    //    [Serializable]
    //    class ConditionalA1A1 : ConditionalA1A
    //    {
    //#if DEBUG
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif
    //        protected ConditionalA1A1 (Argument1 predicate, Argument1 consequent, SCode alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        public static SCode Make (Argument1 predicate, Argument1 consequent, SCode alternative)
    //        {
    //            return
    //                (alternative is LexicalVariable) ? Unimplemented () :
    //                (alternative is Quotation) ? Unimplemented () :
    //                new ConditionalA1A1 (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA1A1.EvalStep");
    //#endif
    //            object ev0 = environment.Argument1Value;
    //            if (ev0 is bool && (bool) ev0 == false) {
    //#if DEBUG
    //                NoteCalls (this.alternative);
    //#endif
    //                expression = this.alternative;
    //                answer = null;
    //                return true;
    //            }
    //            else {
    //                answer = ev0;
    //                return false;
    //            }
    //        }
    //    }



    //    [Serializable]
    //    class ConditionalA1Q : ConditionalA1
    //    {
    //#if DEBUG
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif
    //        protected readonly object consequentValue;

    //        protected ConditionalA1Q (Argument1 predicate, Quotation consequent, SCode alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //            this.consequentValue = consequent.Quoted;
    //        }

    //        public static SCode Make (Argument1 predicate, Quotation consequent, SCode alternative)
    //        {
    //            return
    //                (alternative is LexicalVariable) ? Unimplemented() :
    //                (alternative is Quotation) ? ConditionalA1QQ.Make (predicate, consequent, (Quotation) alternative) :
    //                new ConditionalA1Q (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA1Q.EvalStep");
    //#endif
    //            object ev = environment.Argument1Value;

    //            if ((ev is bool) && (bool) ev == false) {
    //#if DEBUG
    //                NoteCalls (this.alternative);
    //                alternativeTypeHistogram.Note (this.alternativeType);
    //#endif
    //                expression = this.alternative;
    //                answer = null;
    //                return true;
    //            }
    //            else {
    //                answer = this.consequentValue;
    //                return false;
    //            }
    //        }
    //    }

    //    [Serializable]
    //    sealed class ConditionalA1QQ : ConditionalA1Q
    //    {
    //        public readonly object alternativeValue;

    //        ConditionalA1QQ (Argument1 predicate, Quotation consequent, Quotation alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //            this.alternativeValue = alternative.Quoted;
    //        }

    //        public static SCode Make (Argument1 predicate, Quotation consequent, Quotation alternative)
    //        {
    //            //Debugger.Break ();
    //            return
    //                 new ConditionalA1QQ (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA1QQ.EvalStep");
    //#endif
    //            object ev = environment.Argument1Value;
    //            answer = ((ev is bool) && (bool) ev == false) ? this.alternativeValue : this.consequentValue;
    //            return false;
    //        }
    //    }


    //    [Serializable]
    //    class ConditionalA1SA : ConditionalA1SL
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif

    //        protected ConditionalA1SA (Argument1 predicate, SCode consequent, Argument alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        static SCode Rewrite (Argument1 predicate, SCode consequent)
    //        {
    //            Debug.Write ("\n; Alternative = predicate, rewrite as false.");
    //            return Conditional.Make (predicate, consequent, Quotation.Make (false));
    //        }

    //        public static SCode Make (Argument1 predicate, SCode consequent, Argument alternative)
    //        {
    //            return
    //                (alternative is Argument0) ? ConditionalA1SA0.Make (predicate, consequent, (Argument0) alternative) :
    //                (alternative is Argument1) ? Conditional.Make (predicate, consequent, Quotation.Make (Constant.sharpF)) :
    //                new ConditionalA1SA (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA1SA.EvalStep");
    //#endif
    //            object ev = environment.Argument1Value;

    //            if ((ev is bool) && (bool) ev == false) {
    //                answer = environment.ArgumentValue (this.alternativeOffset);
    //                return false ;
    //            }
    //            else {
    //#if DEBUG
    //                NoteCalls (this.consequent);
    //                consequentTypeHistogram.Note (this.consequentType);
    //#endif
    //                expression = this.consequent;
    //                answer = null;
    //                return true;
    //            }
    //        }
    //    }

    //    [Serializable]
    //    sealed class ConditionalA1SA0 : ConditionalA1SA
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif
    //        ConditionalA1SA0 (Argument1 predicate, SCode consequent, Argument0 alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        public static SCode Make (Argument1 predicate, SCode consequent, Argument0 alternative)
    //        {
    //            return new ConditionalA1SA0 (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA1SA0.EvalStep");
    //#endif
    //            object ev = environment.Argument1Value;

    //            if ((ev is bool) && (bool) ev == false) {
    //                answer = environment.Argument0Value;
    //                return false;
    //            }
    //            else {
    //#if DEBUG
    //                NoteCalls (this.consequent);
    //                consequentTypeHistogram.Note (this.consequentType);
    //#endif
    //                expression = this.consequent;
    //                answer = null;
    //                return true;
    //            }
    //        }
    //    }

 

    //    [Serializable]
    //    sealed class ConditionalA1SQ : ConditionalA1
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif
    //        public readonly object alternativeValue;

    //        ConditionalA1SQ (Argument1 predicate, SCode consequent, Quotation alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //            this.alternativeValue = alternative.Quoted;
    //        }

    //        public static SCode Make (Argument1 predicate, SCode consequent, Quotation alternative)
    //        {
    //            return
    //              new ConditionalA1SQ (predicate, consequent, (Quotation) alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalA1SQ.EvalStep");
    //#endif
    //            object ev = environment.Argument1Value;

    //            if ((ev is bool) && (bool) ev == false) {
    //                answer = this.alternativeValue;
    //                return false;
    //            }
    //            else {
    //#if DEBUG
    //                NoteCalls (this.consequent);
    //                consequentTypeHistogram.Note (this.consequentType);
    //#endif
    //                expression = this.consequent;
    //                answer = null;
    //                return true;
    //            }
    //        }
    //    }



    //    [Serializable]
    //    class ConditionalAA : ConditionalAL
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif
    //        protected ConditionalAA (Argument predicate, Argument consequent, SCode alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        public static SCode Make (Argument predicate, Argument consequent, SCode alternative)
    //        {
    //            return
    //                (consequent is Argument0) ? Unimplemented() :
    //                (consequent is Argument1) ? Unimplemented () :
    //                (predicate.Offset == consequent.Offset) ? Unimplemented():
    //                (alternative is LexicalVariable) ? Unimplemented () :
    //                (alternative is Quotation) ? ConditionalAAQ.Make (predicate, consequent, (Quotation) alternative) :
    //                new ConditionalAA (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //            throw new NotImplementedException ();
    //        }
    //    }

    //    [Serializable]
    //    sealed class ConditionalAAQ : ConditionalAA
    //    {
    //        readonly object alternativeValue;

    //        ConditionalAAQ (Argument predicate, Argument consequent, Quotation alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //            this.alternativeValue = alternative.Quoted;
    //        }

    //        public static SCode Make (Argument predicate, Argument consequent, Quotation alternative)
    //        {
    //            return
    //                new ConditionalAAQ (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalAAQ.EvalStep");
    //#endif
    //            object ev = environment.ArgumentValue (this.predicateOffset);
    //            answer = ((ev is bool) && (bool) ev == false) ?
    //                this.alternativeValue :
    //                environment.ArgumentValue (this.consequentOffset);
    //            return false;
    //        }
    //    }

    //    [Serializable]
    //    class ConditionalAQ : ConditionalA
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif
    //        public readonly object consequentValue;

    //        protected ConditionalAQ (Argument predicate, Quotation consequent, SCode alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //            this.consequentValue = consequent.Quoted;
    //        }

    //        public static SCode Make (Argument predicate, Quotation consequent, SCode alternative)
    //        {
    //            return
    //                (alternative is LexicalVariable) ? Unimplemented () :
    //                (alternative is Quotation) ? ConditionalAQQ.Make (predicate, consequent, (Quotation) alternative) :
    //                new ConditionalAQ (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalAQ.EvalStep");
    //#endif
    //            object ev = environment.ArgumentValue (this.predicateOffset);

    //            if ((ev is bool) && (bool) ev == false) {
    //#if DEBUG
    //                NoteCalls (this.alternative);
    //                alternativeTypeHistogram.Note (this.alternativeType);
    //#endif
    //                expression = this.alternative;
    //                answer = null;
    //                return true;
    //            }
    //            else {
    //                answer = this.consequentValue;
    //                return false;
    //            }
    //        }
    //    }

    //    [Serializable]
    //    sealed class ConditionalAQQ : ConditionalAQ
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif
    //        readonly object alternativeValue;

    //        ConditionalAQQ (Argument predicate, Quotation consequent, Quotation alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //            this.alternativeValue = alternative.Quoted;
    //        }

    //        public static SCode Make (Argument predicate, Quotation consequent, Quotation alternative)
    //        {
    //            //Debugger.Break ();
    //            return
    //                  new ConditionalAQQ (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalAQQ.EvalStep");
    //#endif
    //            object ev = environment.ArgumentValue (this.predicateOffset);
    //            answer = ((ev is bool) && (bool) ev == false) ? this.alternativeValue : this.consequentValue;
    //            return false;
    //        }
    //    }



    //    [Serializable]
    //    sealed class ConditionalASQ : ConditionalA
    //    {
    //#if DEBUG
    //        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif
    //        readonly object alternativeValue;

    //        ConditionalASQ (Argument predicate, SCode consequent, Quotation alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //            this.alternativeValue = alternative.Quoted;
    //        }

    //        public static SCode Make (Argument predicate, SCode consequent, Quotation alternative)
    //        {
    //            return
    //                 new ConditionalASQ (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalASQ.EvalStep");
    //#endif
    //            object ev = environment.ArgumentValue (this.predicateOffset);

    //            if ((ev is bool) && (bool) ev == false) {
    //                answer = this.alternativeValue;
    //                return false;
    //            }
    //            else {
    //#if DEBUG
    //                NoteCalls (this.consequent);
    //                consequentTypeHistogram.Note (this.consequentType);
    //#endif
    //                expression = this.consequent;
    //                answer = null;
    //                return true;
    //            }
    //        }
    //    }



    //    [Serializable]
    //    sealed class ConditionalSA0A1 : ConditionalSA0A
    //    {
    //#if DEBUG
    //        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
    //#endif

    //        ConditionalSA0A1 (SCode predicate, Argument0 consequent, Argument1 alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        public static SCode Make (SCode predicate, Argument0 consequent, Argument1 alternative)
    //        {
    //            return
    //                new ConditionalSA0A1 (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalSA0A1.EvalStep");
    //            NoteCalls (this.predicate);
    //            predicateTypeHistogram.Note (this.predicateType);
    //#endif
    //            object ev;
    //            Control unev = this.predicate;
    //            Environment env = environment;

    //            while (unev.EvalStep (out ev, ref unev, ref env)) { };
    //            if (ev == Interpreter.UnwindStack) {
    //                throw new NotImplementedException ();
    //                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
    //                //environment = env;
    //                //answer = Interpreter.Unwind;
    //                //return false;

    //            }

    //            answer = ((ev is bool) && (bool) ev == false) ? environment.Argument1Value : environment.Argument0Value;
    //            return false;
    //        }
    //    }



    //    [Serializable]
    //    sealed class ConditionalSA0Q : ConditionalSA0
    //    {
    //#if DEBUG
    //        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif
    //        readonly object alternativeValue;

    //        ConditionalSA0Q (SCode predicate, Argument0 consequent, Quotation alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //            this.alternativeValue = alternative.Quoted;
    //        }

    //        public static SCode Make (SCode predicate, Argument0 consequent, Quotation alternative)
    //        {
    //            return
    //                new ConditionalSA0Q (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalSA0Q.EvalStep");
    //            NoteCalls (this.predicate);
    //            predicateTypeHistogram.Note (this.predicateType);
    //#endif
    //            object ev;
    //            Control unev = this.predicate;
    //            Environment env = environment;

    //            while (unev.EvalStep (out ev, ref unev, ref env)) { };
    //            if (ev == Interpreter.UnwindStack) {
    //                throw new NotImplementedException ();
    //                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
    //                //environment = env;
    //                //answer = Interpreter.Unwind;
    //                //return false;

    //            }

    //            if ((ev is bool) && (bool) ev == false) {
    //                answer = this.alternativeValue;
    //                return false;
    //            }
    //            else {
    //                answer = environment.Argument0Value;
    //                return false;
    //            }
    //        }
    //    }

    //    [Serializable]
    //    class ConditionalSA1 : ConditionalSA
    //    {
    //#if DEBUG
    //        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif

    //        protected ConditionalSA1 (SCode predicate, Argument1 consequent, SCode alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        public static SCode Make (SCode predicate, Argument1 consequent, SCode alternative)
    //        {
    //            return
    //                (alternative is LexicalVariable) ? ConditionalSA1L.Make (predicate, consequent, (LexicalVariable) alternative) :
    //                (alternative is Quotation) ? ConditionalSA1Q.Make (predicate, consequent, (Quotation) alternative) :
    //                new ConditionalSA1 (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalSA1.EvalStep");
    //            NoteCalls (this.predicate);
    //            predicateTypeHistogram.Note (this.predicateType);
    //#endif
    //            object ev;
    //            Control unev = this.predicate;
    //            Environment env = environment;

    //            while (unev.EvalStep (out ev, ref unev, ref env)) { };
    //            if (ev == Interpreter.UnwindStack) {
    //                throw new NotImplementedException ();
    //                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
    //                //environment = env;
    //                //answer = Interpreter.Unwind;
    //                //return false;

    //            }

    //            if ((ev is bool) && (bool) ev == false) {
    //#if DEBUG
    //                NoteCalls (this.alternative);
    //                alternativeTypeHistogram.Note (this.alternativeType);
    //#endif
    //                expression = this.alternative;
    //                answer = null;
    //                return true;
    //            }
    //            else {
    //                answer = environment.Argument1Value;
    //                return false;
    //            }
    //        }
    //    }

 
    //    [Serializable]
    //    class ConditionalSA1A : ConditionalSA1L
    //    {
    //#if DEBUG
    //        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
    //#endif

    //        protected ConditionalSA1A (SCode predicate, Argument1 consequent, Argument alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        public static SCode Make (SCode predicate, Argument1 consequent, Argument alternative)
    //        {
    //            return
    //                (alternative is Argument0) ? ConditionalSA1A0.Make (predicate, consequent, (Argument0) alternative) :
    //                (alternative is Argument1) ? Unimplemented () :
    //                new ConditionalSA1A (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalSA1A.EvalStep");
    //            NoteCalls (this.predicate);
    //            predicateTypeHistogram.Note (this.predicateType);
    //#endif
    //            object ev;
    //            Control unev = this.predicate;
    //            Environment env = environment;

    //            while (unev.EvalStep (out ev, ref unev, ref env)) { };
    //            if (ev == Interpreter.UnwindStack) {
    //                throw new NotImplementedException ();
    //                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
    //                //environment = env;
    //                //answer = Interpreter.Unwind;
    //                //return false;

    //            }
    //            answer = ((ev is bool) && (bool) ev == false) ? environment.ArgumentValue (this.alternativeOffset) :
    //                environment.Argument1Value;
    //            return false;
    //        }
    //    }

    //    [Serializable]
    //    sealed class ConditionalSA1A0 : ConditionalSA1A
    //    {
    //#if DEBUG
    //        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
    //        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
    //#endif

    //        ConditionalSA1A0 (SCode predicate, Argument1 consequent, Argument0 alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //        }

    //        public static SCode Make (SCode predicate, Argument1 consequent, Argument0 alternative)
    //        {
    //            return
    //                new ConditionalSA1A0 (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("ConditionalSA1A0.EvalStep");
    //            NoteCalls (this.predicate);
    //            predicateTypeHistogram.Note (this.predicateType);
    //#endif
    //            object ev;
    //            Control unev = this.predicate;
    //            Environment env = environment;

    //            while (unev.EvalStep (out ev, ref unev, ref env)) { };
    //            if (ev == Interpreter.UnwindStack) {
    //                throw new NotImplementedException ();
    //                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
    //                //environment = env;
    //                //answer = Interpreter.Unwind;
    //                //return false;

    //            }

    //            answer = ((ev is bool) && (bool) ev == false) ? environment.Argument0Value : environment.Argument1Value;
    //            return false;
    //        }
    //    }

    //    [Serializable]
    //    sealed class ConditionalSA1Q : ConditionalSA1
    //    {
    //#if DEBUG
    //        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
    //#endif
    //        public readonly object alternativeValue;

    //        ConditionalSA1Q (SCode predicate, Argument1 consequent, Quotation alternative)
    //            : base (predicate, consequent, alternative)
    //        {
    //            this.alternativeValue = alternative.Quoted;
    //        }

    //        public static SCode Make (SCode predicate, Argument1 consequent, Quotation alternative)
    //        {
    //            return
    //                new ConditionalSA1Q (predicate, consequent, alternative);
    //        }

    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("-");
    //            NoteCalls (this.predicate);
    //            predicateTypeHistogram.Note (this.predicateType);
    //            SCode.location = "ConditionalSA1Q.EvalStep";
    //#endif
    //            object ev;
    //            Control unev = this.predicate;
    //            Environment env = environment;

    //            while (unev.EvalStep (out ev, ref unev, ref env)) { };
    //            if (ev == Interpreter.UnwindStack) {
    //                throw new NotImplementedException ();
    //                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
    //                //environment = env;
    //                //answer = Interpreter.Unwind;
    //                //return false;

    //            }

    //            answer = ((ev is bool) && (bool) ev == false) ? this.alternativeValue : environment.Argument1Value;
    //            return false;
    //        }
    //    }


}

