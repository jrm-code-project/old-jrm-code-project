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

        static SCode FoldConditional (Quotation predicate, SCode consequent, SCode alternative)
        {
            Debug.Write ("\n; Folding conditional");
            object val = predicate.Quoted;
            return (val is bool && (bool) val == false) ? alternative : consequent;
        }

        static SCode RewriteConsequentFalse (SCode predicate, bool alternativeValue)
        {
            if (alternativeValue) {
                Debug.Write ("\n Rewrite (if ... #f #t) => (not ...)");
                return PrimitiveCombination1.Make (Primitive.Not, predicate);
            }
            else {
                Debug.Write ("\n Rewrite (if ... #f #f) => (begin ... #f)");
                return Sequence2.Make (predicate, Quotation.Make (false));
            }
        }

        static SCode RewriteNestedConditional (Conditional predicate, SCode consequent, SCode alternative)
        {
            Debug.Write ("Rewrite nested conditional");
            return Conditional.Make (predicate.predicate, consequent, alternative);
        }

        static SCode StandardMake (SCode predicate, SCode consequent, SCode alternative)
        {
            return
                (! Configuration.EnableConditionalOptimization) ? new Conditional(predicate, consequent, alternative) :
                (! Configuration.EnableConditionalSpecialization) ? new Conditional (predicate, consequent, alternative) :

                // Rewrite (if x x ...) => (or x ...)
                (predicate is Variable &&
                 consequent is Variable &&
                 ((Variable) predicate).Name == ((Variable) consequent).Name &&
                 Configuration.EnableCodeRewriting &&
                 Configuration.EnableDisjunctionConversion) ? RewriteAsDisjunction (predicate, alternative) :


                  (predicate is Conditional &&
                  ((Conditional) predicate).Consequent is Quotation &&
                  ((Quotation) ((Conditional) predicate).Consequent).Quoted is bool &&
                  ((bool) ((Quotation) ((Conditional) predicate).Consequent).Quoted == true) &&
                  ((Conditional) predicate).Alternative is Quotation &&
                  ((Quotation) ((Conditional) predicate).Alternative).Quoted is bool &&
                  ((bool) ((Quotation) ((Conditional) predicate).Alternative).Quoted == false) &&
                  Configuration.EnableCodeRewriting) ? RewriteNestedConditional ((Conditional) predicate, consequent, alternative) :

                  (predicate is Conditional &&
                  ((Conditional) predicate).Consequent is Quotation &&
                  ((Quotation) ((Conditional) predicate).Consequent).Quoted is bool &&
                  ((bool) ((Quotation) ((Conditional) predicate).Consequent).Quoted == false) &&
                  ((Conditional) predicate).Alternative is Quotation &&
                  ((Quotation) ((Conditional) predicate).Alternative).Quoted is bool &&
                  ((bool) ((Quotation) ((Conditional) predicate).Alternative).Quoted == true) &&
                  Configuration.EnableCodeRewriting) ? RewriteNestedConditional ((Conditional) predicate, alternative, consequent) :

                 // Rewrite (if ... #f #t) => (not ...)
                 // Rewrite (if ... #f #f) => (begin ... #f)
                 (consequent is Quotation &&
                  (((Quotation)consequent).Quoted is bool) &&
                  ((bool) ((Quotation)consequent).Quoted == false) &&
                  alternative is Quotation &&
                  (((Quotation) alternative).Quoted is bool) &&
                  Configuration.EnableCodeRewriting) ? RewriteConsequentFalse (predicate, (bool)(((Quotation) alternative).Quoted)) :


                (Configuration.EnablePrimitiveConditional1 &&
                 predicate is PrimitiveCombination1) ? PCond1.Make ((PrimitiveCombination1) predicate, consequent, alternative) :
                (Configuration.EnablePrimitiveConditional2 && 
                 predicate is PrimitiveCombination2) ? PCond2.Make ((PrimitiveCombination2) predicate, consequent, alternative) :
                (predicate is Argument) ? ConditionalA.Make ((Argument) predicate, consequent, alternative) :
                (predicate is Quotation) ? (Configuration.EnableCodeRewriting ?
                                            FoldConditional ((Quotation) predicate, consequent, alternative) 
                                            : ConditionalQ.Make ((Quotation) predicate, consequent, alternative)) :
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
            Debug.Write ("\n; Rewrite conditonal as disjunction.");
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
            SCode.location = "Conditional";
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Conditional";
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
                SCode.location = "Conditional";
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
                SCode.location = "Conditional";
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
                (consequent is Argument) ? ConditionalAA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? ConditionalAQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? ConditionalAS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? ConditionalAXA.Make (predicate, consequent, (Argument) alternative):
                (alternative is Quotation) ? ConditionalAXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? ConditionalAXS.Make (predicate, consequent, (StaticVariable) alternative) :
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
                (consequent is Argument) ? ConditionalA0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? ConditionalA0Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is Argument) ? ConditionalA0XA.Make (predicate, consequent, (Argument) alternative) :
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
    class ConditionalA0A : ConditionalA0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected ConditionalA0A (Argument0 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (Argument0 predicate, Argument consequent, SCode alternative)
        {
#if DEBUG
            if (consequent is Argument0) Debugger.Break ();
#endif
            return
                new ConditionalA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA0A");
#endif
            object ev = environment.Argument0Value;

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalA0A";
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
            object temp;
            if (ObjectModel.Eq (out temp, consequent.Quoted, alternative.Quoted))
                throw new NotImplementedException ();
            if ((bool) temp == true) Debugger.Break ();
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
    class ConditionalA0XA : ConditionalA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected ConditionalA0XA (Argument0 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (Argument0 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? ConditionalA0XA0.Make (predicate, consequent, (Argument0) alternative) :
                new ConditionalA0XA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA0XA");
#endif
            object ev = environment.Argument0Value;

            if ((ev is bool) && (bool) ev == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "ConditionalA0XA";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }

    }

    [Serializable]
    class ConditionalA0XA0 : ConditionalA0XA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected ConditionalA0XA0 (Argument0 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument0 predicate, SCode consequent, Argument0 alternative)
        {
            throw new NotImplementedException ();
            return
                new ConditionalA0XA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalA0XA0");
#endif
            object ev = environment.Argument0Value;

            if ((ev is bool) && (bool) ev == false) {
                answer = environment.Argument0Value ;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "ConditionalA0XA";
#endif
                expression = this.consequent;
                answer = null;
                return true;
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
    class ConditionalAA : ConditionalA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected ConditionalAA (Argument predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (Argument predicate, Argument consequent, SCode alternative)
        {
#if DEBUG
            if (predicate.Offset == consequent.Offset) Debugger.Break ();
#endif
            return
                (consequent is Argument0) ? ConditionalAA0.Make (predicate, (Argument0) consequent, alternative):
                (alternative is Quotation) ? ConditionalAAQ.Make (predicate, consequent, (Quotation) alternative) :
                new ConditionalAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalAA");
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalAA";
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
    class ConditionalAA0 : ConditionalAA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected ConditionalAA0 (Argument predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument predicate, Argument0 consequent, SCode alternative)
        {
            return
                new ConditionalAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalAA0");
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalAA0";
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
    class ConditionalAAQ : ConditionalAA
    {
        public readonly object alternativeValue;
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
#if DEBUG
            Warm ("ConditionalAAQ");
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

            if ((ev is bool) && (bool) ev == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
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
                (alternative is StaticVariable) ? new ConditionalAQS (predicate, consequent, (StaticVariable) alternative) :
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
            object temp;
            if (ObjectModel.Eq (out temp, consequent.Quoted, alternative.Quoted))
                throw new NotImplementedException ();
            if ((bool) temp == true) Debugger.Break ();
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
    sealed class ConditionalAQS : ConditionalAQ
    {
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        internal ConditionalAQS (Argument predicate, Quotation consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalAQS");
#endif
            object ev = environment.ArgumentValue(this.predicateOffset);

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
    class ConditionalAS : ConditionalA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;
        protected ConditionalAS (Argument predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (Argument predicate, StaticVariable consequent, SCode alternative)
        {
            return
                //(alternative is Quotation) ? ConditionalAAQ.Make (predicate, consequent, (Quotation) alternative) :
                new ConditionalAS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalAS");
#endif
            object ev = environment.ArgumentValue (this.predicateOffset);

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalAS";
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
    class ConditionalAXA : ConditionalA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected ConditionalAXA (Argument predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (Argument predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? ConditionalAXA0.Make (predicate, consequent, (Argument0) alternative) :
                new ConditionalAXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalAXA");
#endif
            object ev = environment.ArgumentValue(this.predicateOffset);

            if ((ev is bool) && (bool) ev == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;

            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "ConditionalAXA";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }

    }

    [Serializable]
    class ConditionalAXA0 : ConditionalAXA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalAXA0 (Argument predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (Argument predicate, SCode consequent, Argument alternative)
        {
            return
                new ConditionalAXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalAXA0");
#endif
            object ev = environment.ArgumentValue(this.predicateOffset);

            if ((ev is bool) && (bool) ev == false) {
                answer = environment.Argument0Value;
                return false;

            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "ConditionalAXA0";
#endif
                expression = this.consequent;
                answer = null;
                return true;
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
    class ConditionalAXS : ConditionalA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected ConditionalAXS (Argument predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (Argument predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new ConditionalAXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalAXS");
#endif
            object ev = environment.ArgumentValue(this.predicateOffset);

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

                SCode.location = "ConditionalAXS";
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
#if DEBUG
            Debugger.Break();
            Debug.WriteLine ("Folding constant conditional.");
#endif
            object pred = predicate.Quoted;
            return (pred is bool && (bool) pred == false)
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
                (consequent is Argument) ? ConditionalSA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? ConditionalSQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? Unimplemented () :
                (alternative is Argument) ? ConditionalSXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? ConditionalSXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? Unimplemented() :
                new ConditionalS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "ConditionalS";
#endif
            object ev;
            if (environment.StaticValue (out ev, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalS";
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
                SCode.location = "ConditionalS";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class ConditionalSA : ConditionalS
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected ConditionalSA (StaticVariable predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (StaticVariable predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? ConditionalSA0.Make (predicate, (Argument0) consequent, alternative) :
                new ConditionalSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "ConditionalSA";
#endif
            object ev;
            if (environment.StaticValue (out ev, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalSA";
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
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalSA0 (StaticVariable predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (StaticVariable predicate, Argument0 consequent, SCode alternative)
        {
            return
                new ConditionalSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "ConditionalSA";
#endif
            object ev;
            if (environment.StaticValue (out ev, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalSA0";
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
            SCode.location = "ConditionalSQ";
#endif
            object ev;
            if (environment.StaticValue (out ev, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "ConditionalSQ";
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
    class ConditionalSXA : ConditionalS
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;

        protected ConditionalSXA (StaticVariable predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (StaticVariable predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? ConditionalSXA0.Make (predicate, consequent, (Argument0) alternative) :
                new ConditionalSXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "ConditionalSXA";
#endif
            object ev;
            if (environment.StaticValue (out ev, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if ((ev is bool) && (bool) ev == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;

            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "ConditionalSXA";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class ConditionalSXA0 : ConditionalSXA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected ConditionalSXA0 (StaticVariable predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (StaticVariable predicate, SCode consequent, Argument0 alternative)
        {
            return
                new ConditionalSXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "ConditionalSXA0";
#endif
            object ev;
            if (environment.StaticValue (out ev, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if ((ev is bool) && (bool) ev == false) {
                answer = environment.Argument0Value;
                return false;

            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "ConditionalSXA";
#endif
                expression = this.consequent;
                answer = null;
                return true;
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
            SCode.location = "ConditionalSXQ";
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
                SCode.location = "ConditionalSXQ";
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
            SCode.location = "ConditionalSA";
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
            Warm ("ConditionalXA0");
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
                new ConditionalXA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXA0A");
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
            Debugger.Break ();
            return
                new ConditionalXA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXA0A0");
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
            SCode.location = "ConditionalXA0Q";
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "ConditionalXA0Q";
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
            if (consequent.Offset == alternative.Offset) Debugger.Break ();
            return
                (alternative is Argument0) ? ConditionalXAA0.Make (predicate, consequent, (Argument0) alternative) :
                new ConditionalXAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXAA");
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
            Warm ("ConditionalXAA0");
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
            SCode.location = "ConditionalXAQ";
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
//            Warm ("ConditionalXA0A");
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
                new ConditionalXQA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXQA");
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
            Warm ("ConditionalXQA0");
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
            Warm ("ConditionalXQS");
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
            object temp;
            if (ObjectModel.Eq (out temp, consequent.Quoted, alternative.Quoted))
                throw new NotImplementedException ();
            if ((bool) temp == true) Debugger.Break ();
            return new ConditionalXQQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "ConditionalXQQ";
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "ConditionalXQQ";
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
                (alternative is StaticVariable) ? ConditionalXSS.Make (predicate, consequent, (StaticVariable) alternative) :
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
    class ConditionalXSS : ConditionalXS
    {
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif

        protected ConditionalXSS (SCode predicate, StaticVariable consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (SCode predicate, StaticVariable consequent, StaticVariable alternative)
        {
            return
                //(Configuration.EnableCodeRewriting) ? Sequence2XS.Make (predicate, consequent) :
                new ConditionalXSS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "ConditionalXSS";
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "ConditionalXSS";
#endif
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
                new ConditionalXXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("ConditionalXXA");
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
            Warm ("ConditionalXXA0");
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
            SCode.location = "ConditionalXXQ";
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "ConditionalXXQ";
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
                SCode.location = "ConditionalXXQ";
#endif
                answer = this.alternativeQuoted;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "ConditionalXXQ";
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
            Warm ("ConditionalXXS");
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

}

