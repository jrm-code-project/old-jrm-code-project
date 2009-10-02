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
                //(Configuration.EnablePrimitiveConditional1 &&
                // predicate is PrimitiveCombination1) ? PCond1.Make ((PrimitiveCombination1) predicate, consequent, alternative) :
                //(Configuration.EnablePrimitiveConditional2 && 
                // predicate is PrimitiveCombination2) ? PCond2.Make ((PrimitiveCombination2) predicate, consequent, alternative):
                //(Configuration.EnableConditionalSpecialization &&
                //predicate is LexicalVariable) ? ConditionalL.Make ((LexicalVariable) predicate, consequent, alternative) :
                //(Configuration.EnableConditionalSpecialization &&
                //consequent is LexicalVariable) ? ConditionalSL.Make (predicate, (LexicalVariable) consequent, alternative) :
                //(Configuration.EnableConditionalSpecialization &&
                //consequent is Quotation) ? ConditionalSQ.Make (predicate, (Quotation) consequent, alternative) :
                //(Configuration.EnableConditionalSpecialization &&
                //alternative is LexicalVariable) ? ConditionalSSL.Make (predicate, consequent, (LexicalVariable) alternative) :
                //(Configuration.EnableConditionalSpecialization &&
                // alternative is Quotation) ? ConditionalSSQ.Make (predicate, consequent, (Quotation) alternative) :
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
            return
               new Conditional (predicate, consequent, alternative);
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

        public override ICollection<Symbol> ComputeFreeVariables ()
        {
            return new List<Symbol> (this.predicate.ComputeFreeVariables ().Union (new List<Symbol> (this.consequent.ComputeFreeVariables ().Union (this.alternative.ComputeFreeVariables ()))));
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

        internal override PartialResult PartialEval (Environment environment)
        {
            PartialResult p = this.predicate.PartialEval (environment);
            PartialResult c = this.consequent.PartialEval (environment);
            PartialResult a = this.alternative.PartialEval (environment);
            return new PartialResult (p.Residual == this.predicate &&
                c.Residual == this.consequent &&
                a.Residual == this.alternative ? this : Conditional.Make (p.Residual, c.Residual, a.Residual));
        }

        public override int LambdaCount ()
        {
            return 
                this.predicate.LambdaCount () +
                this.consequent.LambdaCount () +
                this.alternative.LambdaCount ();
        }
    }

    [Serializable]
    internal sealed class ConditionalDeserializer : IObjectReference
    {
        // This object has no fields (although it could).
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

//    [Serializable]
//    class ConditionalL : Conditional
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object predicateName;
//        public readonly int predicateDepth;
//        public readonly int predicateOffset;

//        protected ConditionalL (LexicalVariable predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.predicateName = predicate.Name;
//            this.predicateDepth = predicate.Depth;
//            this.predicateOffset = predicate.Offset;
//        }

//        public static SCode Make (LexicalVariable predicate, SCode consequent, SCode alternative)
//        {
//            return 
//               (predicate is Argument) ? ConditionalA.Make ((Argument) predicate, consequent, alternative) :
//               (predicate is LexicalVariable1) ? ConditionalL1.Make ((LexicalVariable1) predicate, consequent, alternative) :
//               (consequent is LexicalVariable) ? Unimplemented() :
//               (consequent is Quotation) ? ConditionalLQ.Make (predicate, (Quotation) consequent, alternative) :
//               (alternative is LexicalVariable) ? ConditionalLSL.Make (predicate, consequent, (LexicalVariable) alternative) :
//               (alternative is Quotation) ? ConditionalLSQ.Make (predicate, consequent, (Quotation) alternative) :
//               new ConditionalL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalL.EvalStep");
//#endif
//            object ev;
//            if (closureEnvironment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
//                throw new NotImplementedException ();

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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA.EvalStep");
//#endif
//            object ev = closureEnvironment.ArgumentValue (this.predicateOffset);

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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA0.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument0Value;

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
//    class ConditionalA0L : ConditionalA0
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalA0L (Argument0 predicate, LexicalVariable consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (Argument0 predicate, LexicalVariable consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument) ? ConditionalA0A.Make (predicate, (Argument) consequent, alternative) :
//                (consequent is LexicalVariable1) ? ConditionalA0L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
//                (alternative is LexicalVariable) ? Unimplemented() :
//                (alternative is Quotation) ? Unimplemented () :
//                new ConditionalA0L (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("ConditionalA0.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument0Value;

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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("ConditionalA0.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument0Value;

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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA0A0.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument0Value;

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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA0A1.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument0Value;

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
//                answer = closureEnvironment.Argument1Value;
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA0A1Q.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument0Value;
//            answer = ((ev is bool) && (bool) ev == false) ? this.alternativeValue : closureEnvironment.Argument1Value;
//            return false;
//        }
//    }



//    [Serializable]
//    class ConditionalA0L1 : ConditionalA0L
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected ConditionalA0L1 (Argument0 predicate, LexicalVariable1 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (Argument0 predicate, LexicalVariable1 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? ConditionalA0L1L.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? Unimplemented () :
//                new ConditionalA0L1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class ConditionalA0L1L : ConditionalA0L1
//    {
//        protected ConditionalA0L1L (Argument0 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (Argument0 predicate, LexicalVariable1 consequent, LexicalVariable alternative)
//        {
//            return
//                (alternative is Argument) ? Unimplemented() :
//                (alternative is LexicalVariable1) ? Unimplemented () :
//                new ConditionalA0L1L (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA0Q.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument0Value;

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
//    class ConditionalA0QL : ConditionalA0Q
//    {
//        protected readonly object alternativeName;
//        protected readonly int alternativeDepth;
//        protected readonly int alternativeOffset;

//        protected ConditionalA0QL (Argument0 predicate, Quotation consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }

//        public static SCode Make (Argument0 predicate, Quotation consequent, LexicalVariable alternative)
//        {
//            return
//                (alternative is Argument) ? ConditionalA0QA.Make (predicate, consequent, (Argument) alternative) :
//                (alternative is LexicalVariable1) ? ConditionalA0QL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
//                new ConditionalA0QL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA0QL.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;

//            if ((ev0 is bool) && (bool) ev0 == false) {
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class ConditionalA0QL1 : ConditionalA0QL
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalA0QL1 (Argument0 predicate, Quotation consequent, LexicalVariable1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (Argument0 predicate, Quotation consequent, LexicalVariable1 alternative)
//        {
//            return
//                new ConditionalA0QL1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA0QQ.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;
//            answer = (ev0 is bool && (bool) ev0 == false) ? this.alternativeValue : this.consequentValue;
//            return false;
//        }
//    }

//    [Serializable]
//    class ConditionalA0SL : ConditionalA0
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected readonly object alternativeName;
//        protected readonly int alternativeDepth;
//        protected readonly int alternativeOffset;

//        protected ConditionalA0SL (Argument0 predicate, SCode consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }

//        public static SCode Make (Argument0 predicate, SCode consequent, LexicalVariable alternative)
//        {
//            return
//                (alternative is Argument) ? ConditionalA0SA.Make (predicate, consequent, (Argument) alternative) :
//                (alternative is LexicalVariable1) ? ConditionalA0SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
//                new ConditionalA0SL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA0SL.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument0Value;

//            if ((ev is bool) && (bool) ev == false) {
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA0SA.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument0Value;

//            if ((ev is bool) && (bool) ev == false) {
//                answer = closureEnvironment.ArgumentValue (this.alternativeOffset);
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA0SA0.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument0Value;

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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA0SA1.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument0Value;

//            if ((ev is bool) && (bool) ev == false) {
//                answer = closureEnvironment.Argument1Value;
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
//    sealed class ConditionalA0SL1 : ConditionalA0SL
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        ConditionalA0SL1 (Argument0 predicate, SCode consequent, LexicalVariable1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (Argument0 predicate, SCode consequent, LexicalVariable1 alternative)
//        {
//            return
//                 new ConditionalA0SL1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA0SL1.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument0Value;

//            if ((ev is bool) && (bool) ev == false) {
//                if (closureEnvironment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
//                    throw new NotImplementedException ();
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA0SQ.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument0Value;

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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA1.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument1Value;

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
//    class ConditionalA1L : ConditionalA1
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object consequentName;
//        public readonly int consequentDepth;
//        public readonly int consequentOffset;

//        protected ConditionalA1L (Argument1 predicate, LexicalVariable consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentName = consequent.Name;
//            this.consequentDepth = consequent.Depth;
//            this.consequentOffset = consequent.Offset;
//        }

//        public static SCode Make (Argument1 predicate, LexicalVariable consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument) ? ConditionalA1A.Make (predicate, (Argument) consequent, alternative) :
//                (consequent is LexicalVariable1) ? Unimplemented() :
//                (alternative is LexicalVariable) ? Unimplemented() :
//                (alternative is Quotation) ? Unimplemented() :
//                new ConditionalA1L (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA1A1.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument1Value;
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA1Q.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument1Value;

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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA1QQ.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument1Value;
//            answer = ((ev is bool) && (bool) ev == false) ? this.alternativeValue : this.consequentValue;
//            return false;
//        }
//    }

//    [Serializable]
//    class ConditionalA1SL : ConditionalA1
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected readonly object alternativeName;
//        protected readonly int alternativeDepth;
//        protected readonly int alternativeOffset;

//        protected ConditionalA1SL (Argument1 predicate, SCode consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }

//        public static SCode Make (Argument1 predicate, SCode consequent, LexicalVariable alternative)
//        {
//            return
//                (alternative is Argument) ? ConditionalA1SA.Make (predicate, consequent, (Argument) alternative) :
//                (alternative is LexicalVariable1) ? ConditionalA1SL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
//                new ConditionalA1SL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA1SA.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument1Value;

//            if ((ev is bool) && (bool) ev == false) {
//                answer = closureEnvironment.ArgumentValue (this.alternativeOffset);
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA1SA0.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument1Value;

//            if ((ev is bool) && (bool) ev == false) {
//                answer = closureEnvironment.Argument0Value;
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
//    sealed class ConditionalA1SL1 : ConditionalA1SL
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//#endif
//        ConditionalA1SL1 (Argument1 predicate, SCode consequent, LexicalVariable1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (Argument1 predicate, SCode consequent, LexicalVariable1 alternative)
//        {
//            return
//                 new ConditionalA1SL1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA1SL1.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument1Value;

//            if ((ev is bool) && (bool) ev == false) {
//                if (closureEnvironment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
//                    throw new NotImplementedException();
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalA1SQ.EvalStep");
//#endif
//            object ev = closureEnvironment.Argument1Value;

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
//    class ConditionalAL : ConditionalA
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected readonly object consequentName;
//        protected readonly int consequentDepth;
//        protected readonly int consequentOffset;

//        protected ConditionalAL (Argument predicate, LexicalVariable consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentName = consequent.Name;
//            this.consequentDepth = consequent.Depth;
//            this.consequentOffset = consequent.Offset;
//        }

//        public static SCode Make (Argument predicate, LexicalVariable consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument) ? ConditionalAA.Make (predicate, (Argument) consequent, alternative) :
//                (consequent is LexicalVariable1) ? Unimplemented() :
//                (alternative is LexicalVariable) ? Unimplemented() :
//                (alternative is Quotation) ? Unimplemented() :
//                new ConditionalAL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalAAQ.EvalStep");
//#endif
//            object ev = closureEnvironment.ArgumentValue (this.predicateOffset);
//            answer = ((ev is bool) && (bool) ev == false) ?
//                this.alternativeValue :
//                closureEnvironment.ArgumentValue (this.consequentOffset);
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalAQ.EvalStep");
//#endif
//            object ev = closureEnvironment.ArgumentValue (this.predicateOffset);

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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalAQQ.EvalStep");
//#endif
//            object ev = closureEnvironment.ArgumentValue (this.predicateOffset);
//            answer = ((ev is bool) && (bool) ev == false) ? this.alternativeValue : this.consequentValue;
//            return false;
//        }
//    }

//    [Serializable]
//    class ConditionalASL : ConditionalA
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected readonly object alternativeName;
//        protected readonly int alternativeDepth;
//        protected readonly int alternativeOffset;

//        protected ConditionalASL (Argument predicate, SCode consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }

//        public static SCode Make (Argument predicate, SCode consequent, LexicalVariable alternative)
//        {
//            return
//                (alternative is Argument) ? ConditionalASA.Make (predicate, consequent, (Argument) alternative) :
//                (alternative is LexicalVariable1) ? Unimplemented() :
//                new ConditionalASL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalASQ.EvalStep");
//#endif
//            object ev = closureEnvironment.ArgumentValue (this.predicateOffset);

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
//    class ConditionalL1 : ConditionalL
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalL1 (LexicalVariable1 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (LexicalVariable1 predicate, SCode consequent, SCode alternative)
//        {
//            return
//                (consequent is LexicalVariable) ? ConditionalL1L.Make (predicate, (LexicalVariable) consequent, alternative) :
//                (consequent is Quotation) ? ConditionalL1Q.Make (predicate, (Quotation) consequent, alternative) :
//                (alternative is LexicalVariable) ? ConditionalL1SL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? ConditionalL1SQ.Make (predicate, consequent, (Quotation) alternative) :
//                new ConditionalL1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalL1.EvalStep");
//#endif
//            object ev;
//            if (closureEnvironment.FastLexicalRef1 (out ev, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();

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
//    class ConditionalL1L : ConditionalL1
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected readonly object consequentName;
//        protected readonly int consequentDepth;
//        protected readonly int consequentOffset;

//        protected ConditionalL1L (LexicalVariable1 predicate, LexicalVariable consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentName = consequent.Name;
//            this.consequentDepth = consequent.Depth;
//            this.consequentOffset = consequent.Offset;
//        }

//        public static SCode Make (LexicalVariable1 predicate, LexicalVariable consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument) ? ConditionalL1A.Make (predicate, (Argument) consequent, alternative) :
//                (consequent is LexicalVariable1) ? ConditionalL1L1.Make (predicate, (LexicalVariable1) consequent, alternative) :
//                (alternative is LexicalVariable) ? Unimplemented ():
//                (alternative is Quotation) ? ConditionalL1LQ.Make (predicate, consequent, (Quotation) alternative) :
//                new ConditionalL1L (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalL1L.EvalStep");
//#endif
//            object ev;
//            if (closureEnvironment.FastLexicalRef1 (out ev, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();

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
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalL1A : ConditionalL1L
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected ConditionalL1A (LexicalVariable1 predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (LexicalVariable1 predicate, Argument consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument0) ? ConditionalL1A0.Make (predicate, (Argument0) consequent, alternative) :
//                (consequent is Argument1) ? ConditionalL1A1.Make (predicate, (Argument1) consequent, alternative) :
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? Unimplemented() :
//                new ConditionalL1A (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalL1A.EvalStep");
//#endif
//            object ev;
//            if (closureEnvironment.FastLexicalRef1 (out ev, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();

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
//                answer = closureEnvironment.ArgumentValue (this.consequentOffset);
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalL1A0 : ConditionalL1A
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected ConditionalL1A0 (LexicalVariable1 predicate, Argument0 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (LexicalVariable1 predicate, Argument0 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? ConditionalL1A0Q.Make (predicate, consequent, (Quotation) alternative) :
//                new ConditionalL1A0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalL1A0.EvalStep");
//#endif
//            object ev;
//            if (closureEnvironment.FastLexicalRef1 (out ev, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();

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
//                answer = closureEnvironment.Argument0Value;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalL1A0Q : ConditionalL1A0
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        ConditionalL1A0Q (LexicalVariable1 predicate, Argument0 consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (LexicalVariable1 predicate, Argument0 consequent, Quotation alternative)
//        {
//            return
//                new ConditionalL1A0Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("ConditionalL1A0.EvalStep");
//#endif
//            object ev;
//            if (closureEnvironment.FastLexicalRef1 (out ev, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();

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
//                answer = closureEnvironment.Argument0Value;
//                return false;
//            }
//        }
//    }



//    [Serializable]
//    class ConditionalL1A1 : ConditionalL1A
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalL1A1 (LexicalVariable1 predicate, Argument1 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (LexicalVariable1 predicate, Argument1 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? ConditionalL1A1Q.Make (predicate, consequent, (Quotation) alternative) :
//                new ConditionalL1A1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("ConditionalL1L.EvalStep");
//#endif
//            object ev;
//            if (closureEnvironment.FastLexicalRef1 (out ev, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();

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
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    sealed class ConditionalL1A1Q : ConditionalL1A1
//    {
//        readonly object alternativeValue;
 
//        ConditionalL1A1Q (LexicalVariable1 predicate, Argument1 consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (LexicalVariable1 predicate, Argument1 consequent, Quotation alternative)
//        {
//            return
//                new ConditionalL1A1Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalL1A1Q.EvalStep");
//#endif
//            object ev;
//            if (closureEnvironment.FastLexicalRef1 (out ev, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();

//            answer = ((ev is bool) && (bool) ev == false) ?
//                this.alternativeValue :
//                closureEnvironment.Argument1Value;
//            return false;
//        }
//    }

//    [Serializable]
//    class ConditionalL1L1 : ConditionalL1L
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalL1L1 (LexicalVariable1 predicate, LexicalVariable1 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (LexicalVariable1 predicate, LexicalVariable1 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? ConditionalL1LQ.Make (predicate, consequent, (Quotation) alternative) :
//                (predicate.Offset == consequent.Offset) ? Unimplemented():
//                new ConditionalL1L1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    sealed class ConditionalL1LQ : ConditionalL1L
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object alternativeValue;

//        ConditionalL1LQ (LexicalVariable1 predicate, LexicalVariable consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (LexicalVariable1 predicate, LexicalVariable consequent, Quotation alternative)
//        {
//            return new ConditionalL1LQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class ConditionalL1Q : ConditionalL1
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected readonly object consequentValue;
//        protected ConditionalL1Q (LexicalVariable1 predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }

//        public static SCode Make (LexicalVariable1 predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? ConditionalL1QL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? ConditionalL1QQ.Make (predicate, consequent, (Quotation) alternative) :
//                new ConditionalL1Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalL1Q.EvalStep");
//#endif
//            object ev;
//            if (closureEnvironment.FastLexicalRef1 (out ev, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();

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
//    class ConditionalL1QL : ConditionalL1Q
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected readonly object alternativeName;
//        protected readonly int alternativeDepth;
//        protected readonly int alternativeOffset;
         
//        protected ConditionalL1QL (LexicalVariable1 predicate, Quotation consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }

//        public static SCode Make (LexicalVariable1 predicate, Quotation consequent, LexicalVariable alternative)
//        {
//            return
//                (alternative is Argument) ? Unimplemented() :
//                (alternative is LexicalVariable1) ? Unimplemented() :
//                new ConditionalL1QL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    sealed class ConditionalL1QQ : ConditionalL1Q
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        readonly object alternativeValue;
//        ConditionalL1QQ (LexicalVariable1 predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (LexicalVariable1 predicate, Quotation consequent, Quotation alternative)
//        {
//            //Debugger.Break ();
//            return
//                 new ConditionalL1QQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class ConditionalL1SL : ConditionalL1
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected readonly object alternativeName;
//        protected readonly int alternativeDepth;
//        protected readonly int alternativeOffset;

//        protected ConditionalL1SL (LexicalVariable1 predicate, SCode consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }

//        public static SCode Make (LexicalVariable1 predicate, SCode consequent, LexicalVariable alternative)
//        {
//            return
//                (alternative is Argument) ? ConditionalL1SA.Make (predicate, consequent, (Argument) alternative) :
//                (alternative is LexicalVariable1) ? Unimplemented() :
//                new ConditionalL1SL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class ConditionalL1SA : ConditionalL1SL
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalL1SA (LexicalVariable1 predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (LexicalVariable1 predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? ConditionalL1SA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? Unimplemented () :
//                new ConditionalL1SA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    sealed class ConditionalL1SA0 : ConditionalL1SA
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//#endif
//        ConditionalL1SA0 (LexicalVariable1 predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (LexicalVariable1 predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new ConditionalL1SA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalL1SA0.EvalStep");
//#endif
//            object ev;
//            if (closureEnvironment.FastLexicalRef1 (out ev, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();

//            if ((ev is bool) && (bool) ev == false) {
//                answer = closureEnvironment.Argument0Value;
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
//    class ConditionalLQ : ConditionalL
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected readonly object consequentValue;

//        protected ConditionalLQ (LexicalVariable predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }

//        public static SCode Make (LexicalVariable predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? ConditionalLQQ.Make (predicate, consequent, (Quotation) alternative) :
//                new ConditionalLQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalL.EvalStep");
//#endif
//            object ev;
//            if (closureEnvironment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
//                throw new NotImplementedException ();

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
//    sealed class ConditionalLQQ : ConditionalLQ
//    {
//        readonly object alternativeValue;

//        ConditionalLQQ (LexicalVariable predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (LexicalVariable predicate, Quotation consequent, Quotation alternative)
//        {
//            return
//                new ConditionalLQQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    sealed class ConditionalLSQ : ConditionalL
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object alternativeValue;

//        ConditionalLSQ (LexicalVariable predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (LexicalVariable predicate, SCode consequent, Quotation alternative)
//        {
//            return
//                 new ConditionalLSQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalLSQ.EvalStep");
//#endif
//            object ev;
//            if (closureEnvironment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
//                throw new NotImplementedException ();

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
//    sealed class ConditionalL1SQ : ConditionalL1
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        readonly object alternativeValue;
//        ConditionalL1SQ (LexicalVariable1 predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (LexicalVariable1 predicate, SCode consequent, Quotation alternative)
//        {
//            return new ConditionalL1SQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalL1SQ.EvalStep");
//#endif
//            object ev;
//            if (closureEnvironment.FastLexicalRef1 (out ev, this.predicateName, this.predicateOffset))
//                throw new NotImplementedException ();

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
//    class ConditionalSL : Conditional
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object consequentName;
//        public readonly int consequentDepth;
//        public readonly int consequentOffset;

//        protected ConditionalSL (SCode predicate, LexicalVariable consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentName = consequent.Name;
//            this.consequentDepth = consequent.Depth;
//            this.consequentOffset = consequent.Offset;
//        }

//        public static SCode Make (SCode predicate, LexicalVariable consequent, SCode alternative)
//        {
//            return 
//                (consequent is Argument) ? ConditionalSA.Make (predicate, (Argument) consequent, alternative) :
//                (consequent is LexicalVariable1) ? ConditionalSL1.Make (predicate, (LexicalVariable1) consequent, alternative):
//                (alternative is LexicalVariable) ? ConditionalSLL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? ConditionalSLQ.Make (predicate, consequent, (Quotation) alternative) :
//                new ConditionalSL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSL.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
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
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalSA : ConditionalSL
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalSA (SCode predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, Argument consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument0) ? ConditionalSA0.Make (predicate, (Argument0) consequent, alternative) :
//                (consequent is Argument1) ? ConditionalSA1.Make (predicate, (Argument1) consequent, alternative) :
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? ConditionalSAQ.Make (predicate, consequent, (Quotation) alternative) :
//                new ConditionalSA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//            SCode.location = "ConditionalSA.EvalStep";
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "ConditionalSA.EvalStep.1";
//#endif
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;

//            }

//            if ((ev is bool) && (bool) ev == false) {
//#if DEBUG
//                SCode.location = "-";
//                NoteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//                SCode.location = "ConditionalSA.EvalStep.1";
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = closureEnvironment.ArgumentValue (this.consequentOffset);
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalSA0 : ConditionalSA
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalSA0 (SCode predicate, Argument0 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, Argument0 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? ConditionalSA0L.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? ConditionalSA0Q.Make (predicate, consequent, (Quotation) alternative) :
//                new ConditionalSA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSA0.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
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
//                answer = closureEnvironment.Argument0Value;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalSA0L : ConditionalSA0
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalSA0L (SCode predicate, Argument0 consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, Argument0 consequent, LexicalVariable alternative)
//        {
//            return
//                (alternative is Argument) ? ConditionalSA0A.Make (predicate, consequent, (Argument) alternative) :
//                (alternative is LexicalVariable1) ? ConditionalSA0L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
//                new ConditionalSA0L (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("ConditionalSA0.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
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
//                answer = closureEnvironment.Argument0Value;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalSA0A : ConditionalSA0L
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalSA0A (SCode predicate, Argument0 consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, Argument0 consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? Unimplemented() :
//                (alternative is Argument1) ? ConditionalSA0A1.Make (predicate, consequent, (Argument1) alternative) :
//                new ConditionalSA0A (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("ConditionalSA0.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
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
//                answer = closureEnvironment.Argument0Value;
//                return false;
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSA0A1.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;

//            }

//            answer = ((ev is bool) && (bool) ev == false) ? closureEnvironment.Argument1Value : closureEnvironment.Argument0Value;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class ConditionalSA0L1 : ConditionalSA0L
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        ConditionalSA0L1 (SCode predicate, Argument0 consequent, LexicalVariable1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, Argument0 consequent, LexicalVariable1 alternative)
//        {
//            return
//                new ConditionalSA0L1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("ConditionalSA0.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
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
//                answer = closureEnvironment.Argument0Value;
//                return false;
//            }
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSA0Q.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;

//            }

//            if ((ev is bool) && (bool) ev == false) {
//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//                answer = closureEnvironment.Argument0Value;
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSA1.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
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
//                answer = closureEnvironment.Argument1Value;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalSA1L : ConditionalSA1
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object alternativeName;
//        public readonly int alternativeDepth;
//        public readonly int alternativeOffset;

//        protected ConditionalSA1L (SCode predicate, Argument1 consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }

//        public static SCode Make (SCode predicate, Argument1 consequent, LexicalVariable alternative)
//        {
//            return
//                (alternative is Argument) ? ConditionalSA1A.Make (predicate, consequent, (Argument) alternative) :
//                (alternative is LexicalVariable1) ? Unimplemented () :
//                new ConditionalSA1L (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("ConditionalSA1.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
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
//                answer = closureEnvironment.Argument1Value;
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSA1A.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;

//            }
//            answer = ((ev is bool) && (bool) ev == false) ? closureEnvironment.ArgumentValue (this.alternativeOffset) :
//                closureEnvironment.Argument1Value;
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSA1A0.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;

//            }

//            answer = ((ev is bool) && (bool) ev == false) ? closureEnvironment.Argument0Value : closureEnvironment.Argument1Value;
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//            SCode.location = "ConditionalSA1Q.EvalStep";
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;

//            }

//            answer = ((ev is bool) && (bool) ev == false) ? this.alternativeValue : closureEnvironment.Argument1Value;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class ConditionalSAQ : ConditionalSA
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object alternativeValue;
//        ConditionalSAQ (SCode predicate, Argument consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (SCode predicate, Argument consequent, Quotation alternative)
//        {
//            return
//                new ConditionalSAQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//            SCode.location = "ConditionalSAQ.EvalStep";
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "ConditionalSAQ.EvalStep.1";
//#endif
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;

//            }

//            answer = ((ev is bool) && (bool) ev == false) ? this.alternativeValue : closureEnvironment.ArgumentValue (this.consequentOffset);
//            return false;
//        }
//    }

//    [Serializable]
//    class ConditionalSL1 : ConditionalSL
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalSL1 (SCode predicate, LexicalVariable1 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, LexicalVariable1 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? ConditionalSL1L.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? ConditionalSL1Q.Make (predicate, consequent, (Quotation) alternative) :
//                new ConditionalSL1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSL1.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
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
//                if (closureEnvironment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalSL1L : ConditionalSL1
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalSL1L (SCode predicate, LexicalVariable1 consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, LexicalVariable1 consequent, LexicalVariable alternative)
//        {
//            return
//                (alternative is Argument) ? Unimplemented() : //ConditionalSL1A.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is LexicalVariable1) ? ConditionalSL1L1.Make (predicate, consequent, (LexicalVariable1) alternative) :
//                new ConditionalSL1L (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSL1.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
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
//                if (closureEnvironment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    sealed class ConditionalSL1L1 : ConditionalSL1L
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        ConditionalSL1L1 (SCode predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, LexicalVariable1 consequent, LexicalVariable1 alternative)
//        {
//            //if (consequent.Offset == alternative.Offset) Debugger.Break ();
//            return
//                new ConditionalSL1L1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("ConditionalSL1.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
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
//                if (closureEnvironment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }



//    [Serializable]
//    sealed class ConditionalSL1Q : ConditionalSL1
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        object alternativeValue;
//        ConditionalSL1Q (SCode predicate, LexicalVariable1 consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (SCode predicate, LexicalVariable1 consequent, Quotation alternative)
//        {
//            return
//                new ConditionalSL1Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSL1Q.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;

//            }

//            if ((ev is bool) && (bool) ev == false) {
//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef1 (out answer, this.consequentName, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalSLL : ConditionalSL
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object alternativeName;
//        public readonly int alternativeDepth;
//        public readonly int alternativeOffset;

//        protected ConditionalSLL (SCode predicate, LexicalVariable consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }

//        public static SCode Make (SCode predicate, LexicalVariable consequent, LexicalVariable alternative)
//        {
//            return
//                (alternative is Argument) ? Unimplemented() :
//                (alternative is LexicalVariable1) ? ConditionalSLL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
//                new ConditionalSLL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSLL.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;

//            }

//            if ((ev is bool) && (bool) ev == false) {
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalSLL1 : ConditionalSLL
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//#endif
//        ConditionalSLL1 (SCode predicate, LexicalVariable consequent, LexicalVariable1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, LexicalVariable consequent, LexicalVariable1 alternative)
//        {
//            return
//                new ConditionalSLL1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSLL1.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;

//            }

//            if ((ev is bool) && (bool) ev == false) {
//                if (closureEnvironment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return true;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }


//    [Serializable]
//    sealed class ConditionalSLQ : ConditionalSL
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object alternativeValue;

//        ConditionalSLQ (SCode predicate, LexicalVariable consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (SCode predicate, LexicalVariable consequent, Quotation alternative)
//        {
//            return
//                new ConditionalSLQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSLQ.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;

//            }

//            if ((ev is bool) && (bool) ev == false) {
//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalSQ : Conditional
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected object consequentValue;

//        protected ConditionalSQ (SCode predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }

//        public static SCode Make (SCode predicate, Quotation consequent, SCode alternative)
//        {
//            return 
//                (alternative is LexicalVariable) ? ConditionalSQL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? ConditionalSQQ.Make (predicate, consequent, (Quotation) alternative) :
//                new ConditionalSQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSQ.EvalStep");
//            predicateTypeHistogram.Note (this.predicateType);
//            NoteCalls (this.predicate);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
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
//                expression = this.consequent;
//                answer = this.consequentValue;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalSQL : ConditionalSQ
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object alternativeName;
//        public readonly int alternativeDepth;
//        public readonly int alternativeOffset;

//        protected ConditionalSQL (SCode predicate, Quotation consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }

//        public static SCode Make (SCode predicate, Quotation consequent, LexicalVariable alternative)
//        {
//            return 
//                (alternative is Argument) ? ConditionalSQA.Make (predicate, consequent, (Argument) alternative) :
//                (alternative is LexicalVariable1) ? ConditionalSQL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
//                new ConditionalSQL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSQL.EvalStep");
//            predicateTypeHistogram.Note (this.predicateType);
//            NoteCalls (this.predicate);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;

//            }

//            if ((ev is bool) && (bool) ev == false) {
//                expression = this.alternative;
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//                expression = this.consequent;
//                answer = this.consequentValue;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalSQA : ConditionalSQL
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//#endif
//        protected ConditionalSQA (SCode predicate, Quotation consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//    }

//        public static SCode Make (SCode predicate, Quotation consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? ConditionalSQA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? ConditionalSQA1.Make (predicate, consequent, (Argument1) alternative) :
//                new ConditionalSQA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSQA.EvalStep");
//            predicateTypeHistogram.Note (this.predicateType);
//            NoteCalls (this.predicate);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            if ((ev is bool) && (bool) ev == false) {
//                answer = closureEnvironment.ArgumentValue (this.alternativeOffset);
//                return false;
//            }
//            else {
//                expression = this.consequent;
//                answer = this.consequentValue;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    sealed class ConditionalSQA0 : ConditionalSQA
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//#endif
//        ConditionalSQA0 (SCode predicate, Quotation consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, Quotation consequent, Argument0 alternative)
//        {
//            return
//                new ConditionalSQA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSQA0.EvalStep");
//            predicateTypeHistogram.Note (this.predicateType);
//            NoteCalls (this.predicate);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            if ((ev is bool) && (bool) ev == false) {
//                answer = closureEnvironment.Argument0Value;
//                return false;
//            }
//            else {
//                expression = this.consequent;
//                answer = this.consequentValue;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    sealed class ConditionalSQA1 : ConditionalSQA
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//#endif
//        ConditionalSQA1 (SCode predicate, Quotation consequent, Argument1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, Quotation consequent, Argument1 alternative)
//        {
//            return
//                new ConditionalSQA1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSQA1.EvalStep");
//            predicateTypeHistogram.Note (this.predicateType);
//            NoteCalls (this.predicate);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            if ((ev is bool) && (bool) ev == false) {
//                answer = closureEnvironment.Argument1Value;
//                return false;
//            }
//            else {
//                expression = this.consequent;
//                answer = this.consequentValue;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    sealed class ConditionalSQL1 : ConditionalSQL
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//#endif
//        ConditionalSQL1 (SCode predicate, Quotation consequent, LexicalVariable1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, Quotation consequent, LexicalVariable1 alternative)
//        {
//            return
//                new ConditionalSQL1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSQL1.EvalStep");
//            predicateTypeHistogram.Note (this.predicateType);
//            NoteCalls (this.predicate);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            if ((ev is bool) && (bool) ev == false) {
//                expression = this.alternative;
//                if (closureEnvironment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//                expression = this.consequent;
//                answer = this.consequentValue;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    sealed class ConditionalSQQ : ConditionalSQ
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//#endif
//        object alternativeValue;

//        ConditionalSQQ (SCode predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (SCode predicate, Quotation consequent, Quotation alternative)
//        {
//            //object test;
//            //ObjectModel.Eq (out test, consequent.Quoted, alternative.Quoted);
//            //if ((bool) test == true)
//            //    Debugger.Break ();

//            ////if (consequent.Quoted == Constant.Unspecific) Debugger.Break ();
//            ////if (alternative.Quoted == Constant.Unspecific) Debugger.Break ();
//            ////if (consequent.Quoted is bool) Debugger.Break ();
//            ////if (alternative.Quoted is bool) Debugger.Break ();

//            //if (consequent.Quoted == alternative.Quoted) {
//            //    Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//            //    return Sequence2.Make (predicate, consequent);
//            //}
//            //else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//            //    Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//            //    return Sequence2.Make (predicate, alternative);
//            //}
//            //else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//            //    Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//            //    return Sequence2.Make (predicate, consequent);
//            //}
//            return new ConditionalSQQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSQQ.EvalStep");
//            predicateTypeHistogram.Note (this.predicateType);
//            NoteCalls (this.predicate);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            if ((ev is bool) && (bool) ev == false) {
//                expression = this.alternative;
//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//                expression = this.consequent;
//                answer = this.consequentValue;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class ConditionalSSL : Conditional
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object alternativeName;
//        public readonly int alternativeDepth;
//        public readonly int alternativeOffset;

//        protected ConditionalSSL (SCode predicate, SCode consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }

//        public static SCode Make (SCode predicate, SCode consequent, LexicalVariable alternative)
//        {
//            return 
//                (alternative is Argument) ? ConditionalSSA.Make (predicate, consequent, (Argument) alternative) :
//                (alternative is LexicalVariable1) ? ConditionalSSL1.Make (predicate, consequent, (LexicalVariable1) alternative) :
//                new ConditionalSSL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSSL.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                closureEnvironment = env;
//                answer = Interpreter.Unwind;
//                return false;

//            }

//            if ((ev is bool) && (bool) ev == false) {
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
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
//    class ConditionalSSA : ConditionalSSL
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalSSA (SCode predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? ConditionalSSA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? ConditionalSSA1.Make (predicate, consequent, (Argument1) alternative) :
//                new ConditionalSSA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSSA.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                closureEnvironment = env;
//                answer = Interpreter.Unwind;
//                return false;
//            }

//            if ((ev is bool) && (bool) ev == false) {
//                answer = closureEnvironment.ArgumentValue (this.alternativeOffset);
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
//    sealed class ConditionalSSA0 : ConditionalSSA
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//#endif

//        ConditionalSSA0 (SCode predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new ConditionalSSA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalSSA0.EvalStep");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                closureEnvironment = env;
//                answer = Interpreter.Unwind;
//                return false;

//            }

//            if ((ev is bool) && (bool) ev == false) {
//                answer = closureEnvironment.Argument0Value;
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
//    sealed class ConditionalSSA1 : ConditionalSSA
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//#endif

//        ConditionalSSA1 (SCode predicate, SCode consequent, Argument1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, SCode consequent, Argument1 alternative)
//        {
//            return
//                new ConditionalSSA1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//            SCode.location = "ConditionalSSA1.EvalStep";
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                closureEnvironment = env;
//                answer = Interpreter.Unwind;
//                return false;
//            }

//            if ((ev is bool) && (bool) ev == false) {
//                answer = closureEnvironment.Argument1Value;
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
//    sealed class ConditionalSSL1 : ConditionalSSL
//    {
//#if DEBUG
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//#endif
//        ConditionalSSL1 (SCode predicate, SCode consequent, LexicalVariable1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (SCode predicate, SCode consequent, LexicalVariable1 alternative)
//        {
//            return
//                new ConditionalSSL1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//            SCode.location = "ConditionalSSL1.EvalStep";
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                closureEnvironment = env;
//                answer = Interpreter.Unwind;
//                return false;
//            }

//            if ((ev is bool) && (bool) ev == false) {
//                if (closureEnvironment.FastLexicalRef1 (out answer, this.alternativeName, this.alternativeOffset))
//                    throw new NotImplementedException ();
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
//    sealed class ConditionalSSQ : Conditional
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
//#endif
//        object alternativeQuoted;

//        ConditionalSSQ (SCode predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeQuoted = alternative.Quoted;
//        }

//        public static SCode Make (SCode predicate, SCode consequent, Quotation alternative)
//        {
//            return new ConditionalSSQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//            SCode.location = "ConditionalSSQ.EvalStep";
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "ConditionalSSQ.EvalStep";
//#endif
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;

//            }

//            if ((ev is bool) && (bool) ev == false) {
//#if DEBUG
//                SCode.location = "ConditionalSSQ.EvalStep";
//#endif
//                answer = this.alternativeQuoted;
//                return false;
//            }
//            else {
//#if DEBUG
//                SCode.location = "ConditionalSSQ.EvalStep";
//                NoteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//        }
//    }



//    // ---------------------------


////    [Serializable]
////    class ConditionalLLL : Conditional
////    {
////        public readonly object predicateName;
////        public readonly int predicateDepth;
////        public readonly int predicateOffset;

////        public readonly object consequentName;
////        public readonly int consequentDepth;
////        public readonly int consequentOffset;

////        public readonly object alternativeName;
////        public readonly int alternativeDepth;
////        public readonly int alternativeOffset;

////        protected ConditionalLLL (LexicalVariable predicate, LexicalVariable consequent, LexicalVariable alternative)
////            : base (predicate, consequent, alternative)
////        {
////            this.predicateName = predicate.Name;
////            this.predicateDepth = predicate.Depth;
////            this.predicateOffset = predicate.Offset;

////            this.consequentName = consequent.Name;
////            this.consequentDepth = consequent.Depth;
////            this.consequentOffset = consequent.Offset;

////            this.alternativeName = alternative.Name;
////            this.alternativeDepth = alternative.Depth;
////            this.alternativeOffset = alternative.Offset;
////        }

////        public static SCode Make (LexicalVariable predicate, LexicalVariable consequent, LexicalVariable alternative)
////        {
////            return new ConditionalLLL (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////            throw new NotImplementedException ();
////        }
////    }

////    [Serializable]
////    class ConditionalSLL : Conditional
////    {
////#if DEBUG
////        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
////#endif
////        public readonly object consequentName;
////        public readonly int consequentDepth;
////        public readonly int consequentOffset;

////        public readonly object alternativeName;
////        public readonly int alternativeDepth;
////        public readonly int alternativeOffset;

////        protected ConditionalSLL (SCode predicate, LexicalVariable consequent, LexicalVariable alternative)
////            : base (predicate, consequent, alternative)
////        {
////            this.consequentName = consequent.Name;
////            this.consequentDepth = consequent.Depth;
////            this.consequentOffset = consequent.Offset;

////            this.alternativeName = alternative.Name;
////            this.alternativeDepth = alternative.Depth;
////            this.alternativeOffset = alternative.Offset;
////        }

////        public static SCode Make (SCode predicate, LexicalVariable consequent, LexicalVariable alternative)
////        {
////            return new ConditionalSLL (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////            NoteCalls (this.predicate);
////            predicateTypeHistogram.Note (this.predicateType);
////#endif
////            object ev;
////            Control unev = this.predicate;
////            Environment env = closureEnvironment;

////            while (unev.EvalStep (out ev, ref unev, ref env)) { };
////            if (ev == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
////                //closureEnvironment = env;
////                //answer = Interpreter.Unwind;
////                //return false;

////            }

////            if ((ev is bool) && (bool) ev == false) {
////                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
////                    throw new NotImplementedException ();
////                return false;
////            }
////            else {
////                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
////                    throw new NotImplementedException ();
////                return false;
////            } 
////        }
////    }

//    [Serializable]
//    class ConditionalLSL : ConditionalL
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//#endif

//        public readonly object alternativeName;
//        public readonly int alternativeDepth;
//        public readonly int alternativeOffset;

//        protected ConditionalLSL (LexicalVariable predicate, SCode consequent, LexicalVariable alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeName = alternative.Name;
//            this.alternativeDepth = alternative.Depth;
//            this.alternativeOffset = alternative.Offset;
//        }

//        public static SCode Make (LexicalVariable predicate, SCode consequent, LexicalVariable alternative)
//        {
//            return
//                (alternative is Argument) ? ConditionalLSA.Make (predicate, consequent, (Argument) alternative) :
//                (alternative is LexicalVariable1) ? Unimplemented () :
//                new ConditionalLSL (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            object ev;
//            if (closureEnvironment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
//                throw new NotImplementedException ();

//            if ((ev is bool) && (bool) ev == false) {
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
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
//    class ConditionalLSA : ConditionalLSL
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//#endif

//        protected ConditionalLSA (LexicalVariable predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (LexicalVariable predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? ConditionalLSA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? Unimplemented () :
//                new ConditionalLSA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ();
//#endif
//            object ev;
//            if (closureEnvironment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
//                throw new NotImplementedException ();

//            if ((ev is bool) && (bool) ev == false) {
//                if (closureEnvironment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
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
//    sealed class ConditionalLSA0 : ConditionalLSA
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//#endif
//        ConditionalLSA0 (LexicalVariable predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (LexicalVariable predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new ConditionalLSA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalLSA0.EvalStep");
//#endif
//            object ev;
//            if (closureEnvironment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
//                throw new NotImplementedException ();

//            if ((ev is bool) && (bool) ev == false) {
//                answer = closureEnvironment.Argument0Value;
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


////    [Serializable]
////    class ConditionalLLS : Conditional
////    {
////#if DEBUG
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
////#endif

////        public readonly object predicateName;
////        public readonly int predicateDepth;
////        public readonly int predicateOffset;

////        public readonly object consequentName;
////        public readonly int consequentDepth;
////        public readonly int consequentOffset;

////        protected ConditionalLLS (LexicalVariable predicate, LexicalVariable consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////            this.predicateName = predicate.Name;
////            this.predicateDepth = predicate.Depth;
////            this.predicateOffset = predicate.Offset;

////            this.consequentName = consequent.Name;
////            this.consequentDepth = consequent.Depth;
////            this.consequentOffset = consequent.Offset;
////        }

////        public static SCode Make (LexicalVariable predicate, LexicalVariable consequent, SCode alternative)
////        {
////            return new ConditionalLLS (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////#endif
////            object ev;
////            if (closureEnvironment.FastLexicalRef (out ev, this.predicateName, this.predicateDepth, this.predicateOffset))
////                throw new NotImplementedException ();

////            if ((ev is bool) && (bool) ev == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
                
////#endif
////                expression = this.alternative;
////                answer = null;
////                return true;
////            }
////            else {
////                if (closureEnvironment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
////                    throw new NotImplementedException ();
////                return false;
////            }
////        }
////    }

    [Serializable]
    class Disjunction : SCode, ISerializable, ISystemPair
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.DISJUNCTION; } }

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
            : base ()
        {
            this.predicate = predicate;
            this.alternative = alternative;
#if DEBUG
            this.predicateType = predicate.GetType ();
            this.alternativeType = alternative.GetType ();
#endif
        }

        static SCode RewriteDisjunction (Disjunction predicate, SCode alternative)
        {
            //Debug.Write ("\n; Flatten disjunction.");
            return Disjunction.Make (predicate.Predicate,
                                     Disjunction.Make (predicate.Alternative, alternative));
        }

        static SCode RewriteDisjunction (Sequence2 predicate, SCode alternative)
        {
            //Debug.Write ("\n; rewrite sequence2 disjunction.");
            return Sequence2.Make (predicate.First,
                                     Disjunction.Make (predicate.Second, alternative));
        }

        static SCode DistributeDisjunction (Conditional predicate, SCode alternative)
        {
            //Debug.Write ("\n; Distribute disjunction.");
            return Conditional.Make (predicate.Predicate,
                                     Disjunction.Make (predicate.Consequent, alternative),
                                     Disjunction.Make (predicate.Alternative, alternative));
        }

        static SCode FoldDisjunction (Quotation predicate, SCode alternative)
        {
            //Debug.Write ("\n; Fold disjunction.");
            return (predicate.Quoted is Boolean && (bool) predicate.Quoted == false) ? alternative : predicate;
        }

        public static SCode Make (SCode predicate, SCode alternative)
        {
            return
                //(! Configuration.EnableDisjunctionOptimization) ? new Disjunction (predicate, alternative) :
                //(predicate is Conditional) ? DistributeDisjunction((Conditional) predicate, alternative) :
                //(predicate is Disjunction) ? RewriteDisjunction ((Disjunction) predicate, alternative):

                //(predicate is Sequence2) ?  RewriteDisjunction ((Sequence2) predicate, alternative) :
                //(predicate is Sequence3) ? Unimplemented() :
                //(predicate is Quotation) ? FoldDisjunction ((Quotation) predicate, alternative) :
                //(predicate is Variable &&
                // alternative is Variable &&
                // ((Variable) predicate).Name == ((Variable) alternative).Name) ? Unimplemented():
                //(alternative is Quotation) ? DisjunctionSQ.Make (predicate, (Quotation) alternative) :
                new Disjunction (predicate, alternative);
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
            NoteCalls (this.predicate);
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
                NoteCalls (this.alternative);
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

        public override bool MutatesAny (Symbol [] formals)
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

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (DisjunctionDeserializer));
            info.AddValue ("predicate", this.predicate);
            info.AddValue ("alternative", this.alternative);
        }

        #endregion

        public override ICollection<Symbol> ComputeFreeVariables ()
        {
            return new List<Symbol> (this.predicate.ComputeFreeVariables ().Union (this.alternative.ComputeFreeVariables ()));
        }

        internal override PartialResult PartialEval (Environment environment)
        {
            PartialResult pred = this.predicate.PartialEval (environment);
            PartialResult alt = this.alternative.PartialEval (environment);
            return new PartialResult ((pred.Residual == this.predicate &&
                alt.Residual == this.alternative) ? this : Disjunction.Make (pred.Residual, alt.Residual));
        }

        public override int LambdaCount ()
        {
            return
                this.predicate.LambdaCount () +
                this.alternative.LambdaCount ();
        }
    }

    [Serializable]
    internal sealed class DisjunctionDeserializer : IObjectReference
    {
        SCode predicate;
        SCode alternative;

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public Object GetRealObject (StreamingContext context)
        {
            return Disjunction.Make (this.predicate, this.alternative);
        }
        // Muffle compiler
        SCode Predicate { set { this.predicate = value; } }
        SCode Alternative { set { this.alternative = value; } }
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

//    [Serializable]
//    class DisjunctionSQ : Disjunction
//    {
//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        public readonly object alternativeValue;

//        protected DisjunctionSQ (SCode predicate, Quotation alternative)
//            : base (predicate, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (SCode predicate, Quotation alternative)
//        {
//            return (alternative.Quoted is bool && (bool) alternative.Quoted == false) ? predicate :
//                new DisjunctionSQ (predicate, alternative);
//        }

 

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.predicate);
//            SCode.location = "DisjunctionSQ.EvalStep";
//#endif
//            Environment env = closureEnvironment;
//            Control pred = this.predicate;
//            object ev;
//            while (pred.EvalStep (out ev, ref pred, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new DisjunctionFrame (this, closureEnvironment));
//                closureEnvironment = env;
//                answer = Interpreter.Unwind;
//                return false;
//            }

//            answer = (ev is bool && (bool) ev == false)
//                ? this.alternativeValue
//                : ev;
//                return false;
//        }
//    }


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

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////#endif
////            object ev = closureEnvironment.ArgumentValue (this.randOffset);

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

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////            predicateTypeHistogram.Note (this.predicateType);
////            NoteCalls (this.predicate);
////#endif
////            object ev;
////            Control unev = this.predicate;
////            Environment env = closureEnvironment;

////            while (unev.EvalStep (out ev, ref unev, ref env)) { };
////            if (ev == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
////                //closureEnvironment = env;
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
////                answer = closureEnvironment.ArgumentValue (this.consequentOffset);
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

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////            predicateTypeHistogram.Note (this.predicateType);
////            NoteCalls (this.predicate);
////#endif
////            object ev;
////            Control unev = this.predicate;
////            Environment env = closureEnvironment;

////            while (unev.EvalStep (out ev, ref unev, ref env)) { };
////            if (ev == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
////                //closureEnvironment = env;
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
////                answer = closureEnvironment.Argument0Value;
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

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////            predicateTypeHistogram.Note (this.predicateType);
////            NoteCalls (this.predicate);
////#endif
////            object ev;
////            Control unev = this.predicate;
////            Environment env = closureEnvironment;

////            while (unev.EvalStep (out ev, ref unev, ref env)) { };
////            if (ev == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
////                //closureEnvironment = env;
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
////                answer = closureEnvironment.Argument1Value;
////                return false;
////            }
////        }
////    }


////    [Serializable]
////    class PCond2EqCarA0LA0S : ConditionalSA0S
////    {
        
////#if DEBUG
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        public readonly object lambdaName;
////        public readonly int randDepth;
////        public readonly int randOffset;

////        protected PCond2EqCarA0LA0S (PrimComb2EqCarA0L predicate, Argument0 consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////            this.lambdaName = predicate.lambdaName;
////            this.randDepth = predicate.randDepth;
////            this.randOffset = predicate.randOffset;
////        }

////        public static SCode Make (PrimComb2EqCarA0L predicate, Argument0 consequent, SCode alternative)
////        {
////            return new PCond2EqCarA0LA0S (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////#endif
////            object ev1;
////            if (closureEnvironment.FastLexicalRef (out ev1, this.lambdaName, this.randDepth, this.randOffset))
////                throw new NotImplementedException ();

////            Cons ev0 = closureEnvironment.Argument0Value as Cons;
////            if (ev0 == null) throw new NotImplementedException ();

////            object ev;
////            if (ObjectModel.Eq (out ev, ev0.Car, ev1))
////                throw new NotImplementedException ();

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
////                answer = closureEnvironment.Argument0Value;
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            object ev = closureEnvironment.ArgumentValue (this.predicateOffset);

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
//                answer = closureEnvironment.ArgumentValue (this.consequentOffset);
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalASA.EvalStep");
//#endif
//            object ev = closureEnvironment.ArgumentValue (this.predicateOffset);

//            if ((ev is bool) && (bool) ev == false) {
//#if DEBUG

                
//#endif
//                expression = this.alternative;
//                answer = closureEnvironment.ArgumentValue (this.alternativeOffset);
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("ConditionalASA1.EvalStep");
//#endif
//            object ev = closureEnvironment.ArgumentValue (this.predicateOffset);

//            if ((ev is bool) && (bool) ev == false) {
//                answer = closureEnvironment.Argument1Value;
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ();
//            NoteCalls (this.predicate);
//            predicateTypeHistogram.Note (this.predicateType);
//#endif
//            object ev;
//            Control unev = this.predicate;
//            Environment env = closureEnvironment;

//            while (unev.EvalStep (out ev, ref unev, ref env)) { };
//            if (ev == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
//                //closureEnvironment = env;
//                //answer = Interpreter.Unwind;
//                //return false;

//            }
//            if ((ev is bool) && (bool) ev == false) {
//#if DEBUG

                
//#endif
//                answer = closureEnvironment.ArgumentValue (this.alternativeOffset);
//                return false;
//            }
//            else {
//#if DEBUG
                
//#endif
//                answer = closureEnvironment.ArgumentValue (this.consequentOffset);
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

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();

//        }
//    }


 



////    [Serializable]
////    class PrimitiveConditional1A : PrimitiveConditional1
////    {
////#if DEBUG
////        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        int arg0offset;

////        protected PrimitiveConditional1A (PrimitiveCombination1A predicate, SCode consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////            this.arg0offset = predicate.randOffset;
////        }

////        public static SCode Make (PrimitiveCombination1A predicate, SCode consequent, SCode alternative)
////        {
////            return
////                (predicate is PrimitiveCombination1A0) ? PrimitiveConditional1A0.Make ((PrimitiveCombination1A0) predicate, consequent, alternative)
////                : (predicate is PrimitiveCombination1A1) ? PrimitiveConditional1A1.Make ((PrimitiveCombination1A1) predicate, consequent, alternative)
////                : new PrimitiveConditional1A (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////            procedureHistogram.Note (this.procedure);
////#endif
////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.procedure);
////#endif
////            if (this.method (out answer, closureEnvironment.ArgumentValue (this.arg0offset))) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null; // dispose of the evidence
////                    // set up the interpreter for a tail call
////                    Control cExpression = tci.Expression;
////                    Environment cEnvironment = tci.Environment;
////                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
////                }
////            }

////            //Control unev = this.predicate;
////            //Environment env = closureEnvironment;
////            //while (unev.EvalStep (out answer, ref unev, ref env)) { };
////            //if (answer == Interpreter.Unwind) {
////            //    ((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
////            //    closureEnvironment = env;
////            //    answer = Interpreter.Unwind;
////            //    return false;

////            //}

////            if ((answer is bool) && (bool) answer == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////                return true;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////                return true;
////            }
////        }
////    }

////    [Serializable]
////    class PrimitiveConditional1A0 : PrimitiveConditional1A
////    {
////#if DEBUG
////        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        protected PrimitiveConditional1A0 (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////        }

////        public static SCode Make (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
////        {
////            return
////                (predicate.Operator == Primitive.Find ("NULL?", 1)) ? PrimitiveConditionalIsNullA0.Make (predicate, consequent, alternative)     
////                : (predicate.Operator == Primitive.Find ("PAIR?", 1)) ? PrimitiveConditionalIsPairA0.Make (predicate, consequent, alternative)
////                : (predicate.Operator == Primitive.Find ("%RECORD?", 1)) ? PrimitiveConditionalIsRecordA0.Make (predicate, consequent, alternative)
////              :  new PrimitiveConditional1A0 (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////            procedureHistogram.Note (this.procedure);
////#endif
////            //Control unev0 = this.arg0;
////            //Environment env = closureEnvironment;
////            //object ev0 = null;
////            //while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
////            //if (ev0 == Interpreter.Unwind) {
////            //    throw new NotImplementedException ();
////            //    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
////            //    //answer = Interpreter.Unwind;
////            //    //closureEnvironment = env;
////            //    //return false;
////            //}

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.procedure);
////#endif
////            if (this.method (out answer, closureEnvironment.Argument0Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null; // dispose of the evidence
////                    // set up the interpreter for a tail call
////                    Control cExpression = tci.Expression;
////                    Environment cEnvironment = tci.Environment;
////                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
////                }
////            }

////            //Control unev = this.predicate;
////            //Environment env = closureEnvironment;
////            //while (unev.EvalStep (out answer, ref unev, ref env)) { };
////            //if (answer == Interpreter.Unwind) {
////            //    ((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
////            //    closureEnvironment = env;
////            //    answer = Interpreter.Unwind;
////            //    return false;

////            //}

////            if ((answer is bool) && (bool) answer == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////                return true;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////                return true;
////            }
////        }
////    }

////    [Serializable]
////    class PrimitiveConditional1A1 : PrimitiveConditional1
////    {
////#if DEBUG
////        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif

////        protected PrimitiveConditional1A1 (PrimitiveCombination1A1 predicate, SCode consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////        }

////        public static SCode Make (PrimitiveCombination1A1 predicate, SCode consequent, SCode alternative)
////        {
////            return
////                 (predicate.Operator == Primitive.Find ("NULL?", 1)) ? PrimitiveConditionalIsNullA1.Make (predicate, consequent, alternative)     
////                : (predicate.Operator == Primitive.Find ("PAIR?", 1)) ? PrimitiveConditionalIsPairA1.Make (predicate, consequent, alternative)
////                : new PrimitiveConditional1A1 (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////            procedureHistogram.Note (this.procedure);
////#endif
////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.procedure);
////#endif
////            if (this.method (out answer, closureEnvironment.Argument1Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null; // dispose of the evidence
////                    // set up the interpreter for a tail call
////                    Control cExpression = tci.Expression;
////                    Environment cEnvironment = tci.Environment;
////                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
////                }
////            }

////            //Control unev = this.predicate;
////            //Environment env = closureEnvironment;
////            //while (unev.EvalStep (out answer, ref unev, ref env)) { };
////            //if (answer == Interpreter.Unwind) {
////            //    ((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
////            //    closureEnvironment = env;
////            //    answer = Interpreter.Unwind;
////            //    return false;

////            //}

////            if ((answer is bool) && (bool) answer == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////                return true;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////                return true;
////            }
////        }
////    }

////    [Serializable]
////    class PrimitiveConditionalIsNullA0 : PrimitiveConditional1A0
////    {
////#if DEBUG
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        PrimitiveConditionalIsNullA0 (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////        }

////        public static new SCode Make (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
////        {
////            return (consequent is Quotation) ? PrimitiveIsNullA0QS.Make (predicate, (Quotation) consequent, alternative)
////                : new PrimitiveConditionalIsNullA0 (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////#endif

////            if (closureEnvironment.Argument0Value == null) {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////            }
////            answer = null;
////            return true;
////        }
////    }

////    [Serializable]
////    class PrimitiveIsNullA0QS : PrimitiveConditional1A
////    {
////#if DEBUG
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        protected object quoted;
////        PrimitiveIsNullA0QS (PrimitiveCombination1A0 predicate, Quotation consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////            this.quoted = consequent.Quoted;
////        }

////        public static SCode Make (PrimitiveCombination1A0 predicate, Quotation consequent, SCode alternative)
////        {
////            return new PrimitiveIsNullA0QS (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////#endif

////            if (closureEnvironment.Argument0Value == null) {
////                answer = this.quoted;
////                return false;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////                answer = null;
////                return true;
////            }

////        }
////    }

////    [Serializable]
////    class PrimitiveConditionalIsPairA0 : PrimitiveConditional1A0
////    {
////#if DEBUG
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        protected PrimitiveConditionalIsPairA0 (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////        }

////        public static new SCode Make (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
////        {
////            return (consequent is PrimitiveCarA0) ? PrimitiveIsPairA0CarA0.Make (predicate, (PrimitiveCarA0) consequent, alternative)
////                : (consequent is Quotation) ? PrimitiveIsPairA0QS.Make (predicate, (Quotation) consequent, alternative)
////                : (alternative is Quotation) ? PrimitiveIsPairA0SQ.Make (predicate, consequent, (Quotation) alternative)
////                :  new PrimitiveConditionalIsPairA0 (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////#endif

////            if (closureEnvironment.Argument0Value is Cons) {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;

////            }
////            answer = null;
////            return true;
////        }
////    }

////    [Serializable]
////    class PrimitiveIsPairA0CarA0 : PrimitiveConditionalIsPairA0
////    {
////#if DEBUG
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        PrimitiveIsPairA0CarA0 (PrimitiveCombination1A0 predicate, PrimitiveCarA0 consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////        }

////        public static SCode Make (PrimitiveCombination1A0 predicate, PrimitiveCarA0 consequent, SCode alternative)
////        {
////            return new PrimitiveIsPairA0CarA0 (predicate, consequent, alternative);
////        }


////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////#endif
////            Cons a0 = closureEnvironment.Argument0Value as Cons;
////            if (a0 == null) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////                answer = null;
////                return true;

////            }
////            else {
////                answer = a0.Car;
////                return false;
////            }

////        }
////    }

////    [Serializable]
////    class PrimitiveIsPairA0QS : PrimitiveConditionalIsPairA0
////    {
////#if DEBUG
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        public object quoted;
////        protected PrimitiveIsPairA0QS (PrimitiveCombination1A0 predicate, Quotation consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////            this.quoted = consequent.Quoted;
////        }

////        public static SCode Make (PrimitiveCombination1A0 predicate, Quotation consequent, SCode alternative)
////        {
////            return new PrimitiveIsPairA0QS (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////#endif
////            if (closureEnvironment.Argument0Value is Cons) {
////                answer = this.quoted;
////                return false;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////                answer = null;
////                return true;
////            }
////        }
////    }

////    [Serializable]
////    class PrimitiveIsPairA0SQ : PrimitiveConditionalIsPairA0
////    {
////#if DEBUG
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////#endif
////        public object quoted;
////        protected PrimitiveIsPairA0SQ (PrimitiveCombination1A0 predicate, SCode consequent, Quotation alternative)
////            : base (predicate, consequent, alternative)
////        {
////            this.quoted = alternative.Quoted;
////        }

////        public static SCode Make (PrimitiveCombination1A0 predicate, SCode consequent, Quotation alternative)
////        {
////            return new PrimitiveIsPairA0SQ (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////#endif
////            if (closureEnvironment.Argument0Value is Cons) {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////                answer = null;
////                return true;
////            }
////            else {
////                answer = this.quoted;
////                return false;

////            }
////        }
////    }

////    [Serializable]
////    class PrimitiveConditionalIsRecordA0 : PrimitiveConditional1A0
////    {
////#if DEBUG
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        protected PrimitiveConditionalIsRecordA0 (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////        }

////        public static new SCode Make (PrimitiveCombination1A0 predicate, SCode consequent, SCode alternative)
////        {
////            return //(consequent is Quotation) ? PrimitiveIsNullA0QS.Make (predicate, (Quotation) consequent, alternative)
////                //: 
////                new PrimitiveConditionalIsRecordA0 (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////#endif

////            if (closureEnvironment.Argument0Value is Record) {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////            }
////            answer = null;
////            return true;
////        }
////    }



////    [Serializable]
////    class PrimitiveConditionalIsNullA1 : PrimitiveConditional1A1
////    {
////#if DEBUG
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        protected PrimitiveConditionalIsNullA1 (PrimitiveCombination1A1 predicate, SCode consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////        }

////        public static new SCode Make (PrimitiveCombination1A1 predicate, SCode consequent, SCode alternative)
////        {
////            return // (consequent is Quotation) ? PrimitiveIsNullA0QS.Make (procedure, arg, (Quotation) consequent, alternative)
////                //    : 
////            new PrimitiveConditionalIsNullA1 (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////#endif

////            if (closureEnvironment.Argument1Value == null) {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////            }
////            answer = null;
////            return true;
////        }
////    }

////    [Serializable]
////    class PrimitiveConditionalIsPairA1 : PrimitiveConditional1A1
////    {
////#if DEBUG
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        protected PrimitiveConditionalIsPairA1 (PrimitiveCombination1A1 predicate, SCode consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////        }

////        public static new SCode Make (PrimitiveCombination1A1 predicate, SCode consequent, SCode alternative)
////        {
////            return
////                //(consequent is PrimitiveCarA0)
////                //? PrimitiveIsPairA0CarA0.Make (procedure, arg, (PrimitiveCarA0) consequent, alternative)
////                //: 
////                new PrimitiveConditionalIsPairA1 (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////#endif

////            if (closureEnvironment.Argument1Value is Cons) {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;

////            }
////            answer = null;
////            return true;
////        }
////    }


////   [Serializable]
////    class PrimitiveConditional1SQS : PrimitiveConditional1
////    {
////#if DEBUG
////        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
////        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        protected readonly object quoted;

////        protected PrimitiveConditional1SQS (PrimitiveCombination1 predicate, Quotation consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////            this.quoted = consequent.Quoted;
////        }

////        public static SCode Make (PrimitiveCombination1 predicate, Quotation consequent, SCode alternative)
////        {
////            return new PrimitiveConditional1SQS (predicate, consequent, alternative);
////        }



////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////            NoteCalls (this.arg0);
////            procedureHistogram.Note (this.procedure);
////            arg0TypeHistogram.Note (this.arg0Type);
////#endif
////            Control unev0 = this.arg0;
////            Environment env = closureEnvironment;
////            object ev0;
////            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
////            if (ev0 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
////                //answer = Interpreter.Unwind;
////                //closureEnvironment = env;
////                //return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.procedure);
////#endif
////            if (this.method (out answer, ev0)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null; // dispose of the evidence
////                    // set up the interpreter for a tail call
////                    Control cExpression = tci.Expression;
////                    Environment cEnvironment = tci.Environment;
////                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
////                }
////            }

////            //Control unev = this.predicate;
////            //Environment env = closureEnvironment;
////            //while (unev.EvalStep (out answer, ref unev, ref env)) { };
////            //if (answer == Interpreter.Unwind) {
////            //    ((UnwinderState) env).AddFrame (new ConditionalFrame (this, closureEnvironment));
////            //    closureEnvironment = env;
////            //    answer = Interpreter.Unwind;
////            //    return false;

////            //}

////            if ((answer is bool) && (bool) answer == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////                return true;
////            }
////            else {
////                answer = this.quoted;
////                return false;
////            }
////        }
////    }

////    [Serializable]
////    class PrimitiveIsEq : PrimitiveConditional2
////    {
////#if DEBUG
////        [NonSerialized]
////        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

////#endif
////        protected PrimitiveIsEq (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
////            : base (predicate, consequent, alternative)
////        {
////        }

////        public static new SCode Make (PrimitiveCombination2 predicate, SCode consequent, SCode alternative)
////        {
////            return new PrimitiveIsEq (predicate, consequent, alternative);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////            NoteCalls (this.arg0);
////            NoteCalls (this.arg1);
////            rand0TypeHistogram.Note (this.rand0Type);
////            rand1TypeHistogram.Note (this.rand1Type);
////#endif
////            Control unev = this.arg1;
////            Environment env = closureEnvironment;
////            object ev1;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////            }

////            unev = this.arg0;
////            env = closureEnvironment;
////            object ev0;
////            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            if (ev0 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
////                //answer = Interpreter.Unwind;
////                //closureEnvironment = env;
////                //return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.procedure);
////#endif
////            if (ObjectModel.Eq (out answer, ev0, ev1))
////                throw new NotImplementedException ();
////            //if (this.method (out answer, ev0, ev1)) {
////            //    TailCallInterpreter tci = answer as TailCallInterpreter;
////            //    if (tci != null) {
////            //        answer = null; // dispose of the evidence
////            //        // set up the interpreter for a tail call
////            //        Control cExpression = tci.Expression;
////            //        Environment cEnvironment = tci.Environment;
////            //        while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
////            //    }
////            //}

////            if ((answer is bool) && (bool) answer == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////                return true;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////                return true;
////            }
////        }
////    }


////    [Serializable]
////    class PrimitiveConditional2AS : PrimitiveConditional2
////    {
////#if DEBUG
////        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
////        [NonSerialized]
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
////        protected readonly int a0offset;

////        protected PrimitiveConditional2AS (Primitive2 procedure, Argument arg0, SCode arg1, SCode consequent, SCode alternative)
////            : base (procedure, arg0, arg1, consequent, alternative)
////        {
////            this.a0offset = arg0.Offset;
////        }

////        public static SCode Make (Primitive2 procedure, Argument arg0, SCode arg1, SCode consequent, SCode alternative)
////        {
////            return
////                (arg0 is Argument0) ? PrimitiveConditional2A0S.Make (procedure, (Argument0) arg0, arg1, consequent, alternative)
////                : (arg0 is Argument1) ? PrimitiveConditional2A1S.Make (procedure, (Argument1) arg0, arg1, consequent, alternative)
////                : new PrimitiveConditional2AS (procedure, arg0, arg1, consequent, alternative);
////        }


////        public override SCode Bind (LexicalMap ctenv)
////        {
////            throw new NotImplementedException ("shouldn't be necessary");
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////            NoteCalls (this.arg1);
////#endif
////            Control unev = this.arg1;
////            Environment env = closureEnvironment;
////            object ev1 = null;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////            }

////            //unev = this.arg0;
////            //env = closureEnvironment;
////            //object ev0 = null;
////            //while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            //if (ev0 == Interpreter.Unwind) {
////            //    throw new NotImplementedException ();
////            //    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
////            //    //answer = Interpreter.Unwind;
////            //    //closureEnvironment = env;
////            //    //return false;
////            //}

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.procedure);
////#endif
////            if (this.method (out answer, closureEnvironment.ArgumentValue (this.a0offset), ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null; // dispose of the evidence
////                    // set up the interpreter for a tail call
////                    Control cExpression = tci.Expression;
////                    Environment cEnvironment = tci.Environment;
////                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
////                }
////            }

////            if ((answer is bool) && (bool) answer == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////                return true;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////                return true;
////            }
////        }
////    }

////    [Serializable]
////    class PrimitiveConditional2A0S : PrimitiveConditional2AS
////    {
////#if DEBUG
////        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
////        [NonSerialized]
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif

////        protected PrimitiveConditional2A0S (Primitive2 procedure, Argument0 arg0, SCode arg1, SCode consequent, SCode alternative)
////            : base (procedure, arg0, arg1, consequent, alternative)
////        {
////        }

////        public static SCode Make (Primitive2 procedure, Argument0 arg0, SCode arg1, SCode consequent, SCode alternative)
////        {
////            return (arg1 is Quotation)
////                ? PrimitiveConditional2A0Q.Make (procedure, arg0, (Quotation)arg1, consequent, alternative)
////                : new PrimitiveConditional2A0S (procedure, arg0, arg1, consequent, alternative);
////        }

////        public override SCode Bind (LexicalMap ctenv)
////        {
////            throw new NotImplementedException ("shouldn't be necessary");
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////            NoteCalls (this.arg1);
////            procedureHistogram.Note (this.procedure);
////            rand1TypeHistogram.Note (this.rand1Type);
////#endif
////            Control unev = this.arg1;
////            Environment env = closureEnvironment;
////            object ev1 = null;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////            }

////            //unev = this.arg0;
////            //env = closureEnvironment;
////            //object ev0 = null;
////            //while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            //if (ev0 == Interpreter.Unwind) {
////            //    throw new NotImplementedException ();
////            //    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
////            //    //answer = Interpreter.Unwind;
////            //    //closureEnvironment = env;
////            //    //return false;
////            //}

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.procedure);
////#endif
////            if (this.method (out answer, closureEnvironment.Argument0Value, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null; // dispose of the evidence
////                    // set up the interpreter for a tail call
////                    Control cExpression = tci.Expression;
////                    Environment cEnvironment = tci.Environment;
////                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
////                }
////            }

////            if ((answer is bool) && (bool) answer == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////                return true;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////                return true;
////            }
////        }
////    }

////    [Serializable]
////    class PrimitiveConditional2A1S : PrimitiveConditional2
////    {
////#if DEBUG
////        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
////        [NonSerialized]
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif

////        protected PrimitiveConditional2A1S (Primitive2 procedure, Argument1 arg0, SCode arg1, SCode consequent, SCode alternative)
////            : base (procedure, arg0, arg1, consequent, alternative)
////        {
////        }

////        public static SCode Make (Primitive2 procedure, Argument1 arg0, SCode arg1, SCode consequent, SCode alternative)
////        {
////            return new PrimitiveConditional2A1S (procedure, arg0, arg1, consequent, alternative);
////        }


////        public override SCode Bind (LexicalMap ctenv)
////        {
////            throw new NotImplementedException ("shouldn't be necessary");
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////            NoteCalls (this.arg1);
////#endif
////            Control unev = this.arg1;
////            Environment env = closureEnvironment;
////            object ev1 = null;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////            }

////            //unev = this.arg0;
////            //env = closureEnvironment;
////            //object ev0 = null;
////            //while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            //if (ev0 == Interpreter.Unwind) {
////            //    throw new NotImplementedException ();
////            //    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
////            //    //answer = Interpreter.Unwind;
////            //    //closureEnvironment = env;
////            //    //return false;
////            //}

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.procedure);
////#endif
////            if (this.method (out answer, closureEnvironment.Argument1Value, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null; // dispose of the evidence
////                    // set up the interpreter for a tail call
////                    Control cExpression = tci.Expression;
////                    Environment cEnvironment = tci.Environment;
////                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
////                }
////            }

////            if ((answer is bool) && (bool) answer == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////                return true;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////                return true;
////            }
////        }
////    }

////    [Serializable]
////    class PrimitiveConditional2A0Q : PrimitiveConditional2A0S
////    {
////#if DEBUG
////        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
////        [NonSerialized]
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        protected object quoted;

////        protected PrimitiveConditional2A0Q (Primitive2 procedure, Argument0 arg0, Quotation arg1, SCode consequent, SCode alternative)
////            : base (procedure, arg0, arg1, consequent, alternative)
////        {
////            this.quoted = arg1.Quoted;
////        }

////        public static SCode Make (Primitive2 procedure, Argument0 arg0, Quotation arg1, SCode consequent, SCode alternative)
////        {
////            return new PrimitiveConditional2A0Q (procedure, arg0, arg1, consequent, alternative);
////        }

////        public override SCode Bind (LexicalMap ctenv)
////        {
////            throw new NotImplementedException ("shouldn't be necessary");
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////            procedureHistogram.Note (this.procedure);
////#endif

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.procedure);
////#endif
////            if (this.method (out answer, closureEnvironment.Argument0Value, this.quoted)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null; // dispose of the evidence
////                    // set up the interpreter for a tail call
////                    Control cExpression = tci.Expression;
////                    Environment cEnvironment = tci.Environment;
////                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
////                }
////            }

////            if ((answer is bool) && (bool) answer == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////                return true;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////                return true;
////            }
////        }
////    }


////    [Serializable]
////    class PrimitiveConditional2AL : PrimitiveConditional2AS
////    {
////#if DEBUG
////        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
////        [NonSerialized]
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
////        protected readonly string l1name;
////        protected readonly int l1depth;
////        protected readonly int l1offset;


////        protected PrimitiveConditional2AL (Primitive2 procedure, Argument arg0, LexicalVariable arg1, SCode consequent, SCode alternative)
////            : base (procedure, arg0, arg1, consequent, alternative)
////        {

////            this.l1name = arg1.ratorName;
////            this.l1depth = arg1.Depth;
////            this.l1offset = arg1.Offset;
////        }

////        public static SCode Make (Primitive2 procedure, Argument arg0, LexicalVariable arg1, SCode consequent, SCode alternative)
////        {
////            return new PrimitiveConditional2AL (procedure, arg0, arg1, consequent, alternative);
////        }


////        public override SCode Bind (LexicalMap ctenv)
////        {
////            throw new NotImplementedException ("shouldn't be necessary");
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////#endif
////            //Control unev = this.arg1;
////            //Environment env = closureEnvironment;
////            //object ev1 = null;
////            //while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            //if (ev1 == Interpreter.Unwind) {
////            //    throw new NotImplementedException ();
////            //}

////            //unev = this.arg0;
////            //env = closureEnvironment;
////            //object ev0 = null;
////            //while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            //if (ev0 == Interpreter.Unwind) {
////            //    throw new NotImplementedException ();
////            //    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
////            //    //answer = Interpreter.Unwind;
////            //    //closureEnvironment = env;
////            //    //return false;
////            //}

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.procedure);
////#endif
////            object ev1 = null;
////            if (closureEnvironment.FastLexicalRef (out ev1, this.l1name, this.l1depth, this.l1offset))
////                throw new NotImplementedException ();
////            if (this.method (out answer, closureEnvironment.ArgumentValue (this.a0offset), ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null; // dispose of the evidence
////                    // set up the interpreter for a tail call
////                    Control cExpression = tci.Expression;
////                    Environment cEnvironment = tci.Environment;
////                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
////                }
////            }
////                        if ((answer is bool) && (bool) answer == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note(this.alternativeType);
////#endif
////                expression = this.alternative;
////                return true;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////                return true;
////            }
////        }
////    }

////    [Serializable]
////    class PrimitiveConditional2AQ : PrimitiveConditional2AS
////    {
////#if DEBUG
////        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
////        [NonSerialized]
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

////#endif
////        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
////        protected readonly object q1quoted;


////        PrimitiveConditional2AQ (Primitive2 procedure, Argument arg0, Quotation arg1, SCode consequent, SCode alternative)
////            : base (procedure, arg0, arg1, consequent, alternative)
////        {
////            this.q1quoted = arg1.Quoted;
////        }

////        public static SCode Make (Primitive2 procedure, Argument arg0, Quotation arg1, SCode consequent, SCode alternative)
////        {
////            return new PrimitiveConditional2AQ (procedure, arg0, arg1, consequent, alternative);
////        }

////        public override SCode Bind (LexicalMap ctenv)
////        {
////            throw new NotImplementedException ("shouldn't be necessary");
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////            procedureHistogram.Note (this.procedure);
////#endif
////            //Control unev = this.arg1;
////            //Environment env = closureEnvironment;
////            //object ev1 = null;
////            //while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            //if (ev1 == Interpreter.Unwind) {
////            //    throw new NotImplementedException ();
////            //}

////            //unev = this.arg0;
////            //env = closureEnvironment;
////            //object ev0 = null;
////            //while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            //if (ev0 == Interpreter.Unwind) {
////            //    throw new NotImplementedException ();
////            //    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
////            //    //answer = Interpreter.Unwind;
////            //    //closureEnvironment = env;
////            //    //return false;
////            //}

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.procedure);
////#endif
////            if (this.method (out answer, closureEnvironment.ArgumentValue (this.a0offset), this.q1quoted)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null; // dispose of the evidence
////                    // set up the interpreter for a tail call
////                    Control cExpression = tci.Expression;
////                    Environment cEnvironment = tci.Environment;
////                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
////                }
////            }

////                        if ((answer is bool) && (bool) answer == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note(this.alternativeType);
////#endif
////                expression = this.alternative;
////                return true;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////                return true;
////            }
////        }
////    }

////    [Serializable]
////    class PrimitiveConditional2LS : PrimitiveConditional2
////    {
////#if DEBUG
////        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
////        [NonSerialized]
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
////        protected readonly object l0name;
////        protected readonly int l0depth;
////        protected readonly int l0offset;

////        PrimitiveConditional2LS (Primitive2 procedure, LexicalVariable arg0, SCode arg1, SCode consequent, SCode alternative)
////            : base (procedure, arg0, arg1, consequent, alternative)
////        {
////            this.l0name = arg0.Name;
////            this.l0depth = arg0.Depth;
////            this.l0offset = arg0.Offset;
////        }

////        public static SCode Make (Primitive2 procedure, LexicalVariable arg0, SCode arg1, SCode consequent, SCode alternative)
////        {
////            return new PrimitiveConditional2LS (procedure, arg0, arg1, consequent, alternative);
////        }

////        public override SCode Bind (LexicalMap ctenv)
////        {
////            throw new NotImplementedException ("shouldn't be necessary");
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////            NoteCalls (this.arg1);
////            procedureHistogram.Note (this.procedure);
////            rand1TypeHistogram.Note (this.rand1Type);
////#endif
////            Control unev = this.arg1;
////            Environment env = closureEnvironment;
////            object ev1 = null;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.procedure);
////#endif
////            object ev0 = null;
////            if (closureEnvironment.FastLexicalRef (out ev0, this.l0name, this.l0depth, this.l0offset))
////                throw new NotImplementedException ();

////            if (this.method (out answer, ev0, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null; // dispose of the evidence
////                    // set up the interpreter for a tail call
////                    Control cExpression = tci.Expression;
////                    Environment cEnvironment = tci.Environment;
////                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
////                }
////            }
////            if ((answer is bool) && (bool) answer == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////                return true;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////                return true;
////            }

////        }
////    }

////    [Serializable]
////    class PrimitiveConditional2QS : PrimitiveConditional2
////    {
////#if DEBUG
////        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
////        protected readonly object quotedValue;

////        protected PrimitiveConditional2QS (Primitive2 procedure, Quotation arg0, SCode arg1, SCode consequent, SCode alternative)
////            : base (procedure, arg0, arg1, consequent, alternative)
////        {
////            this.quotedValue = arg0.Quoted;
////        }

////        public static SCode Make (Primitive2 procedure, Quotation arg0, SCode arg1, SCode consequent, SCode alternative)
////        {
////            return //(consequent is Quotation) ? PrimitiveConditional2QSQS.Make (procedure, arg0, arg1, (Quotation) consequent, alternative)
////                //: 
////                new PrimitiveConditional2QS (procedure, arg0, arg1, consequent, alternative);
////        }

////        public override SCode Bind (LexicalMap ctenv)
////        {
////            throw new NotImplementedException ("shouldn't be necessary");
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////            NoteCalls (this.arg1);
////            procedureHistogram.Note (this.procedure);
////            rand1TypeHistogram.Note (this.rand1Type);
////#endif
////            Control unev = this.arg1;
////            Environment env = closureEnvironment;
////            object ev1 = null;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.procedure);
////#endif
////            if (this.method (out answer, this.quotedValue, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null; // dispose of the evidence
////                    // set up the interpreter for a tail call
////                    Control cExpression = tci.Expression;
////                    Environment cEnvironment = tci.Environment;
////                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
////                }
////            }

////            if ((answer is bool) && (bool) answer == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////                return true;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////                return true;
////            }
////        }
////    }

////    [Serializable]
////    class PrimitiveConditional2SL : PrimitiveConditional2
////    {
////#if DEBUG
////        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
////        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
////        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
////#endif
////        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
////        protected readonly string l1name;
////        protected readonly int l1depth;
////        protected readonly int l1offset;

////        PrimitiveConditional2SL (Primitive2 procedure, SCode arg0, LexicalVariable arg1, SCode consequent, SCode alternative)
////            : base (procedure, arg0, arg1, consequent, alternative)
////        {
////            this.l1name = arg1.ratorName;
////            this.l1depth = arg1.Depth;
////            this.l1offset = arg1.Offset;
////        }

////        public static SCode Make (Primitive2 procedure, SCode arg0, LexicalVariable arg1, SCode consequent, SCode alternative)
////        {
////            return new PrimitiveConditional2SL (procedure, arg0, arg1, consequent, alternative);
////        }

////        public override SCode Bind (LexicalMap ctenv)
////        {
////            throw new NotImplementedException ("shouldn't be necessary");
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ();
////            NoteCalls (this.arg0);
////            procedureHistogram.Note (this.procedure);
////            rand0TypeHistogram.Note (this.rand0Type);
////#endif
////            object ev1 = null;
////            if (closureEnvironment.FastLexicalRef (out ev1, this.l1name, this.l1depth, this.l1offset))
////                throw new NotImplementedException ();

////            Control unev = this.arg0;
////            Environment env = closureEnvironment;
////            object ev0 = null;
////            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            if (ev0 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.procedure);
////#endif
////            if (this.method (out answer, ev0, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null; // dispose of the evidence
////                    // set up the interpreter for a tail call
////                    Control cExpression = tci.Expression;
////                    Environment cEnvironment = tci.Environment;
////                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
////                }
////            }
////            if ((answer is bool) && (bool) answer == false) {
////#if DEBUG
////                NoteCalls (this.alternative);
////                alternativeTypeHistogram.Note (this.alternativeType);
////#endif
////                expression = this.alternative;
////                return true;
////            }
////            else {
////#if DEBUG
////                NoteCalls (this.consequent);
////                consequentTypeHistogram.Note (this.consequentType);
////#endif
////                expression = this.consequent;
////                return true;
////            }
////        }
////    }
}

