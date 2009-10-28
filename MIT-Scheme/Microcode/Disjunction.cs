using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    [Serializable]
    class Disjunction : SCode, ISerializable, ISystemPair
    {
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.DISJUNCTION; } }

#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type>();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
        public readonly Type predicateType;
        public readonly Type alternativeType;
#endif

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        protected readonly SCode predicate;

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        protected readonly SCode alternative;

        protected Disjunction(SCode predicate, SCode alternative)
            : base()
        {
            this.predicate = predicate;
            this.alternative = alternative;
#if DEBUG
            this.predicateType = predicate.GetType();
            this.alternativeType = alternative.GetType();
#endif
        }

        static SCode RewriteDisjunction(Disjunction predicate, SCode alternative)
        {
            //Debug.Write ("\n; Flatten disjunction.");
            return Disjunction.Make(predicate.Predicate,
                                     Disjunction.Make(predicate.Alternative, alternative));
        }

        static SCode RewriteDisjunction(Sequence2 predicate, SCode alternative)
        {
            //Debug.Write ("\n; rewrite sequence2 disjunction.");
            return Sequence2.Make(predicate.First,
                                     Disjunction.Make(predicate.Second, alternative));
        }

        static SCode DistributeDisjunction(Conditional predicate, SCode alternative)
        {
            //Debug.Write ("\n; Distribute disjunction.");
            return Conditional.Make(predicate.Predicate,
                                     Disjunction.Make(predicate.Consequent, alternative),
                                     Disjunction.Make(predicate.Alternative, alternative));
        }

        static SCode FoldDisjunction(Quotation predicate, SCode alternative)
        {
            //Debug.Write ("\n; Fold disjunction.");
            return (predicate.Quoted is Boolean && (bool)predicate.Quoted == false) ? alternative : predicate;
        }

        public static SCode Make(SCode predicate, SCode alternative)
        {
            return
                (! Configuration.EnableDisjunctionOptimization) ? new Disjunction (predicate, alternative) :
                (! Configuration.EnableDisjunctionSpecialization) ? new Disjunction (predicate, alternative) :
                (Configuration.EnablePrimitiveDisjunction1 &&
                 predicate is PrimitiveCombination1) ? POr1.Make((PrimitiveCombination1)predicate, alternative) :
                (Configuration.EnablePrimitiveDisjunction2 &&
                 predicate is PrimitiveCombination2) ? POr2.Make((PrimitiveCombination2)predicate, alternative) :
                //(predicate is Conditional) ? DistributeDisjunction((Conditional) predicate, alternative) :
                //(predicate is Disjunction) ? RewriteDisjunction ((Disjunction) predicate, alternative):

                //(predicate is Sequence2) ?  RewriteDisjunction ((Sequence2) predicate, alternative) :
                //(predicate is Sequence3) ? Unimplemented() :
                //(predicate is Quotation) ? FoldDisjunction ((Quotation) predicate, alternative) :
                //(predicate is Variable &&
                // alternative is Variable &&
                // ((Variable) predicate).Name == ((Variable) alternative).Name) ? Unimplemented():
                //(alternative is Quotation) ? DisjunctionSQ.Make (predicate, (Quotation) alternative) :
                new Disjunction(predicate, alternative);
        }

        public static SCode Make(object predicate, object alternative)
        {
            return Make(EnsureSCode(predicate), EnsureSCode(alternative));
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

        public override bool CallsTheEnvironment()
        {
            return this.predicate.CallsTheEnvironment()
                || this.alternative.CallsTheEnvironment();
        }

        [SchemePrimitive("DISJUNCTION?", 1, true)]
        public static bool IsDisjunction(out object answer, object arg)
        {
            answer = arg is Disjunction;
            return false;
        }

        public override bool EvalStep(out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("Disjunction.EvalStep");
            NoteCalls(this.predicate);
            predicateTypeHistogram.Note(this.predicateType);
#endif
            Environment env = environment;
            Control pred = this.predicate;
            object ev;
            while (pred.EvalStep(out ev, ref pred, ref env)) { };
            if (ev == Interpreter.UnwindStack)
            {
                ((UnwinderState)env).AddFrame(new DisjunctionFrame(this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            if (ev is bool && (bool)ev == false)
            {
#if DEBUG
                NoteCalls(this.alternative);
                alternativeTypeHistogram.Note(this.alternativeType);
#endif
                // tail call alternative
                expression = this.alternative;
                answer = null;
                return true;
            }
            else
            {
                // return answer
                answer = ev;
                return false;
            }
        }

        public override bool MutatesAny(Symbol[] formals)
        {
            return this.predicate.MutatesAny(formals)
                || this.alternative.MutatesAny(formals);
        }

        #region ISystemPair Members

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            get
            {
                return this.predicate;
            }
            set
            {
                throw new NotImplementedException();
            }
        }
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        public object SystemPairCdr
        {
            get
            {
                return this.alternative;
            }
            set
            {
                throw new NotImplementedException();
            }
        }

        #endregion

        #region ISerializable Members

        [SecurityPermissionAttribute(SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData(SerializationInfo info, StreamingContext context)
        {
            info.SetType(typeof(DisjunctionDeserializer));
            info.AddValue("predicate", this.predicate);
            info.AddValue("alternative", this.alternative);
        }

        #endregion

        internal override PartialResult PartialEval(PartialEnvironment environment)
        {
            PartialResult pred = this.predicate.PartialEval(environment);
            PartialResult alt = this.alternative.PartialEval(environment);
            return new PartialResult((pred.Residual == this.predicate &&
                alt.Residual == this.alternative) ? this : Disjunction.Make(pred.Residual, alt.Residual));
        }

        public override void CollectFreeVariables(HashSet<Symbol> freeVariableSet)
        {
            this.predicate.CollectFreeVariables(freeVariableSet);
            this.alternative.CollectFreeVariables(freeVariableSet);
        }

        internal override SCode SubstituteStatics (object [] statics)
        {
            SCode newPredicate = this.predicate.SubstituteStatics (statics);
            SCode newAlternative = this.alternative.SubstituteStatics (statics);
            return (newPredicate == this.predicate &&
                newAlternative == this.alternative) ?
                this :
                Disjunction.Make (newPredicate, newAlternative);
        }
    }

    [Serializable]
    internal sealed class DisjunctionDeserializer : IObjectReference
    {
        SCode predicate;
        SCode alternative;

        [SecurityPermissionAttribute(SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public Object GetRealObject(StreamingContext context)
        {
            return Disjunction.Make(this.predicate, this.alternative);
        }
        // Muffle compiler
        SCode Predicate { set { this.predicate = value; } }
        SCode Alternative { set { this.alternative = value; } }
    }

    [Serializable]
    sealed class DisjunctionFrame : SubproblemContinuation<Disjunction>, ISystemVector
    {
        public DisjunctionFrame(Disjunction disjunction, Environment environment)
            : base(disjunction, environment)
        {
        }

        public override bool Continue(out object answer, ref Control expression, ref Environment environment, object value)
        {
            answer = value;
            if (value is bool && (bool)value == false)
            {
                // tail call alternative
                expression = this.expression.Alternative;
                return true;
            }
            else
            {
                return false;
            }
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { throw new NotImplementedException(); }
        }

        public object SystemVectorRef(int index)
        {
            throw new NotImplementedException();
        }

        public object SystemVectorSet(int index, object newValue)
        {
            throw new NotImplementedException();
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



    //        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    //        {
    //#if DEBUG
    //            Warm ("-");
    //            NoteCalls (this.predicate);
    //            SCode.location = "DisjunctionSQ.EvalStep";
    //#endif
    //            Environment env = environment;
    //            Control pred = this.predicate;
    //            object ev;
    //            while (pred.EvalStep (out ev, ref pred, ref env)) { };
    //            if (ev == Interpreter.UnwindStack) {
    //                ((UnwinderState) env).AddFrame (new DisjunctionFrame (this, environment));
    //                environment = env;
    //                answer = Interpreter.Unwind;
    //                return false;
    //            }

    //            answer = (ev is bool && (bool) ev == false)
    //                ? this.alternativeValue
    //                : ev;
    //                return false;
    //        }
    //    }


}
