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
                
                (Configuration.EnablePrimitiveDisjunction1 &&
                 predicate is PrimitiveCombination1) ? POr1.Make((PrimitiveCombination1)predicate, alternative) :
                (Configuration.EnablePrimitiveDisjunction2 &&
                 predicate is PrimitiveCombination2) ? POr2.Make((PrimitiveCombination2)predicate, alternative) :
                (! Configuration.EnableDisjunctionSpecialization) ? new Disjunction (predicate, alternative) :
                //(predicate is Conditional) ? DistributeDisjunction((Conditional) predicate, alternative) :
                //(predicate is Disjunction) ? RewriteDisjunction ((Disjunction) predicate, alternative):

                //(predicate is Sequence2) ?  RewriteDisjunction ((Sequence2) predicate, alternative) :
                //(predicate is Sequence3) ? Unimplemented() :

                //(predicate is Variable &&
                // alternative is Variable &&
                // ((Variable) predicate).Name == ((Variable) alternative).Name) ? Unimplemented():
                (! Configuration.EnableDisjunctionSpecialization) ? new Disjunction (predicate, alternative) :
                (predicate is Argument) ? DisjunctionA.Make ((Argument) predicate, alternative) :
                (predicate is Quotation) ? DisjunctionQ.Make ((Quotation) predicate, alternative) :
                (predicate is StaticVariable) ? DisjunctionS.Make ((StaticVariable) predicate, alternative) :
                (alternative is Argument) ? DisjunctionXA.Make (predicate, (Argument) alternative) :
                (alternative is Quotation) ? DisjunctionXQ.Make (predicate, (Quotation) alternative) :
                (alternative is StaticVariable) ? DisjunctionXS.Make (predicate, (StaticVariable) alternative) :
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
            Warm("-");
            NoteCalls(this.predicate);
            predicateTypeHistogram.Note(this.predicateType);
            SCode.location = "Disjunction";
#endif
            Environment env = environment;
            Control pred = this.predicate;
            object ev;
            while (pred.EvalStep(out ev, ref pred, ref env)) { };
#if DEBUG
            SCode.location = "Disjunction";
#endif
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
                SCode.location = "-";
                NoteCalls(this.alternative);
                alternativeTypeHistogram.Note(this.alternativeType);
                SCode.location = "Disjunction";
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

    [Serializable]
    class DisjunctionA : Disjunction
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int predicateOffset;

        protected DisjunctionA (Argument predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.predicateOffset = predicate.Offset;
        }

        public static SCode Make (Argument predicate, SCode alternative)
        {
            return
                (predicate is Argument0) ? DisjunctionA0.Make ((Argument0) predicate, alternative) :
                (predicate is Argument1) ? DisjunctionA1.Make ((Argument1) predicate, alternative) :
                (alternative is Argument) ? DisjunctionAA.Make (predicate, (Argument) alternative) :
                (alternative is Quotation) ? DisjunctionAQ.Make (predicate, (Quotation) alternative) :
                (alternative is StaticVariable) ? DisjunctionAS.Make (predicate, (StaticVariable) alternative) :
                new DisjunctionA (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionA");
#endif
            answer = environment.ArgumentValue (this.predicateOffset);
            if (answer is bool && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "DisjunctionA";
#endif
                // tail call alternative
                expression = this.alternative;
                return true;
            }
            else {
                // return answer
                return false;
            }
        }
 }

    [Serializable]
    class DisjunctionA0 : DisjunctionA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected DisjunctionA0 (Argument0 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (Argument0 predicate, SCode alternative)
        {
            return
                (alternative is Argument) ? DisjunctionA0A.Make (predicate, (Argument) alternative) :
                (alternative is Quotation) ? DisjunctionA0Q.Make (predicate, (Quotation) alternative) :
                (alternative is StaticVariable) ? DisjunctionA0S.Make (predicate, (StaticVariable) alternative) :
                new DisjunctionA0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionA0");
#endif
            answer = environment.Argument0Value;
            if (answer is bool && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "DisjunctionA0";
#endif
                // tail call alternative
                expression = this.alternative;
                return true;
            }
            else {
                // return answer
                return false;
            }
        }
    }

    [Serializable]
    class DisjunctionA0A : DisjunctionA0
    {
        public readonly int alternativeOffset;
        protected DisjunctionA0A (Argument0 predicate, Argument alternative)
            : base (predicate, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (Argument0 predicate, Argument alternative)
        {
            return
                (alternative is Argument0) ? DisjunctionA0A0.Make (predicate, (Argument0) alternative) :
                (alternative is Argument1) ? DisjunctionA0A1.Make (predicate, (Argument1) alternative) :
                new DisjunctionA0A (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionA0A");
#endif
            answer = environment.Argument0Value;
            if (answer is bool && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionA0A0 : DisjunctionA0A
    {
        protected DisjunctionA0A0 (Argument0 predicate, Argument0 alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (Argument0 predicate, Argument0 alternative)
        {
            Debugger.Break ();
            return
                new DisjunctionA0A0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionA0A0");
#endif
            answer = environment.Argument0Value;
            if (answer is bool && (bool) answer == false) {
                answer = environment.Argument0Value;
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionA0A1 : DisjunctionA0A
    {
        protected DisjunctionA0A1 (Argument0 predicate, Argument1 alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (Argument0 predicate, Argument1 alternative)
        {
            return
                new DisjunctionA0A1 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionA0A1");
#endif
            answer = environment.Argument0Value;
            if (answer is bool && (bool) answer == false) {
                answer = environment.Argument1Value;
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionA0Q : DisjunctionA0
    {
        public readonly object alternativeValue;
        protected DisjunctionA0Q (Argument0 predicate, Quotation alternative)
            : base (predicate, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (Argument0 predicate, Quotation alternative)
        {
            return
                new DisjunctionA0Q (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionA0Q");
#endif
            answer = environment.Argument0Value;
            if (answer is bool && (bool) answer == false) {
                answer = this.alternativeValue;
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionA0S : DisjunctionA0
    {
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected DisjunctionA0S (Argument0 predicate, StaticVariable alternative)
            : base (predicate, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (Argument0 predicate, StaticVariable alternative)
        {
            return
                new DisjunctionA0S (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionA0S");
#endif
            answer = environment.Argument0Value;
            if (answer is bool && (bool) answer == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionA1 : DisjunctionA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected DisjunctionA1 (Argument1 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (Argument1 predicate, SCode alternative)
        {
            return
                //(alternative is Argument) ? DisjunctionA1A.Make (predicate, (Argument) alternative) :
                //(alternative is Quotation) ? DisjunctionA1Q.Make (predicate, (Quotation) alternative) :
                //(alternative is StaticVariable) ? DisjunctionA1S.Make (predicate, (StaticVariable) alternative) :
                new DisjunctionA1 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionA1");
#endif
            answer = environment.Argument1Value;
            if (answer is bool && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "DisjunctionA1";
#endif
                // tail call alternative
                expression = this.alternative;
                return true;
            }
            else {
                // return answer
                return false;
            }
        }
    }

    [Serializable]
    class DisjunctionA1A : DisjunctionA1
    {
        public readonly int alternativeOffset;
        protected DisjunctionA1A (Argument1 predicate, Argument alternative)
            : base (predicate, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (Argument1 predicate, Argument alternative)
        {
            return
                (alternative is Argument0) ? DisjunctionA1A0.Make (predicate, (Argument0) alternative) :
                (alternative is Argument1) ? DisjunctionA1A1.Make (predicate, (Argument1) alternative) :
                new DisjunctionA1A (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionA1A");
#endif
            answer = environment.Argument1Value;
            if (answer is bool && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionA1A0 : DisjunctionA1A
    {
        protected DisjunctionA1A0 (Argument1 predicate, Argument0 alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (Argument1 predicate, Argument0 alternative)
        {
            return
                new DisjunctionA1A0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionA1A0");
#endif
            answer = environment.Argument1Value;
            if (answer is bool && (bool) answer == false) {
                answer = environment.Argument0Value;
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionA1A1 : DisjunctionA1A
    {
        protected DisjunctionA1A1 (Argument1 predicate, Argument1 alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (Argument1 predicate, Argument1 alternative)
        {
            Debugger.Break ();
            return
                new DisjunctionA1A1 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionA1A1");
#endif
            answer = environment.Argument1Value;
            if (answer is bool && (bool) answer == false) {
                answer = environment.Argument1Value;
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionA1Q : DisjunctionA1
    {
        public readonly object alternativeValue;
        protected DisjunctionA1Q (Argument1 predicate, Quotation alternative)
            : base (predicate, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (Argument1 predicate, Quotation alternative)
        {
            return
                new DisjunctionA1Q (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionA1Q");
#endif
            answer = environment.Argument1Value;
            if (answer is bool && (bool) answer == false) {
                answer = this.alternativeValue;
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionA1S : DisjunctionA1
    {
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected DisjunctionA1S (Argument1 predicate, StaticVariable alternative)
            : base (predicate, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (Argument1 predicate, StaticVariable alternative)
        {
            return
                new DisjunctionA1S (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionA1S");
#endif
            answer = environment.Argument1Value;
            if (answer is bool && (bool) answer == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionAA : DisjunctionA
    {
        public readonly int alternativeOffset;
        protected DisjunctionAA (Argument predicate, Argument alternative)
            : base (predicate, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (Argument predicate, Argument alternative)
        {
            return
                (alternative is Argument0) ? DisjunctionAA0.Make (predicate, (Argument0) alternative) :
                (alternative is Argument1) ? DisjunctionAA1.Make (predicate, (Argument1) alternative) :
                new DisjunctionAA (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionAA");
#endif
            answer = environment.ArgumentValue(this.predicateOffset);
            if (answer is bool && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionAA0 : DisjunctionAA
    {
        protected DisjunctionAA0 (Argument predicate, Argument0 alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (Argument predicate, Argument0 alternative)
        {
            return
                new DisjunctionAA0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionAA0");
#endif
            answer = environment.ArgumentValue(this.predicateOffset);
            if (answer is bool && (bool) answer == false) {
                answer = environment.Argument0Value;
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionAA1 : DisjunctionAA
    {
        protected DisjunctionAA1 (Argument predicate, Argument1 alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (Argument predicate, Argument1 alternative)
        {
            return
                new DisjunctionAA1 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionAA1");
#endif
            answer = environment.ArgumentValue(this.predicateOffset);
            if (answer is bool && (bool) answer == false) {
                answer = environment.Argument1Value;
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionAQ : DisjunctionA
    {
        public readonly object alternativeValue;
        protected DisjunctionAQ (Argument predicate, Quotation alternative)
            : base (predicate, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (Argument predicate, Quotation alternative)
        {
            return
                new DisjunctionAQ (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionAQ");
#endif
            answer = environment.ArgumentValue (this.predicateOffset);
            if (answer is bool && (bool) answer == false) {
                answer = this.alternativeValue;
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionAS : DisjunctionA
    {
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected DisjunctionAS (Argument predicate, StaticVariable alternative)
            : base (predicate, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (Argument predicate, StaticVariable alternative)
        {
            return
                new DisjunctionAS (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionAS");
#endif
            answer = environment.ArgumentValue (this.predicateOffset);
            if (answer is bool && (bool) answer == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionQ : Disjunction
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object predicateValue;

        protected DisjunctionQ (Quotation predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.predicateValue = predicate.Quoted;
        }

        public static SCode Make (Quotation predicate, SCode alternative)
        {
            object predicateValue = predicate.Quoted;
#if DEBUG
            Debug.WriteLine ("Folding constant disjunction");
#endif
            return (predicateValue is bool && (bool) predicateValue == false)
                ? predicate : alternative;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class DisjunctionS : Disjunction
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol predicateName;
        public readonly int predicateOffset;

        protected DisjunctionS (StaticVariable predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.predicateName = predicate.Name;
            this.predicateOffset = predicate.Offset;
        }

        public static SCode Make (StaticVariable predicate, SCode alternative)
        {
            return
                (alternative is Argument) ? DisjunctionSA.Make (predicate, (Argument) alternative) :
                (alternative is Quotation) ? DisjunctionSQ.Make (predicate, (Quotation) alternative) :
                (alternative is StaticVariable) ? DisjunctionSS.Make (predicate, (StaticVariable) alternative) :
                new DisjunctionS (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionS");
#endif
            if (environment.StaticValue (out answer, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();
            if (answer is bool && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "DisjunctionS";
#endif
                // tail call alternative
                expression = this.alternative;
                return true;
            }
            else {
                // return answer
                return false;
            }
        }
    }

    [Serializable]
    class DisjunctionSA : DisjunctionS
    {
        public readonly int alternativeOffset;

        protected DisjunctionSA (StaticVariable predicate, Argument alternative)
            : base (predicate, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (StaticVariable predicate, Argument alternative)
        {
            return
                (alternative is Argument0) ? DisjunctionSA0.Make (predicate, (Argument0) alternative) :
                (alternative is Argument1) ? DisjunctionSA1.Make (predicate, (Argument1) alternative) :
                new DisjunctionSA (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionSA");
#endif
            if (environment.StaticValue (out answer, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();
            if (answer is bool && (bool) answer == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionSA0 : DisjunctionSA
    {
        protected DisjunctionSA0 (StaticVariable predicate, Argument0 alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (StaticVariable predicate, Argument0 alternative)
        {
            return
                new DisjunctionSA0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionSA0");
#endif
            if (environment.StaticValue (out answer, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();
            if (answer is bool && (bool) answer == false) {
                answer = environment.Argument0Value;
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionSA1 : DisjunctionSA
    {
        protected DisjunctionSA1 (StaticVariable predicate, Argument1 alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (StaticVariable predicate, Argument1 alternative)
        {
            return
                new DisjunctionSA1 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionSA0");
#endif
            if (environment.StaticValue (out answer, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();
            if (answer is bool && (bool) answer == false) {
                answer = environment.Argument1Value;
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionSQ : DisjunctionS
    {
        public readonly object alternativeValue;

        protected DisjunctionSQ (StaticVariable predicate, Quotation alternative)
            : base (predicate, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (StaticVariable predicate, Quotation alternative)
        {
            return
                new DisjunctionSQ (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionSQ");
#endif
            if (environment.StaticValue (out answer, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();
            if (answer is bool && (bool) answer == false) {
                answer = this.alternativeValue;
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionSS : DisjunctionS
    {
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected DisjunctionSS (StaticVariable predicate, StaticVariable alternative)
            : base (predicate, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (StaticVariable predicate, StaticVariable alternative)
        {
            return
                new DisjunctionSS (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("DisjunctionSS");
#endif
            if (environment.StaticValue (out answer, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();
            if (answer is bool && (bool) answer == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class DisjunctionXA : Disjunction
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;

        protected DisjunctionXA (SCode predicate, Argument alternative)
            : base (predicate, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (SCode predicate, Argument alternative)
        {
            return
                (alternative is Argument0) ? DisjunctionXA0.Make (predicate, (Argument0) alternative) :
                (alternative is Argument1) ? DisjunctionXA1.Make (predicate, (Argument1) alternative) :
                new DisjunctionXA (predicate, alternative);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "DisjunctionXA";
#endif
            Environment env = environment;
            Control pred = this.predicate;
            object ev;
            while (pred.EvalStep (out ev, ref pred, ref env)) { };
#if DEBUG
            SCode.location = "DisjunctionXA";
#endif
            if (ev == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new DisjunctionFrame (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            if (ev is bool && (bool) ev == false) {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
            else {
                answer = ev;
                return false;
            }
        }
    }

    [Serializable]
    class DisjunctionXA0 : DisjunctionXA
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif
        protected DisjunctionXA0 (SCode predicate, Argument0 alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (SCode predicate, Argument0 alternative)
        {
            return
                new DisjunctionXA0 (predicate, alternative);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "DisjunctionXA0";
#endif
            Environment env = environment;
            Control pred = this.predicate;
            object ev;
            while (pred.EvalStep (out ev, ref pred, ref env)) { };
#if DEBUG
            SCode.location = "DisjunctionXA0";
#endif
            if (ev == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new DisjunctionFrame (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            if (ev is bool && (bool) ev == false) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
                answer = ev;
                return false;
            }
        }
    }


    [Serializable]
    class DisjunctionXA1 : DisjunctionXA
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif
        protected DisjunctionXA1 (SCode predicate, Argument1 alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (SCode predicate, Argument1 alternative)
        {
            return
                new DisjunctionXA1 (predicate, alternative);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "DisjunctionXA1";
#endif
            Environment env = environment;
            Control pred = this.predicate;
            object ev;
            while (pred.EvalStep (out ev, ref pred, ref env)) { };
#if DEBUG
            SCode.location = "DisjunctionXA1";
#endif
            if (ev == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new DisjunctionFrame (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            if (ev is bool && (bool) ev == false) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
                answer = ev;
                return false;
            }
        }
    }

    [Serializable]
    class DisjunctionXQ : Disjunction
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected DisjunctionXQ (SCode predicate, Quotation alternative)
            : base (predicate, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (SCode predicate, Quotation alternative)
        {
            return
                new DisjunctionXQ (predicate, alternative);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "DisjunctionXQ";
#endif
            Environment env = environment;
            Control pred = this.predicate;
            object ev;
            while (pred.EvalStep (out ev, ref pred, ref env)) { };
#if DEBUG
            SCode.location = "DisjunctionXQ";
#endif
            if (ev == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new DisjunctionFrame (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            if (ev is bool && (bool) ev == false) {
                answer = this.alternativeValue;
                return false;
            }
            else {
                answer = ev;
                return false;
            }
        }
    }


    [Serializable]
    class DisjunctionXS : Disjunction
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected DisjunctionXS (SCode predicate, StaticVariable alternative)
            : base (predicate, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (SCode predicate, StaticVariable alternative)
        {
            return
                new DisjunctionXS (predicate, alternative);

        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
            SCode.location = "DisjunctionXS";
#endif
            Environment env = environment;
            Control pred = this.predicate;
            object ev;
            while (pred.EvalStep (out ev, ref pred, ref env)) { };
#if DEBUG
            SCode.location = "DisjunctionXS";
#endif
            if (ev == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new DisjunctionFrame (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            if (ev is bool && (bool) ev == false) {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
                answer = ev;
                return false;
            }
        }
    }

}
