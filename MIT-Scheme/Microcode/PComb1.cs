using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    [Serializable]
    class PrimitiveCombination1 : SCode, ISerializable, ISystemPair
    {
#if DEBUG
        static Histogram<Primitive1> ratorHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
        Type randType;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.PCOMB1; } }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Primitive1 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        protected PrimitiveMethod1 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode arg0;

        protected PrimitiveCombination1 (Primitive1 procedure, SCode arg0)
            : base ()
        {
            this.procedure = procedure;
            this.method = procedure.Method;
            this.arg0 = arg0;
#if DEBUG
            this.randType = arg0.GetType();
#endif
        }

        [OnDeserialized ()]
        internal void OnDeserializedMethod (StreamingContext context)
        {
            this.method = procedure.Method;
        }

        static bool IsLet (SCode rand)
        {
            return
                (rand is Combination && ((Combination) rand).Operator is Lambda) ||
                (rand is Combination1 && ((Combination1) rand).Operator is Lambda) ||
                (rand is Combination2 && ((Combination2) rand).Operator is Lambda) // ||
                //(rand is Combination3 && ((Combination3) rand).Operator is Lambda)
                ;
        }

        static SCode RewriteLetN (Primitive1 rator, Combination rand) { throw new NotImplementedException (); }
        static SCode RewriteLet1 (Primitive1 rator, Lambda lambda, SCode operand) 
        {
            throw new NotImplementedException ();
            //return Combination1.Make (Lambda.Make (lambda.Name, lambda.Formals, lambda.Body, lambda.ComputeFreeVariables()), operand);
        }
        static SCode RewriteLet2 (Primitive1 rator, Combination2 rand) { throw new NotImplementedException (); }
        //static SCode RewriteLet3 (Primitive1 rator, Combination3 rand) { throw new NotImplementedException (); }

        static SCode RewriteLet (Primitive1 rator, SCode rand)
        {
            return
                (rand is Combination) ? RewriteLetN (rator, (Combination) rand) :
                (rand is Combination1) ? RewriteLet1 (rator, (Lambda) ((Combination1) rand).Operator, ((Combination1) rand).Operand) :
                (rand is Combination2) ? RewriteLet2 (rator, (Combination2) rand) :
                //(rand is Combination3) ? RewriteLet3 (rator, (Combination3) rand) :
                Unimplemented();
        }

        static SCode RewriteConditional (Primitive1 rator, Conditional rand)
        {
            return Conditional.Make (rand.Predicate,
                                     PrimitiveCombination1.Make (rator, rand.Consequent),
                                     PrimitiveCombination1.Make (rator, rand.Alternative));
        }

        static SCode RewriteDisjunction (Primitive1 rator, Disjunction rand)
        {
            SCode first = rand.Predicate;
            SCode second = rand.Alternative;
            if (first is Variable)
                return Conditional.Make (first,
                                         PrimitiveCombination1.Make (rator, first),
                                         PrimitiveCombination1.Make (rator, second));
            else
                return new PrimitiveCombination1 (rator, rand);
        }

        static SCode RewriteSequence2 (Primitive1 rator, SCode first, SCode second)
        {
            return Sequence2.Make (first, PrimitiveCombination1.Make (rator, second));
        }

        static SCode RewriteSequence3 (Primitive1 rator, SCode first, SCode second, SCode third)
        {
            return Sequence3.Make (first, second, PrimitiveCombination1.Make (rator, third));
        }

        static SCode InlineMake (Primitive1 rator, SCode rand)
        {
            return
                //(rator == Primitive.Add1) ? PrimitiveAdd1.Make (rator, rand) :
                (rator == Primitive.Car) ? PrimitiveCar.Make (rator, rand) :
                //(rator == Primitive.Caar) ? PrimitiveCaar.Make (rator, rand) :
                (rator == Primitive.Cdr) ? PrimitiveCdr.Make (rator, rand) :
                //(rator == Primitive.CharToInteger) ? PrimitiveCharToInteger.Make (rator, rand) :
                //(rator == Primitive.FixnumAdd1) ? PrimitiveFixnumAdd1.Make (rator, rand) :
                (rator == Primitive.IsBigFixnum) ? PrimitiveIsType<long>.Make (rator, rand) :
                (rator == Primitive.IsBigFlonum) ? PrimitiveIsType<double>.Make (rator, rand) :
                (rator == Primitive.IsChar) ? PrimitiveIsType<char>.Make (rator, rand) :
                (rator == Primitive.IsComplex) ? PrimitiveIsType<Complex>.Make (rator, rand) :
                (rator == Primitive.IsFixnum) ? PrimitiveIsType<int>.Make (rator, rand) :
                //(rator == Primitive.IsNegative) ? PrimitiveIsNegative.Make (rator, rand) :
                (rator == Primitive.IsNull) ? PrimitiveIsNull.Make (rator, rand) :
                (rator == Primitive.IsPair) ? PrimitiveIsType<Cons>.Make (rator, rand) :
                (rator == Primitive.IsRatnum) ? PrimitiveIsType<Ratnum>.Make (rator, rand) :
                (rator == Primitive.IsRecord) ? PrimitiveIsType<Record>.Make (rator, rand) :
                //(rator == Primitive.IsSharpT) ? PrimitiveIsSharpT.Make (rator, rand) :
                //(rator == Primitive.IsSymbol) ? PrimitiveIsSymbol.Make (rator, rand) :
                (rator == Primitive.IsVector) ? PrimitiveIsType<object []>.Make (rator, rand) :
                //(rator == Primitive.Not) ? PrimitiveNot.Make (rator, rand) :
                (rator == Primitive.ObjectType) ? PrimitiveObjectType.Make (rator, rand) :
                //(rator == Primitive.SystemPairCar) ? PrimitiveSystemPairCar.Make (rator, rand) :
                //(rator == Primitive.VectorLength) ? PrimitiveVectorLength.Make (rator, rand) :
                //(rand is Conditional) ? RewriteConditional (rator, (Conditional) rand) :
                //(rand is Disjunction) ? RewriteDisjunction (rator, (Disjunction) rand) :
                //(rand is Sequence2) ? RewriteSequence2 (rator, ((Sequence2) rand).First, ((Sequence2) rand).Second) :
                //(rand is Sequence3) ? RewriteSequence3 (rator, ((Sequence3) rand).First, ((Sequence3) rand).Second, ((Sequence3) rand).Third) :
                //IsLet (rand) ? RewriteLet (rator, rand) :
                SpecializedMake (rator, rand);
        }

        static SCode SpecializedMake (Primitive1 rator, SCode rand)
        {
            return
                (!Configuration.EnablePrimitive1Specialization) ? new PrimitiveCombination1 (rator, rand) :
                (rand is Argument) ? PrimitiveCombination1A.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveCombination1S.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveCombination1Q.Make (rator, (Quotation) rand) :
                new PrimitiveCombination1 (rator, rand);
        }

        public static SCode Make (Primitive1 rator, SCode rand)
        {
            if (rator == null) 
                throw new ArgumentNullException ("rator");
            if (rand == null)
                throw new ArgumentNullException ("rand");
            return
                (!Configuration.EnablePrimitiveCombination1Optimization) ? new PrimitiveCombination1 (rator, rand) :
                (!Configuration.EnableInlinePrimitive1) ? SpecializedMake (rator, rand) :
                InlineMake (rator, rand);
        }

        public static SCode Make (Primitive1 rator, object rand)
        {
            if (rator == null) 
                throw new ArgumentNullException ("rator");
            return PrimitiveCombination1.Make (rator, EnsureSCode (rand));
        }

        public Primitive1 Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.procedure;
            }
        }

        public SCode Operand
        {
            [DebuggerStepThrough]
            get
            {
                return this.arg0;
            }
        }

        [SchemePrimitive ("PRIMITIVE-COMBINATION1?", 1, true)]
        public static bool IsPrimitiveCombination1 (out object answer, object arg)
        {
            answer = arg is PrimitiveCombination1;
            return false;
        }

        public override bool CallsTheEnvironment ()
        {
            return this.arg0.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            ratorHistogram.Note (this.procedure);
            randTypeHistogram.Note (this.randType);
            SCode.location = "PrimitiveCombination1.EvalStep";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination1.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
#if DEBUG
            SCode.location = this.procedure.Name.ToString();
            Primitive.hotPrimitives.Note(this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    expression = tci.Expression;
                    environment = tci.Environment;
                    return true;
                }
                else
                    throw new NotImplementedException ();
            }
            else return false;
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            return this.arg0.MutatesAny (formals);
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (PComb1Deserializer));
            info.AddValue ("procedure", this.procedure);
            info.AddValue ("operand", this.arg0);
        }

        #endregion

        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            get
            {
                return this.procedure;
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
                return this.arg0;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialResult rand0 = this.arg0.PartialEval (environment);
            return new PartialResult (rand0.Residual == this.arg0 ? this : PrimitiveCombination1.Make (this.procedure, rand0.Residual));
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            this.arg0.CollectFreeVariables (freeVariableSet);
        }

        internal override SCode SubstituteStatics (object [] statics) 
        {
            SCode newArg0 = this.arg0.SubstituteStatics (statics);
            return (newArg0 == this.arg0) ?
                this :
                PrimitiveCombination1.Make (this.procedure, newArg0);
        }
    }

    [Serializable]
    internal sealed class PComb1Deserializer : IObjectReference
    {
        // This object has no fields (although it could).
        Primitive1 procedure;
        SCode operand;

        // GetRealObject is called after this object is deserialized.
        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public Object GetRealObject (StreamingContext context)
        {
            return PrimitiveCombination1.Make (this.procedure, this.operand);
        }

        public void SetProcedure (Primitive1 value) { this.procedure = value; }
        public void SetOperand (SCode value) { this.operand = value; }
    }

    [Serializable]
    sealed class PrimitiveCombination1Frame0 : SubproblemContinuation<PrimitiveCombination1>, ISystemVector
    {
        public PrimitiveCombination1Frame0 (PrimitiveCombination1 expression, Environment environment)
            : base (expression, environment)
        {
        }

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            throw new NotImplementedException ();
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

    #region Specialized
    /// <summary>
    /// A call to a primitive with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveCombination1A : PrimitiveCombination1
    {
#if DEBUG
        static Histogram<Primitive1> ratorHistogram = new Histogram<Primitive1> ();
#endif
        readonly int offset;

        protected PrimitiveCombination1A (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveCombination1A0.Make (rator, (Argument0) rand) :
                (rand is Argument1) ? PrimitiveCombination1A1.Make (rator, (Argument1) rand) :
                new PrimitiveCombination1A (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            Primitive.hotPrimitives.Note (this.procedure);
            //SCode.location = this.procedure.Name.ToString ();
            SCode.location = "PrimitiveCombination1A";
#endif
            if (this.method (out answer, environment.ArgumentValue(this.offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    expression = tci.Expression;
                    environment = tci.Environment;
                    return true;
                }
                else
                    throw new NotImplementedException ();
            }
            else return false;
        }
    }

    /// <summary>
    /// A call to a primitive with an argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCombination1A0 : PrimitiveCombination1A
    {
#if DEBUG
        static Histogram<Primitive1> ratorHistogram = new Histogram<Primitive1> ();
#endif
        PrimitiveCombination1A0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveCombination1A0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            Primitive.hotPrimitives.Note (this.procedure);
            //SCode.location = this.procedure.Name.ToString ();
            SCode.location = "PrimitiveCombination1A0";
#endif
            if (this.method (out answer, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    expression = tci.Expression;
                    environment = tci.Environment;
                    return true;
                }
                else
                    throw new NotImplementedException ();
            }
            else return false;
        }
    }

    /// <summary>
    /// A call to a primitive with an argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCombination1A1 : PrimitiveCombination1A
    {
#if DEBUG
        static Histogram<Primitive1> ratorHistogram = new Histogram<Primitive1> ();
#endif
        PrimitiveCombination1A1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument1 rand)
        {
            return
                new PrimitiveCombination1A1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            Primitive.hotPrimitives.Note (this.procedure);
            //SCode.location = this.procedure.Name.ToString ();
            SCode.location = "PrimitiveCombination1A1";
#endif
            if (this.method (out answer, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    expression = tci.Expression;
                    environment = tci.Environment;
                    return true;
                }
                else
                    throw new NotImplementedException ();
            }
            else return false;
        }
    }
 
    /// <summary>
    /// A call to a primitive with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCombination1S : PrimitiveCombination1
    {
#if DEBUG
        static Histogram<Primitive1> ratorHistogram = new Histogram<Primitive1> ();
#endif
        //public readonly object randValue;
        readonly Symbol varname;
        readonly int offset;

        PrimitiveCombination1S (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.varname = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveCombination1S (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            Primitive.hotPrimitives.Note (this.procedure);
            //SCode.location = this.procedure.Name.ToString ();
            SCode.location = "PrimitiveCombination1S";
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.varname, this.offset)) {
                throw new NotImplementedException ();
            }

            if (this.method (out answer, randValue)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    expression = tci.Expression;
                    environment = tci.Environment;
                    return true;
                }
                else
                    throw new NotImplementedException ();
            }
            else return false;
        }
    }

    /// <summary>
    /// A call to a primitive with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCombination1Q : PrimitiveCombination1
    {
#if DEBUG
        static Histogram<Primitive1> ratorHistogram = new Histogram<Primitive1> ();
#endif
        public readonly object randValue;

        PrimitiveCombination1Q (Primitive1 procedure, Quotation arg0)
            : base (procedure, arg0)
        {
            this.randValue = arg0.Quoted;
        }

        public static SCode Make (Primitive1 rator, Quotation rand)
        {
            return
                new PrimitiveCombination1Q (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            Primitive.hotPrimitives.Note (this.procedure);
            //SCode.location = this.procedure.Name.ToString();
            SCode.location = "PrimitiveCombination1Q";
#endif
            if (this.method (out answer, this.randValue)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    expression = tci.Expression;
                    environment = tci.Environment;
                    return true;
                }
                else
                    throw new NotImplementedException ();
            }
            else return false;
        }
    }

    #endregion

    #region Car

    [Serializable]
    class PrimitiveCar : PrimitiveCombination1
    {
        protected PrimitiveCar (Primitive1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveCarA.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveCarS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveCarQ.Make (rator, (Quotation) rand) :
                new PrimitiveCar (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PrimitiveCar.EvalStep";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCar.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = ((Cons) ev0).Car;
            return false;
        }
    }

    /// <summary>
    /// A call to CAR with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveCarA : PrimitiveCar
    {
        public readonly int offset;

        protected PrimitiveCarA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveCarA0.Make (rator, (Argument0) rand) :
                (rand is Argument1) ? PrimitiveCarA1.Make (rator, (Argument1) rand) :
                new PrimitiveCarA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCarA.EvalStep");
#endif
            answer = ((Cons) environment.ArgumentValue(this.offset)).Car;
            return false;
        }
    }

    /// <summary>
    /// A call to CAR with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCarA0 : PrimitiveCarA
    {
        PrimitiveCarA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveCarA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCarA0.EvalStep");
#endif
            answer = ((Cons) environment.Argument0Value).Car;
            return false;
        }
    }

    /// <summary>
    /// A call to CAR with an argument1.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCarA1 : PrimitiveCarA
    {
        PrimitiveCarA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument1 rand)
        {
            return
                new PrimitiveCarA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCarA1.EvalStep");
#endif
            answer = ((Cons) environment.Argument1Value).Car;
            return false;
        }
    }

    /// <summary>
    /// A call to CAR with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCarS : PrimitiveCar
    {
        //public readonly object randValue;
        readonly Symbol varname;
        readonly int offset;

        PrimitiveCarS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.varname = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveCarS (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCarS.EvalStep");
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.varname, this.offset)) {
                throw new NotImplementedException ();
            }
            answer = ((Cons) randValue).Car;
            return false;
        }
    }

    /// <summary>
    /// A call to CAR with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCarQ : PrimitiveCar
    {
        public readonly object randValue;

        PrimitiveCarQ (Primitive1 procedure, Quotation arg0)
            : base (procedure, arg0)
        {
            this.randValue = arg0.Quoted;
        }

        public static SCode Make (Primitive1 rator, Quotation rand)
        {
            return
                new PrimitiveCarQ (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCarQ.EvalStep");
#endif
            throw new NotImplementedException();
        }
    }

    #endregion

    #region Cdr

    [Serializable]
    class PrimitiveCdr : PrimitiveCombination1
    {
        protected PrimitiveCdr (Primitive1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveCdrA.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveCdrS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveCdrQ.Make (rator, (Quotation) rand) :
                new PrimitiveCdr (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PrimitiveCdr.EvalStep";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCdr.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = ((Cons) ev0).Cdr;
            return false;
        }
    }

    /// <summary>
    /// A call to Cdr with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveCdrA : PrimitiveCdr
    {
        readonly int offset;

        protected PrimitiveCdrA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveCdrA0.Make (rator, (Argument0) rand) :
                (rand is Argument1) ? PrimitiveCdrA1.Make (rator, (Argument1) rand) :
                new PrimitiveCdrA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCdrA");
#endif
            answer = ((Cons) environment.ArgumentValue (this.offset)).Cdr;
            return false;
        }
    }

    /// <summary>
    /// A call to Cdr with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCdrA0 : PrimitiveCdrA
    {
        PrimitiveCdrA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveCdrA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCdrA0");
#endif
            answer = ((Cons) environment.Argument0Value).Cdr;
            return false;
        }
    }

    /// <summary>
    /// A call to Cdr with an argument1.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCdrA1 : PrimitiveCdrA
    {
        PrimitiveCdrA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument1 rand)
        {
            return
                new PrimitiveCdrA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCdrA1");
#endif
            answer = ((Cons) environment.Argument1Value).Cdr;
            return false;
        }
    }

    /// <summary>
    /// A call to Cdr with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCdrS : PrimitiveCdr
    {
        //public readonly object randValue;
        readonly Symbol varname;
        readonly int offset;

        PrimitiveCdrS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.varname = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveCdrS (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCdrS");
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.varname, this.offset)) {
                throw new NotImplementedException ();
            }
            answer = ((Cons) randValue).Cdr;
            return false;
        }
    }

    /// <summary>
    /// A call to Cdr with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCdrQ : PrimitiveCdr
    {
        public readonly object randValue;

        PrimitiveCdrQ (Primitive1 procedure, Quotation arg0)
            : base (procedure, arg0)
        {
            this.randValue = arg0.Quoted;
        }

        public static SCode Make (Primitive1 rator, Quotation rand)
        {
            return
                Quotation.Make (((Cons) rand.Quoted).Cdr);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCdrQ");
#endif
            throw new NotImplementedException ();
        }
    }

    #endregion

    #region IsType

    [Serializable]
    class PrimitiveIsType<SType> : PrimitiveCombination1
    {
        protected PrimitiveIsType(Primitive1 rator, SCode rand)
            : base(rator, rand)
        {
        }

        public static new SCode Make(Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveIsTypeA<SType>.Make(rator, (Argument)rand) :
                (rand is StaticVariable) ? PrimitiveIsTypeS<SType>.Make(rator, (StaticVariable)rand) :
                (rand is Quotation) ? PrimitiveIsTypeQ<SType>.Make(rator, (Quotation)rand) :
                new PrimitiveIsType<SType>(rator, rand);
        }

        public override bool EvalStep(out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("-");
            NoteCalls(this.arg0);
            SCode.location = "PrimitiveIsType";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep(out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsType";
#endif
            if (ev0 == Interpreter.UnwindStack)
            {
                ((UnwinderState)env).AddFrame(new PrimitiveCombination1Frame0(this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = ev0 is SType ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsType with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveIsTypeA<SType> : PrimitiveIsType<SType>
    {
        public readonly int offset;

        protected PrimitiveIsTypeA(Primitive1 procedure, Argument arg0)
            : base(procedure, arg0)
        {
            this.offset = arg0.Offset;
        }

        public static SCode Make(Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsTypeA0<SType>.Make(rator, (Argument0)rand) :
                (rand is Argument1) ? PrimitiveIsTypeA1<SType>.Make(rator, (Argument1)rand) :
                new PrimitiveIsTypeA<SType>(rator, rand);
        }

        public override bool EvalStep(out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("PrimitiveIsTypeA");
#endif
            answer = environment.ArgumentValue(this.offset) is SType ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsType with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsTypeA0<SType> : PrimitiveIsTypeA<SType>
    {
        PrimitiveIsTypeA0(Primitive1 procedure, Argument0 arg0)
            : base(procedure, arg0)
        {
        }

        public static SCode Make(Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveIsTypeA0<SType>(rator, rand);
        }

        public override bool EvalStep(out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("PrimitiveIsTypeA0");
#endif
            answer = environment.Argument0Value is SType ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsType with an argument1.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsTypeA1<SType> : PrimitiveIsTypeA<SType>
    {
        PrimitiveIsTypeA1(Primitive1 procedure, Argument1 arg0)
            : base(procedure, arg0)
        {
        }

        public static SCode Make(Primitive1 rator, Argument1 rand)
        {
            return
                new PrimitiveIsTypeA1<SType>(rator, rand);
        }

        public override bool EvalStep(out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("PrimitiveIsTypeA1");
#endif
            answer = environment.Argument1Value is SType ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsComplex with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsTypeS<SType> : PrimitiveIsType<SType>
    {
        //public readonly object randValue;
        readonly Symbol varname;
        readonly int offset;

        PrimitiveIsTypeS(Primitive1 procedure, StaticVariable arg0)
            : base(procedure, arg0)
        {
            this.varname = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make(Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveIsTypeS<SType>(rator, rand);
        }

        public override bool EvalStep(out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("PrimitiveIsTypeS");
#endif
            object randValue;
            if (environment.StaticValue(out randValue, this.varname, this.offset))
            {
                throw new NotImplementedException();
            }
            answer = randValue is SType ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsType with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsTypeQ<SType> : PrimitiveIsType<SType>
    {
        readonly object randValue;

        PrimitiveIsTypeQ(Primitive1 procedure, Quotation arg0)
            : base(procedure, arg0)
        {
            this.randValue = arg0.Quoted;
        }

        public static SCode Make(Primitive1 rator, Quotation rand)
        {
            return
                new PrimitiveIsTypeQ<SType>(rator, rand);
        }

        public override bool EvalStep(out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("PrimitiveIsTypeQ");
#endif
            throw new NotImplementedException();
        }
    }

    #endregion

    #region IsNull

    [Serializable]
    class PrimitiveIsNull : PrimitiveCombination1
    {
        protected PrimitiveIsNull (Primitive1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveIsNullA.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveIsNullS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveIsNullQ.Make (rator, (Quotation) rand) :
                new PrimitiveIsNull (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PrimitiveIsNull";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsNull";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = ev0 == null ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsNull with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveIsNullA : PrimitiveIsNull
    {
        public readonly int arg0Offset;

        protected PrimitiveIsNullA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.arg0Offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsNullA0.Make (rator, (Argument0) rand) :
                (rand is Argument1) ? PrimitiveIsNullA1.Make (rator, (Argument1) rand) :
                new PrimitiveIsNullA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsNullA");
#endif
            answer = environment.ArgumentValue (this.arg0Offset) == null ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsNull with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsNullA0 : PrimitiveIsNullA
    {
        PrimitiveIsNullA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveIsNullA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsNullA0");
#endif
            answer = environment.Argument0Value == null ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsNull with an argument1.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsNullA1 : PrimitiveIsNullA
    {
        PrimitiveIsNullA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument1 rand)
        {
            return
                new PrimitiveIsNullA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PrimitiveIsNullA1";
#endif
            answer = environment.Argument1Value == null ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsNull with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsNullS : PrimitiveIsNull
    {
        //public readonly object randValue;
        readonly Symbol varname;
        readonly int offset;

        PrimitiveIsNullS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.varname = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveIsNullS (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsNullS");
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.varname, this.offset)) {
                throw new NotImplementedException ();
            }

            answer = randValue == null ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsNull with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsNullQ : PrimitiveIsNull
    {
        readonly object randValue;

        PrimitiveIsNullQ (Primitive1 procedure, Quotation arg0)
            : base (procedure, arg0)
        {
            this.randValue = arg0.Quoted;
        }

        public static SCode Make (Primitive1 rator, Quotation rand)
        {
            return
                new PrimitiveIsNullQ (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsNullQ");
            SCode.location = "PrimitiveCombination1";
#endif
            throw new NotImplementedException ();
        }
    }

    #endregion

    #region ObjectType

    [Serializable]
    class PrimitiveObjectType : PrimitiveCombination1
    {
        protected PrimitiveObjectType (Primitive1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveObjectTypeA.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveObjectTypeS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveObjectTypeQ.Make (rator, (Quotation) rand) :
                new PrimitiveObjectType (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PrimitiveObjectType.EvalStep";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveObjectType.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            ObjectModel.PrimitiveObjectType (out answer, ev0);
            return false;
        }
    }

    /// <summary>
    /// A call to ObjectType with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveObjectTypeA : PrimitiveObjectType
    {
        readonly int offset;

        protected PrimitiveObjectTypeA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveObjectTypeA0.Make (rator, (Argument0) rand) :
                (rand is Argument1) ? PrimitiveObjectTypeA1.Make (rator, (Argument1) rand) :
                new PrimitiveObjectTypeA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveObjectTypeA");
#endif
            ObjectModel.PrimitiveObjectType (out answer, environment.ArgumentValue (this.offset));
            return false;
        }
    }

    /// <summary>
    /// A call to ObjectType with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveObjectTypeA0 : PrimitiveObjectTypeA
    {
        PrimitiveObjectTypeA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveObjectTypeA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveObjectTypeA0");
#endif
            ObjectModel.PrimitiveObjectType (out answer, environment.Argument0Value);
            return false;
        }
    }

    /// <summary>
    /// A call to ObjectType with an argument1.
    /// </summary>
    [Serializable]
    sealed class PrimitiveObjectTypeA1 : PrimitiveObjectTypeA
    {
        PrimitiveObjectTypeA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument1 rand)
        {
            return
                new PrimitiveObjectTypeA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveObjectTypeA1");
#endif
            ObjectModel.PrimitiveObjectType (out answer, environment.Argument1Value);
            return false;
        }
    }

    /// <summary>
    /// A call to ObjectType with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveObjectTypeS : PrimitiveObjectType
    {
        //public readonly object randValue;
        readonly Symbol varname;
        readonly int offset;

        PrimitiveObjectTypeS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.varname = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveObjectTypeS (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveObjectTypeS");
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.varname, this.offset)) {
                throw new NotImplementedException ();
            }
            ObjectModel.PrimitiveObjectType (out answer, randValue);
            return false;
        }
    }

    /// <summary>
    /// A call to ObjectType with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveObjectTypeQ : PrimitiveObjectType
    {
        public readonly object randValue;

        PrimitiveObjectTypeQ (Primitive1 procedure, Quotation arg0)
            : base (procedure, arg0)
        {
            this.randValue = arg0.Quoted;
        }

        public static SCode Make (Primitive1 rator, Quotation rand)
        {
            return
                new PrimitiveObjectTypeQ (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveObjectTypeQ");
#endif
            throw new NotImplementedException ();
        }
    }

    #endregion

#region OldCode

      #region Caar

//    [Serializable]
//    class PrimitiveCaar : PrimitiveCombination1
//    {
//        protected PrimitiveCaar (Primitive1 procedure, SCode arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static new SCode Make (Primitive1 rator, SCode rand)
//        {
//            return
//                //(rand is PrimitiveCar) ? PrimitiveCaar.Make (rator, ((PrimitiveCar) rand).Operand) :
//                //(rand is PrimitiveCdr) ? Unimplemented () :
//                (rand is LexicalVariable) ? PrimitiveCaarL.Make (rator, (LexicalVariable) rand) :
//                //isFoldableCxr(rand, Primitive.Car) ? RewriteCaar (((PrimitiveCar) rand).Operand) :
//                //isFoldableCxr(rand, Primitive.Cdr) ? RewriteCadr (((PrimitiveCdr) rand).Operand) :
//                //isFoldableCxr(rand, Primitive.GeneralCarCdr) ? RewriteGeneralCarCdr ((PrimitiveCombination2) rand) :
//                (rand is Quotation) ? Unimplemented () :
//                new PrimitiveCaar (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.arg0);
//            SCode.location = "PrimitiveCaar.EvalStep";
//#endif
//            throw new NotImplementedException ();
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }
//            Cons evpair = ev0 as Cons;
//            if (evpair == null) throw new NotImplementedException ();
//            answer = evpair.Car;
//            return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveCaarL : PrimitiveCaar
//    {
//        public readonly object randName;
//        public readonly int randDepth;
//        public readonly int randOffset;

//        protected PrimitiveCaarL (Primitive1 procedure, LexicalVariable arg0)
//            : base (procedure, arg0)
//        {
//            this.randName = arg0.Name;
//            this.randDepth = arg0.Depth;
//            this.randOffset = arg0.Offset;
//        }

//        public static SCode Make (Primitive1 rator, LexicalVariable rand)
//        {
//            return
//                (rand is Argument) ? PrimitiveCaarA.Make (rator, (Argument) rand) :
//                (rand is LexicalVariable1) ? PrimitiveCaarL1.Make (rator, (LexicalVariable1) rand) :
//                new PrimitiveCaarL (rator, rand);
//        }

//        public object OperandName { get { return this.randName; } }
//        public int OperandDepth { get { return this.randDepth; } }
//        public int OperandOffset { get { return this.randOffset; } }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveCarL.EvalStep");
//#endif
//            throw new NotImplementedException ();
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.randName, this.randDepth, this.randOffset))
//                throw new NotImplementedException ();
//            Cons evpair = ev0 as Cons;
//            if (evpair == null) throw new NotImplementedException ();
//            answer = evpair.Car;
//            return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveCaarA : PrimitiveCaarL
//    {
//        protected PrimitiveCaarA (Primitive1 procedure, Argument arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static SCode Make (Primitive1 rator, Argument rand)
//        {
//            return
//                (rand is Argument0) ? PrimitiveCaarA0.Make (rator, (Argument0) rand) :
//                (rand is Argument1) ? PrimitiveCaarA1.Make (rator, (Argument1) rand) :
//                new PrimitiveCaarA (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveCarA.EvalStep");
//#endif
//            throw new NotImplementedException ();
//            Cons evpair = closureEnvironment.ArgumentValue (this.randOffset) as Cons;
//            if (evpair == null) throw new NotImplementedException ();
//            answer = evpair.Car;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveCaarA0 : PrimitiveCaarA
//    {
//        PrimitiveCaarA0 (Primitive1 procedure, Argument0 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static SCode Make (Primitive1 rator, Argument0 rand)
//        {
//            return new PrimitiveCaarA0 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveCaarA0.EvalStep");
//#endif
//            Cons evpair = closureEnvironment.Argument0Value as Cons;
//            if (evpair == null) throw new NotImplementedException ();
//            Cons temp = evpair.Car as Cons;
//            if (temp == null) throw new NotImplementedException ();
//            answer = temp.Car;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveCaarA1 : PrimitiveCaarA
//    {
//        PrimitiveCaarA1 (Primitive1 procedure, Argument1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static SCode Make (Primitive1 rator, Argument1 rand)
//        {
//            return new PrimitiveCaarA1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveCaarA1.EvalStep");
//#endif
//            throw new NotImplementedException ();
//            Cons ev0 = closureEnvironment.Argument1Value as Cons;
//            if (ev0 == null) throw new NotImplementedException ();
//            answer = ev0.Car;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveCaarL1 : PrimitiveCaarL
//    {
//        PrimitiveCaarL1 (Primitive1 procedure, LexicalVariable1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static SCode Make (Primitive1 rator, LexicalVariable1 rand)
//        {
//            return new PrimitiveCaarL1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveCaarL1.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.randName, this.randOffset))
//                throw new NotImplementedException ();
//            Cons evpair = ev0 as Cons;
//            if (evpair == null) throw new NotImplementedException ();
//            Cons temp = evpair.Car as Cons;
//            if (temp == null) throw new NotImplementedException ();
//            answer = temp.Car;
//            return false;
//        }
//    }

      #endregion

      #region CharToInteger

//    [Serializable]
//    class PrimitiveCharToInteger : PrimitiveCombination1
//    {
//        protected PrimitiveCharToInteger (Primitive1 procedure, SCode arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static new SCode Make (Primitive1 rator, SCode rand)
//        {
//            return
//                (rand is LexicalVariable) ? PrimitiveCharToIntegerL.Make (rator, (LexicalVariable) rand)
//                : (rand is Quotation) ? Unimplemented ()
//                : new PrimitiveCharToInteger (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveCharToInteger.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }
//            answer = (int) (char) ev0;
//            return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveCharToIntegerL : PrimitiveCharToInteger
//    {
//        protected readonly object argName;
//        protected readonly int argDepth;
//        protected readonly int argOffset;

//        protected PrimitiveCharToIntegerL (Primitive1 procedure, LexicalVariable arg0)
//            : base (procedure, arg0)
//        {
//            this.argName = arg0.Name;
//            this.argDepth = arg0.Depth;
//            this.argOffset = arg0.Offset;
//        }

//        public static PrimitiveCharToIntegerL Make (Primitive1 rator, LexicalVariable rand)
//        {
//            return
//                (rand is Argument) ? PrimitiveCharToIntegerA.Make (rator, (Argument) rand)
//                : new PrimitiveCharToIntegerL (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveCharToIntegerL.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
//                throw new NotImplementedException ();
//            answer = (int) (char) ev0;
//            return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveCharToIntegerA : PrimitiveCharToIntegerL
//    {
//        protected PrimitiveCharToIntegerA (Primitive1 procedure, Argument arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveCharToIntegerA Make (Primitive1 rator, Argument rand)
//        {
//            return
//                (rand is Argument0) ? PrimitiveCharToIntegerA0.Make (rator, (Argument0) rand)
//                : (rand is Argument1) ? PrimitiveCharToIntegerA1.Make (rator, (Argument1) rand)
//                : new PrimitiveCharToIntegerA (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveCharToIntegerA.EvalStep");
//#endif
//            answer = (int) (char) closureEnvironment.ArgumentValue (this.argOffset);
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveCharToIntegerA0 : PrimitiveCharToIntegerA
//    {
//        PrimitiveCharToIntegerA0 (Primitive1 procedure, Argument0 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveCharToIntegerA0 Make (Primitive1 rator, Argument0 rand)
//        {
//            return new PrimitiveCharToIntegerA0 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveCharToIntegerA0.EvalStep");
//#endif
//            answer = (int) (char) closureEnvironment.Argument0Value;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveCharToIntegerA1 : PrimitiveCharToIntegerA
//    {
//        PrimitiveCharToIntegerA1 (Primitive1 procedure, Argument1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveCharToIntegerA1 Make (Primitive1 rator, Argument1 rand)
//        {
//            return new PrimitiveCharToIntegerA1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveCharToIntegerA1.EvalStep");
//#endif
//            answer = (int) (char) closureEnvironment.Argument1Value;
//            return false;

//        }
//    }

//    [Serializable]
//    sealed class PrimitiveCharToIntegerL1 : PrimitiveCharToIntegerL
//    {
//        PrimitiveCharToIntegerL1 (Primitive1 procedure, LexicalVariable1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveCharToIntegerL1 Make (Primitive1 rator, LexicalVariable1 rand)
//        {
//            return new PrimitiveCharToIntegerL1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveCharToIntegerL1.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
//                throw new NotImplementedException ();
//            answer = (int) (char) ev0;
//            return false;
//        }
//    }

      #endregion

      #region FixnumAdd1

//    [Serializable]
//    class PrimitiveFixnumAdd1 : PrimitiveCombination1
//    {
//        protected PrimitiveFixnumAdd1 (Primitive1 procedure, SCode arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static new SCode Make (Primitive1 rator, SCode rand)
//        {
//            return
//                (rand is LexicalVariable) ? PrimitiveFixnumAdd1L.Make (rator, (LexicalVariable) rand)
//                : (rand is Quotation) ? Unimplemented ()
//                : new PrimitiveFixnumAdd1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveFixnumAdd1.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }
//            answer = (int) ev0 + 1;
//            return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveFixnumAdd1L : PrimitiveFixnumAdd1
//    {
//        protected readonly object argName;
//        protected readonly int argDepth;
//        protected readonly int argOffset;

//        protected PrimitiveFixnumAdd1L (Primitive1 procedure, LexicalVariable arg0)
//            : base (procedure, arg0)
//        {
//            this.argName = arg0.Name;
//            this.argDepth = arg0.Depth;
//            this.argOffset = arg0.Offset;
//        }

//        public static PrimitiveFixnumAdd1L Make (Primitive1 rator, LexicalVariable rand)
//        {
//            return
//                (rand is Argument) ? PrimitiveFixnumAdd1A.Make (rator, (Argument) rand) :
//                (rand is LexicalVariable1) ? PrimitiveFixnumAdd1L1.Make (rator, (LexicalVariable1) rand) :
//                new PrimitiveFixnumAdd1L (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveFixnumAdd1L.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
//                throw new NotImplementedException ();
//            answer = (int) ev0 + 1;
//            return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveFixnumAdd1A : PrimitiveFixnumAdd1L
//    {
//        protected PrimitiveFixnumAdd1A (Primitive1 procedure, Argument arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveFixnumAdd1A Make (Primitive1 rator, Argument rand)
//        {
//            return
//                (rand is Argument0) ? PrimitiveFixnumAdd1A0.Make (rator, (Argument0) rand)
//                : (rand is Argument1) ? PrimitiveFixnumAdd1A1.Make (rator, (Argument1) rand)
//                : new PrimitiveFixnumAdd1A (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveFixnumAdd1A.EvalStep");
//#endif
//            answer = (int) closureEnvironment.ArgumentValue (this.argOffset) + 1;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveFixnumAdd1A0 : PrimitiveFixnumAdd1A
//    {
//        PrimitiveFixnumAdd1A0 (Primitive1 procedure, Argument0 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveFixnumAdd1A0 Make (Primitive1 rator, Argument0 rand)
//        {
//            return new PrimitiveFixnumAdd1A0 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveFixnumAdd1A0.EvalStep");
//#endif
//            answer = (int) closureEnvironment.Argument0Value + 1;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveFixnumAdd1A1 : PrimitiveFixnumAdd1A
//    {
//        PrimitiveFixnumAdd1A1 (Primitive1 procedure, Argument1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveFixnumAdd1A1 Make (Primitive1 rator, Argument1 rand)
//        {
//            return new PrimitiveFixnumAdd1A1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveFixnumAdd1A1.EvalStep");
//#endif
//            answer = (int) closureEnvironment.Argument1Value + 1;
//            return false;

//        }
//    }

//    [Serializable]
//    sealed class PrimitiveFixnumAdd1L1 : PrimitiveFixnumAdd1L
//    {
//        PrimitiveFixnumAdd1L1 (Primitive1 procedure, LexicalVariable1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveFixnumAdd1L1 Make (Primitive1 rator, LexicalVariable1 rand)
//        {
//            return new PrimitiveFixnumAdd1L1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveFixnumAdd1L1.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
//                throw new NotImplementedException ();
//            answer = (int) ev0 + 1;
//            return false;
//        }
//    }

      #endregion

      #region Add1

//    [Serializable]
//    class PrimitiveAdd1 : PrimitiveCombination1
//    {
//        protected PrimitiveAdd1 (Primitive1 procedure, SCode arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static new SCode Make (Primitive1 rator, SCode rand)
//        {
//            return
//                (rand is LexicalVariable) ? PrimitiveAdd1L.Make (rator, (LexicalVariable) rand) :
//                (rand is Quotation) ? Unimplemented () :
//                new PrimitiveAdd1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveAdd1.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }
//            if (ev0 is int) {
//                answer = (int) ev0 + 1;
//                return false;
//            }
//            return GenericArithmetic.Increment (out answer, ev0);
//        }
//    }

//    [Serializable]
//    class PrimitiveAdd1L : PrimitiveAdd1
//    {
//        protected readonly object argName;
//        protected readonly int argDepth;
//        protected readonly int argOffset;

//        protected PrimitiveAdd1L (Primitive1 procedure, LexicalVariable arg0)
//            : base (procedure, arg0)
//        {
//            this.argName = arg0.Name;
//            this.argDepth = arg0.Depth;
//            this.argOffset = arg0.Offset;
//        }

//        public static PrimitiveAdd1L Make (Primitive1 rator, LexicalVariable rand)
//        {
//            return
//                (rand is Argument) ? PrimitiveAdd1A.Make (rator, (Argument) rand) :
//                (rand is LexicalVariable1) ? PrimitiveAdd1L1.Make (rator, (LexicalVariable1) rand) :
//                new PrimitiveAdd1L (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveAdd1L.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
//                throw new NotImplementedException ();
//            if (ev0 is int) {
//                answer = (int) ev0 + 1;
//                return false;
//            }
//            return GenericArithmetic.Increment (out answer, ev0);
//        }
//    }

//    [Serializable]
//    class PrimitiveAdd1A : PrimitiveAdd1L
//    {
//        protected PrimitiveAdd1A (Primitive1 procedure, Argument arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveAdd1A Make (Primitive1 rator, Argument rand)
//        {
//            return
//                (rand is Argument0) ? PrimitiveAdd1A0.Make (rator, (Argument0) rand)
//                : (rand is Argument1) ? PrimitiveAdd1A1.Make (rator, (Argument1) rand)
//                : new PrimitiveAdd1A (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveAdd1A.EvalStep");
//#endif
//            object ev0 = closureEnvironment.ArgumentValue (this.argOffset);
//            if (ev0 is int) {
//                answer = (int) ev0 + 1;
//                return false;
//            }
//            return GenericArithmetic.Increment (out answer, ev0);
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveAdd1A0 : PrimitiveAdd1A
//    {
//        PrimitiveAdd1A0 (Primitive1 procedure, Argument0 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveAdd1A0 Make (Primitive1 rator, Argument0 rand)
//        {
//            return new PrimitiveAdd1A0 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveAdd1A0.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;
//            if (ev0 is int) {
//                answer = (int) ev0 + 1;
//                return false;
//            }
//            return GenericArithmetic.Increment (out answer, ev0);
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveAdd1A1 : PrimitiveAdd1A
//    {
//        PrimitiveAdd1A1 (Primitive1 procedure, Argument1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveAdd1A1 Make (Primitive1 rator, Argument1 rand)
//        {
//            return new PrimitiveAdd1A1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveAdd1A1.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument1Value;
//            if (ev0 is int) {
//                answer = (int) ev0 + 1;
//                return false;
//            }
//            return GenericArithmetic.Increment (out answer, ev0);
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveAdd1L1 : PrimitiveAdd1L
//    {
//        PrimitiveAdd1L1 (Primitive1 procedure, LexicalVariable1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveAdd1L1 Make (Primitive1 rator, LexicalVariable1 rand)
//        {
//            return new PrimitiveAdd1L1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveAdd1L1.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
//                throw new NotImplementedException ();
//            if (ev0 is int) {
//                answer = (int) ev0 + 1;
//                return false;
//            }
//            return GenericArithmetic.Increment (out answer, ev0);
//        }
//    }

      #endregion

      #region IsNegative

//    [Serializable]
//    class PrimitiveIsNegative : PrimitiveCombination1
//    {
//        protected PrimitiveIsNegative (Primitive1 procedure, SCode arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static new SCode Make (Primitive1 rator, SCode rand)
//        {
//            return
//                (rand is LexicalVariable) ? PrimitiveIsNegativeL.Make (rator, (LexicalVariable) rand)
//                : (rand is Quotation) ? Unimplemented ()
//                : new PrimitiveIsNegative (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsNegative.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }
//            answer = ((int) ev0 < 0) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveIsNegativeL : PrimitiveIsNegative
//    {
//        protected readonly object argName;
//        protected readonly int argDepth;
//        protected readonly int argOffset;

//        protected PrimitiveIsNegativeL (Primitive1 procedure, LexicalVariable arg0)
//            : base (procedure, arg0)
//        {
//            this.argName = arg0.Name;
//            this.argDepth = arg0.Depth;
//            this.argOffset = arg0.Offset;
//        }

//        public static PrimitiveIsNegativeL Make (Primitive1 rator, LexicalVariable rand)
//        {
//            return
//                (rand is Argument) ? PrimitiveIsNegativeA.Make (rator, (Argument) rand)
//                : (rand is LexicalVariable1) ? PrimitiveIsNegativeL1.Make (rator, (LexicalVariable1) rand)
//                : new PrimitiveIsNegativeL (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("SCode.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
//                throw new NotImplementedException ();
//            answer = ((int) ev0 < 0) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveIsNegativeA : PrimitiveIsNegativeL
//    {
//        protected PrimitiveIsNegativeA (Primitive1 procedure, Argument arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveIsNegativeA Make (Primitive1 rator, Argument rand)
//        {
//            return
//                (rand is Argument0) ? PrimitiveIsNegativeA0.Make (rator, (Argument0) rand)
//                : (rand is Argument1) ? PrimitiveIsNegativeA1.Make (rator, (Argument1) rand)
//                : new PrimitiveIsNegativeA (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsNegativeA.EvalStep");
//#endif
//            answer = ((int) closureEnvironment.ArgumentValue (this.argOffset) < 0) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveIsNegativeA0 : PrimitiveIsNegativeA
//    {
//        PrimitiveIsNegativeA0 (Primitive1 procedure, Argument0 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveIsNegativeA0 Make (Primitive1 rator, Argument0 rand)
//        {
//            return new PrimitiveIsNegativeA0 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsNegativeA0.EvalStep");
//#endif
//            answer = ((int) closureEnvironment.Argument0Value < 0) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveIsNegativeA1 : PrimitiveIsNegativeA
//    {
//        PrimitiveIsNegativeA1 (Primitive1 procedure, Argument1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveIsNegativeA1 Make (Primitive1 rator, Argument1 rand)
//        {
//            return new PrimitiveIsNegativeA1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsNegativeA1.EvalStep");
//#endif
//            answer = ((int) closureEnvironment.Argument1Value < 0) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveIsNegativeL1 : PrimitiveIsNegativeL
//    {

//        PrimitiveIsNegativeL1 (Primitive1 procedure, LexicalVariable1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveIsNegativeL1 Make (Primitive1 rator, LexicalVariable1 rand)
//        {
//            return new PrimitiveIsNegativeL1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsNegativeL1.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
//                throw new NotImplementedException ();
//            answer = ((int) ev0 < 0) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

      #endregion

      #region IsPairCar

//    [Serializable]
//    class PrimitiveIsPairCar : PrimitiveIsPair
//    {
//        protected PrimitiveIsPairCar (Primitive1 procedure, PrimitiveCar arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static SCode Make (Primitive1 rator, PrimitiveCar rand)
//        {
//            return
//                (rand is PrimitiveCarL) ? PrimitiveIsPairCarL.Make (rator, (PrimitiveCarL) rand) :
//                //(rand is PrimitiveCarQ) ? Unimplemented () :
//                new PrimitiveIsPairCar (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    class PrimitiveIsPairCarL : PrimitiveIsPairCar
//    {
//        protected readonly object argName;
//        protected readonly int argDepth;
//        protected readonly int argOffset;

//        protected PrimitiveIsPairCarL (Primitive1 procedure, PrimitiveCarL arg0)
//            : base (procedure, arg0)
//        {
//            this.argName = arg0.OperandName;
//            this.argDepth = arg0.OperandDepth;
//            this.argOffset = arg0.OperandOffset;
//        }

//        public static PrimitiveIsPairCarL Make (Primitive1 rator, PrimitiveCarL rand)
//        {
//            return
//                (rand is PrimitiveCarA) ? PrimitiveIsPairCarA.Make (rator, (PrimitiveCarA) rand) :
//                (rand is PrimitiveCarL1) ? PrimitiveIsPairCarL1.Make (rator, (PrimitiveCarL1) rand) :
//                new PrimitiveIsPairCarL (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsPairCarL.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
//                throw new NotImplementedException ();
//            Cons temp = ev0 as Cons;
//            if (temp == null)
//                throw new NotImplementedException ();
//            answer = (temp.Car is Cons) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveIsPairCarA : PrimitiveIsPairCarL
//    {
//        protected PrimitiveIsPairCarA (Primitive1 procedure, PrimitiveCarA arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveIsPairCarA Make (Primitive1 rator, PrimitiveCarA rand)
//        {
//            return
//                (rand is PrimitiveCarA0) ? PrimitiveIsPairCarA0.Make (rator, (PrimitiveCarA0) rand):
//                (rand is PrimitiveCarA1) ? PrimitiveIsPairCarA1.Make (rator, (PrimitiveCarA1) rand):
//                new PrimitiveIsPairCarA (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsPairCarA.EvalStep");
//#endif
//            Unimplemented ();
//            answer = (closureEnvironment.ArgumentValue (this.argOffset) is Cons) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveIsPairCarA0 : PrimitiveIsPairCarA
//    {
//        PrimitiveIsPairCarA0 (Primitive1 procedure, PrimitiveCarA0 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveIsPairCarA0 Make (Primitive1 rator, PrimitiveCarA0 rand)
//        {
//            return new PrimitiveIsPairCarA0 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsPairCarA0.EvalStep");
//#endif
//            Cons arg = closureEnvironment.Argument0Value as Cons;
//            if (arg0 == null) throw new NotImplementedException ("Back out of PrimitiveIsPairCarA0");
//            answer = (arg.Car is Cons) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveIsPairCarA1 : PrimitiveIsPairCarA
//    {
//        PrimitiveIsPairCarA1 (Primitive1 procedure, PrimitiveCarA1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveIsPairCarA1 Make (Primitive1 rator, PrimitiveCarA1 rand)
//        {
//            return new PrimitiveIsPairCarA1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsPairCarA1.EvalStep");
//#endif
//            Unimplemented ();
//            answer = (closureEnvironment.Argument1Value is Cons) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveIsPairCarL1 : PrimitiveIsPairCarL
//    {
//        PrimitiveIsPairCarL1 (Primitive1 procedure, PrimitiveCarL1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveIsPairCarL1 Make (Primitive1 rator, PrimitiveCarL1 rand)
//        {
//            return new PrimitiveIsPairCarL1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsPairCarL1.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
//                throw new NotImplementedException ();
//            Cons temp = ev0 as Cons;
//            if (temp == null) throw new NotImplementedException ();
//            answer = (temp.Car is Cons) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

      #endregion

      #region IsSharpT

//    [Serializable]
//    class PrimitiveIsSharpT : PrimitiveCombination1
//    {
//        protected PrimitiveIsSharpT (Primitive1 procedure, SCode arg0)
//            : base (procedure, arg0)
//        {
//        }

//        static bool IsRedundantTest (SCode rand)
//        {
//            if (rand is PrimitiveCombination1) {
//                Primitive op = ((PrimitiveCombination1) rand).Operator;
//                if (op.Name.ToString() != "SYSTEM-PAIR-CAR")
//                    Debugger.Break ();
//            }
//            else if (rand is PrimitiveCombination2) {
//                Primitive op = ((PrimitiveCombination2) rand).Rator;
//                if (op.Name.ToString() != "GENERAL-CAR-CDR")
//                    Debugger.Break (); 
//            }
//            return false;
//        }

//        public static new SCode Make (Primitive1 rator, SCode rand)
//        {
//            return
//                (rand is LexicalVariable) ? PrimitiveIsSharpTL.Make (rator, (LexicalVariable) rand) :
//                (rand is Quotation) ? Unimplemented() :
//                IsRedundantTest(rand) ? Unimplemented() :
//                new PrimitiveIsSharpT (rator, rand);
//                //: (rand is Quotation) ? Unimplemented ()
//                //: new PrimitiveIsSharpT (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsSharpT.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }
//            answer = (ev0 is Boolean && (bool) ev0) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveIsSharpTL : PrimitiveIsSharpT
//    {
//        protected readonly object argName;
//        protected readonly int argDepth;
//        protected readonly int argOffset;

//        protected PrimitiveIsSharpTL (Primitive1 procedure, LexicalVariable arg0)
//            : base (procedure, arg0)
//        {
//            this.argName = arg0.Name;
//            this.argDepth = arg0.Depth;
//            this.argOffset = arg0.Offset;
//        }

//        public static PrimitiveIsSharpTL Make (Primitive1 rator, LexicalVariable rand)
//        {
//            return
//                (rand is Argument) ? PrimitiveIsSharpTA.Make (rator, (Argument) rand)
//                : (rand is LexicalVariable1) ? PrimitiveIsSharpTL1.Make (rator, (LexicalVariable1) rand)
//                : new PrimitiveIsSharpTL (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsSharpTL.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
//                throw new NotImplementedException ();
//            answer = (ev0 is Boolean && (bool) ev0) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveIsSharpTA : PrimitiveIsSharpTL
//    {
//        protected PrimitiveIsSharpTA (Primitive1 procedure, Argument arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveIsSharpTA Make (Primitive1 rator, Argument rand)
//        {
//            return
//                (rand is Argument0) ? PrimitiveIsSharpTA0.Make (rator, (Argument0) rand)
//                : (rand is Argument1) ? PrimitiveIsSharpTA1.Make (rator, (Argument1) rand)
//                : new PrimitiveIsSharpTA (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsSharpTA.EvalStep");
//#endif
//            object ev0 = closureEnvironment.ArgumentValue (this.argOffset);
//            answer = (ev0 is Boolean && (bool) ev0) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveIsSharpTA0 : PrimitiveIsSharpTA
//    {
//        PrimitiveIsSharpTA0 (Primitive1 procedure, Argument0 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveIsSharpTA Make (Primitive1 rator, Argument0 rand)
//        {
//            return new PrimitiveIsSharpTA0 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsSharpTA0.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;
//            answer = (ev0 is Boolean && (bool) ev0) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveIsSharpTA1 : PrimitiveIsSharpTA
//    {
//        PrimitiveIsSharpTA1 (Primitive1 procedure, Argument1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveIsSharpTA1 Make (Primitive1 rator, Argument1 rand)
//        {
//            return new PrimitiveIsSharpTA1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsSharpTA1.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument1Value;
//            answer = (ev0 is Boolean && (bool) ev0) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveIsSharpTL1 : PrimitiveIsSharpTL
//    {
//        PrimitiveIsSharpTL1 (Primitive1 procedure, LexicalVariable1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveIsSharpTL1 Make (Primitive1 rator, LexicalVariable1 rand)
//        {
//            return new PrimitiveIsSharpTL1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsSharpTL1.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
//                throw new NotImplementedException ();
//            answer = (ev0 is Boolean && (bool) ev0) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

      #endregion

      #region Not

//    [Serializable]
//    class PrimitiveNot : PrimitiveCombination1
//    {
//        protected PrimitiveNot (Primitive1 procedure, SCode arg0)
//            : base (procedure, arg0)
//        {
//        }

//        static bool IsDoubleNegative (SCode rand)
//        {
//            return rand is PrimitiveCombination1 &&
//                ((PrimitiveCombination1) rand).Operator == Primitive.Not;
//        }

//        public static new SCode Make (Primitive1 rator, SCode rand)
//        {
//            return
//                (rand is LexicalVariable) ? PrimitiveNotL.Make (rator, (LexicalVariable) rand) :
//                // IsDoubleNegative (rand) ? Unimplemented() :
//                // (rand is Quotation) ? Unimplemented () :
//                new PrimitiveNot (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveNot.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }
//            answer = (ev0 is Boolean && (bool) ev0 == false) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveNotL : PrimitiveNot
//    {
//        protected readonly object argName;
//        protected readonly int argDepth;
//        protected readonly int argOffset;

//        protected PrimitiveNotL (Primitive1 procedure, LexicalVariable arg0)
//            : base (procedure, arg0)
//        {
//            this.argName = arg0.Name;
//            this.argDepth = arg0.Depth;
//            this.argOffset = arg0.Offset;
//        }

//        public static PrimitiveNotL Make (Primitive1 rator, LexicalVariable rand)
//        {
//            return
//                (rand is Argument) ? PrimitiveNotA.Make (rator, (Argument) rand)
//                : (rand is LexicalVariable1) ? PrimitiveNotL1.Make (rator, (LexicalVariable1) rand)
//                : new PrimitiveNotL (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveNotL.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
//                throw new NotImplementedException ();
//            answer = (ev0 is Boolean && (bool) ev0 == false) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveNotA : PrimitiveNotL
//    {
//        protected PrimitiveNotA (Primitive1 procedure, Argument arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveNotA Make (Primitive1 rator, Argument rand)
//        {
//            return
//                (rand is Argument0) ? PrimitiveNotA0.Make (rator, (Argument0) rand)
//                : (rand is Argument1) ? PrimitiveNotA1.Make (rator, (Argument1) rand)
//                : new PrimitiveNotA (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveNotA.EvalStep");
//#endif
//            object ev0 = closureEnvironment.ArgumentValue (this.argOffset);
//            answer = (ev0 is Boolean && (bool) ev0 == false) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveNotA0 : PrimitiveNotA
//    {
//        PrimitiveNotA0 (Primitive1 procedure, Argument0 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveNotA Make (Primitive1 rator, Argument0 rand)
//        {
//            return new PrimitiveNotA0 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveNotA0.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument0Value;
//            answer = (ev0 is Boolean && (bool) ev0 == false) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveNotA1 : PrimitiveNotA
//    {
//        PrimitiveNotA1 (Primitive1 procedure, Argument1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveNotA1 Make (Primitive1 rator, Argument1 rand)
//        {
//            return new PrimitiveNotA1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveNotA1.EvalStep");
//#endif
//            object ev0 = closureEnvironment.Argument1Value;
//            answer = (ev0 is Boolean && (bool) ev0 == false) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveNotL1 : PrimitiveNotL
//    {
//        PrimitiveNotL1 (Primitive1 procedure, LexicalVariable1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveNotL1 Make (Primitive1 rator, LexicalVariable1 rand)
//        {
//            return new PrimitiveNotL1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveNotL1.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
//                throw new NotImplementedException ();
//            answer = (ev0 is Boolean && (bool) ev0 == false) ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

      #endregion

      #region SystemPairCar

//    [Serializable]
//    class PrimitiveSystemPairCar : PrimitiveCombination1
//    {
//        protected PrimitiveSystemPairCar (Primitive1 procedure, SCode arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static new SCode Make (Primitive1 rator, SCode rand)
//        {
//            return
//                (rand is LexicalVariable) ? PrimitiveSystemPairCarL.Make (rator, (LexicalVariable) rand) :
//                (rand is Quotation) ? Unimplemented () :
//                new PrimitiveSystemPairCar (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.arg0);
//            SCode.location = "PrimitiveSystemPairCar.EvalStep";
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }
//            ISystemPair evpair = ev0 as ISystemPair;
//            if (evpair == null) throw new NotImplementedException ();
//            answer = evpair.SystemPairCar;
//            return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveSystemPairCarL : PrimitiveSystemPairCar
//    {
//        public readonly object randName;
//        public readonly int randDepth;
//        public readonly int randOffset;

//        protected PrimitiveSystemPairCarL (Primitive1 procedure, LexicalVariable arg0)
//            : base (procedure, arg0)
//        {
//            this.randName = arg0.Name;
//            this.randDepth = arg0.Depth;
//            this.randOffset = arg0.Offset;
//        }

//        public static SCode Make (Primitive1 rator, LexicalVariable rand)
//        {
//            return
//                (rand is Argument) ? PrimitiveSystemPairCarA.Make (rator, (Argument) rand) :
//                (rand is LexicalVariable1) ? PrimitiveSystemPairCarL1.Make (rator, (LexicalVariable1) rand) :
//                new PrimitiveSystemPairCarL (rator, rand);
//        }

//        public object OperandName { get { return this.randName; } }
//        public int OperandDepth { get { return this.randDepth; } }
//        public int OperandOffset { get { return this.randOffset; } }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveSystemPairCarL.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.randName, this.randDepth, this.randOffset))
//                throw new NotImplementedException ();
//            ISystemPair evpair = ev0 as ISystemPair;
//            if (evpair == null) throw new NotImplementedException ();
//            answer = evpair.SystemPairCar;
//            return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveSystemPairCarA : PrimitiveSystemPairCarL
//    {
//        protected PrimitiveSystemPairCarA (Primitive1 procedure, Argument arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static SCode Make (Primitive1 rator, Argument rand)
//        {
//            return
//                (rand is Argument0) ? PrimitiveSystemPairCarA0.Make (rator, (Argument0) rand) :
//                (rand is Argument1) ? PrimitiveSystemPairCarA1.Make (rator, (Argument1) rand) :
//                new PrimitiveSystemPairCarA (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveSystemPairCarA.EvalStep");
//#endif
//            ISystemPair evpair = closureEnvironment.ArgumentValue (this.randOffset) as ISystemPair;
//            if (evpair == null) throw new NotImplementedException ();
//            answer = evpair.SystemPairCar;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveSystemPairCarA0 : PrimitiveSystemPairCarA
//    {
//        PrimitiveSystemPairCarA0 (Primitive1 procedure, Argument0 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static SCode Make (Primitive1 rator, Argument0 rand)
//        {
//            return new PrimitiveSystemPairCarA0 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveSystemPairCarA0.EvalStep");
//#endif
//            ISystemPair evpair = closureEnvironment.Argument0Value as ISystemPair;
//            if (evpair == null) throw new NotImplementedException ();
//            answer = evpair.SystemPairCar;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveSystemPairCarA1 : PrimitiveSystemPairCarA
//    {
//        PrimitiveSystemPairCarA1 (Primitive1 procedure, Argument1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static SCode Make (Primitive1 rator, Argument1 rand)
//        {
//            return new PrimitiveSystemPairCarA1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveSystemPairCarA1.EvalStep");
//#endif
//            ISystemPair ev0 = closureEnvironment.Argument1Value as ISystemPair;
//            if (ev0 == null) throw new NotImplementedException ();
//            answer = ev0.SystemPairCar;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveSystemPairCarL1 : PrimitiveSystemPairCarL
//    {
//        PrimitiveSystemPairCarL1 (Primitive1 procedure, LexicalVariable1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static SCode Make (Primitive1 rator, LexicalVariable1 rand)
//        {
//            return new PrimitiveSystemPairCarL1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveSystemPairCarL1.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.randName, this.randOffset))
//                throw new NotImplementedException ();
//            ISystemPair evpair = ev0 as ISystemPair;
//            if (evpair == null) throw new NotImplementedException ();
//            answer = evpair.SystemPairCar;
//            return false;
//        }
    //    }

      #endregion

    #region VectorLength

    //    [Serializable]
//    class PrimitiveVectorLength : PrimitiveCombination1
//    {
//        protected PrimitiveVectorLength (Primitive1 procedure, SCode arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static new SCode Make (Primitive1 rator, SCode rand)
//        {
//            return
//                (rand is LexicalVariable) ? PrimitiveVectorLengthL.Make (rator, (LexicalVariable) rand)
//                : (rand is Quotation) ? Unimplemented ()
//                : new PrimitiveVectorLength (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveVectorLength.EvalStep");
//            NoteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }
//            answer = ((object [])  ev0).Length;
//            return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveVectorLengthL : PrimitiveVectorLength
//    {
//        protected readonly object argName;
//        protected readonly int argDepth;
//        protected readonly int argOffset;

//        protected PrimitiveVectorLengthL (Primitive1 procedure, LexicalVariable arg0)
//            : base (procedure, arg0)
//        {
//            this.argName = arg0.Name;
//            this.argDepth = arg0.Depth;
//            this.argOffset = arg0.Offset;
//        }

//        public static PrimitiveVectorLengthL Make (Primitive1 rator, LexicalVariable rand)
//        {
//            return
//                (rand is Argument) ? PrimitiveVectorLengthA.Make (rator, (Argument) rand)
//                : new PrimitiveVectorLengthL (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveVectorLengthL.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
//                throw new NotImplementedException ();
//            answer = ((object [])  ev0).Length;
//            return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveVectorLengthA : PrimitiveVectorLengthL
//    {
//        protected PrimitiveVectorLengthA (Primitive1 procedure, Argument arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveVectorLengthA Make (Primitive1 rator, Argument rand)
//        {
//            return
//                (rand is Argument0) ? PrimitiveVectorLengthA0.Make (rator, (Argument0) rand)
//                : (rand is Argument1) ? PrimitiveVectorLengthA1.Make (rator, (Argument1) rand)
//                : new PrimitiveVectorLengthA (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveVectorLengthA.EvalStep");
//#endif
//            answer = ((object [])  closureEnvironment.ArgumentValue (this.argOffset)).Length;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveVectorLengthA0 : PrimitiveVectorLengthA
//    {
//        PrimitiveVectorLengthA0 (Primitive1 procedure, Argument0 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveVectorLengthA0 Make (Primitive1 rator, Argument0 rand)
//        {
//            return new PrimitiveVectorLengthA0 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveVectorLengthA0.EvalStep");
//#endif
//            answer = ((object [])  closureEnvironment.Argument0Value).Length;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveVectorLengthA1 : PrimitiveVectorLengthA
//    {
//        PrimitiveVectorLengthA1 (Primitive1 procedure, Argument1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveVectorLengthA1 Make (Primitive1 rator, Argument1 rand)
//        {
//            return new PrimitiveVectorLengthA1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveVectorLengthA1.EvalStep");
//#endif
//            answer = ((object [])  closureEnvironment.Argument1Value).Length;
//            return false;

//        }
//    }

//    [Serializable]
//    sealed class PrimitiveVectorLengthL1 : PrimitiveVectorLengthL
//    {
//        PrimitiveVectorLengthL1 (Primitive1 procedure, LexicalVariable1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static PrimitiveVectorLengthL1 Make (Primitive1 rator, LexicalVariable1 rand)
//        {
//            return new PrimitiveVectorLengthL1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveVectorLengthL1.EvalStep");
//#endif
//            object ev0;
//            if (closureEnvironment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
//                throw new NotImplementedException ();
//            answer = ((object [])  ev0).Length;
//            return false;
//        }
//    }

      #endregion

#endregion
}
