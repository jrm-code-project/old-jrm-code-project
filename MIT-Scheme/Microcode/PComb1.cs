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
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly Type randType;
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
                (rator == Primitive.Caar) ? PrimitiveCaar.Make (rator, rand) :
                (rator == Primitive.Cdr) ? PrimitiveCdr.Make (rator, rand) :
                (rator == Primitive.CharToInteger) ? PrimitiveCharToInteger.Make (rator, rand) :
                //(rator == Primitive.FixnumAdd1) ? PrimitiveFixnumAdd1.Make (rator, rand) :
                (rator == Primitive.FixnumNot) ? PrimitiveFixnumNot.Make (rator, rand) :
                (rator == Primitive.IntegerToChar) ? PrimitiveIntegerToChar.Make (rator, rand) :
                (rator == Primitive.IsBigFixnum) ? PrimitiveIsType<long>.Make (rator, rand) :
                (rator == Primitive.IsBigFlonum) ? PrimitiveIsType<double>.Make (rator, rand) :
                (rator == Primitive.IsChar) ? PrimitiveIsType<char>.Make (rator, rand) :
                (rator == Primitive.IsComplex) ? PrimitiveIsType<Complex>.Make (rator, rand) :
                (rator == Primitive.IsEntity) ? PrimitiveIsType<Entity>.Make (rator, rand):
                (rator == Primitive.IsIntegerZero) ? PrimitiveIsIntegerZero.Make (rator, rand) :
                //(rator == Primitive.IsExtendedProcedure) ? PrimitiveIsType<StandardExtendedClosure>.Make (rator, rand) :
                (rator == Primitive.IsFixnum) ? PrimitiveIsType<int>.Make (rator, rand) :
                //(rator == Primitive.IsNegative) ? PrimitiveIsNegative.Make (rator, rand) :
                (rator == Primitive.IsNull) ? PrimitiveIsNull.Make (rator, rand) :
                (rator == Primitive.IsPair) ? PrimitiveIsType<Cons>.Make (rator, rand) :
                (rator == Primitive.IsPrimitiveProcedure) ? PrimitiveIsType<Primitive>.Make (rator, rand) :
                (rator == Primitive.IsRatnum) ? PrimitiveIsType<Ratnum>.Make (rator, rand) :
                (rator == Primitive.IsRecord) ? PrimitiveIsType<Record>.Make (rator, rand) :
                //(rator == Primitive.IsSharpT) ? PrimitiveIsSharpT.Make (rator, rand) :
                (rator == Primitive.IsString) ? PrimitiveIsType<char []>.Make (rator, rand) :
                (rator == Primitive.IsSymbol) ? PrimitiveIsSymbol.Make (rator, rand) :
                (rator == Primitive.IsUninternedSymbol) ? PrimitiveIsUninternedSymbol.Make (rator, rand) :
                (rator == Primitive.IsVector) ? PrimitiveIsType<object []>.Make (rator, rand) :
                (rator == Primitive.Not) ? PrimitiveNot.Make (rator, rand) :
                (rator == Primitive.ObjectType) ? PrimitiveObjectType.Make (rator, rand) :
                (rator == Primitive.StringAllocate) ? PrimitiveStringAllocate.Make (rator, rand) :
                (rator == Primitive.StringLength) ? PrimitiveStringLength.Make (rator, rand) :
                (rator == Primitive.SystemPairCar) ? PrimitiveSystemPairCar.Make (rator, rand) :
                (rator == Primitive.SystemPairCdr) ? PrimitiveSystemPairCdr.Make (rator, rand) :
                (rator == Primitive.VectorLength) ? PrimitiveVectorLength.Make (rator, rand) :
                (rator == Primitive.IsWeakCons) ? PrimitiveIsType<WeakCons>.Make (rator, rand) :
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
            SCode.location = "PrimitiveCombination1";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination1";
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
           // Console.WriteLine (this.procedure.Name.ToString ());
           // if (this.procedure.Name.ToString () == "PROCEDURE?") Debugger.Break ();
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
            else {
                return false;
            }
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
        public readonly int offset;

        protected PrimitiveCombination1A (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveCombination1A0.Make (rator, (Argument0) rand) :
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
            SCode.location = this.procedure.Name.ToString ();
            //SCode.location = "PrimitiveCombination1A0";
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
    /// A call to a primitive with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCombination1S : PrimitiveCombination1
    {
#if DEBUG
        static Histogram<Primitive1> ratorHistogram = new Histogram<Primitive1> ();
#endif
        //public readonly object randValue;
        public readonly Symbol randName;
        public readonly int randOffset;

        PrimitiveCombination1S (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.randName = arg0.Name;
            this.randOffset = arg0.Offset;
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
            SCode.location = this.procedure.Name.ToString ();
            //SCode.location = "PrimitiveCombination1S";
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.randName, this.randOffset)) {
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
                (rand is PrimitiveCar) ? PrimitiveCombination1.Make (Primitive.Caar, ((PrimitiveCar) rand).Operand) :
                (rand is StaticVariable) ? PrimitiveCarS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveCarQ.Make (rator, (Quotation) rand) :
                new PrimitiveCar (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PrimitiveCar";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCar";
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
                new PrimitiveCarA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCarA");
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
            Warm ("PrimitiveCarA0");
#endif
            answer = ((Cons) environment.Argument0Value).Car;
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
        public readonly Symbol name;
        public readonly int offset;

        PrimitiveCarS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.name = arg0.Name;
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
            Warm ("PrimitiveCarS");
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.name, this.offset)) {
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
            Warm ("PrimitiveCarQ");
#endif
            throw new NotImplementedException();
        }
    }

    #endregion

    #region Caar

    [Serializable]
    class PrimitiveCaar : PrimitiveCombination1
    {
        protected PrimitiveCaar (Primitive1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveCaarA.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveCaarS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveCaarQ.Make (rator, (Quotation) rand) :
                new PrimitiveCaar (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PrimitiveCaar";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCaar";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = ((Cons) ((Cons) ev0).Car).Car;
            return false;
        }
    }

    /// <summary>
    /// A call to Caar with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveCaarA : PrimitiveCaar
    {
        public readonly int offset;

        protected PrimitiveCaarA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveCaarA0.Make (rator, (Argument0) rand) :
                new PrimitiveCaarA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCaarA");
#endif
            answer = ((Cons) ((Cons) environment.ArgumentValue (this.offset)).Car).Car;
            return false;
        }
    }

    /// <summary>
    /// A call to Caar with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCaarA0 : PrimitiveCaarA
    {
        PrimitiveCaarA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveCaarA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCaarA0");
#endif
            answer = ((Cons) ((Cons) environment.Argument0Value).Car).Car;
            return false;
        }
    }

    /// <summary>
    /// A call to Caar with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCaarS : PrimitiveCaar
    {
        //public readonly object randValue;
        public readonly Symbol name;
        public readonly int offset;

        PrimitiveCaarS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.name = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveCaarS (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCaarS");
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.name, this.offset)) {
                throw new NotImplementedException ();
            }
            answer = ((Cons) ((Cons) randValue).Car).Car;
            return false;
        }
    }

    /// <summary>
    /// A call to Caar with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCaarQ : PrimitiveCaar
    {
        public readonly object randValue;

        PrimitiveCaarQ (Primitive1 procedure, Quotation arg0)
            : base (procedure, arg0)
        {
            this.randValue = arg0.Quoted;
        }

        public static SCode Make (Primitive1 rator, Quotation rand)
        {
            return
                new PrimitiveCaarQ (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCaarQ");
#endif
            throw new NotImplementedException ();
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
            SCode.location = "PrimitiveCdr";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCdr";
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

    #region CharToInteger

    [Serializable]
    class PrimitiveCharToInteger : PrimitiveCombination1
    {
        protected PrimitiveCharToInteger (Primitive1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveCharToIntegerA.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveCharToIntegerS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveCharToIntegerQ.Make (rator, (Quotation) rand) :
                new PrimitiveCharToInteger (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PrimitiveCharToInteger";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCharToInteger";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = (int) (char) ev0;
            return false;
        }
    }

    /// <summary>
    /// A call to CharToInteger with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveCharToIntegerA : PrimitiveCharToInteger
    {
        readonly int offset;

        protected PrimitiveCharToIntegerA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveCharToIntegerA0.Make (rator, (Argument0) rand) :
                new PrimitiveCharToIntegerA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCharToIntegerA");
#endif
            answer = (int) (char) environment.ArgumentValue (this.offset);
            return false;
        }
    }

    /// <summary>
    /// A call to CharToInteger with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCharToIntegerA0 : PrimitiveCharToIntegerA
    {
        PrimitiveCharToIntegerA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveCharToIntegerA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCharToIntegerA0");
#endif
            answer = (int) (char) environment.Argument0Value;
            return false;
        }
    }

    /// <summary>
    /// A call to CharToInteger with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCharToIntegerS : PrimitiveCharToInteger
    {
        //public readonly object randValue;
        readonly Symbol varname;
        readonly int offset;

        PrimitiveCharToIntegerS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.varname = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveCharToIntegerS (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCharToIntegerS");
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.varname, this.offset)) {
                throw new NotImplementedException ();
            }
            answer =  (int)(char) randValue;
            return false;
        }
    }

    /// <summary>
    /// A call to CharToInteger with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCharToIntegerQ : PrimitiveCharToInteger
    {
        public readonly object randValue;

        PrimitiveCharToIntegerQ (Primitive1 procedure, Quotation arg0)
            : base (procedure, arg0)
        {
            this.randValue = arg0.Quoted;
        }

        public static SCode Make (Primitive1 rator, Quotation rand)
        {
            return
                Quotation.Make ((int)(char) rand.Quoted);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCharToIntegerQ");
#endif
            throw new NotImplementedException ();
        }
    }

    #endregion

    #region FixnumNot

    [Serializable]
    class PrimitiveFixnumNot : PrimitiveCombination1
    {
        protected PrimitiveFixnumNot (Primitive1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveFixnumNotA.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveFixnumNotS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveFixnumNotQ.Make (rator, (Quotation) rand) :
                new PrimitiveFixnumNot (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PrimitiveFixnumNot";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveFixnumNot";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = ~((int) ev0);
            return false;
        }
    }

    /// <summary>
    /// A call to FixnumNot with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveFixnumNotA : PrimitiveFixnumNot
    {
        readonly int offset;

        protected PrimitiveFixnumNotA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveFixnumNotA0.Make (rator, (Argument0) rand) :
                new PrimitiveFixnumNotA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveFixnumNotA");
#endif
            answer = ~((int) environment.ArgumentValue (this.offset));
            return false;
        }
    }

    /// <summary>
    /// A call to FixnumNot with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveFixnumNotA0 : PrimitiveFixnumNotA
    {
        PrimitiveFixnumNotA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveFixnumNotA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveFixnumNotA0");
#endif
            answer = ~((int) environment.Argument0Value);
            return false;
        }
    }

    /// <summary>
    /// A call to FixnumNot with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveFixnumNotS : PrimitiveFixnumNot
    {
        //public readonly object randValue;
        readonly Symbol varname;
        readonly int offset;

        PrimitiveFixnumNotS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.varname = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveFixnumNotS (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveFixnumNotS");
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.varname, this.offset)) {
                throw new NotImplementedException ();
            }
            answer = ~((int) randValue);
            return false;
        }
    }

    /// <summary>
    /// A call to FixnumNot with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveFixnumNotQ : PrimitiveFixnumNot
    {
        public readonly object randValue;

        PrimitiveFixnumNotQ (Primitive1 procedure, Quotation arg0)
            : base (procedure, arg0)
        {
            this.randValue = arg0.Quoted;
        }

        public static SCode Make (Primitive1 rator, Quotation rand)
        {
            return
                Quotation.Make (~((int) rand.Quoted));
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveFixnumNotQ");
#endif
            throw new NotImplementedException ();
        }
    }

    #endregion

    #region IntegerToChar

    [Serializable]
    class PrimitiveIntegerToChar : PrimitiveCombination1
    {
        protected PrimitiveIntegerToChar (Primitive1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveIntegerToCharA.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveIntegerToCharS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveIntegerToCharQ.Make (rator, (Quotation) rand) :
                new PrimitiveIntegerToChar (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PrimitiveIntegerToChar";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIntegerToChar";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = (char) (int) ev0;
            return false;
        }
    }

    /// <summary>
    /// A call to IntegerToChar with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveIntegerToCharA : PrimitiveIntegerToChar
    {
        readonly int offset;

        protected PrimitiveIntegerToCharA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIntegerToCharA0.Make (rator, (Argument0) rand) :
                new PrimitiveIntegerToCharA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIntegerToCharA");
#endif
            answer = (char) (int) environment.ArgumentValue (this.offset);
            return false;
        }
    }

    /// <summary>
    /// A call to IntegerToChar with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIntegerToCharA0 : PrimitiveIntegerToCharA
    {
        PrimitiveIntegerToCharA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveIntegerToCharA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIntegerToCharA0");
#endif
            answer = (char)(int) environment.Argument0Value;
            return false;
        }
    }

    /// <summary>
    /// A call to IntegerToChar with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIntegerToCharS : PrimitiveIntegerToChar
    {
        //public readonly object randValue;
        readonly Symbol varname;
        readonly int offset;

        PrimitiveIntegerToCharS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.varname = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveIntegerToCharS (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIntegerToCharS");
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.varname, this.offset)) {
                throw new NotImplementedException ();
            }
            answer = (char)(int) randValue;
            return false;
        }
    }

    /// <summary>
    /// A call to IntegerToChar with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIntegerToCharQ : PrimitiveIntegerToChar
    {
        public readonly object randValue;

        PrimitiveIntegerToCharQ (Primitive1 procedure, Quotation arg0)
            : base (procedure, arg0)
        {
            this.randValue = arg0.Quoted;
        }

        public static SCode Make (Primitive1 rator, Quotation rand)
        {
            return
                Quotation.Make ((char)(int) rand.Quoted);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIntegerToCharQ");
#endif
            throw new NotImplementedException ();
        }
    }

    #endregion

    #region IsIntegerZero

    [Serializable]
    class PrimitiveIsIntegerZero : PrimitiveCombination1
    {
        protected PrimitiveIsIntegerZero (Primitive1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveIsIntegerZeroA.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveIsIntegerZeroS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveIsIntegerZeroQ.Make (rator, (Quotation) rand) :
                new PrimitiveIsIntegerZero (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PrimitiveIsIntegerZero";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsIntegerZero";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = ((int) ev0 == 0) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsIntegerZero with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveIsIntegerZeroA : PrimitiveIsIntegerZero
    {
        readonly int offset;

        protected PrimitiveIsIntegerZeroA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsIntegerZeroA0.Make (rator, (Argument0) rand) :
                new PrimitiveIsIntegerZeroA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntegerZeroA");
#endif
            object temp = environment.ArgumentValue (this.offset);
            answer = ((temp is int) && (int) temp == 0) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsIntegerZero with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsIntegerZeroA0 : PrimitiveIsIntegerZeroA
    {
        PrimitiveIsIntegerZeroA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveIsIntegerZeroA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntegerZeroA0");
#endif
            object i = environment.Argument0Value;
            answer = (
                ((i is long) && ((long) i == 0)) ||
                ((i is int) && ((int) i == 0))
                )
                 ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsIntegerZero with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsIntegerZeroS : PrimitiveIsIntegerZero
    {
        //public readonly object randValue;
        readonly Symbol varname;
        readonly int offset;

        PrimitiveIsIntegerZeroS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.varname = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveIsIntegerZeroS (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntegerZeroS");
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.varname, this.offset)) {
                throw new NotImplementedException ();
            }
            answer = ((int) randValue == 0) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsIntegerZero with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsIntegerZeroQ : PrimitiveIsIntegerZero
    {
        public readonly object randValue;

        PrimitiveIsIntegerZeroQ (Primitive1 procedure, Quotation arg0)
            : base (procedure, arg0)
        {
            this.randValue = arg0.Quoted;
        }

        public static SCode Make (Primitive1 rator, Quotation rand)
        {
            return
                Quotation.Make (( (int) rand.Quoted == 0) ? Constant.sharpT : Constant.sharpF);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsIntegerZeroQ");
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
                (rand is PrimitiveCar) ? PrimitiveIsTypeCar<SType>.Make (rator, (PrimitiveCar) rand) :
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
    /// A call to IsComplex with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsTypeS<SType> : PrimitiveIsType<SType>
    {
        //public readonly object randValue;
        public readonly Symbol varname;
        public readonly int offset;

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

    #region IsTypeCar

    [Serializable]
    class PrimitiveIsTypeCar<SType> : PrimitiveIsType<SType>
    {
        public readonly SCode randArg;
        protected PrimitiveIsTypeCar (Primitive1 rator, PrimitiveCar rand)
            : base (rator, rand)
        {
            this.randArg = rand.Operand;
        }

        public static new SCode Make (Primitive1 rator, PrimitiveCar rand)
        {
            return
               (rand is PrimitiveCarA) ? PrimitiveIsTypeCarA<SType>.Make (rator, (PrimitiveCarA) rand) :
               (rand is PrimitiveCarS) ? PrimitiveIsTypeCarS<SType>.Make (rator, (PrimitiveCarS) rand) :
               // (rand is Quotation) ? PrimitiveIsTypeCarQ<SType>.Make (rator, (Quotation) rand) :
                new PrimitiveIsTypeCar<SType> (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.randArg);
            SCode.location = "PrimitiveIsTypeCar";
#endif
            Control unev0 = this.randArg;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsTypeCar";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = ((Cons) ev0).Car is SType ? Constant.sharpT : Constant.sharpF;
            return false;
        }

        public SCode InnerOperand
        {
            [DebuggerStepThrough]
            get
            {
                return this.randArg;
            }
        }
    }

    /// <summary>
    /// A call to IsTypeCar with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveIsTypeCarA<SType> : PrimitiveIsTypeCar<SType>
    {
        public readonly int offset;

        protected PrimitiveIsTypeCarA (Primitive1 procedure, PrimitiveCarA arg0)
            : base (procedure, arg0)
        {
            this.offset = arg0.offset;
        }

        public static SCode Make (Primitive1 rator, PrimitiveCarA rand)
        {
            return
                (rand is PrimitiveCarA0) ? PrimitiveIsTypeCarA0<SType>.Make (rator, (PrimitiveCarA0) rand) :
                new PrimitiveIsTypeCarA<SType> (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsTypeCarA");
#endif
            answer = ((Cons) environment.ArgumentValue (this.offset)).Car is SType ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsTypeCar with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsTypeCarA0<SType> : PrimitiveIsTypeCarA<SType>
    {
        PrimitiveIsTypeCarA0 (Primitive1 procedure, PrimitiveCarA0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, PrimitiveCarA0 rand)
        {
            return
                new PrimitiveIsTypeCarA0<SType> (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsTypeCarA0");
#endif
            answer = ((Cons) environment.Argument0Value).Car is SType ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsTypeCarS with a static variable.
    /// </summary>
    [Serializable]
    class PrimitiveIsTypeCarS<SType> : PrimitiveIsTypeCar<SType>
    {
        public readonly Symbol name;
        public readonly int offset;

        protected PrimitiveIsTypeCarS (Primitive1 procedure, PrimitiveCarS arg0)
            : base (procedure, arg0)
        {
            this.name = arg0.name;
            this.offset = arg0.offset;
        }

        public static SCode Make (Primitive1 rator, PrimitiveCarS rand)
        {
            return
                new PrimitiveIsTypeCarS<SType> (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsTypeCarS");
#endif
            object evarg;
            if (environment.StaticValue (out evarg, this.name, this.offset))
                throw new NotImplementedException ();
            answer = ((Cons) evarg).Car is SType ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

//    /// <summary>
//    /// A call to IsType with the car of a static variable.
//    /// </summary>
//    [Serializable]
//    sealed class PrimitiveIsTypeCarS<SType> : PrimitiveIsTypeCar<SType>
//    {
//        //public readonly object randValue;
//        public readonly Symbol varname;
//        public readonly int offset;

//        PrimitiveIsTypeCarS (Primitive1 procedure, StaticVariable arg0)
//            : base (procedure, arg0)
//        {
//            this.varname = arg0.Name;
//            this.offset = arg0.Offset;
//        }

//        public static SCode Make (Primitive1 rator, StaticVariable rand)
//        {
//            return
//                new PrimitiveIsTypeCarS<SType> (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsTypeCarS");
//#endif
//            object randValue;
//            if (environment.StaticValue (out randValue, this.varname, this.offset)) {
//                throw new NotImplementedException ();
//            }
//            answer = randValue is SType ? Constant.sharpT : Constant.sharpF;
//            return false;
//        }
//    }

//    /// <summary>
//    /// A call to IsTypeCar with a quoted argument.
//    /// </summary>
//    [Serializable]
//    sealed class PrimitiveIsTypeCarQ<SType> : PrimitiveIsTypeCar<SType>
//    {
//        readonly object randValue;

//        PrimitiveIsTypeCarQ (Primitive1 procedure, Quotation arg0)
//            : base (procedure, arg0)
//        {
//            this.randValue = arg0.Quoted;
//        }

//        public static SCode Make (Primitive1 rator, Quotation rand)
//        {
//            return
//                new PrimitiveIsTypeCarQ<SType> (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PrimitiveIsTypeCarQ");
//#endif
//            throw new NotImplementedException ();
//        }
//    }

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
    /// A call to IsNull with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsNullS : PrimitiveIsNull
    {
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        PrimitiveIsNullS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.rand0Name = arg0.Name;
            this.rand0Offset = arg0.Offset;
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
            if (environment.StaticValue (out randValue, this.rand0Name, this.rand0Offset)) {
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
        public readonly object randValue;

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

    #region IsSymbol

    [Serializable]
    class PrimitiveIsSymbol : PrimitiveCombination1
    {
        protected PrimitiveIsSymbol (Primitive1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveIsSymbolA.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveIsSymbolS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveIsSymbolQ.Make (rator, (Quotation) rand) :
                new PrimitiveIsSymbol (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PrimitiveIsSymbol";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsSymbol";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
            answer = (ev0 is Symbol && ((Symbol) ev0).TypeCode == TC.INTERNED_SYMBOL) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsSymbol with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveIsSymbolA : PrimitiveIsSymbol
    {
        public readonly int arg0Offset;

        protected PrimitiveIsSymbolA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.arg0Offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsSymbolA0.Make (rator, (Argument0) rand) :
                new PrimitiveIsSymbolA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsSymbolA");
#endif
            object arg = environment.ArgumentValue (this.arg0Offset);
            answer = (arg is Symbol && ((Symbol) arg).TypeCode == TC.INTERNED_SYMBOL) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsSymbol with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsSymbolA0 : PrimitiveIsSymbolA
    {
        PrimitiveIsSymbolA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveIsSymbolA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsSymbolA0");
#endif
            object arg = environment.Argument0Value;
            answer = (arg is Symbol && ((Symbol) arg).TypeCode == TC.INTERNED_SYMBOL) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsSymbol with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsSymbolS : PrimitiveIsSymbol
    {
        //public readonly object randValue;
        public readonly Symbol varname;
        public readonly int offset;

        PrimitiveIsSymbolS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.varname = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveIsSymbolS (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsSymbolS");
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.varname, this.offset)) {
                throw new NotImplementedException ();
            }
            answer = (randValue is Symbol && ((Symbol) randValue).TypeCode == TC.INTERNED_SYMBOL) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsSymbol with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsSymbolQ : PrimitiveIsSymbol
    {
        readonly object randValue;

        PrimitiveIsSymbolQ (Primitive1 procedure, Quotation arg0)
            : base (procedure, arg0)
        {
            this.randValue = arg0.Quoted;
        }

        public static SCode Make (Primitive1 rator, Quotation rand)
        {
            return
                new PrimitiveIsSymbolQ (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsSymbolQ");
            SCode.location = "PrimitiveCombination1";
#endif
           
            throw new NotImplementedException ();
        }
    }

    #endregion

    #region IsUninternedSymbol

    [Serializable]
    class PrimitiveIsUninternedSymbol : PrimitiveCombination1
    {
        protected PrimitiveIsUninternedSymbol (Primitive1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveIsUninternedSymbolA.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveIsUninternedSymbolS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveIsUninternedSymbolQ.Make (rator, (Quotation) rand) :
                new PrimitiveIsUninternedSymbol (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PrimitiveIsUninternedSymbol";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsUninternedSymbol";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
            answer = (ev0 is Symbol && ((Symbol) ev0).TypeCode == TC.UNINTERNED_SYMBOL) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsUninternedSymbol with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveIsUninternedSymbolA : PrimitiveIsUninternedSymbol
    {
        public readonly int arg0Offset;

        protected PrimitiveIsUninternedSymbolA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.arg0Offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsUninternedSymbolA0.Make (rator, (Argument0) rand) :
                new PrimitiveIsUninternedSymbolA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsUninternedSymbolA");
#endif
            object arg = environment.ArgumentValue (this.arg0Offset);
            answer = (arg is Symbol && ((Symbol) arg).TypeCode == TC.UNINTERNED_SYMBOL) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsUninternedSymbol with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsUninternedSymbolA0 : PrimitiveIsUninternedSymbolA
    {
        PrimitiveIsUninternedSymbolA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveIsUninternedSymbolA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsUninternedSymbolA0");
#endif
            object arg = environment.Argument0Value;
            answer = (arg is Symbol && ((Symbol) arg).TypeCode == TC.UNINTERNED_SYMBOL) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsUninternedSymbol with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsUninternedSymbolS : PrimitiveIsUninternedSymbol
    {
        //public readonly object randValue;
        readonly Symbol varname;
        readonly int offset;

        PrimitiveIsUninternedSymbolS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.varname = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveIsUninternedSymbolS (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsUninternedSymbolS");
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.varname, this.offset)) {
                throw new NotImplementedException ();
            }
            answer = (randValue is Symbol && ((Symbol) randValue).TypeCode == TC.UNINTERNED_SYMBOL) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    /// <summary>
    /// A call to IsUninternedSymbol with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveIsUninternedSymbolQ : PrimitiveIsUninternedSymbol
    {
        readonly object randValue;

        PrimitiveIsUninternedSymbolQ (Primitive1 procedure, Quotation arg0)
            : base (procedure, arg0)
        {
            this.randValue = arg0.Quoted;
        }

        public static SCode Make (Primitive1 rator, Quotation rand)
        {
            return
                new PrimitiveIsUninternedSymbolQ (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsUninternedSymbolQ");
            SCode.location = "PrimitiveCombination1";
#endif

            throw new NotImplementedException ();
        }
    }

    #endregion

    #region Not

    [Serializable]
    class PrimitiveNot : PrimitiveCombination1
    {
#if DEBUG
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif
        protected PrimitiveNot (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        static bool IsDoubleNegative (SCode rand)
        {
            return rand is PrimitiveCombination1 &&
                ((PrimitiveCombination1) rand).Operator == Primitive.Not;
        }

        static SCode MakeLiteral (Quotation rand)
        {
            throw new NotImplementedException ();
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveNotA.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveNotS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? MakeLiteral ((Quotation) rand) :
                // IsDoubleNegative (rand) ? Unimplemented() :
                new PrimitiveNot (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            randTypeHistogram.Note (this.randType);
            SCode.location = "PrimitiveNot";
#endif
            Control unev0 = this.arg0;
            Environment env = closureEnvironment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
                //answer = Interpreter.Unwind;
                //closureEnvironment = env;
                //return false;
            }
            answer = (ev0 is bool && (bool) ev0 == false) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveNotA : PrimitiveNot
    {
        public readonly int randOffset;
        protected PrimitiveNotA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.randOffset = arg0.Offset;
        }

        public static PrimitiveNotA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveNotA0.Make (rator, (Argument0) rand) :
                new PrimitiveNotA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
        {
#if DEBUG
            Warm ("PrimitiveNotA");
#endif
            object ev0 = closureEnvironment.ArgumentValue (this.randOffset);
            answer = (ev0 is bool && (bool) ev0 == false) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveNotA0 : PrimitiveNotA
    {
        PrimitiveNotA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveNotA Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveNotA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
        {
#if DEBUG
            Warm ("PrimitiveNotA0");
#endif
            object ev0 = closureEnvironment.Argument0Value;
            answer = (ev0 is bool && (bool) ev0 == false) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveNotS : PrimitiveNot
    {
        public readonly Symbol randName;
        public readonly int randOffset;

        PrimitiveNotS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.randName = arg0.Name;
            this.randOffset = arg0.Offset;
        }

        public static PrimitiveNotS Make (Primitive1 rator, StaticVariable rand)
        {
            return new PrimitiveNotS (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
        {
#if DEBUG
            Warm ("PrimitiveNotS");
#endif
            object ev0;
            if (closureEnvironment.StaticValue (out ev0, this.randName, this.randOffset))
                throw new NotImplementedException ();
            answer = (ev0 is bool && (bool) ev0 == false) ? Constant.sharpT : Constant.sharpF;
            return false;
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
            SCode.location = "PrimitiveObjectType";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveObjectType";
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

    #region StringAllocate

    [Serializable]
    class PrimitiveStringAllocate : PrimitiveCombination1
    {
        protected PrimitiveStringAllocate (Primitive1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveStringAllocateA.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveStringAllocateS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveStringAllocateQ.Make (rator, (Quotation) rand) :
                new PrimitiveStringAllocate (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PrimitiveStringAllocate";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveStringAllocate";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = new char [(int) ev0];
            return false;
        }
    }

    /// <summary>
    /// A call to StringAllocate with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveStringAllocateA : PrimitiveStringAllocate
    {
        readonly int offset;

        protected PrimitiveStringAllocateA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveStringAllocateA0.Make (rator, (Argument0) rand) :
                new PrimitiveStringAllocateA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveStringAllocateA");
#endif
            answer = new char [(int) environment.ArgumentValue (this.offset)];
            return false;
        }
    }

    /// <summary>
    /// A call to StringAllocate with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveStringAllocateA0 : PrimitiveStringAllocateA
    {
        PrimitiveStringAllocateA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveStringAllocateA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveStringAllocateA0");
#endif
            answer = new char [(int) environment.Argument0Value];
            return false;
        }
    }

    /// <summary>
    /// A call to StringAllocate with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveStringAllocateS : PrimitiveStringAllocate
    {
        //public readonly object randValue;
        readonly Symbol varname;
        readonly int offset;

        PrimitiveStringAllocateS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.varname = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveStringAllocateS (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveStringAllocateS");
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.varname, this.offset)) {
                throw new NotImplementedException ();
            }
            answer = new char [(int) randValue];
            return false;
        }
    }

    /// <summary>
    /// A call to StringAllocate with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveStringAllocateQ : PrimitiveStringAllocate
    {
        public readonly int randValue;

        PrimitiveStringAllocateQ (Primitive1 procedure, Quotation arg0)
            : base (procedure, arg0)
        {
            this.randValue = (int) arg0.Quoted;
        }

        public static SCode Make (Primitive1 rator, Quotation rand)
        {
            return
                new PrimitiveStringAllocateQ (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveStringAllocateQ");
#endif
            answer = new char [this.randValue];
            return false;
        }
    }

    #endregion

    #region StringLength

    [Serializable]
    class PrimitiveStringLength : PrimitiveCombination1
    {
        protected PrimitiveStringLength (Primitive1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveStringLengthA.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveStringLengthS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveStringLengthQ.Make (rator, (Quotation) rand) :
                new PrimitiveStringLength (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PrimitiveStringLength";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveStringLength";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = ((char []) ev0).Length;
            return false;
        }
    }

    /// <summary>
    /// A call to StringLength with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveStringLengthA : PrimitiveStringLength
    {
        readonly int offset;

        protected PrimitiveStringLengthA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveStringLengthA0.Make (rator, (Argument0) rand) :
                new PrimitiveStringLengthA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveStringLengthA");
#endif
            answer = ((char []) environment.ArgumentValue (this.offset)).Length;
            return false;
        }
    }

    /// <summary>
    /// A call to StringLength with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveStringLengthA0 : PrimitiveStringLengthA
    {
        PrimitiveStringLengthA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveStringLengthA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveStringLengthA0");
#endif
            answer = ((char []) environment.Argument0Value).Length;
            return false;
        }
    }

    /// <summary>
    /// A call to StringLength with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveStringLengthS : PrimitiveStringLength
    {
        //public readonly object randValue;
        readonly Symbol varname;
        readonly int offset;

        PrimitiveStringLengthS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.varname = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveStringLengthS (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveStringLengthS");
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.varname, this.offset)) {
                throw new NotImplementedException ();
            }
            answer = ((char []) randValue).Length;
            return false;
        }
    }

    /// <summary>
    /// A call to StringLength with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveStringLengthQ : PrimitiveStringLength
    {
        public readonly object randValue;

        PrimitiveStringLengthQ (Primitive1 procedure, Quotation arg0)
            : base (procedure, arg0)
        {
            this.randValue = arg0.Quoted;
        }

        public static SCode Make (Primitive1 rator, Quotation rand)
        {
            return
                Quotation.Make (((char []) rand.Quoted).Length);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveStringLengthQ");
#endif
            throw new NotImplementedException ();
        }
    }

    #endregion

    #region SystemPairCar

    [Serializable]
    class PrimitiveSystemPairCar : PrimitiveCombination1
    {
        protected PrimitiveSystemPairCar (Primitive1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveSystemPairCarA.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveSystemPairCarS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveSystemPairCarQ.Make (rator, (Quotation) rand) :
                new PrimitiveSystemPairCar (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PrimitiveSystemPairCar";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveSystemPairCar";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = ((ISystemPair) ev0).SystemPairCar;
            return false;
        }
    }

    /// <summary>
    /// A call to SystemPairCar with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveSystemPairCarA : PrimitiveSystemPairCar
    {
        public readonly int offset;

        protected PrimitiveSystemPairCarA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveSystemPairCarA0.Make (rator, (Argument0) rand) :
                new PrimitiveSystemPairCarA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveSystemPairCarA");
#endif
            answer = ((ISystemPair) environment.ArgumentValue (this.offset)).SystemPairCar;
            return false;
        }
    }

    /// <summary>
    /// A call to SystemPairCar with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveSystemPairCarA0 : PrimitiveSystemPairCarA
    {
        PrimitiveSystemPairCarA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveSystemPairCarA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveSystemPairCarA0");
#endif
            answer = ((ISystemPair) environment.Argument0Value).SystemPairCar;
            return false;
        }
    }

    /// <summary>
    /// A call to SystemPairCar with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveSystemPairCarS : PrimitiveSystemPairCar
    {
        //public readonly object randValue;
        readonly Symbol varname;
        readonly int offset;

        PrimitiveSystemPairCarS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.varname = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveSystemPairCarS (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveSystemPairCarS");
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.varname, this.offset)) {
                throw new NotImplementedException ();
            }
            answer = ((ISystemPair) randValue).SystemPairCar;
            return false;
        }
    }

    /// <summary>
    /// A call to SystemPairCar with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveSystemPairCarQ : PrimitiveSystemPairCar
    {
        public readonly object randValue;

        PrimitiveSystemPairCarQ (Primitive1 procedure, Quotation arg0)
            : base (procedure, arg0)
        {
            this.randValue = arg0.Quoted;
        }

        public static SCode Make (Primitive1 rator, Quotation rand)
        {
            return
                new PrimitiveSystemPairCarQ (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveSystemPairCarQ");
#endif
            throw new NotImplementedException ();
        }
    }

    #endregion

    #region SystemPairCdr

    [Serializable]
    class PrimitiveSystemPairCdr : PrimitiveCombination1
    {
        protected PrimitiveSystemPairCdr (Primitive1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveSystemPairCdrA.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveSystemPairCdrS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveSystemPairCdrQ.Make (rator, (Quotation) rand) :
                new PrimitiveSystemPairCdr (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PrimitiveSystemPairCdr";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveSystemPairCdr";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = ((ISystemPair) ev0).SystemPairCdr;
            return false;
        }
    }

    /// <summary>
    /// A call to SystemPairCdr with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveSystemPairCdrA : PrimitiveSystemPairCdr
    {
        readonly int offset;

        protected PrimitiveSystemPairCdrA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveSystemPairCdrA0.Make (rator, (Argument0) rand) :
                new PrimitiveSystemPairCdrA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveSystemPairCdrA");
#endif
            answer = ((ISystemPair) environment.ArgumentValue (this.offset)).SystemPairCdr;
            return false;
        }
    }

    /// <summary>
    /// A call to SystemPairCdr with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveSystemPairCdrA0 : PrimitiveSystemPairCdrA
    {
        PrimitiveSystemPairCdrA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveSystemPairCdrA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveSystemPairCdrA0");
#endif
            answer = ((ISystemPair) environment.Argument0Value).SystemPairCdr;
            return false;
        }
    }

    /// <summary>
    /// A call to SystemPairCdr with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveSystemPairCdrS : PrimitiveSystemPairCdr
    {
        //public readonly object randValue;
        readonly Symbol varname;
        readonly int offset;

        PrimitiveSystemPairCdrS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.varname = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveSystemPairCdrS (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveSystemPairCdrS");
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.varname, this.offset)) {
                throw new NotImplementedException ();
            }
            answer = ((ISystemPair) randValue).SystemPairCdr;
            return false;
        }
    }

    /// <summary>
    /// A call to SystemPairCdr with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveSystemPairCdrQ : PrimitiveSystemPairCdr
    {
        public readonly object randValue;

        PrimitiveSystemPairCdrQ (Primitive1 procedure, Quotation arg0)
            : base (procedure, arg0)
        {
            this.randValue = arg0.Quoted;
        }

        public static SCode Make (Primitive1 rator, Quotation rand)
        {
            return
                Quotation.Make (((ISystemPair) rand.Quoted).SystemPairCdr);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveSystemPairCdrQ");
#endif
            throw new NotImplementedException ();
        }
    }

    #endregion

    #region VectorLength

    [Serializable]
    class PrimitiveVectorLength : PrimitiveCombination1
    {
        protected PrimitiveVectorLength (Primitive1 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Argument) ? PrimitiveVectorLengthA.Make (rator, (Argument) rand) :
                (rand is StaticVariable) ? PrimitiveVectorLengthS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? PrimitiveVectorLengthQ.Make (rator, (Quotation) rand) :
                new PrimitiveVectorLength (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PrimitiveVectorLength";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveVectorLength";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            answer = ((object []) ev0).Length;
            return false;
        }
    }

    /// <summary>
    /// A call to VectorLength with an argument.
    /// </summary>
    [Serializable]
    class PrimitiveVectorLengthA : PrimitiveVectorLength
    {
        readonly int offset;

        protected PrimitiveVectorLengthA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveVectorLengthA0.Make (rator, (Argument0) rand) :
                new PrimitiveVectorLengthA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorLengthA");
#endif
            answer = ((object []) environment.ArgumentValue (this.offset)).Length;
            return false;
        }
    }

    /// <summary>
    /// A call to VectorLength with argument0.
    /// </summary>
    [Serializable]
    sealed class PrimitiveVectorLengthA0 : PrimitiveVectorLengthA
    {
        PrimitiveVectorLengthA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return
                new PrimitiveVectorLengthA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorLengthA0");
#endif
            answer = ((object []) environment.Argument0Value).Length;
            return false;
        }
    }

    /// <summary>
    /// A call to VectorLength with a static variable.
    /// </summary>
    [Serializable]
    sealed class PrimitiveVectorLengthS : PrimitiveVectorLength
    {
        //public readonly object randValue;
        readonly Symbol varname;
        readonly int offset;

        PrimitiveVectorLengthS (Primitive1 procedure, StaticVariable arg0)
            : base (procedure, arg0)
        {
            this.varname = arg0.Name;
            this.offset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, StaticVariable rand)
        {
            return
                new PrimitiveVectorLengthS (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorLengthS");
#endif
            object randValue;
            if (environment.StaticValue (out randValue, this.varname, this.offset)) {
                throw new NotImplementedException ();
            }
            answer = ((object []) randValue).Length;
            return false;
        }
    }

    /// <summary>
    /// A call to VectorLength with a quoted argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveVectorLengthQ : PrimitiveVectorLength
    {
        public readonly object randValue;

        PrimitiveVectorLengthQ (Primitive1 procedure, Quotation arg0)
            : base (procedure, arg0)
        {
            this.randValue = arg0.Quoted;
        }

        public static SCode Make (Primitive1 rator, Quotation rand)
        {
            return
                Quotation.Make (((object []) rand.Quoted).Length);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveVectorLengthQ");
#endif
            throw new NotImplementedException ();
        }
    }

    #endregion

}
