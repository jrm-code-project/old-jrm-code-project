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
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Primitive1 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        protected PrimitiveMethod1 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode arg0;

        protected PrimitiveCombination1 (Primitive1 procedure, SCode arg0)
            : base (TC.PCOMB1)
        {
            this.procedure = procedure;
            this.method = procedure.Method;
            this.arg0 = arg0;
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
                (rand is Combination2 && ((Combination2) rand).Operator is Lambda) ||
                (rand is Combination3 && ((Combination3) rand).Operator is Lambda);
        }

        static SCode RewriteLetN (Primitive1 rator, Combination rand) { throw new NotImplementedException (); }
        static SCode RewriteLet1 (Primitive1 rator, Lambda lambda, SCode operand) 
        {
            return Combination1.Make (Lambda.Make (lambda.Name, lambda.Formals, PrimitiveCombination1.Make (rator, lambda.Body)), operand);
        }
        static SCode RewriteLet2 (Primitive1 rator, Combination2 rand) { throw new NotImplementedException (); }
        static SCode RewriteLet3 (Primitive1 rator, Combination3 rand) { throw new NotImplementedException (); }

        static SCode RewriteLet (Primitive1 rator, SCode rand)
        {
            return
                (rand is Combination) ? RewriteLetN (rator, (Combination) rand) :
                (rand is Combination1) ? RewriteLet1 (rator, (Lambda) ((Combination1) rand).Operator, ((Combination1) rand).Operand) :
                (rand is Combination2) ? RewriteLet2 (rator, (Combination2) rand) :
                (rand is Combination3) ? RewriteLet3 (rator, (Combination3) rand) :
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

        static SCode StandardMake (Primitive1 rator, SCode rand)
        {
            return
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.Add1) ? PrimitiveAdd1.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.Car) ? PrimitiveCar.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.Cdr) ? PrimitiveCdr.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.CharToInteger) ? PrimitiveCharToInteger.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.FixnumAdd1) ? PrimitiveFixnumAdd1.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.IsBigFixnum) ? PrimitiveIsBigFixnum.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.IsBigFlonum) ? PrimitiveIsBigFlonum.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.IsChar) ? PrimitiveIsChar.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.IsComplex) ? PrimitiveIsComplex.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.IsFixnum) ? PrimitiveIsFixnum.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.IsNegative) ? PrimitiveIsNegative.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.IsNull) ? PrimitiveIsNull.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.IsPair) ? PrimitiveIsPair.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.IsRatnum) ? PrimitiveIsRatnum.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.IsRecord) ? PrimitiveIsRecord.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.IsSharpT) ? PrimitiveIsSharpT.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.IsSymbol) ? PrimitiveIsSymbol.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.IsVector) ? PrimitiveIsVector.Make (rator, rand) :
                (Configuration.EnableSuperOperators && 
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.Not) ? PrimitiveNot.Make (rator, rand) :
                (Configuration.EnableSuperOperators && 
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.ObjectType) ? PrimitiveObjectType.Make (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive1 && rator == Primitive.SystemPairCar) ? PrimitiveSystemPairCar.Make (rator, rand) :
                (rand is Conditional) ? RewriteConditional (rator, (Conditional) rand) :
                (rand is Disjunction) ? RewriteDisjunction (rator, (Disjunction) rand) :
                (rand is Sequence2) ? RewriteSequence2 (rator, ((Sequence2) rand).First, ((Sequence2) rand).Second) :
                (rand is Sequence3) ? RewriteSequence3 (rator, ((Sequence3) rand).First, ((Sequence3) rand).Second, ((Sequence3) rand).Third) :
                //IsLet (rand) ? RewriteLet (rator, rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnablePrimitive1Specialization &&
                 rand is LexicalVariable) ? PrimitiveCombination1L.Make (rator, (LexicalVariable) rand) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnablePrimitive1Specialization &&
                 rand is Quotation) ? PrimitiveCombination1Q.Make (rator, (Quotation) rand) :
                new PrimitiveCombination1 (rator, rand);
        }

        static SCode SpecialMake (Primitive1 rator, Quotation rand)
        {
            //if (rator != Primitive.ClearInterrupts
            //    && rator != Primitive.ExitWithValue
            //    && rator != Primitive.FloatingVectorCons
            //    && rator != Primitive.InitializeCCompiledBlock
            //    && (rator != Primitive.ObjectDatum || !(rand.Quoted is Boolean) || (bool) rand.Quoted != false)
            //    && rator != Primitive.SetInterruptEnables
            //    && rator != Primitive.SetTrapState
            //    && rator != Primitive.StringAllocate
            //    && rator != Primitive.SystemVectorSize
            //    && rator != Primitive.RequestInterrupts
            //    )
           // Debugger.Break();

            return 
                StandardMake (rator, rand);
        }

        public static SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is Quotation) ? SpecialMake (rator, (Quotation) rand)
                : StandardMake (rator, rand);
        }

        public static SCode Make (Primitive1 rator, object rand)
        {
            if (rator == null) throw new ArgumentNullException ("rator");
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

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode optArg0 = this.arg0.Bind (ctenv);
            return (optArg0 == this.arg0) ? this
                : PrimitiveCombination1.Make (this.procedure, optArg0);
        }

        public override bool CallsTheEnvironment ()
        {
            return this.arg0.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.arg0);
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

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString();
#endif
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

        public override bool Uses (Symbol formal)
        {
            return this.arg0.Uses (formal);
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
    }

    [Serializable]
    internal sealed class PComb1Deserializer : IObjectReference
    {
        // This object has no fields (although it could).
        Primitive1 procedure;
        SCode operand;

        // GetRealObject is called after this object is deserialized.
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

    [Serializable]
    class PrimitiveCombination1L : PrimitiveCombination1
    {
#if DEBUG
        static Histogram<Primitive1> ratorHistogram = new Histogram<Primitive1> ();
#endif
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveCombination1L (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveCombination1A.Make (rator, (Argument) rand) :
                (rand is LexicalVariable1) ? PrimitiveCombination1L1.Make (rator, (LexicalVariable1) rand) :
                new PrimitiveCombination1L (rator, rand);
        }

        public object OperandName { get { return this.argName; } }
        public int OperandDepth { get { return this.argDepth; } }
        public int OperandOffset { get { return this.argOffset; } }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            object ev;
            if (environment.FastLexicalRef (out ev, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
#if DEBUG
            SCode.location = this.procedure.Name.ToString();
#endif
            if (this.method (out answer, ev)) {
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
    /// A call to a primitive with argument as the argument.
    /// </summary>
    [Serializable]
    class PrimitiveCombination1A : PrimitiveCombination1L
    {
#if DEBUG
        static Histogram<Primitive1> ratorHistogram = new Histogram<Primitive1> ();
#endif
        protected PrimitiveCombination1A (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
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
            Primitive.hotPrimitives.Note (this.procedure);
            ratorHistogram.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString();
#endif
            if (this.method (out answer, environment.ArgumentValue (this.argOffset))) {
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
    /// A call to a primitive with argument0 as the argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCombination1A0 : PrimitiveCombination1A
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
#endif
        PrimitiveCombination1A0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveCombination1A0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString();
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
    /// A call to a primitive with argument1 as the argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCombination1A1 : PrimitiveCombination1A
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
#endif
        PrimitiveCombination1A1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveCombination1A1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            PrimitiveCombination1A1.procedureHistogram.Note (this.procedure);
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString();
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
    /// A call to a primitive with a lexical1 as the argument.
    /// </summary>
    [Serializable]
    sealed class PrimitiveCombination1L1 : PrimitiveCombination1L
    {
#if DEBUG
        static Histogram<Primitive1> ratorHistogram = new Histogram<Primitive1> ();
#endif
        PrimitiveCombination1L1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveCombination1L1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveCombination1L1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = "PrimitiveCombination1L1.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            object ev;
            if (environment.FastLexicalRef1 (out ev, this.argName, this.argOffset))
                throw new NotImplementedException ();
#if DEBUG
            SCode.location = this.procedure.Name.ToString();
#endif
            if (this.method (out answer, ev)) {
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
            SCode.location = this.procedure.Name.ToString();
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

    #region Car

    [Serializable]
    class PrimitiveCar : PrimitiveCombination1
    {
        protected PrimitiveCar (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        static SCode RewriteCaar (SCode operand)
        {
            return BuildGCC (operand, 7); //#b 111
        }

        static SCode RewriteCadr (SCode operand)
        {
            return BuildGCC (operand, 6); // #b110
        }

        static SCode BuildGCC (SCode rand, int code)
        {
            return PrimitiveCombination2.Make (Primitive.GeneralCarCdr, rand, Quotation.Make (code));
        }

        static SCode RewriteGeneralCarCdr (PrimitiveCombination2 operand)
        {
            switch ((int) ((Quotation) operand.Rand1).Quoted) {
                case 4: // 100 => 1100
                    return BuildGCC (operand.Rand0, 12);
                case 6: // 110 => 1110
                    return BuildGCC (operand.Rand0, 14);
                case 12:// #b1100 => #b11100
                    return BuildGCC (operand.Rand0, 28);
                case 16:// #b10000 => #b110000
                    return BuildGCC (operand.Rand0, 48);
                default:
                    throw new NotImplementedException ();
            }
        }

        static bool isFoldableCxr (SCode form, Primitive prim)
        {
            return Configuration.EnableFoldCarCdr &&
                ((form is PrimitiveCombination1 && ((PrimitiveCombination1) form).Operator == prim) ||
                 (form is PrimitiveCombination2 && 
                 ((PrimitiveCombination2) form).Rator == prim &&
                 ((PrimitiveCombination2) form).Rand1 is Quotation));
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveCarL.Make (rator, (LexicalVariable) rand) :
                isFoldableCxr(rand, Primitive.Car) ? RewriteCaar (((PrimitiveCar) rand).Operand) :
                isFoldableCxr(rand, Primitive.Cdr) ? RewriteCadr (((PrimitiveCdr) rand).Operand) :
                isFoldableCxr(rand, Primitive.GeneralCarCdr) ? RewriteGeneralCarCdr ((PrimitiveCombination2) rand) :
                (rand is Quotation) ? Unimplemented() :
                new PrimitiveCar (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.arg0);
            SCode.location = "PrimitiveCar.EvalStep";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            Cons evpair = ev0 as Cons;
            if (evpair == null) throw new NotImplementedException ();
            answer = evpair.Car;
            return false;
        }
    }

    [Serializable]
    class PrimitiveCarL : PrimitiveCar
    {
        public readonly object randName;
        public readonly int randDepth;
        public readonly int randOffset;

        protected PrimitiveCarL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.randName = arg0.Name;
            this.randDepth = arg0.Depth;
            this.randOffset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveCarA.Make (rator, (Argument) rand) :
                (rand is LexicalVariable1) ? PrimitiveCarL1.Make (rator, (LexicalVariable1) rand) :
                new PrimitiveCarL (rator, rand);
        }

        public object OperandName { get { return this.randName; } }
        public int OperandDepth { get { return this.randDepth; } }
        public int OperandOffset { get { return this.randOffset; } }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCarL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.randName, this.randDepth, this.randOffset))
                throw new NotImplementedException ();
            Cons evpair = ev0 as Cons;
            if (evpair == null) throw new NotImplementedException ();
            answer = evpair.Car;
            return false;
        }
    }

    [Serializable]
    class PrimitiveCarA : PrimitiveCarL
    {
        protected PrimitiveCarA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
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
            Cons evpair = environment.ArgumentValue (this.randOffset) as Cons;
            if (evpair == null) throw new NotImplementedException ();
            answer = evpair.Car;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCarA0 : PrimitiveCarA
    {
        PrimitiveCarA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveCarA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCarA0.EvalStep");
#endif
            Cons evpair = environment.Argument0Value as Cons;
            if (evpair == null) throw new NotImplementedException ();
            answer = evpair.Car;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCarA1 : PrimitiveCarA
    {
        PrimitiveCarA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveCarA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCarA1.EvalStep");
#endif
            Cons ev0 = environment.Argument1Value as Cons;
            if (ev0 == null) throw new NotImplementedException ();
            answer = ev0.Car;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCarL1 : PrimitiveCarL
    {
        PrimitiveCarL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveCarL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCarL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.randName, this.randOffset))
                throw new NotImplementedException ();
            Cons evpair = ev0 as Cons;
            if (evpair == null) throw new NotImplementedException ();
            answer = evpair.Car;
            return false;
        }
    }

    #endregion

    #region Cdr

    [Serializable]
    class PrimitiveCdr : PrimitiveCombination1
    {
        protected PrimitiveCdr (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        static SCode RewriteCdar (SCode rand)
        {
            return BuildGCC (rand, 5); // #b101
        }

        static SCode RewriteCddr (SCode rand)
        {
            return BuildGCC (rand, 4); // #b100
        }

        static SCode BuildGCC (SCode rand, int code)
        {
            //Debug.Write ("\n; => GeneralCarCdr " + code.ToString ());
            return PrimitiveCombination2.Make (Primitive.GeneralCarCdr, rand, Quotation.Make (code));
        }

        static SCode RewriteGeneralCarCdr (PrimitiveCombination2 operand)
        {
            switch ((int) ((Quotation) operand.Rand1).Quoted) {
                case 4: // #b100 => #b1000
                    return BuildGCC (operand.Rand0, 8);
                case 6: // #b110 => #b1010
                    return BuildGCC (operand.Rand0, 12);
                case 12: // #b1010 => 10010
                    return BuildGCC (operand.Rand0, 20);
                default:
                    throw new NotImplementedException ();
            }
        }

        static bool isFoldableCxr (SCode form, Primitive prim)
        {
            return Configuration.EnableFoldCarCdr &&
                ((form is PrimitiveCombination1 && ((PrimitiveCombination1) form).Operator == prim) ||
                 (form is PrimitiveCombination2 &&
                 ((PrimitiveCombination2) form).Rator == prim &&
                 ((PrimitiveCombination2) form).Rand1 is Quotation));
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveCdrL.Make (rator, (LexicalVariable) rand) :
                isFoldableCxr(rand, Primitive.Car) ? RewriteCdar (((PrimitiveCar) rand).Operand) :
                isFoldableCxr(rand, Primitive.Cdr) ? RewriteCddr (((PrimitiveCdr) rand).Operand) :
                isFoldableCxr(rand, Primitive.GeneralCarCdr) ? RewriteGeneralCarCdr ((PrimitiveCombination2)rand):
                (rand is Quotation) ? Unimplemented () :
                new PrimitiveCdr (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.arg0);
            SCode.location = "PrimitiveCdr.EvalStep";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            Cons evpair = ev0 as Cons;
            if (evpair == null) throw new NotImplementedException ();
            answer = evpair.Cdr;
            return false;
        }
    }

    [Serializable]
    class PrimitiveCdrL : PrimitiveCdr
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveCdrL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveCdrA.Make (rator, (Argument) rand) :
                (rand is LexicalVariable1) ? PrimitiveCdrL1.Make (rator, (LexicalVariable1) rand) :
                new PrimitiveCdrL (rator, rand);
        }

        public object OperandName { get { return this.argName; } }
        public int OperandDepth { get { return this.argDepth; } }
        public int OperandOffset { get { return this.argOffset; } }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCdrL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            Cons evpair = ev0 as Cons;
            if (evpair == null) throw new NotImplementedException ();
            answer = evpair.Cdr;
            return false;
        }
    }

    [Serializable]
    class PrimitiveCdrA : PrimitiveCdrL
    {
        protected PrimitiveCdrA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveCdrA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveCdrA0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveCdrA1.Make (rator, (Argument1) rand)
                : new PrimitiveCdrA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCdrA.EvalStep");
#endif
            Cons evpair = environment.ArgumentValue (this.argOffset) as Cons;
            if (evpair == null) throw new NotImplementedException ();
            answer = evpair.Cdr;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCdrA0 : PrimitiveCdrA
    {
        PrimitiveCdrA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveCdrA0 Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveCdrA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCdrA0.EvalStep");
#endif
            Cons evpair = environment.Argument0Value as Cons;
            if (evpair == null) throw new NotImplementedException ();
            answer = evpair.Cdr;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCdrA1 : PrimitiveCdrA
    {
        PrimitiveCdrA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveCdrA1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveCdrA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCdrA1.EvalStep");
#endif
            Cons ev0 = environment.Argument1Value as Cons;
            if (ev0 == null) throw new NotImplementedException ();
            answer = ev0.Cdr;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCdrL1 : PrimitiveCdrL
    {
        PrimitiveCdrL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveCdrL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCdrL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            Cons evpair = ev0 as Cons;
            if (evpair == null) throw new NotImplementedException ();
            answer = evpair.Cdr;
            return false;
        }
    }

    #endregion

    #region CharToInteger

    [Serializable]
    class PrimitiveCharToInteger : PrimitiveCombination1
    {
        protected PrimitiveCharToInteger (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveCharToIntegerL.Make (rator, (LexicalVariable) rand)
                : (rand is Quotation) ? Unimplemented ()
                : new PrimitiveCharToInteger (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCharToInteger.EvalStep");
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            answer = (int) (char) ev0;
            return false;
        }
    }

    [Serializable]
    class PrimitiveCharToIntegerL : PrimitiveCharToInteger
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveCharToIntegerL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveCharToIntegerL Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveCharToIntegerA.Make (rator, (Argument) rand)
                : new PrimitiveCharToIntegerL (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCharToIntegerL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            answer = (int) (char) ev0;
            return false;
        }
    }

    [Serializable]
    class PrimitiveCharToIntegerA : PrimitiveCharToIntegerL
    {
        protected PrimitiveCharToIntegerA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveCharToIntegerA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveCharToIntegerA0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveCharToIntegerA1.Make (rator, (Argument1) rand)
                : new PrimitiveCharToIntegerA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCharToIntegerA.EvalStep");
#endif
            answer = (int) (char) environment.ArgumentValue (this.argOffset);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCharToIntegerA0 : PrimitiveCharToIntegerA
    {
        PrimitiveCharToIntegerA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveCharToIntegerA0 Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveCharToIntegerA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCharToIntegerA0.EvalStep");
#endif
            answer = (int) (char) environment.Argument0Value;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCharToIntegerA1 : PrimitiveCharToIntegerA
    {
        PrimitiveCharToIntegerA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveCharToIntegerA1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveCharToIntegerA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCharToIntegerA1.EvalStep");
#endif
            answer = (int) (char) environment.Argument1Value;
            return false;

        }
    }

    [Serializable]
    sealed class PrimitiveCharToIntegerL1 : PrimitiveCharToIntegerL
    {
        PrimitiveCharToIntegerL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveCharToIntegerL1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveCharToIntegerL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCharToIntegerL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            answer = (int) (char) ev0;
            return false;
        }
    }

    #endregion

    #region FixnumAdd1

    [Serializable]
    class PrimitiveFixnumAdd1 : PrimitiveCombination1
    {
        protected PrimitiveFixnumAdd1 (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveFixnumAdd1L.Make (rator, (LexicalVariable) rand)
                : (rand is Quotation) ? Unimplemented ()
                : new PrimitiveFixnumAdd1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveFixnumAdd1.EvalStep");
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            answer = (int) ev0 + 1;
            return false;
        }
    }

    [Serializable]
    class PrimitiveFixnumAdd1L : PrimitiveFixnumAdd1
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveFixnumAdd1L (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveFixnumAdd1L Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveFixnumAdd1A.Make (rator, (Argument) rand) :
                (rand is LexicalVariable1) ? PrimitiveFixnumAdd1L1.Make (rator, (LexicalVariable1) rand) :
                new PrimitiveFixnumAdd1L (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveFixnumAdd1L.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            answer = (int) ev0 + 1;
            return false;
        }
    }

    [Serializable]
    class PrimitiveFixnumAdd1A : PrimitiveFixnumAdd1L
    {
        protected PrimitiveFixnumAdd1A (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveFixnumAdd1A Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveFixnumAdd1A0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveFixnumAdd1A1.Make (rator, (Argument1) rand)
                : new PrimitiveFixnumAdd1A (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveFixnumAdd1A.EvalStep");
#endif
            answer = (int) environment.ArgumentValue (this.argOffset) + 1;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveFixnumAdd1A0 : PrimitiveFixnumAdd1A
    {
        PrimitiveFixnumAdd1A0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveFixnumAdd1A0 Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveFixnumAdd1A0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveFixnumAdd1A0.EvalStep");
#endif
            answer = (int) environment.Argument0Value + 1;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveFixnumAdd1A1 : PrimitiveFixnumAdd1A
    {
        PrimitiveFixnumAdd1A1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveFixnumAdd1A1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveFixnumAdd1A1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveFixnumAdd1A1.EvalStep");
#endif
            answer = (int) environment.Argument1Value + 1;
            return false;

        }
    }

    [Serializable]
    sealed class PrimitiveFixnumAdd1L1 : PrimitiveFixnumAdd1L
    {
        PrimitiveFixnumAdd1L1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveFixnumAdd1L1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveFixnumAdd1L1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveFixnumAdd1L1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            answer = (int) ev0 + 1;
            return false;
        }
    }

    #endregion

    #region IsChar

    [Serializable]
    class PrimitiveIsChar : PrimitiveCombination1
    {
        protected PrimitiveIsChar (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveIsCharL.Make (rator, (LexicalVariable) rand)
                : (rand is Quotation) ? Unimplemented ()
                : new PrimitiveIsChar (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsChar.EvalStep");
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            answer = (ev0 is char) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsCharL : PrimitiveIsChar
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveIsCharL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveIsCharL Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveIsCharA.Make (rator, (Argument) rand)
                : (rand is LexicalVariable1) ? PrimitiveIsCharL1.Make (rator, (LexicalVariable1) rand)
                : new PrimitiveIsCharL (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is char) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsCharA : PrimitiveIsCharL
    {
        protected PrimitiveIsCharA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsCharA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsCharA0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveIsCharA1.Make (rator, (Argument1) rand)
                : new PrimitiveIsCharA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharA.EvalStep");
#endif
            answer = (environment.ArgumentValue (this.argOffset) is char) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsCharA0 : PrimitiveIsCharA
    {
        PrimitiveIsCharA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsCharA Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveIsCharA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharA0.EvalStep");
#endif
            answer = (environment.Argument0Value is char) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsCharA1 : PrimitiveIsCharA
    {
        PrimitiveIsCharA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsCharA1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveIsCharA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharA1.EvalStep");
#endif
            answer = (environment.Argument1Value is char) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsCharL1 : PrimitiveIsCharL
    {
        PrimitiveIsCharL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsCharL1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveIsCharL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsCharL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is char) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    #endregion

    #region IsComplex

    [Serializable]
    class PrimitiveIsComplex : PrimitiveCombination1
    {
        protected PrimitiveIsComplex (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveIsComplexL.Make (rator, (LexicalVariable) rand)
                : (rand is Quotation) ? Unimplemented ()
                : new PrimitiveIsComplex (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsComplex.EvalStep");
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            answer = (ev0 is Complex) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsComplexL : PrimitiveIsComplex
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveIsComplexL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveIsComplexL Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveIsComplexA.Make (rator, (Argument) rand)
                : (rand is LexicalVariable1) ? PrimitiveIsComplexL1.Make (rator, (LexicalVariable1) rand)
                : new PrimitiveIsComplexL (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsComplexL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is Complex) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsComplexA : PrimitiveIsComplexL
    {
        protected PrimitiveIsComplexA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsComplexA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsComplexA0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveIsComplexA1.Make (rator, (Argument1) rand)
                : new PrimitiveIsComplexA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsComplexA.EvalStep");
#endif
            answer = (environment.ArgumentValue (this.argOffset) is Complex) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsComplexA0 : PrimitiveIsComplexA
    {
        PrimitiveIsComplexA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsComplexA Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveIsComplexA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsComplexA0.EvalStep");
#endif
            answer = (environment.Argument0Value is Complex) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsComplexA1 : PrimitiveIsComplexA
    {
        PrimitiveIsComplexA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsComplexA1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveIsComplexA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsComplexA1.EvalStep");
#endif
            answer = (environment.Argument1Value is Complex) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsComplexL1 : PrimitiveIsComplexL
    {
        PrimitiveIsComplexL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsComplexL1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveIsComplexL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsComplexL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is Complex) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    #endregion

    #region IsBigFixnum

    [Serializable]
    class PrimitiveIsBigFixnum : PrimitiveCombination1
    {
        protected PrimitiveIsBigFixnum (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveIsBigFixnumL.Make (rator, (LexicalVariable) rand)
                : (rand is Quotation) ? Unimplemented ()
                : new PrimitiveIsBigFixnum (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsBigFixnum.EvalStep");
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            answer = (ev0 is double) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsBigFixnumL : PrimitiveIsBigFixnum
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveIsBigFixnumL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveIsBigFixnumL Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveIsBigFixnumA.Make (rator, (Argument) rand)
                : (rand is LexicalVariable1) ? PrimitiveIsBigFixnumL1.Make (rator, (LexicalVariable1) rand)
                : new PrimitiveIsBigFixnumL (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsBigFixnumL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is double) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsBigFixnumA : PrimitiveIsBigFixnumL
    {
        protected PrimitiveIsBigFixnumA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsBigFixnumA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsBigFixnumA0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveIsBigFixnumA1.Make (rator, (Argument1) rand)
                : new PrimitiveIsBigFixnumA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsBigFixnumA.EvalStep");
#endif
            answer = (environment.ArgumentValue (this.argOffset) is double) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsBigFixnumA0 : PrimitiveIsBigFixnumA
    {
        PrimitiveIsBigFixnumA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsBigFixnumA Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveIsBigFixnumA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsBigFixnumA0.EvalStep");
#endif
            answer = (environment.Argument0Value is long) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsBigFixnumA1 : PrimitiveIsBigFixnumA
    {
        PrimitiveIsBigFixnumA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsBigFixnumA1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveIsBigFixnumA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsBigFixnumA1.EvalStep");
#endif
            answer = (environment.Argument1Value is double) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsBigFixnumL1 : PrimitiveIsBigFixnumL
    {
        PrimitiveIsBigFixnumL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsBigFixnumL1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveIsBigFixnumL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsBigFixnumL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is double) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    #endregion

    #region IsBigFlonum

    [Serializable]
    class PrimitiveIsBigFlonum : PrimitiveCombination1
    {
        protected PrimitiveIsBigFlonum (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveIsBigFlonumL.Make (rator, (LexicalVariable) rand)
                : (rand is Quotation) ? Unimplemented ()
                : new PrimitiveIsBigFlonum (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsBigFlonum.EvalStep");
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            answer = (ev0 is Int64) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsBigFlonumL : PrimitiveIsBigFlonum
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveIsBigFlonumL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveIsBigFlonumL Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveIsBigFlonumA.Make (rator, (Argument) rand)
                : (rand is LexicalVariable1) ? PrimitiveIsBigFlonumL1.Make (rator, (LexicalVariable1) rand)
                : new PrimitiveIsBigFlonumL (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsBigFlonumL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is Int64) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsBigFlonumA : PrimitiveIsBigFlonumL
    {
        protected PrimitiveIsBigFlonumA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsBigFlonumA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsBigFlonumA0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveIsBigFlonumA1.Make (rator, (Argument1) rand)
                : new PrimitiveIsBigFlonumA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsBigFlonumA.EvalStep");
#endif
            answer = (environment.ArgumentValue (this.argOffset) is Int64) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsBigFlonumA0 : PrimitiveIsBigFlonumA
    {
        PrimitiveIsBigFlonumA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsBigFlonumA Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveIsBigFlonumA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsBigFlonumA0.EvalStep");
#endif
            answer = (environment.Argument0Value is Int64) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsBigFlonumA1 : PrimitiveIsBigFlonumA
    {
        PrimitiveIsBigFlonumA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsBigFlonumA1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveIsBigFlonumA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsBigFlonumA1.EvalStep");
#endif
            answer = (environment.Argument1Value is Int64) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsBigFlonumL1 : PrimitiveIsBigFlonumL
    {
        PrimitiveIsBigFlonumL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsBigFlonumL1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveIsBigFlonumL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsBigFlonumL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is Int64) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    #endregion

    #region IsFixnum

    [Serializable]
    class PrimitiveIsFixnum : PrimitiveCombination1
    {
        protected PrimitiveIsFixnum (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveIsFixnumL.Make (rator, (LexicalVariable) rand) :
                (rand is Quotation) ? Unimplemented () :
                new PrimitiveIsFixnum (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnum.EvalStep");
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            answer = (ev0 is Int32) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsFixnumL : PrimitiveIsFixnum
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveIsFixnumL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveIsFixnumL Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveIsFixnumA.Make (rator, (Argument) rand) :
                (rand is LexicalVariable1) ? PrimitiveIsFixnumL1.Make (rator, (LexicalVariable1) rand) :
                new PrimitiveIsFixnumL (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is Int32) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsFixnumA : PrimitiveIsFixnumL
    {
        protected PrimitiveIsFixnumA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsFixnumA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsFixnumA0.Make (rator, (Argument0) rand) :
                (rand is Argument1) ? PrimitiveIsFixnumA1.Make (rator, (Argument1) rand) :
                new PrimitiveIsFixnumA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumA.EvalStep");
#endif
            answer = (environment.ArgumentValue (this.argOffset) is Int32) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsFixnumA0 : PrimitiveIsFixnumA
    {
        PrimitiveIsFixnumA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsFixnumA Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveIsFixnumA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumA0.EvalStep");
#endif
            answer = (environment.Argument0Value is Int32) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsFixnumA1 : PrimitiveIsFixnumA
    {
        PrimitiveIsFixnumA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsFixnumA1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveIsFixnumA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumA1.EvalStep");
#endif
            answer = (environment.Argument1Value is Int32) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsFixnumL1 : PrimitiveIsFixnumL
    {
        PrimitiveIsFixnumL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsFixnumL1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveIsFixnumL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsFixnumL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is Int32) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    #endregion

    #region IsRatnum

    [Serializable]
    class PrimitiveIsRatnum : PrimitiveCombination1
    {
        protected PrimitiveIsRatnum (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveIsRatnumL.Make (rator, (LexicalVariable) rand)
                : (rand is Quotation) ? Unimplemented ()
                : new PrimitiveIsRatnum (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsRatnum.EvalStep");
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            answer = (ev0 is Ratnum) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsRatnumL : PrimitiveIsRatnum
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveIsRatnumL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveIsRatnumL Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveIsRatnumA.Make (rator, (Argument) rand)
                : (rand is LexicalVariable1) ? PrimitiveIsRatnumL1.Make (rator, (LexicalVariable1) rand)
                : new PrimitiveIsRatnumL (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsRatnumL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is Ratnum) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsRatnumA : PrimitiveIsRatnumL
    {
        protected PrimitiveIsRatnumA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsRatnumA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsRatnumA0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveIsRatnumA1.Make (rator, (Argument1) rand)
                : new PrimitiveIsRatnumA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsRatnumA.EvalStep");
#endif
            answer = (environment.ArgumentValue (this.argOffset) is Ratnum) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsRatnumA0 : PrimitiveIsRatnumA
    {
        PrimitiveIsRatnumA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsRatnumA Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveIsRatnumA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsRatnumA0.EvalStep");
#endif
            answer = (environment.Argument0Value is Ratnum) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsRatnumA1 : PrimitiveIsRatnumA
    {
        PrimitiveIsRatnumA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsRatnumA1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveIsRatnumA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsRatnumA1.EvalStep");
#endif
            answer = (environment.Argument1Value is Ratnum) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsRatnumL1 : PrimitiveIsRatnumL
    {
        PrimitiveIsRatnumL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsRatnumL1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveIsRatnumL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsRatnumL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is Ratnum) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    #endregion

    #region IsSymbol

    [Serializable]
    class PrimitiveIsSymbol : PrimitiveCombination1
    {
        protected PrimitiveIsSymbol (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveIsSymbolL.Make (rator, (LexicalVariable) rand)
                : (rand is Quotation) ? Unimplemented()
                : new PrimitiveIsSymbol (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsSymbol.EvalStep");
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            string sym = ev0 as string;
            answer = (sym != null && string.IsInterned (sym) == sym) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsSymbolL : PrimitiveIsSymbol
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveIsSymbolL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveIsSymbolL Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveIsSymbolA.Make (rator, (Argument) rand)
                : (rand is LexicalVariable1) ? PrimitiveIsSymbolL1.Make (rator, (LexicalVariable1) rand)
                : new PrimitiveIsSymbolL (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsSymbolL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            string sym = ev0 as string;
            answer = (sym != null && string.IsInterned (sym) == sym) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsSymbolA : PrimitiveIsSymbolL
    {
        protected PrimitiveIsSymbolA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsSymbolA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsSymbolA0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveIsSymbolA1.Make (rator, (Argument1) rand)
                : new PrimitiveIsSymbolA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsSymbolA.EvalStep");
#endif
            string sym = environment.ArgumentValue (this.argOffset) as string;
            answer = (sym != null && string.IsInterned (sym) == sym) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsSymbolA0 : PrimitiveIsSymbolA
    {
        PrimitiveIsSymbolA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsSymbolA Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveIsSymbolA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsSymbolA0.EvalStep");
#endif

            string sym = environment.Argument0Value as string;
            answer = (sym != null && string.IsInterned (sym) == sym) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsSymbolA1 : PrimitiveIsSymbolA
    {
        PrimitiveIsSymbolA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsSymbolA1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveIsSymbolA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsSymbolA1.EvalStep");
#endif

            string sym = environment.Argument1Value as string;
            answer = (sym != null && string.IsInterned (sym) == sym) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsSymbolL1 : PrimitiveIsSymbolL
    {
        PrimitiveIsSymbolL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsSymbolL1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveIsSymbolL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsSymbolL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            string sym = ev0 as string;
            answer = (sym != null && string.IsInterned (sym) == sym) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    #endregion

    #region IsVector

    [Serializable]
    class PrimitiveIsVector : PrimitiveCombination1
    {
        protected PrimitiveIsVector (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveIsVectorL.Make (rator, (LexicalVariable) rand):
                (rand is Quotation) ? Unimplemented ():
                new PrimitiveIsVector (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsVector.EvalStep");
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            answer = (ev0 is object []) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsVectorL : PrimitiveIsVector
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveIsVectorL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveIsVectorL Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveIsVectorA.Make (rator, (Argument) rand)
                : (rand is LexicalVariable1) ? PrimitiveIsVectorL1.Make (rator, (LexicalVariable1) rand)
                : new PrimitiveIsVectorL (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsVectorL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is object []) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsVectorA : PrimitiveIsVectorL
    {
        protected PrimitiveIsVectorA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsVectorA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsVectorA0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveIsVectorA1.Make (rator, (Argument1) rand)
                : new PrimitiveIsVectorA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsVectorA.EvalStep");
#endif
            answer = (environment.ArgumentValue (this.argOffset) is object []) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsVectorA0 : PrimitiveIsVectorA
    {
        PrimitiveIsVectorA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsVectorA Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveIsVectorA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsVectorA0.EvalStep");
#endif
            answer = (environment.Argument0Value is object []) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsVectorA1 : PrimitiveIsVectorA
    {
        PrimitiveIsVectorA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsVectorA1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveIsVectorA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsVectorA1.EvalStep");
#endif
            answer = (environment.Argument1Value is object []) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsVectorL1 : PrimitiveIsVectorL
    {
        PrimitiveIsVectorL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsVectorL1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveIsVectorL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsVectorL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is object []) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    #endregion

    #region Add1

    [Serializable]
    class PrimitiveAdd1 : PrimitiveCombination1
    {
        protected PrimitiveAdd1 (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveAdd1L.Make (rator, (LexicalVariable) rand) :
                (rand is Quotation) ? Unimplemented () :
                new PrimitiveAdd1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveAdd1.EvalStep");
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            if (ev0 is int) {
                answer = (int) ev0 + 1;
                return false;
            }
            return GenericArithmetic.Increment (out answer, ev0);
        }
    }

    [Serializable]
    class PrimitiveAdd1L : PrimitiveAdd1
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveAdd1L (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveAdd1L Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveAdd1A.Make (rator, (Argument) rand) :
                (rand is LexicalVariable1) ? PrimitiveAdd1L1.Make (rator, (LexicalVariable1) rand) :
                new PrimitiveAdd1L (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveAdd1L.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            if (ev0 is int) {
                answer = (int) ev0 + 1;
                return false;
            }
            return GenericArithmetic.Increment (out answer, ev0);
        }
    }

    [Serializable]
    class PrimitiveAdd1A : PrimitiveAdd1L
    {
        protected PrimitiveAdd1A (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveAdd1A Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveAdd1A0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveAdd1A1.Make (rator, (Argument1) rand)
                : new PrimitiveAdd1A (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveAdd1A.EvalStep");
#endif
            object ev0 = environment.ArgumentValue (this.argOffset);
            if (ev0 is int) {
                answer = (int) ev0 + 1;
                return false;
            }
            return GenericArithmetic.Increment (out answer, ev0);
        }
    }

    [Serializable]
    sealed class PrimitiveAdd1A0 : PrimitiveAdd1A
    {
        PrimitiveAdd1A0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveAdd1A0 Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveAdd1A0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveAdd1A0.EvalStep");
#endif
            object ev0 = environment.Argument0Value;
            if (ev0 is int) {
                answer = (int) ev0 + 1;
                return false;
            }
            return GenericArithmetic.Increment (out answer, ev0);
        }
    }

    [Serializable]
    sealed class PrimitiveAdd1A1 : PrimitiveAdd1A
    {
        PrimitiveAdd1A1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveAdd1A1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveAdd1A1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveAdd1A1.EvalStep");
#endif
            object ev0 = environment.Argument1Value;
            if (ev0 is int) {
                answer = (int) ev0 + 1;
                return false;
            }
            return GenericArithmetic.Increment (out answer, ev0);
        }
    }

    [Serializable]
    sealed class PrimitiveAdd1L1 : PrimitiveAdd1L
    {
        PrimitiveAdd1L1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveAdd1L1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveAdd1L1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveAdd1L1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            if (ev0 is int) {
                answer = (int) ev0 + 1;
                return false;
            }
            return GenericArithmetic.Increment (out answer, ev0);
        }
    }

    #endregion

    #region IsNegative

    [Serializable]
    class PrimitiveIsNegative : PrimitiveCombination1
    {
        protected PrimitiveIsNegative (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveIsNegativeL.Make (rator, (LexicalVariable) rand)
                : (rand is Quotation) ? Unimplemented ()
                : new PrimitiveIsNegative (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsNegative.EvalStep");
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            answer = ((int) ev0 < 0) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsNegativeL : PrimitiveIsNegative
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveIsNegativeL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveIsNegativeL Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveIsNegativeA.Make (rator, (Argument) rand)
                : (rand is LexicalVariable1) ? PrimitiveIsNegativeL1.Make (rator, (LexicalVariable1) rand)
                : new PrimitiveIsNegativeL (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SCode.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            answer = ((int) ev0 < 0) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsNegativeA : PrimitiveIsNegativeL
    {
        protected PrimitiveIsNegativeA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsNegativeA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsNegativeA0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveIsNegativeA1.Make (rator, (Argument1) rand)
                : new PrimitiveIsNegativeA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsNegativeA.EvalStep");
#endif
            answer = ((int) environment.ArgumentValue (this.argOffset) < 0) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsNegativeA0 : PrimitiveIsNegativeA
    {
        PrimitiveIsNegativeA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsNegativeA0 Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveIsNegativeA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsNegativeA0.EvalStep");
#endif
            answer = ((int) environment.Argument0Value < 0) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsNegativeA1 : PrimitiveIsNegativeA
    {
        PrimitiveIsNegativeA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsNegativeA1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveIsNegativeA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsNegativeA1.EvalStep");
#endif
            answer = ((int) environment.Argument1Value < 0) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsNegativeL1 : PrimitiveIsNegativeL
    {

        PrimitiveIsNegativeL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsNegativeL1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveIsNegativeL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsNegativeL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            answer = ((int) ev0 < 0) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    #endregion

    #region IsNull

    [Serializable]
    class PrimitiveIsNull : PrimitiveCombination1
    {
        protected PrimitiveIsNull (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveIsNullL.Make (rator, (LexicalVariable) rand)
                : (rand is Quotation) ? Unimplemented ()
                : new PrimitiveIsNull (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsNull.EvalStep");
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            answer = (ev0 == null) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsNullL : PrimitiveIsNull
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveIsNullL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveIsNullL Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveIsNullA.Make (rator, (Argument) rand)
                : (rand is LexicalVariable1) ? PrimitiveIsNullL1.Make (rator, (LexicalVariable1) rand)
                : new PrimitiveIsNullL (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SCode.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 == null) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsNullA : PrimitiveIsNullL
    {
        protected PrimitiveIsNullA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsNullA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsNullA0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveIsNullA1.Make (rator, (Argument1) rand)
                : new PrimitiveIsNullA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsNullA.EvalStep");
#endif
            answer = (environment.ArgumentValue (this.argOffset) == null) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsNullA0 : PrimitiveIsNullA
    {
        PrimitiveIsNullA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsNullA0 Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveIsNullA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsNullA0.EvalStep");
#endif
            answer = (environment.Argument0Value == null) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsNullA1 : PrimitiveIsNullA
    {
        PrimitiveIsNullA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsNullA1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveIsNullA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsNullA1.EvalStep");
#endif
            answer = (environment.Argument1Value == null) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsNullL1 : PrimitiveIsNullL
    {

        PrimitiveIsNullL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsNullL1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveIsNullL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsNullL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 == null) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    #endregion

    #region IsPair

    [Serializable]
    class PrimitiveIsPair : PrimitiveCombination1
    {
        protected PrimitiveIsPair (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveIsPairL.Make (rator, (LexicalVariable) rand) :
                (rand is Quotation) ? Unimplemented () :
                new PrimitiveIsPair (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsPair.EvalStep");
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            answer = (ev0 is Cons) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsPairL : PrimitiveIsPair
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveIsPairL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveIsPairL Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveIsPairA.Make (rator, (Argument) rand)
                : (rand is LexicalVariable1) ? PrimitiveIsPairL1.Make (rator, (LexicalVariable1) rand)
                : new PrimitiveIsPairL (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsPairL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is Cons) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsPairA : PrimitiveIsPairL
    {
        protected PrimitiveIsPairA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsPairA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsPairA0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveIsPairA1.Make (rator, (Argument1) rand)
                : new PrimitiveIsPairA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsPairA.EvalStep");
#endif
            answer = (environment.ArgumentValue (this.argOffset) is Cons) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsPairA0 : PrimitiveIsPairA
    {
        PrimitiveIsPairA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsPairA0 Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveIsPairA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsPairA0.EvalStep");
#endif
            answer = (environment.Argument0Value is Cons) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsPairA1 : PrimitiveIsPairA
    {
        PrimitiveIsPairA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsPairA1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveIsPairA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsPairA1.EvalStep");
#endif
            answer = (environment.Argument1Value is Cons) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsPairL1 : PrimitiveIsPairL
    {
        PrimitiveIsPairL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsPairL1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveIsPairL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsPairL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is Cons) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    #endregion

    #region IsRecord

    [Serializable]
    class PrimitiveIsRecord : PrimitiveCombination1
    {
        protected PrimitiveIsRecord (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveIsRecordL.Make (rator, (LexicalVariable) rand)
                : (rand is Quotation) ? Unimplemented ()
                : new PrimitiveIsRecord (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsRecord.EvalStep");
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            answer = (ev0 is Record) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsRecordL : PrimitiveIsRecord
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveIsRecordL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveIsRecordL Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveIsRecordA.Make (rator, (Argument) rand)
                : (rand is LexicalVariable1) ? PrimitiveIsRecordL1.Make (rator, (LexicalVariable1) rand)
                : new PrimitiveIsRecordL (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsRecordL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is Record) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsRecordA : PrimitiveIsRecordL
    {
        protected PrimitiveIsRecordA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsRecordA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsRecordA0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveIsRecordA1.Make (rator, (Argument1) rand)
                : new PrimitiveIsRecordA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsRecordA.EvalStep");
#endif
            answer = (environment.ArgumentValue (this.argOffset) is Record) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsRecordA0 : PrimitiveIsRecordA
    {
        PrimitiveIsRecordA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsRecordA Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveIsRecordA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsRecordA0.EvalStep");
#endif
            answer = (environment.Argument0Value is Record) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsRecordA1 : PrimitiveIsRecordA
    {
        PrimitiveIsRecordA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsRecordA1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveIsRecordA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsRecordA1.EvalStep");
#endif
            answer = (environment.Argument1Value is Record) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsRecordL1 : PrimitiveIsRecordL
    {
        PrimitiveIsRecordL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsRecordL1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveIsRecordL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsRecordL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is Record) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    #endregion

    #region IsSharpT

    [Serializable]
    class PrimitiveIsSharpT : PrimitiveCombination1
    {
        protected PrimitiveIsSharpT (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        static bool IsRedundantTest (SCode rand)
        {
            if (rand is PrimitiveCombination1) {
                Primitive op = ((PrimitiveCombination1) rand).Operator;
                if (op.Name.ToString() != "SYSTEM-PAIR-CAR")
                    Debugger.Break ();
            }
            else if (rand is PrimitiveCombination2) {
                Primitive op = ((PrimitiveCombination2) rand).Rator;
                Debugger.Break ();
            }
            return false;
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveIsSharpTL.Make (rator, (LexicalVariable) rand) :
                (rand is Quotation) ? Unimplemented() :
                IsRedundantTest(rand) ? Unimplemented() :
                new PrimitiveIsSharpT (rator, rand);
                //: (rand is Quotation) ? Unimplemented ()
                //: new PrimitiveIsSharpT (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsSharpT.EvalStep");
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            answer = (ev0 is Boolean && (bool) ev0) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsSharpTL : PrimitiveIsSharpT
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveIsSharpTL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveIsSharpTL Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveIsSharpTA.Make (rator, (Argument) rand)
                : (rand is LexicalVariable1) ? PrimitiveIsSharpTL1.Make (rator, (LexicalVariable1) rand)
                : new PrimitiveIsSharpTL (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsSharpTL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is Boolean && (bool) ev0) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsSharpTA : PrimitiveIsSharpTL
    {
        protected PrimitiveIsSharpTA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsSharpTA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveIsSharpTA0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveIsSharpTA1.Make (rator, (Argument1) rand)
                : new PrimitiveIsSharpTA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsSharpTA.EvalStep");
#endif
            object ev0 = environment.ArgumentValue (this.argOffset);
            answer = (ev0 is Boolean && (bool) ev0) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsSharpTA0 : PrimitiveIsSharpTA
    {
        PrimitiveIsSharpTA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsSharpTA Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveIsSharpTA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsSharpTA0.EvalStep");
#endif
            object ev0 = environment.Argument0Value;
            answer = (ev0 is Boolean && (bool) ev0) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsSharpTA1 : PrimitiveIsSharpTA
    {
        PrimitiveIsSharpTA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsSharpTA1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveIsSharpTA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsSharpTA1.EvalStep");
#endif
            object ev0 = environment.Argument1Value;
            answer = (ev0 is Boolean && (bool) ev0) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsSharpTL1 : PrimitiveIsSharpTL
    {
        PrimitiveIsSharpTL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveIsSharpTL1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveIsSharpTL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsSharpTL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is Boolean && (bool) ev0) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    #endregion

    #region Not

    [Serializable]
    class PrimitiveNot : PrimitiveCombination1
    {
        protected PrimitiveNot (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        static bool IsDoubleNegative (SCode rand)
        {
            return rand is PrimitiveCombination1 &&
                ((PrimitiveCombination1) rand).Operator == Primitive.Not;
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveNotL.Make (rator, (LexicalVariable) rand) :
                IsDoubleNegative (rand) ? Unimplemented() :
                (rand is Quotation) ? Unimplemented () :
                new PrimitiveNot (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveNot.EvalStep");
            noteCalls (this.arg0);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            answer = (ev0 is Boolean && (bool) ev0 == false) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveNotL : PrimitiveNot
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveNotL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveNotL Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveNotA.Make (rator, (Argument) rand)
                : (rand is LexicalVariable1) ? PrimitiveNotL1.Make (rator, (LexicalVariable1) rand)
                : new PrimitiveNotL (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveNotL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is Boolean && (bool) ev0 == false) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    class PrimitiveNotA : PrimitiveNotL
    {
        protected PrimitiveNotA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveNotA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveNotA0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveNotA1.Make (rator, (Argument1) rand)
                : new PrimitiveNotA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveNotA.EvalStep");
#endif
            object ev0 = environment.ArgumentValue (this.argOffset);
            answer = (ev0 is Boolean && (bool) ev0 == false) ? Constant.sharpT : Constant.sharpF;
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

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveNotA0.EvalStep");
#endif
            object ev0 = environment.Argument0Value;
            answer = (ev0 is Boolean && (bool) ev0 == false) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveNotA1 : PrimitiveNotA
    {
        PrimitiveNotA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveNotA1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveNotA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveNotA1.EvalStep");
#endif
            object ev0 = environment.Argument1Value;
            answer = (ev0 is Boolean && (bool) ev0 == false) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveNotL1 : PrimitiveNotL
    {
        PrimitiveNotL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveNotL1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveNotL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveNotL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            answer = (ev0 is Boolean && (bool) ev0 == false) ? Constant.sharpT : Constant.sharpF;
            return false;
        }
    }

    #endregion

    #region ObjectType

    [Serializable]
    class PrimitiveObjectType : PrimitiveCombination1
    {
        protected PrimitiveObjectType (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveObjectTypeL.Make (rator, (LexicalVariable) rand)
                : (rand is Quotation) ? Unimplemented ()
                : new PrimitiveObjectType (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveObjectType.EvalStep");
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            ObjectModel.PrimitiveObjectType (out answer, ev0);
            return false;
        }
    }

    [Serializable]
    class PrimitiveObjectTypeL : PrimitiveObjectType
    {
        protected readonly object argName;
        protected readonly int argDepth;
        protected readonly int argOffset;

        protected PrimitiveObjectTypeL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.argName = arg0.Name;
            this.argDepth = arg0.Depth;
            this.argOffset = arg0.Offset;
        }

        public static PrimitiveObjectTypeL Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveObjectTypeA.Make (rator, (Argument) rand)
                : new PrimitiveObjectTypeL (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveObjectTypeL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();
            ObjectModel.PrimitiveObjectType (out answer, ev0);
            return false;
        }
    }

    [Serializable]
    class PrimitiveObjectTypeA : PrimitiveObjectTypeL
    {
        protected PrimitiveObjectTypeA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveObjectTypeA Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveObjectTypeA0.Make (rator, (Argument0) rand)
                : (rand is Argument1) ? PrimitiveObjectTypeA1.Make (rator, (Argument1) rand)
                : new PrimitiveObjectTypeA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveObjectTypeA.EvalStep");
#endif
            ObjectModel.PrimitiveObjectType (out answer, environment.ArgumentValue (this.argOffset));
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveObjectTypeA0 : PrimitiveObjectTypeA
    {
        PrimitiveObjectTypeA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveObjectTypeA0 Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveObjectTypeA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveObjectTypeA0.EvalStep");
#endif
            ObjectModel.PrimitiveObjectType (out answer, environment.Argument0Value);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveObjectTypeA1 : PrimitiveObjectTypeA
    {
        PrimitiveObjectTypeA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveObjectTypeA1 Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveObjectTypeA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveObjectTypeA1.EvalStep");
#endif
            ObjectModel.PrimitiveObjectType (out answer, environment.Argument1Value);
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveObjectTypeL1 : PrimitiveObjectTypeL
    {
        PrimitiveObjectTypeL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static PrimitiveObjectTypeL1 Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveObjectTypeL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveObjectTypeL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.argName, this.argOffset))
                throw new NotImplementedException ();
            ObjectModel.PrimitiveObjectType (out answer, ev0);
            return false;
        }
    }

    #endregion

    #region SystemPairCar

    [Serializable]
    class PrimitiveSystemPairCar : PrimitiveCombination1
    {
        protected PrimitiveSystemPairCar (Primitive1 procedure, SCode arg0)
            : base (procedure, arg0)
        {
        }

        public static new SCode Make (Primitive1 rator, SCode rand)
        {
            return
                (rand is LexicalVariable) ? PrimitiveSystemPairCarL.Make (rator, (LexicalVariable) rand) :
                (rand is Quotation) ? Unimplemented () :
                new PrimitiveSystemPairCar (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.arg0);
            SCode.location = "PrimitiveSystemPairCar.EvalStep";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            ISystemPair evpair = ev0 as ISystemPair;
            if (evpair == null) throw new NotImplementedException ();
            answer = evpair.SystemPairCar;
            return false;
        }
    }

    [Serializable]
    class PrimitiveSystemPairCarL : PrimitiveSystemPairCar
    {
        public readonly object randName;
        public readonly int randDepth;
        public readonly int randOffset;

        protected PrimitiveSystemPairCarL (Primitive1 procedure, LexicalVariable arg0)
            : base (procedure, arg0)
        {
            this.randName = arg0.Name;
            this.randDepth = arg0.Depth;
            this.randOffset = arg0.Offset;
        }

        public static SCode Make (Primitive1 rator, LexicalVariable rand)
        {
            return
                (rand is Argument) ? PrimitiveSystemPairCarA.Make (rator, (Argument) rand) :
                (rand is LexicalVariable1) ? PrimitiveSystemPairCarL1.Make (rator, (LexicalVariable1) rand) :
                new PrimitiveSystemPairCarL (rator, rand);
        }

        public object OperandName { get { return this.randName; } }
        public int OperandDepth { get { return this.randDepth; } }
        public int OperandOffset { get { return this.randOffset; } }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveSystemPairCarL.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.randName, this.randDepth, this.randOffset))
                throw new NotImplementedException ();
            ISystemPair evpair = ev0 as ISystemPair;
            if (evpair == null) throw new NotImplementedException ();
            answer = evpair.SystemPairCar;
            return false;
        }
    }

    [Serializable]
    class PrimitiveSystemPairCarA : PrimitiveSystemPairCarL
    {
        protected PrimitiveSystemPairCarA (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument rand)
        {
            return
                (rand is Argument0) ? PrimitiveSystemPairCarA0.Make (rator, (Argument0) rand) :
                (rand is Argument1) ? PrimitiveSystemPairCarA1.Make (rator, (Argument1) rand) :
                new PrimitiveSystemPairCarA (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveSystemPairCarA.EvalStep");
#endif
            ISystemPair evpair = environment.ArgumentValue (this.randOffset) as ISystemPair;
            if (evpair == null) throw new NotImplementedException ();
            answer = evpair.SystemPairCar;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveSystemPairCarA0 : PrimitiveSystemPairCarA
    {
        PrimitiveSystemPairCarA0 (Primitive1 procedure, Argument0 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument0 rand)
        {
            return new PrimitiveSystemPairCarA0 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveSystemPairCarA0.EvalStep");
#endif
            ISystemPair evpair = environment.Argument0Value as ISystemPair;
            if (evpair == null) throw new NotImplementedException ();
            answer = evpair.SystemPairCar;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveSystemPairCarA1 : PrimitiveSystemPairCarA
    {
        PrimitiveSystemPairCarA1 (Primitive1 procedure, Argument1 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, Argument1 rand)
        {
            return new PrimitiveSystemPairCarA1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveSystemPairCarA1.EvalStep");
#endif
            ISystemPair ev0 = environment.Argument1Value as ISystemPair;
            if (ev0 == null) throw new NotImplementedException ();
            answer = ev0.SystemPairCar;
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveSystemPairCarL1 : PrimitiveSystemPairCarL
    {
        PrimitiveSystemPairCarL1 (Primitive1 procedure, LexicalVariable1 arg0)
            : base (procedure, arg0)
        {
        }

        public static SCode Make (Primitive1 rator, LexicalVariable1 rand)
        {
            return new PrimitiveSystemPairCarL1 (rator, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveSystemPairCarL1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.randName, this.randOffset))
                throw new NotImplementedException ();
            ISystemPair evpair = ev0 as ISystemPair;
            if (evpair == null) throw new NotImplementedException ();
            answer = evpair.SystemPairCar;
            return false;
        }
    }

    #endregion
}
