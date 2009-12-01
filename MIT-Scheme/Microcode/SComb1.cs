using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    [Serializable]
    class Combination1 : SCode, ISerializable, ISystemPair
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.COMBINATION_1; } }

#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected Type ratorType;
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected Type randType;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly SCode rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly SCode rand;

        protected Combination1 (SCode rator, SCode rand)
            : base ()
        {
            this.rator = rator;
            this.rand = rand;
#if DEBUG
            this.ratorType = rator.GetType ();
            this.randType = rand.GetType ();
#endif
        }

        static SCode Make (SCode rator, SCode rand)
        {
            if (rand == null)
                throw new ArgumentNullException ("rand");
            return
                (! Configuration.EnableCombination1Optimization) ? new Combination1 (rator, rand) :
                (! Configuration.EnableCombination1Specialization) ? new Combination1 (rator, rand) :
                (rator is StaticVariable) ? Combination1S.Make((StaticVariable) rator, rand) :
                (rator is TopLevelVariable) ? Combination1T.Make ((TopLevelVariable) rator, rand) :
                (Configuration.EnableLet1 &&
                 rator is Lambda) ? Let1.Make ((Lambda) rator, rand)  :
                //(Configuration.EnableCombination1Specialization &&
                //rator is LexicalVariable) ? Combination1L.Make ((LexicalVariable) rator, rand) :
                //(Configuration.EnableCombination1Specialization &&
                //rator is TopLevelVariable) ? Combination1T.Make ((TopLevelVariable) rator, rand) :
                //(rator is Quotation &&
                //! (((Quotation) rator).Quoted is PrimitiveN)) ? Unimplemented() :
                (rand is Argument) ? Combination1XA.Make(rator, (Argument) rand) :
                //  rand is LexicalVariable) ? Combination1SL.Make (rator, (LexicalVariable) rand) :
                //(Configuration.EnableCombination1Specialization &&
                // rand is Quotation) ? Combination1SQ.Make (rator, (Quotation) rand) :
                new Combination1 (rator, rand);
        }

        public static SCode Make (object rator, object arg0)
        {
            if (rator == null)
                throw new ArgumentNullException ("rator");
            SCode srator = EnsureSCode (rator);
            SCode srand = EnsureSCode (arg0);
            return Combination1.Make (srator, srand);
        }

        public SCode Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.rator;
            }
        }

        public SCode Operand
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand;
            }
        }

        [SchemePrimitive ("COMBINATION1?", 1, true)]
        public static bool IsCombination1 (out object answer, object arg)
        {
            answer = arg is Combination1;
            return false;
        }

        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            [DebuggerStepThrough]
            get
            {

                return UnwrapQuoted (this.rator);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCdr
        {
            [DebuggerStepThrough]
            get
            {

                return UnwrapQuoted (this.rand);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        public override bool CallsTheEnvironment ()
        {
            return this.rand.CallsTheEnvironment ()
                || this.rator.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand);
            ratorTypeHistogram.Note (this.ratorType);
            randTypeHistogram.Note (this.randType);
            SCode.location = "Combination1.EvalStep";
#endif
            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination1.EvalStep";
#endif
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object evop;
            Control unevop = this.rator;
            env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination1.EvalStep";
#endif
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            return this.rator.MutatesAny (formals)
                || this.rand.MutatesAny (formals);
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (Combination1Deserializer));
            info.AddValue ("procedure", this.rator);
            info.AddValue ("operand", this.rand);
        }

        #endregion

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialResult rator = this.rator.PartialEval (environment);
            PartialResult rand = this.rand.PartialEval (environment);
            return new PartialResult (rator.Residual == this.rator &&
                    rand.Residual == this.rand ?
                    this :
                    Combination1.Make(rator.Residual, rand.Residual));
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            this.rator.CollectFreeVariables (freeVariableSet);
            this.rand.CollectFreeVariables (freeVariableSet);
        }
    }

    [Serializable]
    internal sealed class Combination1Deserializer : IObjectReference
    {
        SCode procedure;
        SCode operand;

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public Object GetRealObject (StreamingContext context)
        {
            return Combination1.Make (this.procedure, this.operand);
        }
        // Muffle compiler
        SCode Procedure { set { this.procedure = value; } }
        SCode Operand { set { this.operand = value; } }
    }

    [Serializable]
    sealed class Combination1Frame0 : SubproblemContinuation<Combination1>, ISystemVector
    {
        public Combination1Frame0 (Combination1 combination1, Environment environment)
            : base (combination1, environment)
        {
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { return 3; }
        }

        public object SystemVectorRef (int index)
        {
            switch (index) {
                case 0: return ReturnCode.COMB_1_PROCEDURE;
                case 1: return this.expression;
                case 2: return this.environment;
                default:
                    throw new NotImplementedException ();
            }
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            object evop = null;
            Control unevop = this.expression.Operator;
            Environment env = this.environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, value);
        }
    }

    [Serializable]
    sealed class Combination1Frame1 : SubproblemContinuation<Combination1>, ISystemVector
    {
        readonly object evarg;

        public Combination1Frame1 (Combination1 combination1, Environment environment, object evarg)
            : base (combination1, environment)
        {
            this.evarg = evarg;
        }

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            return Interpreter.Call (out answer, ref expression, ref environment, value, this.evarg);
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
    class Combination1S : Combination1
    {
#if DEBUG
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif

        public readonly object ratorName;
        public readonly int ratorOffset;

        protected Combination1S (StaticVariable rator, SCode rand)
            : base (rator, rand)
        {
            this.ratorName = rator.Name;
            this.ratorOffset = rator.Offset;
        }

        public static SCode Make (StaticVariable rator, SCode rand0)
        {
            return
                new Combination1S (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
            SCode.location = "Combination1S";
#endif
            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination1S";
#endif
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    [Serializable]
    class Combination1T : Combination1
    {
#if DEBUG
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif

        public readonly ValueCell ratorCell;

        protected Combination1T (TopLevelVariable rator, SCode rand)
            : base (rator, rand)
        {
            this.ratorCell = rator.valueCell;
        }

        public static SCode Make (TopLevelVariable rator, SCode rand0)
        {
            return
                (rand0 is Argument) ? Combination1TA.Make(rator, (Argument) rand0) :
                new Combination1T (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
            SCode.location = "Combination1T";
#endif
            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination1T";
#endif
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object evop;
            if (this.ratorCell.GetValue(out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    [Serializable]
    class Combination1TA : Combination1T
    {
        public readonly int randOffset;

        protected Combination1TA (TopLevelVariable rator, Argument rand)
            : base (rator, rand)
        {
            this.randOffset = rand.Offset;
        }

        public static SCode Make (TopLevelVariable rator, Argument rand0)
        {
            return
                (rand0 is Argument0) ? Combination1TA0.Make (rator, (Argument0) rand0) :
                (rand0 is Argument1) ? Combination1TA1.Make (rator, (Argument1) rand0) :
                new Combination1TA (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand);
            SCode.location = "Combination1TA";
#endif
            object evarg = environment.ArgumentValue (this.randOffset);

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    [Serializable]
    class Combination1TA0 : Combination1TA
    {
        protected Combination1TA0 (TopLevelVariable rator, Argument0 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (TopLevelVariable rator, Argument0 rand0)
        {
            return
                new Combination1TA0 (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1TA0");
#endif
            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value);
        }
    }

    [Serializable]
    class Combination1TA1 : Combination1TA
    {
        protected Combination1TA1 (TopLevelVariable rator, Argument1 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (TopLevelVariable rator, Argument1 rand0)
        {
            return
                new Combination1TA1 (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1TA0");
#endif
            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value);
        }
    }

    [Serializable]
    class Combination1XA : Combination1
    {
#if DEBUG
                static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int randOffset;

        protected Combination1XA (SCode rator, Argument rand)
            : base (rator, rand)
        {
            this.randOffset = rand.Offset;
        }

        public static SCode Make (SCode rator, Argument rand0)
        {
            return
                (rand0 is Argument0) ? Combination1XA0.Make (rator, (Argument0) rand0) :
                (rand0 is Argument1) ? Combination1XA1.Make (rator, (Argument1) rand0) :
                new Combination1XA (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination1XA";
#endif

            object evop;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination1XA";
#endif
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, environment.ArgumentValue (this.randOffset)));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue(this.randOffset));
        }
    }

    [Serializable]
    class Combination1XA0 : Combination1XA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif

        protected Combination1XA0 (SCode rator, Argument0 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SCode rator, Argument0 rand0)
        {
            return
                new Combination1XA0 (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination1XA0";
#endif

            object evop;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination1XA0";
#endif
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, environment.Argument0Value));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value);
        }
    }

    [Serializable]
    class Combination1XA1 : Combination1XA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif

        protected Combination1XA1 (SCode rator, Argument1 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SCode rator, Argument1 rand0)
        {
            return
                new Combination1XA1 (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination1XA1";
#endif

            object evop;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination1XA1";
#endif
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, environment.Argument1Value));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value);
        }
    }

//    [Serializable]
//    class Combination1L : Combination1
//    {
//#if DEBUG
//        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object ratorName;
//        public readonly LexicalAddress ratorAddress;

//        protected Combination1L (LexicalVariable rator, SCode rand)
//            : base (rator, rand)
//        {
//            this.ratorName = rator.Name;
//            this.ratorAddress = rator.Address;
//        }

//        public static SCode Make (LexicalVariable rator, SCode rand0)
//        {
//            return
//                (rator is Argument) ? Combination1A.Make ((Argument) rator, rand0) :
//                (rator is LexicalVariable1) ? Combination1L1.Make ((LexicalVariable1) rator, rand0) :
//                (rand0 is LexicalVariable) ? Combination1LL.Make (rator, (LexicalVariable) rand0) :
//                (rand0 is PrimitiveCombination1) ? Combination1LPComb1.Make (rator, (PrimitiveCombination1) rand0) :
//                (rand0 is Quotation) ? Combination1LQ.Make (rator, (Quotation) rand0) :
//                new Combination1L (rator, rand0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1L.EvalStep");
//            NoteCalls (this.rand);
//            randTypeHistogram.Note (this.randType);
//#endif
//            object evarg;
//            Control unev = this.rand;
//            Environment env = closureEnvironment;
//            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
//            if (evarg == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new Combination1LFrame0 (this, closureEnvironment));
//                answer = Interpreter.Unwind;
//                closureEnvironment = env;
//                return false;
//            }

//            object evop = null;
//            if (closureEnvironment.FastLexicalRef (out evop, this.ratorName, this.ratorAddress))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg);
//        }
//    }

//    [Serializable]
//    sealed class Combination1LFrame0 : SubproblemContinuation<Combination1L>, ISystemVector
//    {

//        public Combination1LFrame0 (Combination1L combination1, Environment closureEnvironment)
//            : base (combination1, closureEnvironment)
//        {
//        }

//        #region ISystemVector Members

//        public int SystemVectorSize
//        {
//            get { throw new NotImplementedException (); }
//        }

//        public object SystemVectorRef (int index)
//        {
//            throw new NotImplementedException ();
//        }

//        public object SystemVectorSet (int index, object newValue)
//        {
//            throw new NotImplementedException ();
//        }

//        #endregion

//        public override bool Continue (out object answer, ref Control expression, ref Environment closureEnvironment, object evarg)
//        {
//            object evop = null;
//            if (closureEnvironment.FastLexicalRef (out evop, this.expression.ratorName, this.expression.ratorDepth, this.expression.ratorOffset))
//                throw new NotImplementedException ();
//            //Control unevop = this.rator;
//            //env = closureEnvironment;
//            //while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
//            //if (evop == Interpreter.Unwind) {
//            //    ((UnwinderState) env).AddFrame (new Combination1Frame1 (this, closureEnvironment, evarg));
//            //    answer = Interpreter.Unwind;
//            //    closureEnvironment = env;
//            //    return false;
//            //}

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg);
//        }
//    }

//    [Serializable]
//    class Combination1A : Combination1L
//    {
//#if DEBUG
//        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination1A (Argument rator, SCode rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (Argument rator, SCode rand)
//        {
//            return
//                (rator is Argument0) ? Combination1A0.Make ((Argument0) rator, rand) :
//                (rator is Argument1) ? Combination1A1.Make ((Argument1) rator, rand) :
//                (rand is LexicalVariable) ? Combination1AL.Make(rator, (LexicalVariable) rand):
//                (rand is Quotation) ? Combination1AQ.Make (rator, (Quotation) rand) :
//                new Combination1A (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1A.EvalStep");
//            NoteCalls (this.rand);
//            randTypeHistogram.Note (this.randType);
//#endif
//            object evarg;
//            Control unev = this.rand;
//            Environment env = closureEnvironment;
//            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
//            if (evarg == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination1LFrame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//            object evop = closureEnvironment.ArgumentValue (this.ratorOffset);
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg);
//        }
//    }

//    [Serializable]
//    class Combination1A0 : Combination1A
//    {
//        protected Combination1A0 (Argument0 rator, SCode rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (Argument0 rator, SCode rand)
//        {
//            return
//                (rand is LexicalVariable) ? Combination1A0L.Make (rator, (LexicalVariable) rand) :
//                (rand is Quotation) ? Combination1A0Q.Make (rator, (Quotation) rand) :
//                new Combination1A0 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1A0.EvalStep");
//#endif

//            object evarg;
//            Control unev = this.rand;
//            Environment env = closureEnvironment;
//            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
//            if (evarg == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination1LFrame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, closureEnvironment.Argument0Value, evarg);
//        }
//    }

//    [Serializable]
//    class Combination1A0L : Combination1A0
//    {
//        public readonly object randName;
//        public readonly int randDepth;
//        public readonly int randOffset;

//        protected Combination1A0L (Argument0 rator, LexicalVariable rand)
//            : base (rator, rand)
//        {
//            this.randName = rand.Name;
//            this.randDepth = rand.Depth;
//            this.randOffset = rand.Offset;
//        }

//        public static SCode Make (Argument0 rator, LexicalVariable rand)
//        {
//            return
//                (rand is Argument) ? Combination1A0A.Make (rator, (Argument) rand) :
//                (rand is LexicalVariable1) ? Combination1A0L1.Make (rator, (LexicalVariable1) rand) :
//                new Combination1A0L (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1A0L.EvalStep");
//#endif
//            object evrand;
//            if (closureEnvironment.FastLexicalRef (out evrand, this.randName, this.randDepth, this.randOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment,
//                closureEnvironment.Argument0Value,
//                evrand);
//        }
//    }

//    [Serializable]
//    class Combination1A0A : Combination1A0L
//    {
//        protected Combination1A0A (Argument0 rator, Argument rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (Argument0 rator, Argument rand)
//        {
//            return
//                (rand is Argument0) ? Combination1A0A0.Make (rator, (Argument0) rand) :
//                (rand is Argument1) ? Combination1A0A1.Make (rator, (Argument1) rand) :
//                new Combination1A0A (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1A0A.EvalStep");
//#endif

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment,
//                closureEnvironment.Argument0Value,
//                closureEnvironment.ArgumentValue (this.randOffset));
//        }
//    }

//    [Serializable]
//    sealed class Combination1A0A0 : Combination1A0A
//    {

//        Combination1A0A0 (Argument0 rator, Argument0 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (Argument0 rator, Argument0 rand)
//        {
//            return
//                new Combination1A0A0 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1A0A0.EvalStep");
//#endif
//            object x = closureEnvironment.Argument0Value;
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, x, x);
//        }
//    }

//    [Serializable]
//    sealed class Combination1A0A1 : Combination1A0A
//    {
//        Combination1A0A1 (Argument0 rator, Argument1 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (Argument0 rator, Argument1 rand)
//        {
//            return
//                new Combination1A0A1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1A0A1.EvalStep");
//#endif
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment,
//                closureEnvironment.Argument0Value,
//                closureEnvironment.Argument1Value);
//        }
//    }

//    [Serializable]
//    sealed class Combination1A0L1 : Combination1A0L
//    {

//        Combination1A0L1 (Argument0 rator, LexicalVariable1 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (Argument0 rator, LexicalVariable1 rand)
//        {
//            return
//                new Combination1A0L1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1A0L1.EvalStep");
//#endif
//            object evrand;
//            if (closureEnvironment.FastLexicalRef1 (out evrand, this.randName, this.randOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment,
//                closureEnvironment.Argument0Value,
//                evrand);
//        }
//    }

//    [Serializable]
//    sealed class Combination1A0Q : Combination1A0
//    {
//        public readonly object randValue;

//        Combination1A0Q (Argument0 rator, Quotation rand)
//            : base (rator, rand)
//        {
//            this.randValue = rand.Quoted;
//        }

//        public static SCode Make (Argument0 rator, Quotation rand)
//        {
//            return
//                new Combination1A0Q (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1A0Q.EvalStep");
//#endif
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment,
//                closureEnvironment.Argument0Value,
//                this.randValue);
//        }
//    }

//    [Serializable]
//    class Combination1A1 : Combination1A
//    {
//        protected Combination1A1 (Argument1 rator, SCode rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (Argument1 rator, SCode rand)
//        {
//            return
//                (rand is LexicalVariable) ? Combination1A1L.Make (rator, (LexicalVariable) rand) :
//                (rand is Quotation) ? Combination1A1Q.Make (rator, (Quotation) rand) :
//                new Combination1A1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1A1.EvalStep");
//#endif

//            object evarg;
//            Control unev = this.rand;
//            Environment env = closureEnvironment;
//            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
//            if (evarg == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination1LFrame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, closureEnvironment.Argument1Value, evarg);
//        }
//    }

//    [Serializable]
//    class Combination1A1L : Combination1A1
//    {
//        public readonly object randName;
//        public readonly int randDepth;
//        public readonly int randOffset;

//        protected Combination1A1L (Argument1 rator, LexicalVariable rand)
//            : base (rator, rand)
//        {
//            this.randName = rand.Name;
//            this.randDepth = rand.Depth;
//            this.randOffset = rand.Offset;
//        }

//        public static SCode Make (Argument1 rator, LexicalVariable rand)
//        {
//            return
//                (rand is Argument) ? Combination1A1A.Make (rator, (Argument) rand) :
//                (rand is LexicalVariable1) ? Combination1A1L1.Make (rator, (LexicalVariable1) rand) :
//                new Combination1A1L (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1A1L.EvalStep");
//#endif
//            object evrand;
//            if (closureEnvironment.FastLexicalRef (out evrand, this.randName, this.randDepth, this.randOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment,
//                closureEnvironment.Argument1Value,
//                evrand);
//        }
//    }

//    [Serializable]
//    class Combination1A1A : Combination1A1L
//    {
//        protected Combination1A1A (Argument1 rator, Argument rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (Argument1 rator, Argument rand)
//        {
//            return
//                (rand is Argument0) ? Combination1A1A0.Make (rator, (Argument0) rand) :
//                (rand is Argument1) ? Combination1A1A1.Make (rator, (Argument1) rand) :
//                new Combination1A1A (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1A1A.EvalStep");
//#endif

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment,
//                closureEnvironment.Argument1Value,
//                closureEnvironment.ArgumentValue (this.randOffset));
//        }
//    }

//    [Serializable]
//    sealed class Combination1A1A0 : Combination1A1A
//    {
//        Combination1A1A0 (Argument1 rator, Argument0 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (Argument1 rator, Argument0 rand)
//        {
//            return
//                new Combination1A1A0 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1A1A0.EvalStep");
//#endif
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, closureEnvironment.Argument1Value, closureEnvironment.Argument0Value);
//        }
//    }

//    [Serializable]
//    sealed class Combination1A1A1 : Combination1A1A
//    {
//        Combination1A1A1 (Argument1 rator, Argument1 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (Argument1 rator, Argument1 rand)
//        {
//            return
//                new Combination1A1A1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1A1A1.EvalStep");
//#endif
//            object x = closureEnvironment.Argument1Value;
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, x, x);
//        }
//    }

//    [Serializable]
//    sealed class Combination1A1L1 : Combination1A1L
//    {
//        Combination1A1L1 (Argument1 rator, LexicalVariable1 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (Argument1 rator, LexicalVariable1 rand)
//        {
//            return
//                new Combination1A1L1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1A1L1.EvalStep");
//#endif
//            object evrand;
//            if (closureEnvironment.FastLexicalRef1 (out evrand, this.randName, this.randOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment,
//                closureEnvironment.Argument1Value,
//                evrand);
//        }
//    }

//    [Serializable]
//    sealed class Combination1A1Q : Combination1A1
//    {
//        public readonly object randValue;

//        Combination1A1Q (Argument1 rator, Quotation rand)
//            : base (rator, rand)
//        {
//            this.randValue = rand.Quoted;
//        }

//        public static SCode Make (Argument1 rator, Quotation rand)
//        {
//            return
//                new Combination1A1Q (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1A1Q.EvalStep");
//#endif
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment,
//                closureEnvironment.Argument1Value,
//                this.randValue);
//        }
//    }

//    [Serializable]
//    class Combination1AL : Combination1A
//    {
//        public readonly object randName;
//        public readonly int randDepth;
//        public readonly int randOffset;

//        protected Combination1AL (Argument rator, LexicalVariable rand)
//            : base (rator, rand)
//        {
//            this.randName = rand.Name;
//            this.randDepth = rand.Depth;
//            this.randOffset = rand.Offset;
//        }

//        public static SCode Make (Argument rator, LexicalVariable rand)
//        {
//            return
//                (rand is Argument) ? Combination1AA.Make (rator, (Argument) rand) :
//                (rand is LexicalVariable1) ? Combination1AL1.Make (rator, (LexicalVariable1) rand) :
//                new Combination1AL (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1AL.EvalStep");
//#endif
//            object evrand;
//            if (closureEnvironment.FastLexicalRef (out evrand, this.randName, this.randDepth, this.randOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment,
//                closureEnvironment.ArgumentValue (this.ratorOffset),
//                evrand);
//        }
//    }

//    [Serializable]
//    class Combination1AA : Combination1AL
//    {
//        protected Combination1AA (Argument rator, Argument rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (Argument rator, Argument rand)
//        {
//            return
//                (rand is Argument0) ? Combination1AA0.Make (rator, (Argument0) rand) :
//                (rand is Argument1) ? Combination1AA1.Make (rator, (Argument1) rand) :
//                new Combination1AA (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1AA.EvalStep");
//#endif

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment,
//                closureEnvironment.ArgumentValue (this.ratorOffset),
//                closureEnvironment.ArgumentValue (this.randOffset));
//        }
//    }

//    [Serializable]
//    sealed class Combination1AA0 : Combination1AA
//    {

//        Combination1AA0 (Argument rator, Argument0 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (Argument rator, Argument0 rand)
//        {
//            return
//                new Combination1AA0 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1AA0.EvalStep");
//#endif
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment,
//                closureEnvironment.ArgumentValue (this.ratorOffset),
//                closureEnvironment.Argument0Value);
//        }
//    }

//    [Serializable]
//    sealed class Combination1AA1 : Combination1AA
//    {

//        Combination1AA1 (Argument rator, Argument1 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (Argument rator, Argument1 rand)
//        {
//            return
//                new Combination1AA1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1AA1.EvalStep");
//#endif
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment,
//                closureEnvironment.ArgumentValue (this.ratorOffset),
//                closureEnvironment.Argument1Value);
//        }
//    }

//    [Serializable]
//    sealed class Combination1AL1 : Combination1AL
//    {

//        Combination1AL1 (Argument rator, LexicalVariable1 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (Argument rator, LexicalVariable1 rand)
//        {
//            return
//                new Combination1AL1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1AL1.EvalStep");
//#endif
//            object evrand;
//            if (closureEnvironment.FastLexicalRef1 (out evrand, this.randName, this.randOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment,
//                closureEnvironment.ArgumentValue (this.ratorOffset),
//                evrand);
//        }
//    }

//    [Serializable]
//    sealed class Combination1AQ : Combination1A
//    {
//        public readonly object randValue;

//        Combination1AQ (Argument rator, Quotation rand)
//            : base (rator, rand)
//        {
//            this.randValue = rand.Quoted;
//        }

//        public static SCode Make (Argument rator, Quotation rand)
//        {
//            return
//                new Combination1AQ (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1AQ.EvalStep");
//#endif
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment,
//                closureEnvironment.ArgumentValue (this.ratorOffset),
//                this.randValue);
//        }
//    }

//    [Serializable]
//    class Combination1L1 : Combination1L
//    {
//#if DEBUG
//        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination1L1 (LexicalVariable1 rator, SCode rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, SCode rand)
//        {
//            return
//                //(arg0 is PrimitiveCarA0) ? Combination1L1CarA0.Make (rator, (PrimitiveCarA0) arg0) :
//                (rand is PrimitiveCdrA0) ? Combination1L1CdrA0.Make (rator, (PrimitiveCdrA0) rand) :
//                (rand is LexicalVariable) ? Combination1L1L.Make (rator, (LexicalVariable) rand) :
//                (rand is Quotation) ? Combination1L1Q.Make (rator, (Quotation) rand) :
//                new Combination1L1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1L1.EvalStep");
//            NoteCalls (this.rand);
//            randTypeHistogram.Note (this.randType);
//#endif

//            object evarg;
//            Control unev = this.rand;
//            Environment env = closureEnvironment;
//            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
//            if (evarg == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new Combination1L1Frame0 (this, closureEnvironment));
//                answer = Interpreter.Unwind;
//                closureEnvironment = env;
//                return false;
//            }

//            object evop;
//            if (closureEnvironment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg);
//        }
//    }

//    [Serializable]
//    sealed class Combination1L1Frame0 : SubproblemContinuation<Combination1L1>, ISystemVector
//    {
//        public Combination1L1Frame0 (Combination1L1 combination1, Environment closureEnvironment)
//            : base (combination1, closureEnvironment)
//        {
//        }

//        #region ISystemVector Members

//        public int SystemVectorSize
//        {
//            get { throw new NotImplementedException (); }
//        }

//        public object SystemVectorRef (int index)
//        {
//            throw new NotImplementedException ();
//        }

//        public object SystemVectorSet (int index, object newValue)
//        {
//            throw new NotImplementedException ();
//        }

//        #endregion

//        public override bool Continue (out object answer, ref Control expression, ref Environment closureEnvironment, object evarg)
//        {
//            object evop = null;
//            if (closureEnvironment.FastLexicalRef1 (out evop, this.expression.ratorName, this.expression.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg);
//        }
//    }

//    [Serializable]
//    class Combination1L1L : Combination1L1
//    {
//        public readonly object randName;
//        public readonly int randDepth;
//        public readonly int randOffset;

//        protected Combination1L1L (LexicalVariable1 rator, LexicalVariable rand)
//            : base (rator, rand)
//        {
//            this.randName = rand.Name;
//            this.randDepth = rand.Depth;
//            this.randOffset = rand.Offset;
//        }

//        public static SCode Make (LexicalVariable1 rator, LexicalVariable rand)
//        {
//            return
//                (rand is Argument) ? Combination1L1A.Make (rator, (Argument) rand) :
//                (rand is LexicalVariable1) ? Combination1L1L1.Make (rator, (LexicalVariable1) rand):
//                new Combination1L1L (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1L1L.EvalStep");
//#endif
//            object evarg;
//            if (closureEnvironment.FastLexicalRef (out evarg, this.randName, this.randDepth, this.randOffset))
//                throw new NotImplementedException ();
//            object evop;
//            if (closureEnvironment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg);
//        }
//    }

//    [Serializable]
//    class Combination1L1A : Combination1L1L
//    {
//        protected Combination1L1A (LexicalVariable1 rator, Argument rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument rand)
//        {
//            return
//                (rand is Argument0) ? Combination1L1A0.Make (rator, (Argument0) rand) :
//                (rand is Argument1) ? Combination1L1A1.Make (rator, (Argument1) rand) :
//                new Combination1L1A (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1L1A.EvalStep");
//#endif
//            object evop;
//            if (closureEnvironment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, closureEnvironment.ArgumentValue(this.randOffset));
//        }
//    }

//    [Serializable]
//    sealed class Combination1L1A0 : Combination1L1A
//    {
//        Combination1L1A0 (LexicalVariable1 rator, Argument0 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument0 rand)
//        {
//            return
//                new Combination1L1A0 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1L1A0.EvalStep");
//#endif
//            object evop;
//            if (closureEnvironment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, closureEnvironment.Argument0Value);
//        }
//    }

//    [Serializable]
//    sealed class Combination1L1A1 : Combination1L1A
//    {
//        Combination1L1A1 (LexicalVariable1 rator, Argument1 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument1 rand)
//        {
//            return
//                new Combination1L1A1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1L1A1.EvalStep");
//#endif
//            object evop;
//            if (closureEnvironment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, closureEnvironment.Argument1Value);
//        }
//    }

//    [Serializable]
//    sealed class Combination1L1L1 : Combination1L1L
//    {
//        Combination1L1L1 (LexicalVariable1 rator, LexicalVariable1 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, LexicalVariable1 rand)
//        {
//            return
//                new Combination1L1L1 (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1L1A1.EvalStep");
//#endif
//            object evarg;
//            if (closureEnvironment.FastLexicalRef1 (out evarg, this.randName, this.randOffset))
//                throw new NotImplementedException ();

//            object evop;
//            if (closureEnvironment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg);
//        }
//    }

//    [Serializable]
//    sealed class Combination1L1Q : Combination1L1
//    {
//        public readonly object randValue;

//        Combination1L1Q (LexicalVariable1 rator, Quotation rand)
//            : base (rator, rand)
//        {
//            this.randValue = rand.Quoted;
//        }

//        public static SCode Make (LexicalVariable1 rator, Quotation rand)
//        {
//            return
//                new Combination1L1Q (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1L1Q.EvalStep");
//#endif
//            object evop;
//            if (closureEnvironment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, this.randValue);
//        }
//    }

//    [Serializable]
//    class Combination1LL : Combination1L
//    {
//        public readonly object rand0Name;
//        public readonly int rand0Depth;
//        public readonly int rand0Offset;

//        protected Combination1LL (LexicalVariable rator, LexicalVariable rand)
//            : base (rator, rand)
//        {
//            this.rand0Name = rand.Name;
//            this.rand0Depth = rand.Depth;
//            this.rand0Offset = rand.Offset;
//        }

//        public static SCode Make (LexicalVariable rator, LexicalVariable rand0)
//        {
//            return
//                (rand0 is Argument) ? Combination1LA.Make (rator, (Argument) rand0) :
//                (rand0 is LexicalVariable1) ? Combination1LL1.Make (rator, (LexicalVariable1) rand0) :
//                new Combination1LL (rator, rand0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1LL.EvalStep");
//#endif
//            object evarg;
//            if (closureEnvironment.FastLexicalRef (out evarg, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop = null;
//            if (closureEnvironment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg);
//        }
//    }

//    [Serializable]
//    class Combination1LA : Combination1LL
//    {
//        protected Combination1LA (LexicalVariable rator, Argument rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument rand0)
//        {
//            return
//                (rand0 is Argument0) ? Combination1LA0.Make (rator, (Argument0) rand0) :
//                (rand0 is Argument1) ? Combination1LA1.Make (rator, (Argument1) rand0) :
//                new Combination1LA (rator, rand0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1LA.EvalStep");
//#endif
//            object evop = null;
//            if (closureEnvironment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, closureEnvironment.ArgumentValue (this.rand0Offset));
//        }
//    }

//    [Serializable]
//    sealed class Combination1LA0 : Combination1LA
//    {
//        Combination1LA0 (LexicalVariable rator, Argument rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument0 rand0)
//        {
//            return
//                 new Combination1LA0 (rator, rand0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1LA0.EvalStep");
//#endif
//            object evop = null;
//            if (closureEnvironment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, closureEnvironment.Argument0Value);
//        }
//    }

//    [Serializable]
//    sealed class Combination1LA1 : Combination1LA
//    {
//        Combination1LA1 (LexicalVariable rator, Argument1 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument1 rand0)
//        {
//            return
//                 new Combination1LA1 (rator, rand0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1LA1.EvalStep");
//#endif
//            object evop = null;
//            if (closureEnvironment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, closureEnvironment.Argument1Value);
//        }
//    }

//    [Serializable]
//    sealed class Combination1LL1 : Combination1LL
//    {
//        Combination1LL1 (LexicalVariable rator, LexicalVariable1 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, LexicalVariable1 rand0)
//        {
//            return
//                new Combination1LL1 (rator, rand0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1LL1.EvalStep");
//#endif
//            object evarg;
//            if (closureEnvironment.FastLexicalRef1 (out evarg, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop = null;
//            if (closureEnvironment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg);
//        }
//    }

//    [Serializable]
//    sealed class Combination1LQ : Combination1L
//    {
//        public readonly object rand0Value;

//        Combination1LQ (LexicalVariable rator, Quotation rand)
//            : base (rator, rand)
//        {
//            this.rand0Value = rand.Quoted;
//        }

//        public static SCode Make (LexicalVariable rator, Quotation rand0)
//        {
//            return
//                new Combination1LQ (rator, rand0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1LQ.EvalStep");
//#endif
//            object evop = null;
//            if (closureEnvironment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, this.rand0Value);
//        }
//    }

//    [Serializable]
//    class Combination1T : Combination1
//    {
//#if DEBUG
//        static Histogram<object> ratorNameHistogram = new Histogram<object> ();
//#endif
//        public readonly TopLevelVariable ratorVar;
//        protected Combination1T (TopLevelVariable rator, SCode rand)
//            : base (rator, rand)
//        {
//            this.ratorVar = rator;
//        }

//        public static SCode Make (TopLevelVariable rator, SCode arg0)
//        {
//            return
//                (arg0 is LexicalVariable) ? Combination1TL.Make (rator, (LexicalVariable) arg0) :
//                (arg0 is Quotation) ? Combination1TQ.Make (rator, (Quotation) arg0) :
//                new Combination1T (rator, arg0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1T.EvalStep");
//#endif
//            object evarg;
//            Control expr = this.rand;
//            Environment env = closureEnvironment;
//            while (expr.EvalStep (out evarg, ref expr, ref env)) { };
//            if (evarg == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new Combination1TFrame1 (this, closureEnvironment));
//                answer = Interpreter.Unwind;
//                closureEnvironment = env;
//                return false;
//            }

//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg);
//        }
//    }

//    [Serializable]
//    sealed class Combination1TFrame1 : SubproblemContinuation<Combination1T>, ISystemVector
//    {
//        public Combination1TFrame1 (Combination1T combination1, Environment closureEnvironment)
//            : base (combination1, closureEnvironment)
//        {
//        }

//        public override bool Continue (out object answer, ref Control expression, ref Environment closureEnvironment, object evarg)
//        {
//            object evop;
//            if (this.expression.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg);
//        }

//        #region ISystemVector Members

//        public int SystemVectorSize
//        {
//            get { throw new NotImplementedException (); }
//        }

//        public object SystemVectorRef (int index)
//        {
//            throw new NotImplementedException ();
//        }

//        public object SystemVectorSet (int index, object newValue)
//        {
//            throw new NotImplementedException ();
//        }

//        #endregion
//    }

//    [Serializable]
//    class Combination1TL : Combination1T
//    {
//#if DEBUG
//        static Histogram<string> ratorNameHistogram = new Histogram<string> ();
//#endif
//        public readonly object rand0Name;
//        public readonly int rand0Depth;
//        public readonly int rand0Offset;

//        protected Combination1TL (TopLevelVariable rator, LexicalVariable rand)
//            : base (rator, rand)
//        {
//            this.rand0Name = rand.Name;
//            this.rand0Depth = rand.Depth;
//            this.rand0Offset = rand.Offset;
//        }

//        public static SCode Make (TopLevelVariable rator, LexicalVariable arg0)
//        {
//            return
//                (arg0 is Argument) ? Combination1TA.Make (rator, (Argument) arg0) :
//                (arg0 is LexicalVariable1) ? Combination1TL1.Make (rator, (LexicalVariable1) arg0) :
//                new Combination1TL (rator, arg0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            ratorNameHistogram.Note (this.ratorVar.Name.ToString ());
//            Warm ("Combination1TL.EvalStep");
//#endif
//            object evarg;
//            if (closureEnvironment.FastLexicalRef (out evarg, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//#if DEBUG
//            if (this.ratorVar.breakOnReference)
//                Debugger.Break ();
//#endif

//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg);
//        }
//    }

//    [Serializable]
//    class Combination1TA : Combination1TL
//    {
//        protected Combination1TA (TopLevelVariable rator, Argument rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument arg0)
//        {
//            return
//                (arg0 is Argument0) ? Combination1TA0.Make (rator, (Argument0) arg0) :
//                (arg0 is Argument1) ? Combination1TA1.Make (rator, (Argument1) arg0) :
//                new Combination1TA (rator, arg0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1TA.EvalStep");
//#endif
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, closureEnvironment.ArgumentValue (this.rand0Offset));
//        }
//    }

//    [Serializable]
//    sealed class Combination1TA0 : Combination1TA
//    {
//        Combination1TA0 (TopLevelVariable rator, Argument0 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument0 arg0)
//        {
//            return
//                 new Combination1TA0 (rator, arg0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1TA0.EvalStep");
//#endif
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, closureEnvironment.Argument0Value);
//        }
//    }

//    [Serializable]
//    sealed class Combination1TA1 : Combination1TA
//    {
//        Combination1TA1 (TopLevelVariable rator, Argument1 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument1 arg0)
//        {
//            return
//                 new Combination1TA1 (rator, arg0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1TA1.EvalStep");
//#endif
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, closureEnvironment.Argument1Value);
//        }
//    }

//    [Serializable]
//    sealed class Combination1TL1 : Combination1TL
//    {
//        Combination1TL1 (TopLevelVariable rator, LexicalVariable1 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, LexicalVariable1 arg0)
//        {
//            return
//                new Combination1TL1 (rator, arg0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1TL1.EvalStep");
//#endif
//            object evarg;
//            if (closureEnvironment.FastLexicalRef1 (out evarg, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg);
//        }
//    }

//    [Serializable]
//    sealed class Combination1TQ : Combination1T
//    {
//        public readonly object randValue;

//        Combination1TQ (TopLevelVariable rator, Quotation rand)
//            : base (rator, rand)
//        {
//            this.randValue = rand.Quoted;
//        }

//        public static SCode Make (TopLevelVariable rator, Quotation rand)
//        {
//            return new Combination1TQ (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1TQ.EvalStep");
//#endif
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, this.randValue);
//        }
//    }

//    [Serializable]
//    class Combination1SL : Combination1
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object randName;
//        public readonly int randDepth;
//        public readonly int randOffset;

//        protected Combination1SL (SCode rator, LexicalVariable rand)
//            : base (rator, rand)
//        {
//            this.randName = rand.Name;
//            this.randDepth = rand.Depth;
//            this.randOffset = rand.Offset;
//        }

//        public static SCode Make (SCode rator, LexicalVariable rand)
//        {
//            return
//                (rand is Argument) ? Combination1SA.Make (rator, (Argument) rand) :
//                (rand is LexicalVariable1) ? Combination1SL1.Make (rator, (LexicalVariable1) rand) :
//                new Combination1SL (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//            SCode.location = "Combination1SL.EvalStep";
//#endif
//            object evrand;
//            if (closureEnvironment.FastLexicalRef (out evrand, this.randName, this.randDepth, this.randOffset))
//                throw new NotImplementedException ();

//            object evop;
//            Control unev = this.rator;
//            Environment env = closureEnvironment;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evrand);
//        }
//    }

//    [Serializable]
//    class Combination1SA : Combination1SL
//    {
//        protected Combination1SA (SCode rator, Argument rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (SCode rator, Argument arg0)
//        {
//            return 
//                (arg0 is Argument0) ? Combination1SA0.Make (rator, (Argument0) arg0) :
//                (arg0 is Argument1) ? Combination1SA1.Make (rator, (Argument1) arg0) :
//                new Combination1SA (rator, arg0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1SA.EvalStep");
//            NoteCalls (this.rator);
//#endif

//            object evop;
//            Control unev = this.rator;
//            Environment env = closureEnvironment;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination1SAFrame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                // return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, closureEnvironment.ArgumentValue (this.randOffset));
//        }
//    }

//    [Serializable]
//    sealed class Combination1SA0 : Combination1SA
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        Combination1SA0 (SCode rator, Argument0 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (SCode rator, Argument0 arg0)
//        {
//            return new Combination1SA0 (rator, arg0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//            SCode.location = "Combination1SA0.EvalStep";
//#endif

//            object evop;
//            Control unev = this.rator;
//            Environment env = closureEnvironment;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination1SA0.EvalStep";
//#endif
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination1SAFrame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                // return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, closureEnvironment.Argument0Value);
//        }
//    }

//    [Serializable]
//    sealed class Combination1SA1 : Combination1SA
//    {
//        Combination1SA1 (SCode rator, Argument1 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (SCode rator, Argument1 arg0)
//        {
//            return new Combination1SA1 (rator, arg0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rator);
//            SCode.location = "Combination1SA1.EvalStep";
//#endif

//            object evop;
//            Control unev = this.rator;
//            Environment env = closureEnvironment;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination1SAFrame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                // return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, closureEnvironment.Argument1Value);
//        }
//    }

//    [Serializable]
//    sealed class Combination1SL1 : Combination1SL
//    {
//        Combination1SL1 (SCode rator, LexicalVariable1 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (SCode rator, LexicalVariable1 arg0)
//        {
//            return new Combination1SL1 (rator, arg0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rator);
//            SCode.location = "Combination1SL1.EvalStep";
//#endif
//            object evarg;
//            if (closureEnvironment.FastLexicalRef1 (out evarg, this.randName, this.randOffset))
//                throw new NotImplementedException ();

//            object evop;
//            Control unev = this.rator;
//            Environment env = closureEnvironment;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination1SAFrame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                // return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg);
//        }
//    }

//    [Serializable]
//    sealed class Combination1SQ : Combination1
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object randValue;

//        Combination1SQ (SCode rator, Quotation rand)
//            : base (rator, rand)
//        {
//            this.randValue = rand.Quoted;
//        }

//        public static SCode Make (SCode rator, Quotation rand)
//        {
//            return new Combination1SQ (rator, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1SQ.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif
//            object evop;
//            Control unev = this.rator;
//            Environment env = closureEnvironment;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, this.randValue);
//        }
//    }

////-------------------------------------


//    [Serializable]
//    class Combination1LPComb1 : Combination1L
//    {
//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        protected readonly Primitive1 procedure;

//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        [NonSerialized]
//        protected PrimitiveMethod1 method;

//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        protected readonly SCode arg0;

//        protected Combination1LPComb1 (LexicalVariable rator, PrimitiveCombination1 rand)
//            : base (rator, rand)
//        {
//            this.procedure = rand.Operator;
//            this.method = procedure.Method;
//            this.arg0 = rand.Operand;
//        }

//        public static SCode Make (LexicalVariable rator, PrimitiveCombination1 rand0)
//        {
//            return 
//                (rand0 is PrimitiveCar) ? Combination1LCar.Make (rator, (PrimitiveCar) rand0)
//                : (rand0 is PrimitiveCdr) ? Combination1LCdr.Make (rator, (PrimitiveCdr) rand0)
//                : new Combination1LPComb1 (rator, rand0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1LPComb1.EvalStep");
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

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
            
//#endif
//            object evarg;

//            if (this.method (out evarg, ev0)) {
//                TailCallInterpreter tci = evarg as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    expression = tci.Expression;
//                    closureEnvironment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }

//            object evop;
//            if (closureEnvironment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg);
//        }
//    }

//    [Serializable]
//    class Combination1LCar : Combination1LPComb1
//    {

//        protected Combination1LCar (LexicalVariable rator, PrimitiveCar rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, PrimitiveCar rand0)
//        {
//            return
//                (rand0 is PrimitiveCarL) ? Combination1LCarL.Make (rator, (PrimitiveCarL) rand0)
//                : new Combination1LCar (rator, rand0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ();
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

//            Cons evarg = ev0 as Cons;
//            if (evarg == null) throw new NotImplementedException ();

//            object evop;
//            if (closureEnvironment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg.Car);
//        }
//    }

//    [Serializable]
//    class Combination1LCarL : Combination1LCar
//    {
//        public readonly object rand0Name;
//        public readonly int rand0Depth;
//        public readonly int rand0Offset;

//        protected Combination1LCarL (LexicalVariable rator, PrimitiveCarL rand)
//            : base (rator, rand)
//        {
//            this.rand0Name = ((LexicalVariable) rand.Operand).Name;
//            this.rand0Depth = ((LexicalVariable) rand.Operand).Depth;
//            this.rand0Offset = ((LexicalVariable) rand.Operand).Offset;
//        }

//        public static SCode Make (LexicalVariable rator, PrimitiveCarL rand0)
//        {
//            return
//                (rand0 is PrimitiveCarA) ? Combination1LCarA.Make (rator, (PrimitiveCarA) rand0)
//                : new Combination1LCarL (rator, rand0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1LCarL.EvalStep");
//#endif

//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            Cons evarg = ev0 as Cons;
//            if (evarg == null) throw new NotImplementedException ();

//            object evop;
//            if (closureEnvironment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg.Car);
//        }
//    }

//    [Serializable]
//    class Combination1LCarA : Combination1LCarL
//    {
//        protected Combination1LCarA (LexicalVariable rator, PrimitiveCarA rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, PrimitiveCarA rand0)
//        {
//            return
//                (rand0 is PrimitiveCarA0) ? Combination1LCarA0.Make (rator, (PrimitiveCarA0) rand0) :
//                (rand0 is PrimitiveCarA1) ? Combination1LCarA1.Make (rator, (PrimitiveCarA1) rand0) :
//                new Combination1LCarA (rator, rand0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1LCarA.EvalStep");
//#endif
//            Cons evarg = closureEnvironment.ArgumentValue (this.rand0Offset) as Cons;
//            if (evarg == null) throw new NotImplementedException ();

//            object evop;
//            if (closureEnvironment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg.Car);
//        }
//    }

//    [Serializable]
//    sealed class Combination1LCarA0 : Combination1LCarA
//    {
//        Combination1LCarA0 (LexicalVariable rator, PrimitiveCarA0 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, PrimitiveCarA0 rand0)
//        {
//            return
//                new Combination1LCarA0 (rator, rand0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1LCarA0.EvalStep");
//#endif
//            object evop;
//            if (closureEnvironment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, ((Cons) closureEnvironment.Argument0Value).Car);
//        }
//    }

//    [Serializable]
//    sealed class Combination1LCarA1 : Combination1LCarA
//    {
//        Combination1LCarA1 (LexicalVariable rator, PrimitiveCarA1 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, PrimitiveCarA1 rand0)
//        {
//            return
//                new Combination1LCarA1 (rator, rand0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1LCarA1.EvalStep");
//#endif
//            object evop;
//            if (closureEnvironment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, ((Cons) closureEnvironment.Argument1Value).Car);
//        }
//    }

//    [Serializable]
//    class Combination1LCdr : Combination1LPComb1
//    {

//        protected Combination1LCdr (LexicalVariable rator, PrimitiveCdr rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, PrimitiveCdr rand0)
//        {
//            return
//                (rand0 is PrimitiveCdrL) ? Combination1LCdrL.Make (rator, (PrimitiveCdrL) rand0)
//                : new Combination1LCdr (rator, rand0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1LCdr.EvalStep");
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

//            Cons evarg = ev0 as Cons;
//            if (evarg == null) throw new NotImplementedException ();

//            object evop;
//            if (closureEnvironment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg.Cdr);
//        }
//    }

//    [Serializable]
//    class Combination1LCdrL : Combination1LCdr
//    {
//        public readonly object rand0Name;
//        public readonly int rand0Depth;
//        public readonly int rand0Offset;

//        protected Combination1LCdrL (LexicalVariable rator, PrimitiveCdrL rand)
//            : base (rator, rand)
//        {
//            this.rand0Name = rand.OperandName;
//            this.rand0Depth = rand.OperandDepth;
//            this.rand0Offset = rand.OperandOffset;
//        }

//        public static SCode Make (LexicalVariable rator, PrimitiveCdrL rand0)
//        {
//            return
//                (rand0 is PrimitiveCdrA) ? Combination1LCdrA.Make (rator, (PrimitiveCdrA) rand0) :
//                (rand0 is PrimitiveCdrL1) ? Combination1LCdrL1.Make (rator, (PrimitiveCdrL1) rand0) :
//                new Combination1LCdrL (rator, rand0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1LCdrL.EvalStep");
//#endif

//            object ev0;
//            if (closureEnvironment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            Cons evarg = ev0 as Cons;
//            if (evarg == null) throw new NotImplementedException ();

//            object evop;
//            if (closureEnvironment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg.Cdr);
//        }
//    }

//    [Serializable]
//    class Combination1LCdrA : Combination1LCdrL
//    {

//        protected Combination1LCdrA (LexicalVariable rator, PrimitiveCdrA rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, PrimitiveCdrA rand0)
//        {
//            return

//                new Combination1LCdrA (rator, rand0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1LCdrA.EvalStep");
//#endif

//            object ev0 = closureEnvironment.ArgumentValue (this.rand0Offset);

//            Cons evarg = ev0 as Cons;
//            if (evarg == null) throw new NotImplementedException ();

//            object evop;
//            if (closureEnvironment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, evarg.Cdr);
//        }
//    }


 




//    // --------
//    [Serializable]
//    sealed class Combination1LCdrL1 : Combination1LCdrL
//    {
//        Combination1LCdrL1 (LexicalVariable rator, PrimitiveCdrL1 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, PrimitiveCdrL1 arg0)
//        {
//            return new Combination1LCdrL1 (rator, arg0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1LCdrL1");
//#endif
//            object evarg;
//            if (closureEnvironment.FastLexicalRef1 (out evarg, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (closureEnvironment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, ((Cons) evarg).Cdr);
//        }
//    }

//    [Serializable]
//    sealed class Combination1L1CarA0 : Combination1L1
//    {
//        Combination1L1CarA0 (LexicalVariable1 rator, PrimitiveCarA0 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, PrimitiveCarA0 arg0)
//        {
//            return new Combination1L1CarA0 (rator, arg0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1L1CarA0.EvalStep");
//#endif
//            object evop;
//            if (closureEnvironment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, ((Cons) (closureEnvironment.Argument0Value)).Car);
//        }
//    }

//    [Serializable]
//    sealed class Combination1L1CdrA0 : Combination1L1
//    {
//        Combination1L1CdrA0 (LexicalVariable1 rator, PrimitiveCdrA0 rand)
//            : base (rator, rand)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, PrimitiveCdrA0 arg0)
//        {
//            return new Combination1L1CdrA0 (rator, arg0);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination1L1CdrA0.EvalStep");
//#endif
//            object evop;
//            if (closureEnvironment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, evop, ((Cons) (closureEnvironment.Argument0Value)).Cdr);
//        }
//    }
}
