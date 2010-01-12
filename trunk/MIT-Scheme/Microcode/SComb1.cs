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
                (rator is Argument) ? Combination1A.Make ((Argument) rator, rand) :
                (rator is FreeVariable) ? Combination1F.Make ((FreeVariable) rator, rand) :
                (rator is Quotation) ? Combination1Q.Make ((Quotation) rator, rand) :
                (rator is StaticVariable) ? Combination1S.Make((StaticVariable) rator, rand) :
                (rator is TopLevelVariable) ? Combination1T.Make ((TopLevelVariable) rator, rand) :
                (Configuration.EnableLet1 &&
                 rator is Lambda) ? Let1.Make ((Lambda) rator, rand)  :

                //(Configuration.EnableCombination1Specialization &&
                //rator is TopLevelVariable) ? Combination1T.Make ((TopLevelVariable) rator, rand) :
                //(rator is Quotation &&
                //! (((Quotation) rator).Quoted is PrimitiveN)) ? Unimplemented() :
                (rand is Argument) ? Combination1XA.Make(rator, (Argument) rand) :
                (rand is StaticVariable) ? Combination1XS.Make (rator, (StaticVariable) rand) :
                (rand is Quotation) ? Combination1XQ.Make (rator, (Quotation) rand) :
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
            SCode.location = "Combination1";
#endif
            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination1";
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
            SCode.location = "Combination1";
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
    class Combination1A : Combination1
    {
#if DEBUG
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int ratorOffset;

        protected Combination1A (Argument rator, SCode rand)
            : base (rator, rand)
        {
            this.ratorOffset = rator.Offset;
        }

        public static SCode Make (Argument rator, SCode rand0)
        {
            return
                (rator is Argument0) ? Combination1A0.Make ((Argument0) rator, rand0) :
                (rand0 is Argument) ? Combination1AA.Make (rator, (Argument) rand0) :
                (rand0 is Quotation) ? new Combination1AQ (rator, (Quotation) rand0) :
                (rand0 is StaticVariable) ? new Combination1AS (rator, (StaticVariable) rand0) :
                new Combination1A (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
            SCode.location = "Combination1A";
#endif
            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination1A";
#endif
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue(this.ratorOffset), evarg);
        }
    }

    [Serializable]
    class Combination1AA : Combination1A
    {
        public readonly int randOffset;

        protected Combination1AA (Argument rator, Argument rand)
            : base (rator, rand)
        {
            this.randOffset = rand.Offset;
        }

        public static SCode Make (Argument rator, Argument rand0)
        {
            return
                (rand0 is Argument0) ? new Combination1AA0 (rator,(Argument0)  rand0) :
                new Combination1AA (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination1AA";
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), environment.ArgumentValue(this.randOffset));
        }
    }
    
    [Serializable]
    sealed class Combination1AA0 : Combination1AA
    {
        internal Combination1AA0 (Argument rator, Argument0 rand)
            : base (rator, rand)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination1AA0";
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), environment.Argument0Value);
        }
    }


    [Serializable]
    class Combination1A0 : Combination1A
    {
#if DEBUG
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif
        protected Combination1A0 (Argument0 rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (Argument0 rator, SCode rand0)
        {
            return
                (rand0 is Argument) ? Combination1A0A.Make (rator, (Argument) rand0) :
                (rand0 is Quotation) ? new Combination1A0Q (rator, (Quotation) rand0) :
                (rand0 is StaticVariable) ? new Combination1A0S (rator, (StaticVariable) rand0) :
                new Combination1A0 (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
            SCode.location = "Combination1A0";
#endif
            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination1A0";
#endif
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value, evarg);
        }
    }

    [Serializable]
    class Combination1A0A : Combination1A0
    {
        public readonly int randOffset;
        protected Combination1A0A (Argument0 rator, Argument rand)
            : base (rator, rand)
        {
            this.randOffset = rand.Offset;
        }

        public static SCode Make (Argument0 rator, Argument rand0)
        {
            return
                (rand0 is Argument0) ? new Combination1A0A0 (rator, (Argument0) rand0) :
                 new Combination1A0A (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination1A0A";
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value, environment.ArgumentValue(this.randOffset));
        }
    }

    [Serializable]
    sealed class Combination1A0A0 : Combination1A0A
    {
        internal Combination1A0A0 (Argument0 rator, Argument0 rand)
            : base (rator, rand)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination1A0A0";
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value, environment.Argument0Value);
        }
    }

    [Serializable]
    sealed class Combination1A0Q : Combination1A0
    {
        public readonly object randValue;
        internal Combination1A0Q (Argument0 rator, Quotation rand)
            : base (rator, rand)
        {
            this.randValue = rand.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination1A0Q";
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value, this.randValue);
        }
    }

    [Serializable]
    sealed class Combination1A0S : Combination1A0
    {
        public readonly Symbol randName;
        public readonly int randOffset;
        internal Combination1A0S (Argument0 rator, StaticVariable rand)
            : base (rator, rand)
        {
            this.randName = rand.Name;
            this.randOffset = rand.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination1A0S";
#endif
            object evarg;
            if (environment.StaticValue (out evarg, this.randName, this.randOffset))
                throw new NotImplementedException ();
            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value, evarg);
        }
    }


    [Serializable]
    sealed class Combination1AQ : Combination1A
    {
        public readonly object randValue;
        internal Combination1AQ (Argument rator, Quotation rand)
            : base (rator, rand)
        {
            this.randValue = rand.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination1AQ";
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue(this.ratorOffset), this.randValue);
        }
    }

    [Serializable]
    sealed class Combination1AS : Combination1A
    {
        public readonly Symbol randName;
        public readonly int randOffset;
        internal Combination1AS (Argument rator, StaticVariable rand)
            : base (rator, rand)
        {
            this.randName = rand.Name;
            this.randOffset = rand.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination1AS";
#endif
            object evarg;
            if (environment.StaticValue (out evarg, this.randName, this.randOffset))
                throw new NotImplementedException ();
            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue(this.ratorOffset), evarg);
        }
    }

    #region Combination1F

    [Serializable]
    class Combination1F : Combination1
    {
#if DEBUG
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol ratorName;
        protected ValueCell ratorCell;

        protected Combination1F (FreeVariable rator, SCode rand)
            : base (rator, rand)
        {
            this.ratorName = rator.Name;
        }

        public static SCode Make (FreeVariable rator, SCode rand0)
        {
            return
                (rand0 is Argument) ? Combination1FA.Make (rator, (Argument) rand0) :
                (rand0 is StaticVariable) ? Combination1FS.Make (rator, (StaticVariable) rand0) :
                (rand0 is Quotation) ? new Combination1FQ (rator, (Quotation) rand0) :
                new Combination1F (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
            SCode.location = "Combination1F";
#endif
            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination1F";
#endif
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    [Serializable]
    class Combination1FA : Combination1F
    {
        public readonly int randOffset;

        protected Combination1FA (FreeVariable rator, Argument rand)
            : base (rator, rand)
        {
            this.randOffset = rand.Offset;
        }

        public static SCode Make (FreeVariable rator, Argument rand0)
        {
            return
                (rand0 is Argument0) ? Combination1FA0.Make (rator, (Argument0) rand0) :
                new Combination1FA (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1FA");
#endif
            object evarg = environment.ArgumentValue (this.randOffset);

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    [Serializable]
    class Combination1FA0 : Combination1FA
    {
        protected Combination1FA0 (FreeVariable rator, Argument0 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (FreeVariable rator, Argument0 rand0)
        {
            return
                new Combination1FA0 (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1FA0");
#endif
            object evarg = environment.Argument0Value;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    [Serializable]
    sealed class Combination1FQ : Combination1F
    {
        public readonly object randValue;

        internal Combination1FQ (FreeVariable rator, Quotation rand)
            : base (rator, rand)
        {
            this.randValue = rand.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1FQ");
#endif
            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.randValue);
        }
    }


    [Serializable]
    class Combination1FS : Combination1F
    {
        public readonly Symbol randName;
        public readonly int randOffset;

        protected Combination1FS (FreeVariable rator, StaticVariable rand)
            : base (rator, rand)
        {
            this.randName = rand.Name;
            this.randOffset = rand.Offset;
        }

        public static SCode Make (FreeVariable rator,StaticVariable rand0)
        {
            return
                new Combination1FS (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1FS");
#endif
            object evarg;
            if (environment.StaticValue (out evarg, this.randName, this.randOffset))
                throw new NotImplementedException ();

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    #endregion

    [Serializable]
    class Combination1Q : Combination1
    {
#if DEBUG
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif
        public readonly IApplicable ratorValue;

        protected Combination1Q (Quotation rator, SCode rand)
            : base (rator, rand)
        {
            this.ratorValue = (IApplicable) rator.Quoted;
        }

        public static SCode Make (Quotation rator, SCode rand0)
        {
            return
                (rand0 is Quotation) ? new Combination1QQ (rator, (Quotation) rand0) :
                (rand0 is StaticVariable) ? new Combination1QS (rator, (StaticVariable) rand0) :
                new Combination1Q (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
            SCode.location = "Combination1Q";
#endif
            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination1Q";
#endif
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            return this.ratorValue.Call (out answer, ref expression, ref environment, evarg);
        }
    }

    [Serializable]
    sealed class Combination1QQ : Combination1Q
    {
        public readonly object randValue;

        internal Combination1QQ (Quotation rator, Quotation rand)
            : base (rator, rand)
        {
            this.randValue = rand.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1QQ");
#endif
            return this.ratorValue.Call (out answer, ref expression, ref environment, this.randValue);
        }
    }

    sealed class Combination1QS : Combination1Q
    {
        public readonly Symbol randName;
        public readonly int randOffset;

        internal Combination1QS (Quotation rator, StaticVariable rand)
            : base (rator, rand)
        {
            this.randName = rand.Name;
            this.randOffset = rand.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination1QS";
#endif
            object evarg;
            if (environment.StaticValue (out evarg, this.randName, this.randOffset))
                throw new NotImplementedException ();
            return this.ratorValue.Call (out answer, ref expression, ref environment, evarg);
        }
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
                (rand0 is PrimitiveCdrA0) ? Combination1SCdrA0.Make (rator, (PrimitiveCdrA0) rand0) :
                (rand0 is Argument) ? Combination1SA.Make (rator, (Argument) rand0) :
                (rand0 is Quotation) ? new Combination1SQ (rator, (Quotation) rand0) :
                (rand0 is StaticVariable) ? new Combination1SS (rator, (StaticVariable) rand0) :
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
    sealed class Combination1SCdrA0 : Combination1S
    {
        Combination1SCdrA0 (StaticVariable rator, PrimitiveCdrA0 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (StaticVariable rator, PrimitiveCdrA0 rand0)
        {
            return
                new Combination1SCdrA0 (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1SCdrA0");
#endif
            Cons evarg = environment.Argument0Value as Cons;
            if (evarg == null)
                throw new NotImplementedException ();

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            //return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg.Cdr);
            IApplicable op = evop as IApplicable;
            if (op == null) throw new NotImplementedException ("Application of non-procedure object.");
            return op.Call (out answer, ref expression, ref environment, evarg.Cdr);
        }
    }

    [Serializable]
    class Combination1SA : Combination1S
    {
        public readonly int argOffset;

        protected Combination1SA (StaticVariable rator, Argument rand)
            : base (rator, rand)
        {
        this.argOffset = rand.Offset;
        }

        public static SCode Make (StaticVariable rator, Argument rand0)
        {
            return
                (rand0 is Argument0) ? Combination1SA0.Make (rator, (Argument0) rand0) :
               new Combination1SA (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1SA");
#endif
            object evarg = environment.ArgumentValue (this.argOffset);

            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    [Serializable]
    sealed class Combination1SA0 : Combination1SA
    {
        Combination1SA0 (StaticVariable rator, Argument0 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (StaticVariable rator, Argument0 rand0)
        {
            return
                new Combination1SA0 (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1SA0");
#endif
            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value);
        }
    }

    [Serializable]
    sealed class Combination1SQ : Combination1S
    {
        public readonly object rand0Value;

        internal Combination1SQ (StaticVariable rator, Quotation rand)
            : base (rator, rand)
        {
            this.rand0Value = rand.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1SQ");
#endif
            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value);
        }
    }

    [Serializable]
    sealed class Combination1SS : Combination1S
    {
        public readonly Symbol randName;
        public readonly int randOffset;

        internal Combination1SS (StaticVariable rator, StaticVariable rand)
            : base (rator, rand)
        {
            this.randName = rand.Name;
            this.randOffset = rand.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1SS");
#endif
            object evrand;
            if (environment.StaticValue (out evrand, this.randName, this.randOffset))
                throw new NotImplementedException ();
            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evrand);
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
                (rand0 is Argument) ? Combination1TA.Make (rator, (Argument) rand0) :
                (rand0 is StaticVariable) ? Combination1TS.Make (rator, (StaticVariable) rand0) :
                (rand0 is Quotation) ? new Combination1TQ (rator, (Quotation) rand0) :
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
                new Combination1TA (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1TA");
#endif
            object evarg = environment.ArgumentValue (this.randOffset);

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }
    }

    [Serializable]
    sealed class Combination1TA0 : Combination1TA
    {
        Combination1TA0 (TopLevelVariable rator, Argument0 rand)
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
    sealed class Combination1TQ : Combination1T
    {
        public readonly object randValue;

        internal Combination1TQ (TopLevelVariable rator, Quotation rand)
            : base (rator, rand)
        {
            this.randValue = rand.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1TQ");
#endif
            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.randValue);
        }
    }

    [Serializable]
    sealed class Combination1TS : Combination1T
    {
        public readonly Symbol randName;
        public readonly int randOffset;

        Combination1TS (TopLevelVariable rator, StaticVariable rand)
            : base (rator, rand)
        {
            this.randName = rand.Name;
            this.randOffset = rand.Offset;
        }

        public static SCode Make (TopLevelVariable rator, StaticVariable rand0)
        {
            return
                new Combination1TS (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination1TS");
#endif
            object evarg;
            if (environment.StaticValue (out evarg, this.randName, this.randOffset))
                throw new NotImplementedException ();

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
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
    sealed class Combination1XA0 : Combination1XA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        Combination1XA0 (SCode rator, Argument0 rand)
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
    sealed class Combination1XS : Combination1
    {         
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        Combination1XS (SCode rator, StaticVariable rand)
            : base (rator, rand)
        {
            this.rand1Name = rand.Name;
            this.rand1Offset = rand.Offset;
        }

        public static SCode Make (SCode rator, StaticVariable rand0)
        {
            return
                new Combination1XS (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination1XS";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object evop;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination1XS";
#endif
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, ev0));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0);
        }
}

    [Serializable]
    sealed class Combination1XQ : Combination1
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object randValue;

        Combination1XQ (SCode rator, Quotation rand)
            : base (rator, rand)
        {
            this.randValue = rand.Quoted;
        }

        public static SCode Make (SCode rator, Quotation rand0)
        {
            return
                new Combination1XQ (rator, rand0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination1XQ";
#endif

            object evop;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination1XQ";
#endif
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, this.randValue));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.randValue);
        }
    }
}
