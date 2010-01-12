using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    [Serializable]
    class Combination2 : SCode, ISerializable, ISystemHunk3
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.COMBINATION_2; } }

#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<string> histogram = new Histogram<String> ();
        protected readonly string histogramKey;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Type ratorType;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Type rand0Type;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Type rand1Type;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand1;

        protected Combination2 (SCode rator, SCode rand0, SCode rand1)
            : base ()
        {
            this.rator = rator;
            this.rand0 = rand0;
            this.rand1 = rand1;
#if DEBUG
            this.histogramKey = rator.GetType ().ToString () + " " + rand0.GetType ().Name.ToString () + " " + rand1.GetType ().Name.ToString ();
            ratorType = rator.GetType ();
            rand0Type = rand0.GetType ();
            rand1Type = rand1.GetType ();
#endif
        }

        public static SCode Make (SCode rator, SCode rand0, SCode rand1)
        {
            return
                (! Configuration.EnableCombination2Optimization) ? new Combination2(rator, rand0, rand1):

                // (rator is Quotation &&
                // ! (((Quotation) rator).Quoted is PrimitiveN)) ? Unimplemented():

                (! Configuration.EnableCombination2Specialization) ? new Combination2 (rator, rand0, rand1) :
                (Configuration.EnableLet2 && rator is Lambda) ? Let2.Make ((Lambda) rator, rand0, rand1) :
                (rator is Argument) ? Combination2A.Make ((Argument) rator, rand0, rand1) :
                (rator is FreeVariable) ? Combination2F.Make ((FreeVariable) rator, rand0, rand1) :
                (rator is Quotation) ? Combination2Q.Make ((Quotation) rator, rand0, rand1) :
                (rator is TopLevelVariable) ? Combination2T.Make ((TopLevelVariable) rator, rand0, rand1) :
                (rator is StaticVariable) ? Combination2S.Make((StaticVariable) rator, rand0, rand1) :
                (rand0 is Argument) ? Combination2XA.Make (rator, (Argument) rand0, rand1) :
                (rand0 is Quotation) ? Combination2XQ.Make (rator, (Quotation) rand0, rand1) :
                (rand0 is StaticVariable) ? Combination2XS.Make (rator, (StaticVariable) rand0, rand1) :
                (rand1 is Argument) ? Combination2XXA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? Combination2XXQ.Make (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? Combination2XXS.Make (rator, rand0, (StaticVariable) rand1) :
                 new Combination2 (rator, rand0, rand1);
        }

        public static SCode Make (object rator, object rand0, object rand1)
        {
            SCode srator = EnsureSCode (rator);
            SCode srand0 = EnsureSCode (rand0);
            SCode srand1 = EnsureSCode (rand1);
            return Make (srator, srand0, srand1);
        }

        public static SCode Make (Hunk3 init)
        {
            return Make (init.Cxr0, init.Cxr1, init.Cxr2);
        }

        public SCode Operand0
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand0;
            }
        }

        public SCode Operand1
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand1;
            }
        }

        public SCode Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.rator;
            }
        }

        [SchemePrimitive ("COMBINATION2?", 1, true)]
        public static bool IsCombination2 (out object answer, object arg)
        {
            answer = arg is Combination2;
            return false;
        }

        #region ISystemHunk3 Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr0
        {
            get
            {
                return UnwrapQuoted (rator);
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
                return UnwrapQuoted (rand0);
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
                return UnwrapQuoted (rand1);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion


        public override bool CallsTheEnvironment ()
        {
            return this.rator.CallsTheEnvironment ()
                || this.rand0.CallsTheEnvironment ()
                || this.rand1.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            histogram.Note (this.histogramKey);
            ratorTypeHistogram.Note (this.ratorType);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2";
#endif
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            return this.rator.MutatesAny (formals)
                || this.rand0.MutatesAny (formals)
                || this.rand1.MutatesAny (formals);
        }

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (Combination2Deserializer));
            info.AddValue ("rator", this.rator);
            info.AddValue ("rand0", this.rand0);
            info.AddValue ("rand1", this.rand1);
        }

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialResult rator = this.rator.PartialEval (environment);
            PartialResult rand0 = this.rand0.PartialEval (environment);
            PartialResult rand1 = this.rand1.PartialEval (environment);
            return new PartialResult (rator.Residual == this.rator &&
                rand0.Residual == this.rand0 &&
                rand1.Residual == this.rand1 ? this : Combination2.Make (rator.Residual, rand0.Residual, rand1.Residual));
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            this.rator.CollectFreeVariables (freeVariableSet);
            this.rand0.CollectFreeVariables (freeVariableSet);
            this.rand1.CollectFreeVariables (freeVariableSet);
        }
    }

    [Serializable]
    internal sealed class Combination2Deserializer : IObjectReference
    {
        SCode rator;
        SCode rand0;
        SCode rand1;

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public Object GetRealObject (StreamingContext context)
        {
            return Combination2.Make (this.rator, this.rand0, this.rand1);
        }
        SCode Rator { set { this.rator = value; } }
        SCode Rand0 { set { this.rand0 = value; } }
        SCode Rand1 { set { this.rand1 = value; } }
    }

    [Serializable]
    class Combination2Frame0 : SubproblemContinuation<Combination2>, ISystemVector
    {

        public Combination2Frame0 (Combination2 combination2, Environment environment)
            : base (combination2, environment)
        {
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            object ev0 = null;
            Environment env = environment;
            Control unev0 = this.expression.Operand0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object evop = null;
            env = environment;
            Control unevop = this.expression.Operator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, value);
        }
    }

    [Serializable]
    class Combination2Frame1 : SubproblemContinuation<Combination2>, ISystemVector
    {
        readonly object ev1;

        public Combination2Frame1 (Combination2 combination2, Environment environment, object ev1)
            : base (combination2, environment)
        {
            this.ev1 = ev1;
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            object evop = null;
            Environment env = environment;
            Control unevop = this.expression.Operator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, value, this.ev1);
        }
    }

    [Serializable]
    class Combination2Frame2 : SubproblemContinuation<Combination2>, ISystemVector
    {
        readonly object ev0;
        readonly object ev1;

        public Combination2Frame2 (Combination2 combination2, Environment environment, object ev0, object ev1)
            : base (combination2, environment)
        {
            this.ev0 = ev0;
            this.ev1 = ev1;
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            object evop = null;
            Environment env = environment;
            Control unevop = this.expression.Operator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, value, this.ev0, this.ev1);
        }
    }

    [Serializable]
    class Combination2F : Combination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Symbol> ratorNames = new Histogram<Symbol> ();
#endif
        public readonly Symbol ratorName;
        protected ValueCell ratorCell;

        protected Combination2F (FreeVariable rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.ratorName = rator.Name;
        }

        public static SCode Make (FreeVariable rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? Combination2FA.Make (rator, (Argument) rand0, rand1) :
                (rand0 is Quotation) ? Combination2FQ.Make (rator, (Quotation) rand0, rand1) :
                (rand0 is StaticVariable) ? Combination2FS.Make (rator, (StaticVariable) rand0, rand1) :
                (rand1 is Argument) ? Combination2FXA.Make (rator, rand0, (Argument) rand1):
                (rand1 is Quotation) ? new Combination2FXQ (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new Combination2FXS (rator, rand0, (StaticVariable) rand1) :
                 new Combination2F (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            ratorNames.Note (this.ratorName);
            SCode.location = "Combination2F";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2F";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2F.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2FA : Combination2F
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;
        protected Combination2FA (FreeVariable rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (FreeVariable rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? Combination2FA0.Make (rator, (Argument0) rand0, rand1) :
                (rand1 is Argument) ? Combination2FAA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? new Combination2FAQ (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new Combination2FAS (rator, rand0, (StaticVariable) rand1) :
                 new Combination2FA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2FA";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2FA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2FA0 : Combination2FA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2FA0 (FreeVariable rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (FreeVariable rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? Combination2FA0A.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? Combination2FA0S.Make (rator, rand0, (StaticVariable) rand1) :
                (rand1 is Quotation) ? Combination2FA0Q.Make (rator, rand0, (Quotation) rand1) :
                 new Combination2FA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2FA0";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2FA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0 = environment.Argument0Value;

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2FA0A : Combination2FA0
    {
        public readonly int rand1Offset;
        protected Combination2FA0A (FreeVariable rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (FreeVariable rator, Argument0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2FA0A0.Make (rator, rand0, (Argument0) rand1) :
                 new Combination2FA0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2FA0A0");
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2FA0A0 : Combination2FA0A
    {
        Combination2FA0A0 (FreeVariable rator, Argument0 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (FreeVariable rator, Argument0 rand0, Argument0 rand1)
        {
            return
                 new Combination2FA0A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2FA0A0");
#endif
            object ev0 = environment.Argument0Value;

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev0);
        }
    }

    [Serializable]
    sealed class Combination2FA0S : Combination2FA0
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        Combination2FA0S (FreeVariable rator, Argument0 rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (FreeVariable rator, Argument0 rand0, StaticVariable rand1)
        {
            return
                 new Combination2FA0S (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2FA0S");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
            throw new NotImplementedException(); 

            object ev0 = environment.Argument0Value;

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2FA0Q : Combination2FA0
    {
        public readonly object rand1Value;
        Combination2FA0Q (FreeVariable rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (FreeVariable rator, Argument0 rand0, Quotation rand1)
        {
            return
                 new Combination2FA0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2FA0Q");
#endif
            object ev1 = this.rand1Value;

            object ev0 = environment.Argument0Value;

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2FAA : Combination2FA
    {
        public readonly int rand1Offset;
        internal Combination2FAA (FreeVariable rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (FreeVariable rator, Argument rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2FAA0.Make (rator, rand0, (Argument0) rand1) :
               new Combination2FAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2FAA";
#endif


            object ev0 = environment.ArgumentValue (this.rand0Offset);

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.ArgumentValue (this.rand1Offset));
        }
    }

    [Serializable]
    class Combination2FAA0 : Combination2FAA
    {
        internal Combination2FAA0 (FreeVariable rator, Argument rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (FreeVariable rator, Argument rand0, Argument0 rand1)
        {
            return
                new Combination2FAA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2FAA0";
#endif


            object ev0 = environment.ArgumentValue (this.rand0Offset);

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument0Value);
        }
    }

    [Serializable]
    sealed class Combination2FAQ : Combination2FA
    {
        public readonly object rand1Value;
        internal Combination2FAQ (FreeVariable rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2FAQ";
#endif


            object ev0 = environment.ArgumentValue (this.rand0Offset);

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
        }
    }

    [Serializable]
    sealed class Combination2FAS : Combination2FA
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        internal Combination2FAS (FreeVariable rator, Argument rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (FreeVariable rator, Argument rand0, StaticVariable rand1)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2FAS");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.ArgumentValue(this.rand0Offset);

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }


    [Serializable]
    class Combination2FQ : Combination2F
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;

        protected Combination2FQ (FreeVariable rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        public static SCode Make (FreeVariable rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? Combination2FQA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? new Combination2FQQ (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new Combination2FQS (rator, rand0, (StaticVariable) rand1) :
                 new Combination2FQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2FQ";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2FQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
        }
    }

    [Serializable]
    class Combination2FQA : Combination2FQ
    {
        public readonly int rand1Offset;

        protected Combination2FQA (FreeVariable rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (FreeVariable rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2FQA0.Make (rator, rand0, (Argument0) rand1) :
                 new Combination2FQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2FQA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
        }
    }

    [Serializable]
    class Combination2FQA0 : Combination2FQA
    {

        protected Combination2FQA0 (FreeVariable rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (FreeVariable rator, Quotation rand0, Argument0 rand1)
        {
            return
                 new Combination2FQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2FQA0";
#endif
            object ev1 = environment.Argument0Value;

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
        }
    }

    [Serializable]
    sealed class Combination2FQQ : Combination2FQ
    {
        public readonly object rand1Value;

        internal Combination2FQQ (FreeVariable rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2FQ";
#endif
            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, this.rand1Value);
        }
    }

    [Serializable]
    sealed class Combination2FQS : Combination2FQ
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal Combination2FQS (FreeVariable rator, Quotation rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2FQS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
        }
    }


    [Serializable]
    class Combination2FS : Combination2F
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;
        protected Combination2FS (FreeVariable rator, StaticVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (FreeVariable rator, StaticVariable rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? Combination2FSA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? new Combination2FSQ (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new Combination2FSS (rator, rand0, (StaticVariable) rand1) :
                 new Combination2FS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2FS";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2FS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0;
            if (environment.StaticValue(out ev0, this.rand0Name, this.rand0Offset))
            throw new NotImplementedException(); 

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2FSA : Combination2FS
    {
        public readonly int rand1Offset;
        protected Combination2FSA (FreeVariable rator, StaticVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (FreeVariable rator, StaticVariable rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2FSA0.Make (rator, rand0, (Argument0) rand1) :
                new Combination2FSA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2FSA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2FSA0 : Combination2FSA
    {
        protected Combination2FSA0 (FreeVariable rator, StaticVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (FreeVariable rator, StaticVariable rand0, Argument0 rand1)
        {
            return
                 new Combination2FSA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2FSA0";
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2FSQ : Combination2FS
    {
        public readonly object rand1Value;
        internal Combination2FSQ (FreeVariable rator, StaticVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2FSQ";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
        }
    }

    [Serializable]
    sealed class Combination2FSS : Combination2FS
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        internal Combination2FSS (FreeVariable rator, StaticVariable rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2FSS");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2FXA : Combination2F
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif

        public readonly int rand1Offset;
        protected Combination2FXA (FreeVariable rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (FreeVariable rator, SCode rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? new Combination2FXA0 (rator, rand0, (Argument0) rand1) :
                 new Combination2FXA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2FXA";
#endif

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2FXA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }


            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.ArgumentValue(this.rand1Offset));
        }
    }

    [Serializable]
    sealed class Combination2FXA0 : Combination2FXA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        internal Combination2FXA0 (FreeVariable rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2FXA0";
#endif

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2FXA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }


            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument0Value);
        }
    }

    [Serializable]
    sealed class Combination2FXQ : Combination2F
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        internal Combination2FXQ (FreeVariable rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2FXQ";
#endif
            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2FXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
        }
    }


    [Serializable]
    sealed class Combination2FXS : Combination2F
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        internal Combination2FXS (FreeVariable rator, SCode rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2FXS";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2FXS";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }


            object evop;

            if (this.ratorCell == null) {
                Environment baseEnvironment = environment.BaseEnvironment;

                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
            }
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ("Error with free variable " + this.ratorName);

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2T : Combination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly ValueCell ratorCell;

        protected Combination2T (TopLevelVariable rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.ratorCell = rator.valueCell;
        }

        public static SCode Make (TopLevelVariable rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? Combination2TA.Make (rator, (Argument) rand0, rand1) :
                (rand0 is Quotation) ? Combination2TQ.Make (rator, (Quotation) rand0, rand1) :
                (rand0 is StaticVariable) ? Combination2TS.Make (rator, (StaticVariable) rand0, rand1) :
                (rand1 is Argument) ? Combination2TXA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? Combination2TXQ.Make (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? Combination2TXS.Make (rator, rand0, (StaticVariable) rand1) :
                 new Combination2T (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2T";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2T";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2T.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2TA : Combination2T
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;

        protected Combination2TA (TopLevelVariable rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (TopLevelVariable rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? Combination2TA0.Make (rator, (Argument0) rand0, rand1) :
                (rand1 is Argument) ? Combination2TAA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? new Combination2TAQ (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new Combination2TAS (rator, rand0, (StaticVariable) rand1) :
                 new Combination2TA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2TA";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2TA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2TA0 : Combination2TA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected Combination2TA0 (TopLevelVariable rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (TopLevelVariable rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? Combination2TA0A.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? Combination2TA0Q.Make (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? Combination2TA0S.Make (rator, rand0, (StaticVariable) rand1) :
                 new Combination2TA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2TA0";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2TA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0 = environment.Argument0Value;

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2TA0A : Combination2TA0
    {
        public readonly int rand1Offset;
        protected Combination2TA0A (TopLevelVariable rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (TopLevelVariable rator, Argument0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2TA0A0.Make (rator,rand0, (Argument0) rand1):
                 new Combination2TA0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2TA0A");
#endif
            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, environment.ArgumentValue(this.rand1Offset));
        }
    }

    [Serializable]
    sealed class Combination2TA0A0 : Combination2TA0A
    {
        Combination2TA0A0 (TopLevelVariable rator, Argument0 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (TopLevelVariable rator, Argument0 rand0, Argument0 rand1)
        {
            return
                 new Combination2TA0A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2TA0A0");
#endif
            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, environment.Argument0Value);
        }
    }

    [Serializable]
    sealed class Combination2TA0S : Combination2TA0
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        Combination2TA0S (TopLevelVariable rator, Argument0 rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (TopLevelVariable rator, Argument0 rand0, StaticVariable rand1)
        {
            return
                 new Combination2TA0S (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2TA0S");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, ev1);
        }
    }

    [Serializable]
    sealed class Combination2TA0Q : Combination2TA0
    {
        public readonly object rand1Value;
        Combination2TA0Q (TopLevelVariable rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (TopLevelVariable rator, Argument0 rand0, Quotation rand1)
        {
            return
                 new Combination2TA0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2TA0Q");
#endif
            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, this.rand1Value);
        }
    }

    [Serializable]
    class Combination2TAA : Combination2TA
    {

        public readonly int rand1Offset;

        protected Combination2TAA (TopLevelVariable rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (TopLevelVariable rator, Argument rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2TAA0.Make (rator, rand0, (Argument0) rand1) :
                new Combination2TAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2TAA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
 
            object ev0 = environment.ArgumentValue (this.rand0Offset);

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2TAA0 : Combination2TAA
    {
        protected Combination2TAA0 (TopLevelVariable rator, Argument rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (TopLevelVariable rator, Argument rand0, Argument0 rand1)
        {
            return
                 new Combination2TAA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2TAA0";
#endif
            object ev1 = environment.Argument0Value;

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2TAQ : Combination2TA
    {
        public readonly object rand1Value;

        internal Combination2TAQ (TopLevelVariable rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (TopLevelVariable rator, Argument rand0, SCode rand1)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2TAQ";
#endif

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
        }
    }

    [Serializable]
    sealed class Combination2TAS : Combination2TA
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal Combination2TAS (TopLevelVariable rator, Argument rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (TopLevelVariable rator, Argument rand0, SCode rand1)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2TAS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2TQ : Combination2T
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;

        protected Combination2TQ (TopLevelVariable rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        public static SCode Make (TopLevelVariable rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? Combination2TQA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? new Combination2TQQ (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new Combination2TQS (rator, rand0, (StaticVariable) rand1) :
                 new Combination2TQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2TQ";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2TQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
        }
    }

    [Serializable]
    class Combination2TQA : Combination2TQ
    {
        public readonly int rand1Offset;

        protected Combination2TQA (TopLevelVariable rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (TopLevelVariable rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2TQA0.Make (rator, rand0, (Argument0) rand1) :
               new Combination2TQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2TQA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
        }
    }

    [Serializable]
    class Combination2TQA0 : Combination2TQA
    {

        protected Combination2TQA0 (TopLevelVariable rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (TopLevelVariable rator, Quotation rand0, Argument0 rand1)
        {
            return
                new Combination2TQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2TQA0";
#endif
            object ev1 = environment.Argument0Value;

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
        }
    }

    [Serializable]
    sealed class Combination2TQQ : Combination2TQ
    {
        public readonly object rand1Value;

        internal Combination2TQQ (TopLevelVariable rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (TopLevelVariable rator, Quotation rand0, SCode rand1)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2TQQ";
#endif


            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, this.rand1Value);
        }
    }

    [Serializable]
    sealed class Combination2TQS : Combination2TQ
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal Combination2TQS (TopLevelVariable rator, Quotation rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (TopLevelVariable rator, Quotation rand0, SCode rand1)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2TQS";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
        }
    }


    [Serializable]
    class Combination2TS : Combination2T
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected Combination2TS (TopLevelVariable rator, StaticVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (TopLevelVariable rator, StaticVariable rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? Combination2TSA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? new Combination2TSS (rator, rand0, (StaticVariable) rand1) :
                (rand1 is Quotation) ? new Combination2TSQ (rator, rand0, (Quotation) rand1) :
                 new Combination2TS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2TS";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2TS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2TSQ : Combination2TS
    {

        public readonly object rand1Value;
        internal Combination2TSQ (TopLevelVariable rator, StaticVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }


        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2TSQ";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
        }
    }

    [Serializable]
    class Combination2TSA : Combination2TS
    {
        public readonly int rand1Offset;

        protected Combination2TSA (TopLevelVariable rator, StaticVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (TopLevelVariable rator, StaticVariable rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? new Combination2TSA0 (rator, rand0, (Argument0) rand1) :
                 new Combination2TSA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2TSA");
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2TSA0 : Combination2TSA
    {
        internal Combination2TSA0 (TopLevelVariable rator, StaticVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2TSA0");
#endif
            object ev1 = environment.Argument0Value;

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2TSS : Combination2TS
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal Combination2TSS (TopLevelVariable rator, StaticVariable rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2TSS");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2TXA : Combination2T
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;

        protected Combination2TXA (TopLevelVariable rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (TopLevelVariable rator, SCode rand0, Argument rand1)
        {
            return
                 new Combination2TXA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2TXA";
#endif
            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2TXA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2TXQ : Combination2T
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected Combination2TXQ (TopLevelVariable rator, SCode rand0,Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (TopLevelVariable rator, SCode rand0, Quotation rand1)
        {
            return
                 new Combination2TXQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2TXQ";
#endif
            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2TXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }



            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
        }
    }

    [Serializable]
    class Combination2TXS : Combination2T
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Offset;

        protected Combination2TXS (TopLevelVariable rator, SCode rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (TopLevelVariable rator, SCode rand0, StaticVariable rand1)
        {
            return
                 new Combination2TXS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2TXS";
#endif
            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2TXS";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2XA : Combination2
    {
        protected readonly int rand0Offset;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2XA (SCode rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (SCode rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? Combination2XA0.Make (rator, (Argument0) rand0, rand1) :
                (rand1 is Argument) ? Combination2XAA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? Combination2XAS.Make (rator, rand0, (StaticVariable) rand1) :
                (rand1 is Quotation) ? Combination2XAQ.Make (rator, rand0, (Quotation) rand1) :
                new Combination2XA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand1);
            ratorTypeHistogram.Note (this.ratorType);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2XA.EvalStep";
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue(this.rand0Offset), ev1);
        }
    }

    [Serializable]
    class Combination2XAA : Combination2XA
    {
        protected readonly int rand1Offset;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2XAA (SCode rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (SCode rator, Argument rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2XAA0.Make (rator, rand0, (Argument0) rand1) :
                new Combination2XAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XAA.EvalStep";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.ArgumentValue (this.rand0Offset);

            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2XAA0 : Combination2XAA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        Combination2XAA0 (SCode rator, Argument rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (SCode rator, Argument rand0, Argument0 rand1)
        {
            return
                new Combination2XAA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XAA0.EvalStep";
#endif
            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2XAA0.EvalStep";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue(this.rand0Offset), environment.Argument0Value);
        }
    }

    [Serializable]
    sealed class Combination2XAS : Combination2XA
    {
        readonly object rand1Name;
        readonly int rand1Offset;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        Combination2XAS (SCode rator, Argument rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (SCode rator, Argument rand0, StaticVariable rand1)
        {
            return
                new Combination2XAS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XAS.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset)) {
                throw new NotImplementedException ();
            }
            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue(this.rand0Offset), ev1);
        }
    }

    [Serializable]
    sealed class Combination2XAQ : Combination2XA
    {
        public readonly object rand1Value;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif

        Combination2XAQ (SCode rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (SCode rator, Argument rand0, Quotation rand1)
        {
            return new Combination2XAQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XAQ.EvalStep";
#endif

            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue(this.rand0Offset), this.rand1Value);
        }
    }

    [Serializable]
    class Combination2XA0 : Combination2XA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2XA0 (SCode rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (SCode rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? Combination2XA0A.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? Combination2XA0S.Make (rator, rand0, (StaticVariable) rand1) :
                (rand1 is Quotation) ? Combination2XA0Q.Make (rator, rand0, (Quotation) rand1) :
                new Combination2XA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand1);
            ratorTypeHistogram.Note (this.ratorType);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2XA0.EvalStep";
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, environment.Argument0Value, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, ev1);
        }
    }

    [Serializable]
    class Combination2XA0A : Combination2XA
    {
        protected readonly int rand1Offset;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2XA0A (SCode rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (SCode rator, Argument0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2XA0A0.Make (rator, rand0, (Argument0) rand1) :
                new Combination2XA0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XA0A.EvalStep";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument0Value;

            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2XA0A0 : Combination2XAA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        Combination2XA0A0 (SCode rator, Argument0 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (SCode rator, Argument0 rand0, Argument0 rand1)
        {
            return
                new Combination2XA0A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XA0A0.EvalStep";
#endif
            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2XA0A0.EvalStep";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, environment.Argument0Value);
        }
    }

    [Serializable]
    sealed class Combination2XA0S : Combination2XA
    {
        readonly object rand1Name;
        readonly int rand1Offset;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        Combination2XA0S (SCode rator, Argument0 rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (SCode rator, Argument0 rand0, StaticVariable rand1)
        {
            return
                new Combination2XA0S (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XA0S.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset)) {
                throw new NotImplementedException ();
            }
            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, ev1);
        }
    }

    [Serializable]
    sealed class Combination2XA0Q : Combination2XA
    {
        public readonly object rand1Value;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif

        Combination2XA0Q (SCode rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (SCode rator, Argument0 rand0, Quotation rand1)
        {
            return new Combination2XA0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XA0Q.EvalStep";
#endif

            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, this.rand1Value);
        }
    }

    [Serializable]
    class Combination2XQ : Combination2
    {
        protected readonly object rand0Value;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2XQ (SCode rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted; ;
        }

        public static SCode Make (SCode rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? Combination2XQA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? Combination2XQS.Make (rator, rand0, (StaticVariable) rand1) :
                (rand1 is Quotation) ? Combination2XQQ.Make (rator, rand0, (Quotation) rand1) :
                new Combination2XQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand1);
            ratorTypeHistogram.Note (this.ratorType);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2XQ.EvalStep";
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
        }
    }

    [Serializable]
    class Combination2XQA : Combination2XQ
    {

#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif    
        protected readonly int rand1Offset;
        protected Combination2XQA (SCode rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (SCode rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2XQA0.Make (rator, rand0, (Argument0) rand1) :
                new Combination2XQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XQA.EvalStep";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2XQA0 : Combination2XQA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        Combination2XQA0 (SCode rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (SCode rator, Quotation rand0, Argument0 rand1)
        {
            return
                new Combination2XQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XQA0.EvalStep";
#endif
            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2XQA0.EvalStep";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, environment.Argument0Value);
        }
    }

    [Serializable]
    sealed class Combination2XQS : Combination2XQ
    {
        readonly object rand1Name;
        readonly int rand1Offset;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        Combination2XQS (SCode rator, Quotation rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (SCode rator, Quotation rand0, StaticVariable rand1)
        {
            return
                new Combination2XQS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XQS.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset)) {
                throw new NotImplementedException ();
            }
            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
        }
    }

    [Serializable]
    sealed class Combination2XQQ : Combination2XQ
    {
        public readonly object rand1Value;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif

        Combination2XQQ (SCode rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (SCode rator, Quotation rand0, Quotation rand1)
        {
            return new Combination2XQQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XQQ.EvalStep";
#endif

            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, this.rand1Value);
        }
    }

    [Serializable]
    class Combination2XS : Combination2
    {
        protected readonly Symbol rand0Name;
        protected readonly int rand0Offset;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2XS (SCode rator, StaticVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (SCode rator, StaticVariable rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? Combination2XSA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? new Combination2XSS (rator, rand0, (StaticVariable) rand1) :
                (rand1 is Quotation) ? new Combination2XSQ (rator, rand0, (Quotation) rand1) :
                new Combination2XS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand1);
            ratorTypeHistogram.Note (this.ratorType);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2XS.EvalStep";
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2XSA : Combination2XS
    {
        protected readonly int rand1Offset;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        internal Combination2XSA (SCode rator, StaticVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }
        public static SCode Make (SCode rator, StaticVariable rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2XSA0.Make (rator, rand0, (Argument0) rand1) :
                new Combination2XSA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XSA.EvalStep";
#endif

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.ArgumentValue(this.rand1Offset));
        }
    }

       [Serializable]
    class Combination2XSA0 : Combination2XSA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        internal Combination2XSA0 (SCode rator, StaticVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }
        public static SCode Make (SCode rator, StaticVariable rand0, Argument0 rand1)
        {
            return
                new Combination2XSA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XSA0.EvalStep";
#endif

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument0Value);
        }
    }

    [Serializable]
    sealed class Combination2XSQ : Combination2XS
    {
        protected readonly object rand1Value;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        internal Combination2XSQ (SCode rator, StaticVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }


        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XSQ.EvalStep";
#endif

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
        }
    }

    [Serializable]
    sealed class Combination2XSS : Combination2XS
    {
        public  readonly Symbol rand1Name;
        public readonly int rand1Offset;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        internal Combination2XSS (SCode rator, StaticVariable rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }


        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XSS.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }


    [Serializable]
    class Combination2XXA : Combination2
    {
        protected readonly int rand1Offset;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2XXA (SCode rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (SCode rator, SCode rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2XXA0.Make (rator, rand0, (Argument0) rand1) :
                new Combination2XXA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand0);
            ratorTypeHistogram.Note (this.ratorType);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2XXA.EvalStep";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2XXA0 : Combination2XXA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        Combination2XXA0 (SCode rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (SCode rator, SCode rand0, Argument0 rand1)
        {
            return
                new Combination2XXA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand0);
            ratorTypeHistogram.Note (this.ratorType);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2XXA0.EvalStep";
#endif
            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2XXA0.EvalStep.1";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2XXA0.EvalStep.2";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument0Value);
        }
    }

    [Serializable]
    sealed class Combination2XXS : Combination2
    {
        object rand1Name;
        readonly int rand1Offset;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        Combination2XXS (SCode rator, SCode rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (SCode rator, SCode rand0, StaticVariable rand1)
        {
            return
                new Combination2XXS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand0);
            ratorTypeHistogram.Note (this.ratorType);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2XXS.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    sealed class Combination2XXQ : Combination2
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        Combination2XXQ (SCode rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (SCode rator, SCode rand0, Quotation rand1)
        {
            return new Combination2XXQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand0);
            ratorTypeHistogram.Note (this.ratorType);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2XXQ.EvalStep";
#endif
            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
        }
    }
}
