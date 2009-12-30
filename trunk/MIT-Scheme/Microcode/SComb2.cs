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
                //(rand1 is Argument) ? Combination2XXA.Make (rator, rand0, (Argument) rand1) :
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
            SCode.location = "Combination2.EvalStep";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2.EvalStep";
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
            SCode.location = "Combination2.EvalStep";
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
            SCode.location = "Combination2.EvalStep";
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
                (rand0 is Argument1) ? Combination2FA1.Make (rator, (Argument1) rand0, rand1) :
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
                (rand1 is Argument1) ? Combination2FA0A1.Make (rator, rand0, (Argument1) rand1) :
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
    sealed class Combination2FA0A1 : Combination2FA0A
    {
        Combination2FA0A1 (FreeVariable rator, Argument0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (FreeVariable rator, Argument0 rand0, Argument1 rand1)
        {
            return
                 new Combination2FA0A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2FA0A1");
#endif
            object ev1 = environment.Argument1Value;

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
    class Combination2FA1 : Combination2FA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2FA1 (FreeVariable rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (FreeVariable rator, Argument1 rand0, SCode rand1)
        {
            return
               // (rand1 is Argument) ? Combination2FA1A.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? new Combination2FA1Q (rator, rand0, (Quotation) rand1) :
                 new Combination2FA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2FA1";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2FA1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0 = environment.Argument1Value;

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
    sealed class Combination2FA1Q : Combination2FA1
    {
        public readonly object rand1Value;
        internal Combination2FA1Q (FreeVariable rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2FA1Q");
#endif
            object ev1 = this.rand1Value;

            object ev0 = environment.Argument1Value;

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
                //(rand1 is Argument) ? Combination2FQA.Make (rator, rand0, (Argument) rand1) :
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
                //(rand1 is Argument) ? Combination2FSA.Make (rator, rand0, (Argument) rand1) :
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
                (rand1 is Argument1) ? new Combination2FXA1 (rator, rand0, (Argument1) rand1) :
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
    sealed class Combination2FXA1 : Combination2FXA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        internal Combination2FXA1 (FreeVariable rator, SCode rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2FXA1";
#endif

            object ev0;
            Environment env = environment;
            Control unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2FXA1";
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument1Value);
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
                (rand0 is Argument1) ? Combination2TA1.Make (rator, (Argument1) rand0, rand1) :
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
                (rand1 is Argument1) ? Combination2TA0A1.Make (rator, rand0, (Argument1) rand1) :
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
    sealed class Combination2TA0A1 : Combination2TA0A
    {
        Combination2TA0A1 (TopLevelVariable rator, Argument0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (TopLevelVariable rator, Argument0 rand0, Argument1 rand1)
        {
            return
                 new Combination2TA0A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination2TA0A1");
#endif
            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, environment.Argument1Value);
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
    class Combination2TA1 : Combination2TA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected Combination2TA1 (TopLevelVariable rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (TopLevelVariable rator, Argument1 rand0, SCode rand1)
        {
            return
               // (rand1 is Argument) ? Combination2TA1A.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? new Combination2TA1Q (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new Combination2TA1S (rator, rand0, (StaticVariable) rand1) :
                 new Combination2TA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2TA1";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2TA1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0 = environment.Argument1Value;

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }
    }

    [Serializable]
    class Combination2TA1Q : Combination2TA1
    {
        public readonly object rand1Value;

        internal Combination2TA1Q (TopLevelVariable rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (TopLevelVariable rator, Argument1 rand0, SCode rand1)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2TA1Q";
#endif

            object ev0 = environment.Argument1Value;

            object evop;
            if (this.ratorCell.GetValue (out evop))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
        }
    }

    [Serializable]
    sealed class Combination2TA1S : Combination2TA1
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal Combination2TA1S (TopLevelVariable rator, Argument1 rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
        this.rand1Name = rand1.Name;
        this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (TopLevelVariable rator, Argument1 rand0, SCode rand1)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Combination2TA1S";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument1Value;

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
               // (rand1 is Argument) ? Combination2TSA.Make (rator, rand0, (Argument) rand1) :
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



//    class Combination2L : Combination2
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object ratorName;
//        public readonly int ratorDepth;
//        public readonly int ratorOffset;

//        protected Combination2L (LexicalVariable rator, SCode rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.ratorName = rator.Name;
//            this.ratorDepth = rator.Depth;
//            this.ratorOffset = rator.Offset;
//        }

//        public static SCode Make (LexicalVariable rator, SCode rand0, SCode rand1)
//        {
//            return
//                (rator is Argument) ? Combination2A.Make ((Argument) rator, rand0, rand1) :
//                (rator is LexicalVariable1) ? Combination2L1.Make ((LexicalVariable1) rator, rand0, rand1) :
//                (rand0 is LexicalVariable) ? Combination2LL.Make (rator, (LexicalVariable) rand0, rand1) :
//                ////(rand0 is PrimitiveCarA0) ? Combination2LCarA0.Make (rator, (PrimitiveCarA0) rand0, rand1) :
//                ////(rand0 is PrimitiveCdrA0) ? Combination2LCdrA0.Make (rator, (PrimitiveCdrA0) rand0, rand1) :
//                (rand0 is Quotation) ? Combination2LQ.Make (rator, (Quotation) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Combination2LSL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2LSQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2L.EvalStep");
//            NoteCalls (this.rand0);
//            NoteCalls (this.rand1);
//            rand0TypeHistogram.Note (this.rand0Type);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            env = environment;
//            unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2A : Combination2L
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2A (Argument rator, SCode rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, SCode rand0, SCode rand1)
//        {
//            return
//                (rator is Argument0) ? Combination2A0.Make ((Argument0) rator, rand0, rand1) :
//                (rator is Argument1) ? Combination2A1.Make ((Argument1) rator, rand0, rand1) :
//                (rand0 is LexicalVariable) ? Combination2AL.Make (rator, (LexicalVariable) rand0, rand1) :
//                (rand0 is Quotation) ? Combination2AQ.Make (rator, (Quotation) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Combination2ASL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2ASQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2A (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A.EvalStep");
//            NoteCalls (this.rand0);
//            NoteCalls (this.rand1);
//            rand0TypeHistogram.Note (this.rand0Type);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            env = environment;
//            unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, 
//                environment.ArgumentValue (this.ratorOffset), ev0, ev1);
//        }
//    }

//    class Combination2A0 : Combination2A
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2A0 (Argument0 rator, SCode rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument0 rator, SCode rand0, SCode rand1)
//        {
//            return
//                (rand0 is LexicalVariable) ? Combination2A0L.Make (rator, (LexicalVariable) rand0, rand1) :
//                (rand0 is Quotation) ? Combination2A0Q.Make (rator, (Quotation) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Combination2A0SL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2A0SQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2A0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0.EvalStep");
//            NoteCalls (this.rand0);
//            NoteCalls (this.rand1);
//            rand0TypeHistogram.Note (this.rand0Type);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            env = environment;
//            unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument0Value, ev0, ev1);
//        }
//    }

//    class Combination2A0L : Combination2A0
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand0Name;
//        public readonly int rand0Depth;
//        public readonly int rand0Offset;

//        protected Combination2A0L (Argument0 rator, LexicalVariable rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand0Name = rand0.Name;
//            this.rand0Depth = rand0.Depth;
//            this.rand0Offset = rand0.Offset;
//        }

//        public static SCode Make (Argument0 rator, LexicalVariable rand0, SCode rand1)
//        {
//            return
//                (rand0 is Argument) ? Combination2A0A.Make (rator, (Argument) rand0, rand1) :
//                (rand0 is LexicalVariable1) ? Combination2A0L1.Make (rator, (LexicalVariable1) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Combination2A0LL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2A0LQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2A0L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0L.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, 
//                environment.Argument0Value, ev0, ev1);
//        }
//    }

//    class Combination2A0A : Combination2A0L
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2A0A (Argument0 rator, Argument rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument0 rator, Argument rand0, SCode rand1)
//        {
//            return
//                (rand0 is Argument0) ? Unimplemented () :
//                (rand0 is Argument1) ? Combination2A0A1.Make (rator, (Argument1) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Unimplemented () :
//                (rand1 is Quotation) ? Unimplemented () :
//                new Combination2A0A (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0A.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, 
//                environment.Argument0Value, environment.ArgumentValue (this.rand0Offset), ev1);
//        }
//    }

//    // Combination2A0A0

//    // Combination2A0A0L

//    // Combination2A0A0A

//    // Combination2A0A0A0

//    // Combination2A0A0A1

//    // Combination2A0A0L1

//    // Combination2A0A0Q

//    class Combination2A0A1 : Combination2A0A
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2A0A1 (Argument0 rator, Argument1 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument0 rator, Argument1 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2A0A1L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Unimplemented () :
//                new Combination2A0A1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0A1.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, 
//                environment.Argument0Value, environment.Argument1Value, ev1);
//        }
//    }

//    class Combination2A0A1L : Combination2A0A1
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2A0A1L (Argument0 rator, Argument1 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Argument0 rator, Argument1 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Unimplemented () :
//                (rand1 is LexicalVariable1) ? Combination2A0A1L1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2A0A1L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0A1L.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, 
//                environment.Argument0Value, environment.Argument1Value, ev1);
//        }
//    }

//    // Combination2A0A1A

//    // Combination2A0A1A0

//    // Combination2A0A1A1

//    sealed class Combination2A0A1L1 : Combination2A0A1L
//    {
//        Combination2A0A1L1 (Argument0 rator, Argument1 rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument0 rator, Argument1 rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2A0A1L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0A1L1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, 
//                environment.Argument0Value, environment.Argument1Value, ev1);
//        }
//    }

//    // Combination2A0A1Q

//    // Combination2A0L1

//    // Combination2A0L1L

//    // Combination2A0L1A

//    // Combination2A0L1A0

//    // Combination2A0L1A1

//    // Combination2A0L1L1

//    // Combination2A0L1Q

//    // Combination2A0A

//    // Combination2A0AL

//    // Combination2A0AA

//    // Combination2A0AA0

//    // Combination2A0AA1

//    // Combination2A0AL1

//    // Combination2A0AQ

//    class Combination2A0L1 : Combination2A0L
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2A0L1 (Argument0 rator, LexicalVariable1 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument0 rator, LexicalVariable1 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2A0L1L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2A0L1Q.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2A0L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0L1.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument0Value, ev0, ev1);
//        }
//    }

//    class Combination2A0L1L : Combination2A0L1
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2A0L1L (Argument0 rator, LexicalVariable1 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Argument0 rator, LexicalVariable1 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Unimplemented () :
//                (rand1 is LexicalVariable1) ? Combination2A0L1L1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2A0L1L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0L1L.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument0Value, ev0, ev1);
//            throw new NotImplementedException ();
//        }
//    }

//    // Combination2A0L1A

//    // Combination2A0L1A0

//    // Combination2A0L1A1

//    sealed class Combination2A0L1L1 : Combination2A0L1L
//    {
//        Combination2A0L1L1 (Argument0 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument0 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2A0L1L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0L1L1.EvalStep");
//#endif
//            if (this.rand1Offset == this.rand0Offset) Debugger.Break ();
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value, ev0, ev1);
//        }
//    }

//    sealed class Combination2A0L1Q : Combination2A0L1
//    {
//        Combination2A0L1Q (Argument0 rator, LexicalVariable1 rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument0 rator, LexicalVariable1 rand0, Quotation rand1)
//        {
//            return
//                new Combination2A0L1Q (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0L1Q.EvalStep");
//#endif
//            throw new NotImplementedException ();
//        }
//    }

//    class Combination2A0LL : Combination2A0L
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2A0LL (Argument0 rator, LexicalVariable rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Argument0 rator, LexicalVariable rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Unimplemented () :
//                (rand1 is LexicalVariable1) ? Combination2A0LL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2A0LL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0LL.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value, ev0, ev1);
//        }
//    }

//    // Combination2A0LA

//    // Combination2A0LA0

//    // Combination2A0LA1

//    sealed class Combination2A0LL1 : Combination2A0LL
//    {
//        Combination2A0LL1 (Argument0 rator, LexicalVariable rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument0 rator, LexicalVariable rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2A0LL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0LL1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, 
//                environment.Argument0Value, ev0, ev1);
//        }
//    }

//    sealed class Combination2A0LQ : Combination2A0L
//    {
//        public readonly object rand1Value;
//        Combination2A0LQ (Argument0 rator, LexicalVariable rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (Argument0 rator, LexicalVariable rand0, Quotation rand1)
//        {
//            return
//                new Combination2A0LQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0LQ.EvalStep");
//#endif
//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument0Value, ev0, this.rand1Value);
//        }
//    }

//    class Combination2A0Q : Combination2A0
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand0Value;

//        protected Combination2A0Q (Argument0 rator, Quotation rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand0Value = rand0.Quoted;
//        }

//        public static SCode Make (Argument0 rator, Quotation rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2A0QL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2A0QQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2A0Q (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//            SCode.location = "Combination2A0Q.EvalStep";
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value, this.rand0Value, ev1);
//        }
//    }

//    class Combination2A0QL : Combination2A0Q
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2A0QL (Argument0 rator, Quotation rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Argument0 rator, Quotation rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Unimplemented () :
//                (rand1 is LexicalVariable1) ? Combination2A0QL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2A0QL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0QL.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = this.rand0Value;

//            object evop = environment.Argument0Value;

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    // Combination2A0QA

//    // Combination2A0QA0

//    // Combination2A0QA1

//    sealed class Combination2A0QL1 : Combination2A0QL
//    {
//        Combination2A0QL1 (Argument0 rator, Quotation rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument0 rator, Quotation rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2A0QL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0QL1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = this.rand0Value;

//            object evop = environment.Argument0Value;

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2A0QQ : Combination2A0Q
//    {
//        public readonly object rand1Value;

//        Combination2A0QQ (Argument0 rator, Quotation rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (Argument0 rator, Quotation rand0, Quotation rand1)
//        {
//            return
//                new Combination2A0QQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0QQ.EvalStep");
//#endif
//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument0Value, this.rand0Value, this.rand1Value);
//        }
//    }

//    class Combination2A0SL : Combination2A0
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2A0SL (Argument0 rator, SCode rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Argument0 rator, SCode rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2A0SA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2A0SL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2A0SL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0SL.EvalStep");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument0Value, ev0, ev1);
//        }
//    }

//    class Combination2A0SA : Combination2A0SL
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2A0SA (Argument0 rator, SCode rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument0 rator, SCode rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Unimplemented () :
//                (rand1 is Argument1) ? Combination2A0SA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2A0SA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("Combination2L.EvalStep");
//#endif
//        }
//    }

//    // Combination2A0SA0

//    sealed class Combination2A0SA1 : Combination2A0SA
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2A0SA1 (Argument0 rator, SCode rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument0 rator, SCode rand0, Argument1 rand1)
//        {
//            return
//                new Combination2A0SA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("Combination2A0SA1.EvalStep");
//#endif
//        }
//    }

//    sealed class Combination2A0SL1 : Combination2A0SL
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2A0SL1 (Argument0 rator, SCode rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument0 rator, SCode rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2A0SL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0SL1.EvalStep");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument0Value, ev0, ev1);
//        }
//    }

//    sealed class Combination2A0SQ : Combination2A0
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Value;

//        Combination2A0SQ (Argument0 rator, SCode rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (Argument0 rator, SCode rand0, Quotation rand1)
//        {
//            return
//               new Combination2A0SQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A0SQ.EvalStep");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//#endif
//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument0Value, ev0, this.rand1Value);
//        }
//    }

//    class Combination2A1 : Combination2A
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2A1 (Argument1 rator, SCode rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument1 rator, SCode rand0, SCode rand1)
//        {
//            return
//                (rand0 is LexicalVariable) ? Combination2A1L.Make (rator, (LexicalVariable) rand0, rand1) :
//                (rand0 is Quotation) ? Combination2A1Q.Make (rator, (Quotation) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Combination2A1SL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2A1SQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2A1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1.EvalStep");
//            NoteCalls (this.rand0);
//            NoteCalls (this.rand1);
//            rand0TypeHistogram.Note (this.rand0Type);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            env = environment;
//            unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument1Value, ev0, ev1);
//        }
//    }

//    class Combination2A1L : Combination2A1
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand0Name;
//        public readonly int rand0Depth;
//        public readonly int rand0Offset;

//        protected Combination2A1L (Argument1 rator, LexicalVariable rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand0Name = rand0.Name;
//            this.rand0Depth = rand0.Depth;
//            this.rand0Offset = rand0.Offset;
//        }

//        public static SCode Make (Argument1 rator, LexicalVariable rand0, SCode rand1)
//        {
//            return
//                (rand0 is Argument) ? Combination2A1A.Make (rator, (Argument) rand0, rand1) :
//                (rand0 is LexicalVariable1) ? Combination2A1L1.Make (rator, (LexicalVariable1) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Combination2A1LL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2A1LQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2A1L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1L.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, 
//                environment.Argument1Value, ev0, ev1);
//        }
//    }

//    class Combination2A1A : Combination2A1L
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2A1A (Argument1 rator, Argument rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument1 rator, Argument rand0, SCode rand1)
//        {
//            return
//                (rand0 is Argument0) ? Combination2A1A0.Make (rator, (Argument0) rand0, rand1) :
//                (rand0 is Argument1) ? Combination2A1A1.Make (rator, (Argument1) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Unimplemented () :
//                (rand1 is Quotation) ? Unimplemented () :
//                new Combination2A1A (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1A.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, 
//                environment.Argument1Value, environment.ArgumentValue (this.rand0Offset), ev1);
//        }
//    }

//    class Combination2A1A0 : Combination2A1A
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2A1A0 (Argument1 rator, Argument0 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument1 rator, Argument0 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2A1A0L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Unimplemented () :
//                new Combination2A1A0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1A0.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument1Value, environment.Argument0Value, ev1);
//        }
//    }

//    class Combination2A1A0L : Combination2A1A0
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2A1A0L (Argument1 rator, Argument0 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Argument1 rator, Argument0 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Unimplemented () :
//                (rand1 is LexicalVariable1) ? Combination2A1A0L1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2A1A0L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1A0L.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument1Value, environment.Argument0Value, ev1);
//        }
//    }

//    // Combination2A1A0A

//    // Combination2A1A0A0

//    // Combination2A1A0A1

//    sealed class Combination2A1A0L1 : Combination2A1A0L
//    {
//        Combination2A1A0L1 (Argument1 rator, Argument0 rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument1 rator, Argument0 rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2A1A0L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1A0L1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument1Value, environment.Argument0Value, ev1);
//        }
//    }

//    // Combination2A1A0Q

//    class Combination2A1A1 : Combination2A1A
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2A1A1 (Argument1 rator, Argument1 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument1 rator, Argument1 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2A1A1L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Unimplemented () :
//                new Combination2A1A1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1A1.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, 
//                environment.Argument1Value, environment.Argument1Value, ev1);
//        }
//    }

//    class Combination2A1A1L : Combination2A1A1
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2A1A1L (Argument1 rator, Argument1 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Argument1 rator, Argument1 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Unimplemented () :
//                (rand1 is LexicalVariable1) ? Combination2A1A1L1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2A1A1L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1A1L.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, 
//                environment.Argument1Value, environment.Argument1Value, ev1);
//        }
//    }

//    // Combination2A1A1A

//    // Combination2A1A1A0

//    // Combination2A1A1A1

//    sealed class Combination2A1A1L1 : Combination2A1A1L
//    {
//        Combination2A1A1L1 (Argument1 rator, Argument1 rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument1 rator, Argument1 rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2A1A1L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1A1L1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, 
//                environment.Argument1Value, environment.Argument1Value, ev1);
//        }
//    }

//    // Combination2A1A1Q

//    // Combination2A1L1

//    // Combination2A1L1L

//    // Combination2A1L1A

//    // Combination2A1L1A0

//    // Combination2A1L1A1

//    // Combination2A1L1L1

//    // Combination2A1L1Q

//    // Combination2A1A

//    // Combination2A1AL

//    // Combination2A1AA

//    // Combination2A1AA0

//    // Combination2A1AA1

//    // Combination2A1AL1

//    // Combination2A1AQ

//    class Combination2A1L1 : Combination2A1L
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2A1L1 (Argument1 rator, LexicalVariable1 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument1 rator, LexicalVariable1 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2A1L1L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2A1L1Q.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2A1L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1L1.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument1Value, ev0, ev1);
//        }
//    }

//    class Combination2A1L1L : Combination2A1L1
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2A1L1L (Argument1 rator, LexicalVariable1 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Argument1 rator, LexicalVariable1 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Unimplemented () :
//                (rand1 is LexicalVariable1) ? Combination2A1L1L1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2A1L1L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1L1L.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument1Value, ev0, ev1);
//            throw new NotImplementedException ();
//        }
//    }

//    // Combination2A1L1A

//    // Combination2A1L1A0

//    // Combination2A1L1A1

//    sealed class Combination2A1L1L1 : Combination2A1L1L
//    {
//        Combination2A1L1L1 (Argument1 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument1 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2A1L1L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1L1L1.EvalStep");
//#endif
//            if (this.rand1Offset == this.rand0Offset) Debugger.Break ();
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument1Value, ev0, ev1);
//        }
//    }

//    sealed class Combination2A1L1Q : Combination2A1L1
//    {
//        Combination2A1L1Q (Argument1 rator, LexicalVariable1 rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument1 rator, LexicalVariable1 rand0, Quotation rand1)
//        {
//            return
//                new Combination2A1L1Q (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1L1Q.EvalStep");
//#endif
//            throw new NotImplementedException ();
//        }
//    }

//    class Combination2A1LL : Combination2A1L
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2A1LL (Argument1 rator, LexicalVariable rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Argument1 rator, LexicalVariable rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Unimplemented () :
//                (rand1 is LexicalVariable1) ? Combination2A1LL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2A1LL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1LL.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument1Value, ev0, ev1);
//        }
//    }

//    // Combination2A1LA

//    // Combination2A1LA0

//    // Combination2A1LA1

//    sealed class Combination2A1LL1 : Combination2A1LL
//    {
//        Combination2A1LL1 (Argument1 rator, LexicalVariable rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument1 rator, LexicalVariable rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2A1LL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1LL1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, 
//                environment.Argument1Value, ev0, ev1);
//        }
//    }

//    sealed class Combination2A1LQ : Combination2A1L
//    {
//        public readonly object rand1Value;
//        Combination2A1LQ (Argument1 rator, LexicalVariable rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (Argument1 rator, LexicalVariable rand0, Quotation rand1)
//        {
//            return
//                new Combination2A1LQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1LQ.EvalStep");
//#endif
//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument1Value, ev0, this.rand1Value);
//        }
//    }

//    class Combination2A1Q : Combination2A1
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand0Value;

//        protected Combination2A1Q (Argument1 rator, Quotation rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand0Value = rand0.Quoted;
//        }

//        public static SCode Make (Argument1 rator, Quotation rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2A1QL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2A1QQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2A1Q (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//            SCode.location = "Combination2A1Q.EvalStep";
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument1Value, this.rand0Value, ev1);
//        }
//    }

//    class Combination2A1QL : Combination2A1Q
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2A1QL (Argument1 rator, Quotation rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Argument1 rator, Quotation rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Unimplemented () :
//                (rand1 is LexicalVariable1) ? Combination2A1QL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2A1QL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1QL.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = this.rand0Value;

//            object evop = environment.Argument1Value;

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    // Combination2A1QA

//    // Combination2A1QA0

//    // Combination2A1QA1

//    sealed class Combination2A1QL1 : Combination2A1QL
//    {
//        Combination2A1QL1 (Argument1 rator, Quotation rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument1 rator, Quotation rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2A1QL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1QL1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = this.rand0Value;

//            object evop = environment.Argument1Value;

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2A1QQ : Combination2A1Q
//    {
//        public readonly object rand1Value;

//        Combination2A1QQ (Argument1 rator, Quotation rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (Argument1 rator, Quotation rand0, Quotation rand1)
//        {
//            return
//                new Combination2A1QQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1QQ.EvalStep");
//#endif
//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument1Value, this.rand0Value, this.rand1Value);
//        }
//    }

//    class Combination2A1SL : Combination2A1
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2A1SL (Argument1 rator, SCode rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Argument1 rator, SCode rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2A1SA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2A1SL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2A1SL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1SL.EvalStep");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument1Value, ev0, ev1);
//        }
//    }

//    class Combination2A1SA : Combination2A1SL
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2A1SA (Argument1 rator, SCode rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument1 rator, SCode rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Unimplemented () :
//                (rand1 is Argument1) ? Combination2A1SA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2A1SA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("Combination2L.EvalStep");
//#endif
//        }
//    }

//    // Combination2A1SA0

//    sealed class Combination2A1SA1 : Combination2A1SA
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2A1SA1 (Argument1 rator, SCode rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument1 rator, SCode rand0, Argument1 rand1)
//        {
//            return
//                new Combination2A1SA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("Combination2A1SA1.EvalStep");
//#endif
//        }
//    }

//    sealed class Combination2A1SL1 : Combination2A1SL
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2A1SL1 (Argument1 rator, SCode rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument1 rator, SCode rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2A1SL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1SL1.EvalStep");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument1Value, ev0, ev1);
//        }
//    }

//    sealed class Combination2A1SQ : Combination2A1
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Value;

//        Combination2A1SQ (Argument1 rator, SCode rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (Argument1 rator, SCode rand0, Quotation rand1)
//        {
//            return
//               new Combination2A1SQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2A1SQ.EvalStep");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//#endif
//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.Argument1Value, ev0, this.rand1Value);
//        }
//    }


//    class Combination2AL : Combination2A
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand0Name;
//        public readonly int rand0Depth;
//        public readonly int rand0Offset;

//        protected Combination2AL (Argument rator, LexicalVariable rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand0Name = rand0.Name;
//            this.rand0Depth = rand0.Depth;
//            this.rand0Offset = rand0.Offset;
//        }

//        public static SCode Make (Argument rator, LexicalVariable rand0, SCode rand1)
//        {
//            return
//                (rand0 is Argument) ? Combination2AA.Make (rator, (Argument) rand0, rand1) :
//                (rand0 is LexicalVariable1) ? Combination2AL1.Make (rator, (LexicalVariable1) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Combination2ALL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2ALQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2AL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2AL.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset), ev0, ev1);
//        }
//    }

//    class Combination2AA : Combination2AL
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2AA (Argument rator, Argument rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, Argument rand0, SCode rand1)
//        {
//            return
//                //(rand0 is Argument0) ? Combination2AA0.Make (rator, (Argument0) rand0, rand1) :
//                //(rand0 is Argument1) ? Combination2AA1.Make (rator, (Argument1) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Combination2AAL.Make (rator, rand0, (LexicalVariable) rand1):
//                (rand1 is Quotation) ? Combination2AAQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2AA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2AA.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }
//            return Interpreter.Call (out answer, ref expression, ref environment, 
//                environment.ArgumentValue(this.ratorOffset),
//                environment.ArgumentValue (this.rand0Offset),
//                ev1);
//        }
//    }

//    class Combination2AAL : Combination2AA
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2AAL (Argument rator, Argument rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Argument rator, Argument rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2AAA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2AAL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2AAL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2AAL.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset),
//                environment.ArgumentValue (this.rand0Offset),
//                ev1);
//        }
//    }

//    class Combination2AAA : Combination2AAL
//    {
//        protected Combination2AAA (Argument rator, Argument rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, Argument rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Unimplemented() :
//                (rand1 is Argument1) ? Combination2AAA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2AAA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2AAA.EvalStep");
//#endif

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset),
//                environment.ArgumentValue (this.rand0Offset),
//                environment.ArgumentValue (this.rand1Offset));
//        }
//    }

//    // Combination2AAA0

//    sealed class Combination2AAA1 : Combination2AAA
//    {
//        Combination2AAA1 (Argument rator, Argument rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, Argument rand0, Argument1 rand1)
//        {
//            return
//                new Combination2AAA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2AAA1.EvalStep");
//#endif
//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset),
//                environment.ArgumentValue (this.rand0Offset),
//                environment.Argument1Value);
//        }
//    }

//    sealed class Combination2AAL1 : Combination2AAL
//    {
//        Combination2AAL1 (Argument rator, Argument rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, Argument rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2AAL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2AAL1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset),
//                environment.ArgumentValue (this.rand0Offset),
//                ev1);
//        }
//    }

//    sealed class Combination2AAQ : Combination2AA
//    {
//        public readonly object rand1Value;
//        Combination2AAQ (Argument rator, Argument rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (Argument rator, Argument rand0, Quotation rand1)
//        {
//            return
//                new Combination2AAQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2AAQ.EvalStep");
//#endif
//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset),
//                environment.ArgumentValue (this.rand0Offset),
//                this.rand1Value);
//        }
//    }

//    class Combination2AL1 : Combination2AL
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2AL1 (Argument rator, LexicalVariable1 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, LexicalVariable1 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2AL1L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2AL1Q.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2AL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2AL1.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset), ev0, ev1);
//        }
//    }

//    class Combination2AL1L : Combination2AL1
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2AL1L (Argument rator, LexicalVariable1 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Argument rator, LexicalVariable1 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Unimplemented () :
//                (rand1 is LexicalVariable1) ? Combination2AL1L1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2AL1L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2AL1L.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset), ev0, ev1);
//        }
//    }

//    sealed class Combination2AL1L1 : Combination2AL1L
//    {
//        Combination2AL1L1 (Argument rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
//        {
//            return
//               new Combination2AL1L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2AL1L1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset), ev0, ev1);
//        }
//    }

//    sealed class Combination2AL1Q : Combination2AL1
//    {
//        public readonly object rand1Value;

//        Combination2AL1Q (Argument rator, LexicalVariable1 rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (Argument rator, LexicalVariable1 rand0, Quotation rand1)
//        {
//            return
//               new Combination2AL1Q (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2AL1Q.EvalStep");
//#endif

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset), ev0, this.rand1Value);
//        }
//    }

//    class Combination2ALL : Combination2AL
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2ALL (Argument rator, LexicalVariable rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Argument rator, LexicalVariable rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2ALA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2ALL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2ALL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2ALL.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset), ev0, ev1);
//        }
//    }

//    class Combination2ALA : Combination2ALL
//    {
//        protected Combination2ALA (Argument rator, LexicalVariable rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, LexicalVariable rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2ALA0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2ALA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2ALA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2ALA.EvalStep");
//#endif
//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset), ev0, environment.ArgumentValue (this.rand1Offset));
//        }
//    }

//    sealed class Combination2ALA0 : Combination2ALA
//    {
//        Combination2ALA0 (Argument rator, LexicalVariable rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, LexicalVariable rand0, Argument0 rand1)
//        {
//            return
//                new Combination2ALA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2ALA0.EvalStep");
//#endif
//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset), ev0, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2ALA1 : Combination2ALA
//    {
//        Combination2ALA1 (Argument rator, LexicalVariable rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, LexicalVariable rand0, Argument1 rand1)
//        {
//            return
//                new Combination2ALA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2ALA1.EvalStep");
//#endif
//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset), ev0, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2ALL1 : Combination2ALL
//    {
//        Combination2ALL1 (Argument rator, LexicalVariable rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, LexicalVariable rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2ALL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2ALL1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset), ev0, ev1);
//        }
//    }

//    sealed class Combination2ALQ : Combination2AL
//    {
//        public readonly object rand1Value;
//        Combination2ALQ (Argument rator, LexicalVariable rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (Argument rator, LexicalVariable rand0, Quotation rand1)
//        {
//            return
//                new Combination2ALQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2ALQ.EvalStep");
//#endif
//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset), ev0, this.rand1Value);
//        }
//    }

//    class Combination2AQ : Combination2A
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand0Value;

//        protected Combination2AQ (Argument rator, Quotation rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand0Value = rand0.Quoted;
//        }

//        public static SCode Make (Argument rator, Quotation rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2AQL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2AQQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2AQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//            SCode.location = "Combination2AQ.EvalStep";
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset),
//                this.rand0Value,
//                ev1);
//        }
//    }

//    class Combination2AQL : Combination2AQ
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2AQL (Argument rator, Quotation rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Argument rator, Quotation rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2AQA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2AQL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2AQL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2AQL.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset), this.rand0Value, ev1);
//        }
//    }

//    class Combination2AQA : Combination2AQL
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2AQA (Argument rator, Quotation rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, Quotation rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2AQA0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2AQA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2AQA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2AQA.EvalStep");
//#endif
//            object ev1 = environment.ArgumentValue (this.rand1Offset);

//            object ev0 = this.rand0Value;

//            object evop = environment.ArgumentValue (this.ratorOffset);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2AQA0 : Combination2AQA
//    {
//        Combination2AQA0 (Argument rator, Quotation rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, Quotation rand0, Argument0 rand1)
//        {
//            return
//                new Combination2AQA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2AQA0.EvalStep");
//#endif
//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset), this.rand0Value, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2AQA1 : Combination2AQA
//    {
//        Combination2AQA1 (Argument rator, Quotation rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, Quotation rand0, Argument1 rand1)
//        {
//            return
//                new Combination2AQA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2AQA1.EvalStep");
//#endif
//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset), this.rand0Value, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2AQL1 : Combination2AQL
//    {
//        Combination2AQL1 (Argument rator, Quotation rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, Quotation rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2AQL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2AQL1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset), this.rand0Value, ev1);
//        }
//    }

//    sealed class Combination2AQQ : Combination2AQ
//    {
//        public readonly object rand1Value;

//        Combination2AQQ (Argument rator, Quotation rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (Argument rator, Quotation rand0, Quotation rand1)
//        {
//            return
//                new Combination2AQQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2AQQ.EvalStep");
//#endif
//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset), this.rand0Value, this.rand1Value);
//        }
//    }

//    class Combination2ASL : Combination2A
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2ASL (Argument rator, SCode rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Argument rator, SCode rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2ASA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2ASL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2ASL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2ASL.EvalStep");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//#endif

//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (ratorOffset), ev0, ev1);
//        }
//    }

//    class Combination2ASA : Combination2ASL
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination2ASA (Argument rator, SCode rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, SCode rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2ASA0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2ASA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2ASA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ();
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//#endif

//            object ev1 = environment.ArgumentValue (this.rand1Offset);

//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object evop = environment.ArgumentValue (this.ratorOffset);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2ASA0 : Combination2ASA
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2ASA0 (Argument rator, SCode rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, SCode rand0, Argument0 rand1)
//        {
//            return
//                new Combination2ASA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//            SCode.location = "Combination2ASA0.EvalStep";
//#endif
//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object evop = environment.ArgumentValue (this.ratorOffset);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2ASA1 : Combination2ASA
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2ASA1 (Argument rator, SCode rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, SCode rand0, Argument1 rand1)
//        {
//            return
//                new Combination2ASA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//            SCode.location = "Combination2ASA1.EvalStep";
//#endif
//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object evop = environment.ArgumentValue (this.ratorOffset);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2ASL1 : Combination2ASL
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2ASL1 (Argument rator, SCode rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Argument rator, SCode rand0, LexicalVariable1 rand1)
//        {
//            return
//                 new Combination2ASL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//            SCode.location = "Combination2ASL1.EvalStep";
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2ASL1.EvalStep.1";
//#endif
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object evop = environment.ArgumentValue (this.ratorOffset);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2ASQ : Combination2A
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Value;
//        Combination2ASQ (Argument rator, SCode rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (Argument rator, SCode rand0, Quotation rand1)
//        {
//            return
//                new Combination2ASQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2ASQ.EvalStep");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//#endif
//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                environment.ArgumentValue (this.ratorOffset), ev0, this.rand1Value);
//        }
//    }
//    class Combination2L1 : Combination2L
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination2L1 (LexicalVariable1 rator, SCode rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, SCode rand0, SCode rand1)
//        {
//            return
//                (rand0 is LexicalVariable) ? Combination2L1L.Make (rator, (LexicalVariable) rand0, rand1) :
//                //(rand0 is PrimitiveCarA0) ? Combination2L1CarA0.Make (rator, (PrimitiveCarA0) rand0, rand1) :
//                //(rand0 is PrimitiveCdrA0) ? Combination2L1CdrA0.Make (rator, (PrimitiveCdrA0) rand0, rand1) :
//                (rand0 is Quotation) ? Combination2L1Q.Make (rator, (Quotation) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Combination2L1SL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2L1SQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand0);
//            NoteCalls (this.rand1);
//            rand0TypeHistogram.Note (this.rand0Type);
//            rand1TypeHistogram.Note (this.rand1Type);
//            SCode.location = "Combination2L1.EvalStep";
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1.EvalStep.1";
//#endif
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            env = environment;
//            unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1.EvalStep.2";
//#endif
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2L1L : Combination2L1
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected readonly object rand0Name;
//        protected readonly int rand0Depth;
//        protected readonly int rand0Offset;

//        protected Combination2L1L (LexicalVariable1 rator, LexicalVariable rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand0Name = rand0.Name;
//            this.rand0Depth = rand0.Depth;
//            this.rand0Offset = rand0.Offset;
//        }

//        public static SCode Make (LexicalVariable1 rator, LexicalVariable rand0, SCode rand1)
//        {
//            return
//                (rand0 is Argument) ? Combination2L1A.Make (rator, (Argument) rand0, rand1) :
//                (rand0 is LexicalVariable1) ? Combination2L1L1.Make (rator, (LexicalVariable1) rand0, rand1) :
//                new Combination2L1L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand1);
//            rand0TypeHistogram.Note (this.rand0Type);
//            rand1TypeHistogram.Note (this.rand1Type);
//            SCode.location = "Combination2L1L.EvalStep";
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1L.EvalStep.1";
//#endif
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2L1A : Combination2L1L
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination2L1A (LexicalVariable1 rator, Argument rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument rand0, SCode rand1)
//        {
//            return
//                (rand0 is Argument0) ? Combination2L1A0.Make (rator, (Argument0) rand0, rand1) :
//                (rand0 is Argument1) ? Combination2L1A1.Make (rator, (Argument1) rand0, rand1) :
//                new Combination2L1A (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//            SCode.location = "Combination2L1A.EvalStep";
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1A.EvalStep.1";
//#endif
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2L1A0 : Combination2L1A
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination2L1A0 (LexicalVariable1 rator, Argument0 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument0 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2L1A0L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2L1A0Q.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2L1A0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//            SCode.location = "Combination2L1A0.EvalStep";
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1A0.EvalStep.1";
//#endif
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0 = environment.Argument0Value;

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2L1A0L : Combination2L1A0
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2L1A0L (LexicalVariable1 rator, Argument0 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument0 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2L1A0A.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2L1A0L1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2L1A0L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2L1A0L.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, ev1);
//        }
//    }

//    class Combination2L1A0A : Combination2L1A0L
//    {
//        protected Combination2L1A0A (LexicalVariable1 rator, Argument0 rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument0 rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Unimplemented () :
//                (rand1 is Argument1) ? Combination2L1A0A1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2L1A0A (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand1);
//            SCode.location = "Combination2L1A0.EvalStep";
//#endif

//            object ev1 = null;


//            object ev0 = environment.Argument0Value;

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2L1A0A1 : Combination2L1A0A
//    {
//        Combination2L1A0A1 (LexicalVariable1 rator, Argument0 rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument0 rand0, Argument1 rand1)
//        {
//            return
//                new Combination2L1A0A1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2L1A0A1.EvalStep");
//#endif
//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop,
//                environment.Argument0Value, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2L1A0L1 : Combination2L1A0L
//    {
//        Combination2L1A0L1 (LexicalVariable1 rator, Argument0 rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument0 rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2L1A0L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand1);
//            SCode.location = "Combination2L1A0.EvalStep";
//#endif

//            object ev1 = null;


//            object ev0 = environment.Argument0Value;

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2L1A0Q : Combination2L1A0
//    {
//        public readonly object rand1Value;
//        Combination2L1A0Q (LexicalVariable1 rator, Argument0 rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument0 rand0, Quotation rand1)
//        {
//            return
//                new Combination2L1A0Q (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2L1A0Q.EvalStep");
//#endif
//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop,
//                environment.Argument0Value, this.rand1Value);
//        }
//    }

//    class Combination2L1A1 : Combination2L1A
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination2L1A1 (LexicalVariable1 rator, Argument1 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument1 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2L1A1L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Unimplemented () :
//                new Combination2L1A1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//            SCode.location = "Combination2L1A0.EvalStep";
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1A1.EvalStep.1";
//#endif
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0 = environment.Argument1Value;

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2L1A1L : Combination2L1A1
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination2L1A1L (LexicalVariable1 rator, Argument1 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument1 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2L1A1A.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Unimplemented () :
//                new Combination2L1A1L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//            SCode.location = "Combination2L1A0.EvalStep";
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1A1.EvalStep.1";
//#endif
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0 = environment.Argument1Value;

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2L1A1A : Combination2L1A1L
//    {
//        protected Combination2L1A1A (LexicalVariable1 rator, Argument1 rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument1 rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2L1A1A0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Unimplemented () :
//                new Combination2L1A1A (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand1);
//            SCode.location = "Combination2L1A0.EvalStep";
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1A1.EvalStep.1";
//#endif
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0 = environment.Argument1Value;

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2L1A1A0 : Combination2L1A1A
//    {
//        Combination2L1A1A0 (LexicalVariable1 rator, Argument1 rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument1 rand0, Argument0 rand1)
//        {
//            return
//                new Combination2L1A1A0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand1);
//            SCode.location = "Combination2L1A0.EvalStep";
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1A1.EvalStep.1";
//#endif
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0 = environment.Argument1Value;

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2L1L1 : Combination2L1L
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination2L1L1 (LexicalVariable1 rator, LexicalVariable1 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, LexicalVariable1 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2L1L1L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Unimplemented () :
//                new Combination2L1L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//            SCode.location = "Combination2L1L.EvalStep";
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1L.EvalStep.1";
//#endif
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2L1L1L : Combination2L1L1
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2L1L1L (LexicalVariable1 rator, LexicalVariable1 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (LexicalVariable1 rator, LexicalVariable1 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2L1L1A.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2L1L1L1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2L1L1L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("-");
//            SCode.location = "Combination2L1L.EvalStep";
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1L.EvalStep.1";
//#endif
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2L1L1A : Combination2L1L1L
//    {

//        protected Combination2L1L1A (LexicalVariable1 rator, LexicalVariable1 rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, LexicalVariable1 rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2L1L1A0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Unimplemented () :
//                new Combination2L1L1A (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("-");
//            SCode.location = "Combination2L1L.EvalStep";
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1L.EvalStep.1";
//#endif
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2L1L1A0 : Combination2L1L1A
//    {
//        Combination2L1L1A0 (LexicalVariable1 rator, LexicalVariable1 rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, LexicalVariable1 rand0, Argument0 rand1)
//        {
//            return
//                new Combination2L1L1A0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2L1L1A0.EvalStep");
//#endif
//            object ev1 = environment.Argument0Value;

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2L1L1L1 : Combination2L1L1L
//    {
//        Combination2L1L1L1 (LexicalVariable1 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2L1L1L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("-");
//            SCode.location = "Combination2L1L.EvalStep";
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1L.EvalStep.1";
//#endif
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2L1Q : Combination2L1
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand0Value;

//        protected Combination2L1Q (LexicalVariable1 rator, Quotation rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand0Value = rand0.Quoted;
//        }

//        public static SCode Make (LexicalVariable1 rator, Quotation rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2L1QL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2L1QQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2L1Q (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//            SCode.location = "Combination2L1Q.EvalStep";
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1Q.EvalStep.1";
//#endif
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
//        }
//    }

//    class Combination2L1QL : Combination2L1Q
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2L1QL (LexicalVariable1 rator, Quotation rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (LexicalVariable1 rator, Quotation rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2L1QA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2L1QL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2L1QL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2L1QL.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
//        }
//    }

//    class Combination2L1QA : Combination2L1QL
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination2L1QA (LexicalVariable1 rator, Quotation rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Quotation rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2L1QA0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2L1QA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2L1QA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2L1QA.EvalStep");
//#endif
//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, environment.ArgumentValue (this.rand1Offset));
//        }
//    }

//    sealed class Combination2L1QA0 : Combination2L1QA
//    {
//        Combination2L1QA0 (LexicalVariable1 rator, Quotation rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Quotation rand0, Argument0 rand1)
//        {
//            return
//                new Combination2L1QA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2L1QA0.EvalStep");
//#endif
//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2L1QA1 : Combination2L1QA
//    {
//        Combination2L1QA1 (LexicalVariable1 rator, Quotation rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Quotation rand0, Argument1 rand1)
//        {
//            return
//                new Combination2L1QA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2L1QA1.EvalStep");
//#endif
//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2L1QL1 : Combination2L1QL
//    {
//        Combination2L1QL1 (LexicalVariable1 rator, Quotation rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Quotation rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2L1QL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2L1QL1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
//        }
//    }

//    sealed class Combination2L1QQ : Combination2L1Q
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif

//        Combination2L1QQ (LexicalVariable1 rator, Quotation rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Quotation rand0, Quotation rand1)
//        {
//            return
//                new Combination2L1QQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand0);
//            NoteCalls (this.rand1);
//            rand0TypeHistogram.Note (this.rand0Type);
//            rand1TypeHistogram.Note (this.rand1Type);
//            SCode.location = "Combination2L1.EvalStep";
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1.EvalStep.1";
//#endif
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            env = environment;
//            unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1.EvalStep.2";
//#endif
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2L1SL : Combination2L1
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2L1SL (LexicalVariable1 rator, SCode rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (LexicalVariable1 rator, SCode rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2L1SA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2L1SL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2L1SL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//            SCode.location = "Combination2L1SL.EvalStep";
//#endif

//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1.EvalStep.2";
//#endif
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2L1SA : Combination2L1SL
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination2L1SA (LexicalVariable1 rator, SCode rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, SCode rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2L1SA0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2L1SA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2L1SA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//            SCode.location = "Combination2L1SA.EvalStep";
//#endif

//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1SA.EvalStep.2";
//#endif
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.ArgumentValue (this.rand1Offset));
//        }
//    }

//    sealed class Combination2L1SA0 : Combination2L1SA
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif

//        Combination2L1SA0 (LexicalVariable1 rator, SCode rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, SCode rand0, Argument0 rand1)
//        {
//            return
//                new Combination2L1SA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//            SCode.location = "Combination2L1SA0.EvalStep";
//#endif
//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1.EvalStep.2";
//#endif
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2L1SA1 : Combination2L1SA
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif

//        Combination2L1SA1 (LexicalVariable1 rator, SCode rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, SCode rand0, Argument1 rand1)
//        {
//            return
//                new Combination2L1SA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//            SCode.location = "Combination2L1SA1.EvalStep";
//#endif
//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1SA1.EvalStep.2";
//#endif
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2L1SL1 : Combination2L1SL
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif

//        Combination2L1SL1 (LexicalVariable1 rator, SCode rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, SCode rand0, LexicalVariable1 rand1)
//        {
//            return new Combination2L1SL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2L1SL1.EvalStep");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object evop; if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset)) throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2L1SQ : Combination2L1
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Value;

//        Combination2L1SQ (LexicalVariable1 rator, SCode rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (LexicalVariable1 rator, SCode rand0, Quotation rand1)
//        {
//            return
//                new Combination2L1SQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//            SCode.location = "Combination2L1SQ.EvalStep";
//#endif
//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2L1.EvalStep.2";
//#endif
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
//        }
//    }

//    class Combination2LL : Combination2L
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand0Name;
//        public readonly int rand0Depth;
//        public readonly int rand0Offset;

//        protected Combination2LL (LexicalVariable rator, LexicalVariable rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand0Name = rand0.Name;
//            this.rand0Depth = rand0.Depth;
//            this.rand0Offset = rand0.Offset;
//        }

//        public static SCode Make (LexicalVariable rator, LexicalVariable rand0, SCode rand1)
//        {
//            return
//                (rand0 is Argument) ? Combination2LA.Make (rator, (Argument) rand0, rand1) :
//                (rand0 is LexicalVariable1) ? Combination2LL1.Make (rator, (LexicalVariable1) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Combination2LLL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2LLQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2LL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            rand1TypeHistogram.Note (this.rand1Type);
//            NoteCalls (this.rand1);
//            SCode.location = "Combination2LL.EvalStep";
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2LA : Combination2LL
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2LA (LexicalVariable rator, Argument rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument rand0, SCode rand1)
//        {
//            return
//                (rand0 is Argument0) ? Combination2LA0.Make (rator, (Argument0) rand0, rand1) :
//                (rand0 is Argument1) ? Combination2LA1.Make (rator, (Argument1) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Combination2LAL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2LAQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2LA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            rand1TypeHistogram.Note (this.rand1Type);
//            Warm ("Combination2LL.EvalStep");
//            NoteCalls (this.rand1);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2LA0 : Combination2LA
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2LA0 (LexicalVariable rator, Argument0 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument0 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2LA0L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2LA0Q.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2LA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            rand1TypeHistogram.Note (this.rand1Type);
//            Warm ("Combination2LA0.EvalStep");
//            NoteCalls (this.rand1);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object ev0 = environment.Argument0Value;

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2LA0L : Combination2LA0
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;
//        protected Combination2LA0L (LexicalVariable rator, Argument0 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (LexicalVariable rator, Argument0 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2LA0A.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2LA0L1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2LA0L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LA0L.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = environment.Argument0Value;

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2LA0A : Combination2LA0L
//    {
//        protected Combination2LA0A (LexicalVariable rator, Argument0 rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument0 rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2LA0A0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2LA0A1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2LA0A (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LA0A.EvalStep");
//#endif
//            object ev1 = environment.ArgumentValue (this.rand1Offset);

//            object ev0 = environment.Argument0Value;

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2LA0A0 : Combination2LA0A
//    {
//        Combination2LA0A0 (LexicalVariable rator, Argument0 rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument0 rand0, Argument0 rand1)
//        {
//            return
//                new Combination2LA0A0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LA0A0.EvalStep");
//#endif
//            object ev0 = environment.Argument0Value;

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2LA0A1 : Combination2LA0A
//    {
//        Combination2LA0A1 (LexicalVariable rator, Argument0 rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument0 rand0, Argument1 rand1)
//        {
//            return
//                new Combination2LA0A1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LA0A1.EvalStep");
//#endif
//            object ev0 = environment.Argument0Value;

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2LA0L1 : Combination2LA0L
//    {
//        Combination2LA0L1 (LexicalVariable rator, Argument0 rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument0 rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2LA0L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LA0L1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = environment.Argument0Value;

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2LA0Q : Combination2LA0
//    {
//        public readonly object rand1Value;
//        Combination2LA0Q (LexicalVariable rator, Argument0 rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (LexicalVariable rator, Argument0 rand0, Quotation rand1)
//        {
//            return
//                new Combination2LA0Q (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LA0Q.EvalStep");
//#endif
//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment,
//                evop, environment.Argument0Value, this.rand1Value);
//        }
//    }


//    class Combination2LA1 : Combination2LA
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2LA1 (LexicalVariable rator, Argument1 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument1 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2LA1L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2LA1Q.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2LA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            rand1TypeHistogram.Note (this.rand1Type);
//            Warm ("Combination2LA1.EvalStep");
//            NoteCalls (this.rand1);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object ev0 = environment.Argument1Value;

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2LA1L : Combination2LA1
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;
//        protected Combination2LA1L (LexicalVariable rator, Argument1 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (LexicalVariable rator, Argument1 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2LA1A.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2LA1L1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2LA1L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LA1L.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = environment.Argument1Value;

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2LA1A : Combination2LA1L
//    {
//        protected Combination2LA1A (LexicalVariable rator, Argument1 rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument1 rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2LA1A0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2LA1A1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2LA1A (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LA1A.EvalStep");
//#endif
//            object ev1 = environment.ArgumentValue (this.rand1Offset);

//            object ev0 = environment.Argument1Value;

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2LA1A0 : Combination2LA1A
//    {
//        Combination2LA1A0 (LexicalVariable rator, Argument1 rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument1 rand0, Argument0 rand1)
//        {
//            return
//                new Combination2LA1A0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LA1A0.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2LA1A1 : Combination2LA1A
//    {
//        Combination2LA1A1 (LexicalVariable rator, Argument1 rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument1 rand0, Argument1 rand1)
//        {
//            return
//                new Combination2LA1A1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LA1A1.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2LA1L1 : Combination2LA1L
//    {
//        Combination2LA1L1 (LexicalVariable rator, Argument1 rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument1 rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2LA1L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LA1L1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = environment.Argument1Value;

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2LA1Q : Combination2LA1
//    {
//        public readonly object rand1Value;
//        Combination2LA1Q (LexicalVariable rator, Argument1 rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (LexicalVariable rator, Argument1 rand0, Quotation rand1)
//        {
//            return
//                new Combination2LA1Q (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LA1Q.EvalStep");
//#endif
//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment,
//                evop, environment.Argument1Value, this.rand1Value);
//        }
//    }

//    class Combination2LAL : Combination2LA
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;
//        protected Combination2LAL (LexicalVariable rator, Argument rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            rand1Name = rand1.Name;
//            rand1Depth = rand1.Depth;
//            rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (LexicalVariable rator, Argument rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2LAA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2LAL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2LAL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LAL.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2LAA : Combination2LAL
//    {
//        protected Combination2LAA (LexicalVariable rator, Argument rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2LAA0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2LAA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2LAA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LAA.EvalStep");
//#endif
//            object ev1 = environment.ArgumentValue (this.rand1Offset);

//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2LAA0 : Combination2LAA
//    {
//        Combination2LAA0 (LexicalVariable rator, Argument rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument rand0, Argument0 rand1)
//        {
//            return
//                new Combination2LAA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LAA0.EvalStep");
//#endif
//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2LAA1 : Combination2LAA
//    {
//        Combination2LAA1 (LexicalVariable rator, Argument rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument rand0, Argument1 rand1)
//        {
//            return
//                new Combination2LAA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LAA1.EvalStep");
//#endif
//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2LAL1 : Combination2LAL
//    {
//        Combination2LAL1 (LexicalVariable rator, Argument rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Argument rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2LAL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LAL1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2LAQ : Combination2LA
//    {
//        public readonly object rand1Value;
//        Combination2LAQ (LexicalVariable rator, Argument rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (LexicalVariable rator, Argument rand0, Quotation rand1)
//        {
//            return
//                new Combination2LAQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LAQ.EvalStep");
//#endif
//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, 
//                evop, environment.ArgumentValue (this.rand0Offset), this.rand1Value);
//        }
//    }

//    class Combination2LL1 : Combination2LL
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2LL1 (LexicalVariable rator, LexicalVariable1 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, LexicalVariable1 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2LL1L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2LL1Q.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2LL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            rand1TypeHistogram.Note (this.rand1Type);
//            NoteCalls (this.rand1);
//            SCode.location = "Combination2LL1.EvalStep";
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2LL1L : Combination2LL1
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2LL1L (LexicalVariable rator, LexicalVariable1 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (LexicalVariable rator, LexicalVariable1 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2LL1A.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2LL1L1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2LL1L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LL1L.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2LL1A : Combination2LL1L
//    {
//        protected Combination2LL1A (LexicalVariable rator, LexicalVariable1 rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, LexicalVariable1 rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2LL1A0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2LL1A1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2LL1A (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//#if DEBUG
//            Warm ("Combination2LL.EvalStep");
//            NoteCalls (this.rand1);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2LL1A0 : Combination2LL1A
//    {
//        Combination2LL1A0 (LexicalVariable rator, LexicalVariable1 rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, LexicalVariable1 rand0, Argument0 rand1)
//        {
//            return
//                 new Combination2LL1A0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LL1A0.EvalStep");
//#endif
//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2LL1A1 : Combination2LL1A
//    {
//        Combination2LL1A1 (LexicalVariable rator, LexicalVariable1 rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, LexicalVariable1 rand0, Argument1 rand1)
//        {
//            return
//                 new Combination2LL1A1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LL1A1.EvalStep");
//#endif
//            object ev1 = environment.Argument1Value;

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2LL1L1 : Combination2LL1L
//    {
//        Combination2LL1L1 (LexicalVariable rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
//        {
//            if (rand0.Offset == rand1.Offset) Debugger.Break ();
//            return
//                new Combination2LL1L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LL1L1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2LL1Q : Combination2LL1
//    {
//        public readonly object rand1Value;

//        Combination2LL1Q (LexicalVariable rator, LexicalVariable1 rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (LexicalVariable rator, LexicalVariable1 rand0, Quotation rand1)
//        {
//            return
//                 new Combination2LL1Q (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LL1Q.EvalStep");
//#endif
//            object ev1 = this.rand1Value;

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2LLL : Combination2LL
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2LLL (LexicalVariable rator, LexicalVariable rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (LexicalVariable rator, LexicalVariable rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2LLA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2LLL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2LLL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LLL.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2LLA : Combination2LLL
//    {

//        protected Combination2LLA (LexicalVariable rator, LexicalVariable rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, LexicalVariable rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2LLA0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2LLA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2LLA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("Combination2LLL.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2LLA0 : Combination2LLA
//    {

//        Combination2LLA0 (LexicalVariable rator, LexicalVariable rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, LexicalVariable rand0, Argument0 rand1)
//        {
//            return
//                new Combination2LLA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("Combination2LLL.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2LLA1 : Combination2LLA
//    {
//        Combination2LLA1 (LexicalVariable rator, LexicalVariable rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, LexicalVariable rand0, Argument1 rand1)
//        {
//            return
//                new Combination2LLA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("Combination2LLL.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2LLL1 : Combination2LLL
//    {
//        Combination2LLL1 (LexicalVariable rator, LexicalVariable rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, LexicalVariable rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2LLL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LLL1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2LLQ : Combination2LL
//    {
//        public readonly object rand1Value;

//        Combination2LLQ (LexicalVariable rator, LexicalVariable rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (LexicalVariable rator, LexicalVariable rand0, Quotation rand1)
//        {
//            return new Combination2LLQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LLQ.EvalStep");
//#endif
//            object ev1 = this.rand1Value;

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2LQ : Combination2L
//    {
//        public readonly object rand0Value;

//        protected Combination2LQ (LexicalVariable rator, Quotation rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand0Value = rand0.Quoted;
//        }

//        public static SCode Make (LexicalVariable rator, Quotation rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2LQL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2LQQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2LQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LQ.EvalStep");
//            NoteCalls (this.rand1);
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object ev0 = this.rand0Value;

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2LQL : Combination2LQ
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2LQL (LexicalVariable rator, Quotation rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (LexicalVariable rator, Quotation rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2LQA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2LQL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2LQL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LQL.EvalStep");
//#endif

//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = this.rand0Value;

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2LQA : Combination2LQL
//    {
//        protected Combination2LQA (LexicalVariable rator, Quotation rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Quotation rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2LQA0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2LQA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2LQA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LQA.EvalStep");
//#endif
//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, environment.ArgumentValue (this.rand1Offset));
//        }
//    }

//    sealed class Combination2LQA0 : Combination2LQA
//    {
//        Combination2LQA0 (LexicalVariable rator, Quotation rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Quotation rand0, Argument0 rand1)
//        {
//            return
//                new Combination2LQA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LQA0.EvalStep");
//#endif
//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                evop, this.rand0Value, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2LQA1 : Combination2LQA
//    {
//        Combination2LQA1 (LexicalVariable rator, Quotation rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Quotation rand0, Argument1 rand1)
//        {
//            return
//                new Combination2LQA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LQA1.EvalStep");
//#endif
//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2LQL1 : Combination2LQL
//    {
//        Combination2LQL1 (LexicalVariable rator, Quotation rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, Quotation rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2LQL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LQL1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
//        }
//    }

//    sealed class Combination2LQQ : Combination2LQ
//    {
//        public readonly object rand1Value;

//        Combination2LQQ (LexicalVariable rator, Quotation rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (LexicalVariable rator, Quotation rand0, Quotation rand1)
//        {
//            return new Combination2LQQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LQQ.EvalStep");
//#endif
//            object ev1 = this.rand1Value;

//            object ev0 = this.rand0Value;

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2LSL : Combination2L
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2LSL (LexicalVariable rator, SCode rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (LexicalVariable rator, SCode rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2LSA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2LSL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2LSL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LSL.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2LSA : Combination2LSL
//    {
//        protected Combination2LSA (LexicalVariable rator, SCode rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, SCode rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2LSA0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2LSA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2LSA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LSA.EvalStep");
//#endif
//            object ev1 = environment.ArgumentValue (this.rand1Offset);

//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2LSA0 : Combination2LSA
//    {
//        Combination2LSA0 (LexicalVariable rator, SCode rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, SCode rand0, Argument0 rand1)
//        {
//            return
//                new Combination2LSA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LSA0.EvalStep");
//#endif
//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2LSA1 : Combination2LSA
//    {
//        Combination2LSA1 (LexicalVariable rator, SCode rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, SCode rand0, Argument1 rand1)
//        {
//            return
//                new Combination2LSA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LSA1.EvalStep");
//#endif
//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2LSL1 : Combination2LSL
//    {
//        Combination2LSL1 (LexicalVariable rator, SCode rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable rator, SCode rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2LSL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LSLL1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2LSQ : Combination2L
//    {
//        public readonly object rand1Value;

//        Combination2LSQ (LexicalVariable rator, SCode rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (LexicalVariable rator, SCode rand0, Quotation rand1)
//        {
//            return new Combination2LSQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2LSQ.EvalStep");
//            NoteCalls (this.rand0);
//#endif
//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
//        }
//    }
 
//    class Combination2T : Combination2
//    {
//        public readonly TopLevelVariable ratorVar;
//        protected Combination2T (TopLevelVariable rator, SCode rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.ratorVar = rator;
//        }

//        public static SCode Make (TopLevelVariable rator, SCode rand0, SCode rand1)
//        {
//            return
//                (rand0 is LexicalVariable) ? Combination2TL.Make (rator, (LexicalVariable) rand0, rand1) :
//                (rand0 is Quotation) ? Combination2TQ.Make (rator, (Quotation) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Combination2TSL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2TSQ.Make (rator, rand0, (Quotation) rand1) :
//                 new Combination2T (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2T.EvalStep");
//            NoteCalls (this.rand0);
//            NoteCalls (this.rand1);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object ev0;
//            env = environment;
//            unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2TL : Combination2T
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand0Name;
//        public readonly int rand0Depth;
//        public readonly int rand0Offset;

//        protected Combination2TL (TopLevelVariable rator, LexicalVariable rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand0Name = rand0.Name;
//            this.rand0Depth = rand0.Depth;
//            this.rand0Offset = rand0.Offset;
//        }

//        public static SCode Make (TopLevelVariable rator, LexicalVariable rand0, SCode rand1)
//        {
//            return
//                (rand0 is Argument) ? Combination2TA.Make (rator, (Argument) rand0, rand1) :
//                (rand0 is LexicalVariable1) ? Combination2TL1.Make (rator, (LexicalVariable1) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Combination2TLL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2TLQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2TL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TL.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new Combination2TLFrame0 (this, environment));
//                environment = env;
//                answer = Interpreter.Unwind;
//                return false;
//            }

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2TLFrame0 : SubproblemContinuation<Combination2TL>, ISystemVector
//    {

//        public Combination2TLFrame0 (Combination2TL combination2, Environment environment)
//            : base (combination2, environment)
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

//        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev1)
//        {
//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.expression.rand0Name, this.expression.rand0Depth, this.expression.rand0Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.expression.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2TA : Combination2TL
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2TA (TopLevelVariable rator, Argument rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument rand0, SCode rand1)
//        {
//            return
//                (rand0 is Argument0) ? Combination2TA0.Make (rator, (Argument0) rand0, rand1) :
//                (rand0 is Argument1) ? Combination2TA1.Make (rator, (Argument1) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Combination2TAL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2TAQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2TA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TA.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new Combination2TAFrame0 (this, environment));
//                environment = env;
//                answer = Interpreter.Unwind;
//                return false;
//            }

//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2TAFrame0 : SubproblemContinuation<Combination2TA>, ISystemVector
//    {
//        public Combination2TAFrame0 (Combination2TA combination2, Environment environment)
//            : base (combination2, environment)
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

//        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev1)
//        {
//            // Evaluate operator
//            object evop;
//            if (this.expression.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue (this.expression.rand0Offset), ev1);
//        }
//    }

//    class Combination2TA0 : Combination2TA
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2TA0 (TopLevelVariable rator, Argument0 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument0 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2TA0L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2TA0Q.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2TA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TA0.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new Combination2TA0Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.Unwind;
//                return false;
//            }

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, ev1);
//        }
//    }

//    sealed class Combination2TA0Frame0 : SubproblemContinuation<Combination2TA0>, ISystemVector
//    {
//        public Combination2TA0Frame0 (Combination2TA0 combination2, Environment environment)
//            : base (combination2, environment)
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

//        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev1)
//        {
//            // Evaluate operator
//            object evop;
//            if (this.expression.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, ev1);
//        }
//    }

//    class Combination2TA0L : Combination2TA0
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2TA0L (TopLevelVariable rator, Argument0 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (TopLevelVariable rator, Argument0 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2TA0A.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2TA0L1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2TA0L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TA0L.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = environment.Argument0Value;

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2TA0A : Combination2TA0L
//    {
//        protected Combination2TA0A (TopLevelVariable rator, Argument0 rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument0 rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2TA0A0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2TA0A1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2TA0A (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TA0A.EvalStep");
//#endif

//            object ev1 = environment.ArgumentValue (this.rand1Offset);

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, ev1);
//        }
//    }

//    sealed class Combination2TA0A0 : Combination2TA0A
//    {
//        Combination2TA0A0 (TopLevelVariable rator, Argument0 rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument0 rand0, Argument0 rand1)
//        {
//            return
//                new Combination2TA0A0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TA0A0.EvalStep");
//#endif
//            object ev0 = environment.Argument0Value;

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev0);
//        }
//    }

//    sealed class Combination2TA0A1 : Combination2TA0A
//    {
//        Combination2TA0A1 (TopLevelVariable rator, Argument0 rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument0 rand0, Argument1 rand1)
//        {
//            return
//                new Combination2TA0A1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            SCode.location = "Combination2TA0A1.EvalStep";
//#endif
//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2TA0L1 : Combination2TA0L
//    {
//        Combination2TA0L1 (TopLevelVariable rator, Argument0 rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument0 rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2TA0L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TA0L1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = environment.Argument0Value;

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2TA0Q : Combination2TA0
//    {
//        public readonly object rand1Value;

//        Combination2TA0Q (TopLevelVariable rator, Argument0 rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (TopLevelVariable rator, Argument0 rand0, Quotation rand1)
//        {
//            return
//                new Combination2TA0Q (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TA0Q.EvalStep");
//#endif
//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, this.rand1Value);
//        }
//    }

//    class Combination2TA1 : Combination2TA
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2TA1 (TopLevelVariable rator, Argument1 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument1 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2TA1L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2TA1Q.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2TA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TA1.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0 = environment.Argument1Value;

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2TA1L : Combination2TA1
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2TA1L (TopLevelVariable rator, Argument1 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (TopLevelVariable rator, Argument1 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2TA1A.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2TA1L1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2TA1L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TA1L.EvalStep");
//#endif

//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = environment.Argument1Value;

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value, ev1);
//        }
//    }

//    class Combination2TA1A : Combination2TA1L
//    {
//        protected Combination2TA1A (TopLevelVariable rator, Argument1 rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument1 rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2TA1A0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2TA1A1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2TA1A (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TA1A.EvalStep");
//#endif

//            object ev1 = environment.ArgumentValue (this.rand1Offset);

//            object ev0 = environment.Argument1Value;

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2TA1A0 : Combination2TA1A
//    {
//        Combination2TA1A0 (TopLevelVariable rator, Argument1 rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument1 rand0, Argument0 rand1)
//        {
//            return
//                new Combination2TA1A0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TA1A0.EvalStep");
//#endif
//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2TA1A1 : Combination2TA1A
//    {
//        Combination2TA1A1 (TopLevelVariable rator, Argument1 rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument1 rand0, Argument1 rand1)
//        {
//            return
//                new Combination2TA1A1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TA1A1.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev0);
//        }
//    }

//    sealed class Combination2TA1L1 : Combination2TA1L
//    {
//        Combination2TA1L1 (TopLevelVariable rator, Argument1 rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument1 rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2TA1L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TA1L1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value, ev1);
//        }
//    }

//    sealed class Combination2TA1Q : Combination2TA1
//    {
//        public readonly object rand1Value;

//        Combination2TA1Q (TopLevelVariable rator, Argument1 rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (TopLevelVariable rator, Argument1 rand0, Quotation rand1)
//        {
//            return
//                new Combination2TA1Q (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TA1Q.EvalStep");
//#endif
//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value, this.rand1Value);
//        }
//    }

//    class Combination2TAL : Combination2TA
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2TAL (TopLevelVariable rator, Argument rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (TopLevelVariable rator, Argument rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2TAA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2TAL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2TAL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TAL.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2TAA : Combination2TAL
//    {
//#if DEBUG
//        static Histogram<Type> procedureTypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2TAA (TopLevelVariable rator, Argument rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2TAA0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2TAA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2TAA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            SCode.location = "Combination2TAA.EvalStep";
//#endif
//            object ev1 = environment.ArgumentValue (this.rand1Offset);

//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2TAA0 : Combination2TAA
//    {
//#if DEBUG
//        static Histogram<Type> procedureTypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2TAA0 (TopLevelVariable rator, Argument rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument rand0, Argument0 rand1)
//        {
//            return
//                new Combination2TAA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            SCode.location = "Combination2TAA0.EvalStep";
//#endif
//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment,
//                evop, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value);
//        }
//    }

//    sealed class Combination2TAA1 : Combination2TAA
//    {
//#if DEBUG
//        static Histogram<Type> procedureTypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2TAA1 (TopLevelVariable rator, Argument rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument rand0, Argument1 rand1)
//        {
//            return
//                new Combination2TAA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TAA1.EvalStep");
//#endif
//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2TAL1 : Combination2TAL
//    {
//        Combination2TAL1 (TopLevelVariable rator, Argument rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2TAL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TAL1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2TAQ : Combination2TA
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Value;

//        Combination2TAQ (TopLevelVariable rator, Argument rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (TopLevelVariable rator, Argument rand0, Quotation rand1)
//        {
//            return
//                new Combination2TAQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TAQ.EvalStep");
//#endif
//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
//        }
//    }

//    class Combination2TL1 : Combination2TL
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2TL1 (TopLevelVariable rator, LexicalVariable1 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, LexicalVariable1 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2TL1L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2TL1Q.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2TL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TL1.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2TL1L : Combination2TL1
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2TL1L (TopLevelVariable rator, LexicalVariable1 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (TopLevelVariable rator, LexicalVariable1 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2TL1A.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2TL1L1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2TL1L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TL1.EvalStep");
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2TL1A : Combination2TL1L
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination2TL1A (TopLevelVariable rator, LexicalVariable1 rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, LexicalVariable1 rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2TL1A0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2TL1A1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2TL1A (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TL1A.EvalStep");
//#endif
//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.ArgumentValue (this.rand1Offset));
//        }
//    }

//    sealed class Combination2TL1A0 : Combination2TL1A
//    {
//        Combination2TL1A0 (TopLevelVariable rator, LexicalVariable1 rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, LexicalVariable1 rand0, Argument0 rand1)
//        {
//            return
//                new Combination2TL1A0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TL1A0.EvalStep");
//#endif
//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2TL1A1 : Combination2TL1A
//    {
//        Combination2TL1A1 (TopLevelVariable rator, LexicalVariable1 rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, LexicalVariable1 rand0, Argument1 rand1)
//        {
//            return
//                new Combination2TL1A1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TL1A1.EvalStep");
//#endif
//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2TL1L1 : Combination2TL1L
//    {
//#if DEBUG
//        static Histogram<Type> procedureTypeHistogram = new Histogram<Type> ();
//#endif

//        Combination2TL1L1 (TopLevelVariable rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
//        {
//            return new Combination2TL1L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TL1L1.EvalStep");
//#endif

//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }

//    }

//    sealed class Combination2TL1Q : Combination2TL1
//    {
//        public readonly object rand1Value;

//        Combination2TL1Q (TopLevelVariable rator, LexicalVariable1 rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (TopLevelVariable rator, LexicalVariable1 rand0, Quotation rand1)
//        {
//            return
//                new Combination2TL1Q (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TL1Q.EvalStep");
//#endif
//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
//        }
//    }

//    class Combination2TLL : Combination2TL
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2TLL (TopLevelVariable rator, LexicalVariable rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {

//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (TopLevelVariable rator, LexicalVariable rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2TLA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2TLL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2TLL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            SCode.location = "Combination2TLL.EvalStep";
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2TLA : Combination2TLL
//    {
//        protected Combination2TLA (TopLevelVariable rator, LexicalVariable rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, LexicalVariable rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2TLA0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2TLA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2TLA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            SCode.location = "Combination2TLA.EvalStep";
//#endif
//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.ArgumentValue (this.rand1Offset));
//        }
//    }

//    sealed class Combination2TLA0 : Combination2TLA
//    {
//        Combination2TLA0 (TopLevelVariable rator, LexicalVariable rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, LexicalVariable rand0, Argument0 rand1)
//        {
//            return
//                new Combination2TLA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            SCode.location = "Combination2TLA0.EvalStep";
//#endif
//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2TLA1 : Combination2TLA
//    {
//        Combination2TLA1 (TopLevelVariable rator, LexicalVariable rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, LexicalVariable rand0, Argument1 rand1)
//        {
//            return
//                new Combination2TLA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            SCode.location = "Combination2TLA1.EvalStep";
//#endif
//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2TLL1 : Combination2TLL
//    {
//        Combination2TLL1 (TopLevelVariable rator, LexicalVariable rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, LexicalVariable rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2TLL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            SCode.location = "Combination2TLL1.EvalStep";
//#endif

//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2TLQ : Combination2TL
//    {
//        public readonly object rand1Value;

//        Combination2TLQ (TopLevelVariable rator, LexicalVariable rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (TopLevelVariable rator, LexicalVariable rand0, Quotation rand1)
//        {
//            return new Combination2TLQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TLQ.EvalStep");
//#endif
//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
//        }
//    }

//    class Combination2TQ : Combination2T
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand0Value;

//        protected Combination2TQ (TopLevelVariable rator, Quotation rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand0Value = rand0.Quoted;
//        }

//        public static SCode Make (TopLevelVariable rator, Quotation rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2TQL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2TQQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2TQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TQ.EvalStep");
//            NoteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
//        }
//    }

//    class Combination2TQL : Combination2TQ
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2TQL (TopLevelVariable rator, Quotation rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (TopLevelVariable rator, Quotation rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2TQA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2TQL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2TQL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TQL.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
//        }
//    }

//    class Combination2TQA : Combination2TQL
//    {
//        protected Combination2TQA (TopLevelVariable rator, Quotation rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Quotation rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2TQA0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2TQA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2TQA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TQA.EvalStep");
//#endif
//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, environment.ArgumentValue (this.rand1Offset));
//        }
//    }

//    sealed class Combination2TQA0 : Combination2TQA
//    {
//        Combination2TQA0 (TopLevelVariable rator, Quotation rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Quotation rand0, Argument0 rand1)
//        {
//            return
//                new Combination2TQA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TQA0.EvalStep");
//#endif
//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2TQA1 : Combination2TQA
//    {
//        Combination2TQA1 (TopLevelVariable rator, Quotation rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Quotation rand0, Argument1 rand1)
//        {
//            return
//                new Combination2TQA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            SCode.location = "Combination2TQA1.EvalStep";
//#endif
//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2TQL1 : Combination2TQL
//    {
//        Combination2TQL1 (TopLevelVariable rator, Quotation rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Quotation rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2TQL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {

//#if DEBUG
//            Warm ("Combination2TQL1.EvalStep");
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
//        }
//    }

//    sealed class Combination2TQQ : Combination2TQ
//    {
//        public readonly object rand1Value;

//        Combination2TQQ (TopLevelVariable rator, Quotation rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (TopLevelVariable rator, Quotation rand0, Quotation rand1)
//        {
//            return new Combination2TQQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TQQ.EvalStep");
//#endif
//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, this.rand1Value);
//        }
//    }

//    class Combination2TSL : Combination2T
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2TSL (TopLevelVariable rator, SCode rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (TopLevelVariable rator, SCode rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2TSA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2TSL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2TSL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2TSL.EvalStep");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2TSA : Combination2TSL
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2TSA (TopLevelVariable rator, SCode rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, SCode rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2TSA0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2TSA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2TSA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//            SCode.location = "Combination2TSA.EvalStep";
//#endif
//            object ev1 = environment.ArgumentValue (this.rand1Offset);

//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2TSA0 : Combination2TSA
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2TSA0 (TopLevelVariable rator, SCode rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, SCode rand0, Argument0 rand1)
//        {
//            return
//                new Combination2TSA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//            SCode.location = "Combination2TSA0.EvalStep";
//#endif
//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2TSA0.EvalStep.1";
//#endif
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2TSA1 : Combination2TSA
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2TSA1 (TopLevelVariable rator, SCode rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, SCode rand0, Argument1 rand1)
//        {
//            return
//                new Combination2TSA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//            SCode.location = "Combination2TSA1.EvalStep";
//#endif
//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2TSL1 : Combination2TSL
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2TSL1 (TopLevelVariable rator, SCode rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, SCode rand0, LexicalVariable1 rand1)
//        {
//            return new Combination2TSL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//            SCode.location = "Combination2TSL1.EvalStep";
//#endif

//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2TSQ : Combination2T
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Value;

//        Combination2TSQ (TopLevelVariable rator, SCode rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (TopLevelVariable rator, SCode rand0, Quotation rand1)
//        {
//            return new Combination2TSQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//            SCode.location = "Combination2TSQ.EvalStep";
//#endif
//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            // Evaluate operator
//            object evop;
//            if (this.ratorVar.cell.GetValue (out evop))
//                throw new NotImplementedException ();

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
//        }
//    }

//    class Combination2SL : Combination2
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand0Name;
//        public readonly int rand0Depth;
//        public readonly int rand0Offset;

//        protected Combination2SL (SCode rator, LexicalVariable rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand0Name = rand0.Name;
//            this.rand0Depth = rand0.Depth;
//            this.rand0Offset = rand0.Offset;
//        }

//        public static SCode Make (SCode rator, LexicalVariable rand0, SCode rand1)
//        {
//            return
//                (rand0 is Argument) ? Combination2SA.Make (rator, (Argument) rand0, rand1) :
//                (rand0 is LexicalVariable1) ? Combination2SL1.Make (rator, (LexicalVariable1) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Combination2SLL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2SLQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2SL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SL.EvalStep");
//            NoteCalls (this.rator);
//            NoteCalls (this.rand1);
//            ratorTypeHistogram.Note (this.ratorType);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new Combination2SLFrame0 (this, environment));
//                environment = env;
//                answer = Interpreter.Unwind;
//                return false;
//            }

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            env = environment;
//            unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2SLFrame0 : SubproblemContinuation<Combination2SL>, ISystemVector
//    {
//        public Combination2SLFrame0 (Combination2SL combination2, Environment environment)
//            : base (combination2, environment)
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

//        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev1)
//        {
//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.expression.rand0Name, this.expression.rand0Depth, this.expression.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            Environment env = environment;
//            Control unevop = this.expression.Operator;
//            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2SA : Combination2SL
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2SA (SCode rator, Argument rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Argument rand0, SCode rand1)
//        {
//            return
//                (rand0 is Argument0) ? Combination2SA0.Make (rator, (Argument0) rand0, rand1) :
//                (rand0 is Argument1) ? Combination2SA1.Make (rator, (Argument1) rand0, rand1) :
//                (rand1 is LexicalVariable) ? Combination2SAL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2SAQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2SA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SA.EvalStep");
//            NoteCalls (this.rator);
//            NoteCalls (this.rand1);
//            ratorTypeHistogram.Note (this.ratorType);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new Combination2SAFrame0 (this, environment));
//                environment = env;
//                answer = Interpreter.Unwind;
//                return false;
//            }

//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            object evop;
//            env = environment;
//            unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2SAFrame0 : SubproblemContinuation<Combination2SA>, ISystemVector
//    {
//        public Combination2SAFrame0 (Combination2SA combination2, Environment environment)
//            : base (combination2, environment)
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

//        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev1)
//        {
//            object evop;
//            Environment env = environment;
//            Control unevop = this.expression.Operator;
//            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue (this.expression.rand0Offset));
//        }
//    }

//    class Combination2SA0 : Combination2SA
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2SA0 (SCode rator, Argument0 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Argument0 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2SA0L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2SA0Q.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2SA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SA0.EvalStep");
//            NoteCalls (this.rator);
//            NoteCalls (this.rand1);
//            ratorTypeHistogram.Note (this.ratorType);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new Combination2SA0Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.Unwind;
//                return false;
//            }

//            object evop;
//            env = environment;
//            unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, ev1);
//        }
//    }

//    sealed class Combination2SA0Frame0 : SubproblemContinuation<Combination2SA0>, ISystemVector
//    {
//        public Combination2SA0Frame0 (Combination2SA0 combination2, Environment environment)
//            : base (combination2, environment)
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

//        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev1)
//        {
//            object evop;
//            Environment env = environment;
//            Control unevop = this.expression.Operator;
//            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, ev1);
//        }
//    }

//    class Combination2SA0L : Combination2SA0
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2SA0L (SCode rator, Argument0 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (SCode rator, Argument0 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2SA0A.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2SA0L1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2SA0L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SA0L.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = environment.Argument0Value;

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2SA0A : Combination2SA0L
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2SA0A (SCode rator, Argument0 rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Argument0 rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2SA0A0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2SA0A1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2SA0A (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SA0A.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif

//            object ev1 = environment.ArgumentValue (this.rand1Offset);

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, ev1);
//        }
//    }

//    sealed class Combination2SA0A0 : Combination2SA0A
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2SA0A0 (SCode rator, Argument0 rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Argument0 rand0, Argument0 rand1)
//        {
//            return
//                new Combination2SA0A0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SA0A0.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif
//            object ev0 = environment.Argument0Value;

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev0);
//        }
//    }

//    sealed class Combination2SA0A1 : Combination2SA0A
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2SA0A1 (SCode rator, Argument0 rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Argument0 rand0, Argument1 rand1)
//        {
//            return
//                new Combination2SA0A1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//            SCode.location = "Combination2SA0A1.EvalStep";
//#endif
//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2SA0L1 : Combination2SA0L
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2SA0L1 (SCode rator, Argument0 rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Argument0 rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2SA0L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SA0L1.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = environment.Argument0Value;

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2SA0Q : Combination2SA0
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Value;

//        Combination2SA0Q (SCode rator, Argument0 rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (SCode rator, Argument0 rand0, Quotation rand1)
//        {
//            return
//                new Combination2SA0Q (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SA0Q.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif
//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, this.rand1Value);
//        }
//    }

//    class Combination2SA1 : Combination2SA
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2SA1 (SCode rator, Argument1 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Argument1 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2SA1L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2SA1Q.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2SA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SA1.EvalStep");
//            NoteCalls (this.rator);
//            NoteCalls (this.rand1);
//            ratorTypeHistogram.Note (this.ratorType);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0 = environment.Argument1Value;

//            object evop;
//            env = environment;
//            unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2SA1L : Combination2SA1
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2SA1L (SCode rator, Argument1 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (SCode rator, Argument1 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2SA1A.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2SA1L1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2SA1L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SA1L.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif

//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = environment.Argument1Value;

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value, ev1);
//        }
//    }

//    class Combination2SA1A : Combination2SA1L
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination2SA1A (SCode rator, Argument1 rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Argument1 rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2SA1A0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2SA1A1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2SA1A (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SA1A.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif

//            object ev1 = environment.ArgumentValue (this.rand1Offset);

//            object ev0 = environment.Argument1Value;

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2SA1A0 : Combination2SA1A
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2SA1A0 (SCode rator, Argument1 rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Argument1 rand0, Argument0 rand1)
//        {
//            return
//                new Combination2SA1A0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SA1A0.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif
//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2SA1A1 : Combination2SA1A
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2SA1A1 (SCode rator, Argument1 rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Argument1 rand0, Argument1 rand1)
//        {
//            return
//                new Combination2SA1A1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SA1A1.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif
//            object ev0 = environment.Argument1Value;

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev0);
//        }
//    }

//    sealed class Combination2SA1L1 : Combination2SA1L
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2SA1L1 (SCode rator, Argument1 rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Argument1 rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2SA1L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SA1L1.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value, ev1);
//        }
//    }

//    sealed class Combination2SA1Q : Combination2SA1
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Value;

//        Combination2SA1Q (SCode rator, Argument1 rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (SCode rator, Argument1 rand0, Quotation rand1)
//        {
//            return
//                new Combination2SA1Q (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SA1Q.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif
//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value, this.rand1Value);
//        }
//    }

//    class Combination2SAL : Combination2SA
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2SAL (SCode rator, Argument rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (SCode rator, Argument rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2SAA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2SAL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2SAL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SAL.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2SAA : Combination2SAL
//    {
//#if DEBUG
//        static Histogram<Type> procedureTypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2SAA (SCode rator, Argument rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Argument rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2SAA0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2SAA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2SAA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rator);
//            procedureTypeHistogram.Note (this.ratorType);
//            SCode.location = "Combination2SAA.EvalStep";
//#endif
//            object ev1 = environment.ArgumentValue (this.rand1Offset);

//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2SAA0 : Combination2SAA
//    {
//#if DEBUG
//        static Histogram<Type> procedureTypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2SAA0 (SCode rator, Argument rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Argument rand0, Argument0 rand1)
//        {
//            return
//                new Combination2SAA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rator);
//            procedureTypeHistogram.Note (this.ratorType);
//            SCode.location = "Combination2SAA0.EvalStep";
//#endif
//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }
//            return Interpreter.Call (out answer, ref expression, ref environment,
//                evop, environment.ArgumentValue (this.rand0Offset), environment.Argument0Value);
//        }
//    }

//    sealed class Combination2SAA1 : Combination2SAA
//    {
//#if DEBUG
//        static Histogram<Type> procedureTypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2SAA1 (SCode rator, Argument rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Argument rand0, Argument1 rand1)
//        {
//            return
//                new Combination2SAA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SAA1.EvalStep");
//            NoteCalls (this.rator);
//            procedureTypeHistogram.Note (this.ratorType);
//#endif
//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2SAL1 : Combination2SAL
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2SAL1 (SCode rator, Argument rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Argument rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2SAL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SAL1.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2SAQ : Combination2SA
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Value;

//        Combination2SAQ (SCode rator, Argument rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (SCode rator, Argument rand0, Quotation rand1)
//        {
//            return
//                new Combination2SAQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SAQ.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif
//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
//        }
//    }

//    class Combination2SL1 : Combination2SL
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2SL1 (SCode rator, LexicalVariable1 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, LexicalVariable1 rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2SL1L.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2SL1Q.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2SL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SL1.EvalStep");
//            NoteCalls (this.rator);
//            NoteCalls (this.rand1);
//            ratorTypeHistogram.Note (this.ratorType);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            env = environment;
//            unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2SL1L : Combination2SL1
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2SL1L (SCode rator, LexicalVariable1 rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (SCode rator, LexicalVariable1 rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2SL1A.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2SL1L1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2SL1L (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//            SCode.location = "Combination2SL1L.EvalStep";
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            env = environment;
//            unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2SL1A : Combination2SL1L
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination2SL1A (SCode rator, LexicalVariable1 rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, LexicalVariable1 rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2SL1A0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2SL1A1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2SL1A (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SL1A.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif
//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.ArgumentValue (this.rand1Offset));
//        }
//    }

//    sealed class Combination2SL1A0 : Combination2SL1A
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2SL1A0 (SCode rator, LexicalVariable1 rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, LexicalVariable1 rand0, Argument0 rand1)
//        {
//            return
//                new Combination2SL1A0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//            SCode.location = "Combination2SL1A0.EvalStep";
//#endif
//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2SL1A1 : Combination2SL1A
//    {
//        Combination2SL1A1 (SCode rator, LexicalVariable1 rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, LexicalVariable1 rand0, Argument1 rand1)
//        {
//            return
//                new Combination2SL1A1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SL1A1.EvalStep");
//#endif
//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2SL1L1 : Combination2SL1L
//    {
//#if DEBUG
//        static Histogram<Type> procedureTypeHistogram = new Histogram<Type> ();
//#endif

//        Combination2SL1L1 (SCode rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
//        {
//            return new Combination2SL1L1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SL1L1.EvalStep");
//            NoteCalls (this.rator);
//            procedureTypeHistogram.Note (this.ratorType);
//#endif

//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
//                //environment = env;
//                //answer = Interpreter.Unwind;
//                //return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }

//    }

//    sealed class Combination2SL1Q : Combination2SL1
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Value;

//        Combination2SL1Q (SCode rator, LexicalVariable1 rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (SCode rator, LexicalVariable1 rand0, Quotation rand1)
//        {
//            return
//                new Combination2SL1Q (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SL1Q.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif
//            object ev0;
//            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
//        }
//    }

//    class Combination2SLL : Combination2SL
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2SLL (SCode rator, LexicalVariable rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {

//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (SCode rator, LexicalVariable rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2SLA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2SLL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2SLL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//            SCode.location = "Combination2SLL.EvalStep";
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2SLL.EvalStep";
//#endif
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    class Combination2SLA : Combination2SLL
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination2SLA (SCode rator, LexicalVariable rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, LexicalVariable rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2SLA0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2SLA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2SLA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//            SCode.location = "Combination2SLA.EvalStep";
//#endif
//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2SLA.EvalStep";
//#endif
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.ArgumentValue (this.rand1Offset));
//        }
//    }

//    sealed class Combination2SLA0 : Combination2SLA
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif

//        Combination2SLA0 (SCode rator, LexicalVariable rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, LexicalVariable rand0, Argument0 rand1)
//        {
//            return
//                new Combination2SLA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//            SCode.location = "Combination2SLA0.EvalStep";
//#endif
//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2SLA0.EvalStep";
//#endif
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2SLA1 : Combination2SLA
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif

//        Combination2SLA1 (SCode rator, LexicalVariable rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, LexicalVariable rand0, Argument1 rand1)
//        {
//            return
//                new Combination2SLA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//            SCode.location = "Combination2SLA1.EvalStep";
//#endif
//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2SLA1.EvalStep";
//#endif
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2SLL1 : Combination2SLL
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2SLL1 (SCode rator, LexicalVariable rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, LexicalVariable rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2SLL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//            SCode.location = "Combination2SLL1.EvalStep";
//#endif

//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination2SLL.EvalStep";
//#endif
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    sealed class Combination2SLQ : Combination2SL
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Value;

//        Combination2SLQ (SCode rator, LexicalVariable rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (SCode rator, LexicalVariable rand0, Quotation rand1)
//        {
//            return new Combination2SLQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SLQ.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif
//            object ev0;
//            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, this.rand1Value);
//        }
//    }

//    [Serializable]
//    class Combination2SQ : Combination2
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand0Value;

//        protected Combination2SQ (SCode rator, Quotation rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand0Value = rand0.Quoted;
//        }

//        public static SCode Make (SCode rator, Quotation rand0, SCode rand1)
//        {
//            return
//                (rand1 is LexicalVariable) ? Combination2SQL.Make (rator, rand0, (LexicalVariable) rand1) :
//                (rand1 is Quotation) ? Combination2SQQ.Make (rator, rand0, (Quotation) rand1) :
//                new Combination2SQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SQ.EvalStep");
//            NoteCalls (this.rator);
//            NoteCalls (this.rand1);
//            ratorTypeHistogram.Note (this.ratorType);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif
//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object evop;
//            env = environment;
//            unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
//        }
//    }

//    class Combination2SQL : Combination2SQ
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2SQL (SCode rator, Quotation rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (SCode rator, Quotation rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2SQA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2SQL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2SQL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SQL.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);

//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
//        }
//    }

//    class Combination2SQA : Combination2SQL
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2SQA (SCode rator, Quotation rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Quotation rand0, Argument rand1)
//        {
//            return
//                (rand1 is Argument0) ? Combination2SQA0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? Combination2SQA1.Make (rator, rand0, (Argument1) rand1) :
//                new Combination2SQA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SQA.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif
//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, environment.ArgumentValue (this.rand1Offset));
//        }
//    }

//    sealed class Combination2SQA0 : Combination2SQA
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2SQA0 (SCode rator, Quotation rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Quotation rand0, Argument0 rand1)
//        {
//            return
//                new Combination2SQA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//            SCode.location = "Combination2SQA0.EvalStep";
//#endif
//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, environment.Argument0Value);
//        }
//    }

//    sealed class Combination2SQA1 : Combination2SQA
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2SQA1 (SCode rator, Quotation rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Quotation rand0, Argument1 rand1)
//        {
//            return
//                new Combination2SQA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//            SCode.location = "Combination2SQA1.EvalStep";
//#endif
//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, environment.Argument1Value);
//        }
//    }

//    sealed class Combination2SQL1 : Combination2SQL
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//#endif
//        Combination2SQL1 (SCode rator, Quotation rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (SCode rator, Quotation rand0, LexicalVariable1 rand1)
//        {
//            return
//                new Combination2SQL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {

//#if DEBUG
//            Warm ("Combination2SQL1.EvalStep");
//            NoteCalls (this.rator);
//            ratorTypeHistogram.Note (this.ratorType);
//#endif
//            object ev1;
//            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1);
//        }
//    }

//    [Serializable]
//    sealed class Combination2SQQ : Combination2SQ
//    {
//        public readonly object rand1Value;

//        Combination2SQQ (SCode rator, Quotation rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (SCode rator, Quotation rand0, Quotation rand1)
//        {
//            return new Combination2SQQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SQQ.EvalStep");
//            NoteCalls (this.rator);
//#endif
//            object evop;
//            Environment env = environment;
//            Control unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, this.rand1Value);
//        }
//    }

//    class Combination2SSL : Combination2
//    {
//#if DEBUG
//        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected Combination2SSL (SCode rator, SCode rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (SCode rator, SCode rand0, LexicalVariable rand1)
//        {
//            return
//                (rand1 is Argument) ? Combination2SSA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? Combination2SSL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new Combination2SSL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination2SSL.EvalStep");
//            NoteCalls (this.rator);
//            NoteCalls (this.rand0);
//            ratorTypeHistogram.Note (this.ratorType);
//            rand0TypeHistogram.Note (this.rand0Type);
//#endif
//            object ev1;
//            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            object ev0;
//            Environment env = environment;
//            Control unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            object evop;
//            env = environment;
//            unev = this.rator;
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            if (evop == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

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
                (rand0 is Argument1) ? Combination2XA1.Make (rator, (Argument1) rand0, rand1) :
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
                (rand1 is Argument1) ? Combination2XAA1.Make (rator, rand0, (Argument1) rand1) :
                new Combination2XAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand0);
            ratorTypeHistogram.Note (this.ratorType);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2XAA.EvalStep";
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
    sealed class Combination2XAA1 : Combination2XAA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        Combination2XAA1 (SCode rator, Argument rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (SCode rator, Argument rand0, Argument1 rand1)
        {
            return
                new Combination2XAA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XAA1.EvalStep";
#endif
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue(this.rand0Offset), environment.Argument1Value);
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
                (rand1 is Argument1) ? Combination2XA0A1.Make (rator, rand0, (Argument1) rand1) :
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
    sealed class Combination2XA0A1 : Combination2XAA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        Combination2XA0A1 (SCode rator, Argument0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (SCode rator, Argument0 rand0, Argument1 rand1)
        {
            return
                new Combination2XA0A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XA0A1.EvalStep";
#endif
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, environment.Argument1Value);
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
    class Combination2XA1 : Combination2XA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2XA1 (SCode rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (SCode rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? Combination2XA1A.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? Combination2XA1S.Make (rator, rand0, (StaticVariable) rand1) :
                (rand1 is Quotation) ? Combination2XA1Q.Make (rator, rand0, (Quotation) rand1) :
                new Combination2XA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand1);
            ratorTypeHistogram.Note (this.ratorType);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Combination2XA1.EvalStep";
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value, ev1);
        }
    }

    [Serializable]
    class Combination2XA1A : Combination2XA
    {
        protected readonly int rand1Offset;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2XA1A (SCode rator, Argument1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (SCode rator, Argument1 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2XA1A0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? Combination2XA1A1.Make (rator, rand0, (Argument1) rand1) :
                new Combination2XA1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XA1A.EvalStep";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            object ev0 = environment.Argument1Value;

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
    sealed class Combination2XA1A0 : Combination2XAA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        Combination2XA1A0 (SCode rator, Argument1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (SCode rator, Argument1 rand0, Argument0 rand1)
        {
            return
                new Combination2XA1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XA1A0.EvalStep";
#endif
            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination2XA1A0.EvalStep";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.Unwind;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value, environment.Argument0Value);
        }
    }

    [Serializable]
    sealed class Combination2XA1A1 : Combination2XAA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        Combination2XA1A1 (SCode rator, Argument1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (SCode rator, Argument1 rand0, Argument1 rand1)
        {
            return
                new Combination2XA1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XA1A1.EvalStep";
#endif
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value, environment.Argument1Value);
        }
    }

    [Serializable]
    sealed class Combination2XA1S : Combination2XA
    {
        readonly object rand1Name;
        readonly int rand1Offset;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        Combination2XA1S (SCode rator, Argument1 rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (SCode rator, Argument1 rand0, StaticVariable rand1)
        {
            return
                new Combination2XA1S (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XA1S.EvalStep";
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value, ev1);
        }
    }

    [Serializable]
    sealed class Combination2XA1Q : Combination2XA
    {
        public readonly object rand1Value;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif

        Combination2XA1Q (SCode rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (SCode rator, Argument1 rand0, Quotation rand1)
        {
            return new Combination2XA1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XA1Q.EvalStep";
#endif

            object evop;
            Environment env = environment;
            Control unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value, this.rand1Value);
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
        protected readonly int rand1Offset;
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        protected Combination2XQA (SCode rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (SCode rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Combination2XQA0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? Combination2XQA1.Make (rator, rand0, (Argument1) rand1) :
                new Combination2XQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand0);
            ratorTypeHistogram.Note (this.ratorType);
            rand0TypeHistogram.Note (this.rand0Type);
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
    sealed class Combination2XQA1 : Combination2XQA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        Combination2XQA1 (SCode rator, Quotation rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (SCode rator, Quotation rand0, Argument1 rand1)
        {
            return
                new Combination2XQA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Combination2XQA1.EvalStep";
#endif
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, environment.Argument1Value);
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
                //(rand1 is Argument) ? Combination2XAA.Make (rator, rand0, (Argument) rand1) :
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
                (rand1 is Argument1) ? Combination2XXA1.Make (rator, rand0, (Argument1) rand1) :
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
    sealed class Combination2XXA1 : Combination2XXA
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        Combination2XXA1 (SCode rator, SCode rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (SCode rator, SCode rand0, Argument1 rand1)
        {
            return
                new Combination2XXA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand0);
            ratorTypeHistogram.Note (this.ratorType);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "Combination2XXA1.EvalStep";
#endif
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

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, environment.Argument1Value);
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
